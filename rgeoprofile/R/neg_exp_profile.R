## neg_exp_profile
## Jamie Spaulding

#' Negative Exponential Model for Geographic Profiling
#' @description An implementation of variations of the negative exponential
#'     decay model for serial crime analysis. In this model, the decline is at
#'     a constant rate, therefore the likelihood of the perpetrator's home base
#'     drops quickly from the incident locations until it approaches zero
#'     likelihood. The user can select different variants including the 'CrimeStat'
#'     base model, the 'Dragnet' model, or whether a buffer and plateau is present
#'     at the start of the decay function. This model assumes that the likelihood
#'     of the serial perpetrator's home base decreases in a exponential fashion
#'     as the distance increases from the crime incidents.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @param method 'CrimeStat', 'Dragnet', or a custom parameter based negative exponential
#'     decay function. If using the 'CrimeStat' or 'Dragnet' method, values do not
#'     need to be provided from 'a' and 'b' as the default parameters will be
#'     used. Default parameters for the 'CrimeStat' are: \eqn{a = 1.89} \eqn{a = -0.06}.
#'     Default parameters for the 'Dragnet' are: \eqn{a = b = 1}. If using a custom
#'     model, values must be provided for '*a*' and '*b*'.
#' @param buffer TRUE/FALSE. Whether a buffer zone where a likelihood of zero
#'     is fit around the incidents and a plateau of peak likelihood is fit prior
#'     to the negative exponential decay. The function calculates the buffer zone
#'     and the plateau area to each be half of the average nearest neighbor
#'     distance.
#' @param a the slope coefficient which defines the function decrease in distance
#' @param b exponential multiplier for the distance decay function
#' @return A data frame of points depicting a spatial grid of the hunting area
#'     for the given incident locations. Also given are the resultant summed
#'     values (score) for each map point. A higher resultant score indicates
#'     a greater the probability that point contains the offender's anchor point.
#' @param n total number of cells within the spatial grid for the jeopardy surface.
#'     If \code{NULL}, the default value for '*n*' is 40,000.
#' @author Jamie Spaulding, Keith Morris
#' @references Ned Levine, \emph{CrimeStat IV: A Spatial Statistics Program for the
#'     Analysis of Crime Incident Locations (version 4.0)}. Ned Levine & Associates,
#'     Houston, TX, and the National Institute of Justice, Washington, DC, June 2013.
#' @references D Canter, T Coffey, M Huntley & C Missen. (2000). \emph{Predicting
#'     serial killers' home base using a decision support system.} Journal of
#'     quantitative criminology, 16(4), 457-478.
#' @keywords spatial methods
#' @examples
#' \dontshow{
#' data(desalvo)
#' test <- neg_exp_profile(desalvo$lat, desalvo$lon, method = "CrimeStat", n = 4)
#' }
#' \donttest{
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' test <- neg_exp_profile(desalvo$lat, desalvo$lon, method = "CrimeStat")
#' g_map = sp::SpatialPixelsDataFrame(points = test[c("lons", "lats")], data = test)
#' g_map <- raster::raster(g_map)
#' # Assign a Coordinate Reference System for the Raster
#' raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' # Define a Parula Color Pallete for Resultant Jeopardy Surface
#' library(leaflet) #for mapping the geographic profile
#' pal <- colorNumeric(pals::parula(200), raster::values(g_map),
#'     na.color = "transparent")
#' leaflet() %>%
#'     addTiles() %>%
#'     addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
#'     addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon =
#'         awesomeIcons(icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
#'     addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
#'     addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
#'     addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
#'         fill = 'black', stroke = TRUE, fillOpacity = 0.75, weight = 2,
#'         fillColor = "red")
#' }
#' @importFrom geosphere distHaversine
#' @importFrom RANN nn2
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
neg_exp_profile <- function(lat, lon, method = c("CrimeStat", "Dragnet", "Custom"),
                            buffer = FALSE, a = NULL, b = NULL, n = NULL){
  # Set Defaults -----
  if (method == "Custom" & is.null(a)) {
    stop("If using a custom model, both 'a' and 'b' must be specified")
  }
  if (method == "Custom" & is.null(b)) {
    stop("If using a custom model, both 'a' and 'b' must be specified")
    }
  if (method == "CrimeStat") {
    a <- 1.89
    b <- -0.06
  }# Levine (2013)
  if (method == "Dragnet") {
    a <- 1
    b <- -1
  } #Canter et al. (2000)
  if (is.null(n)) {n <- 40000}

  # Computation of Map Boundaries/ Hunting Area -----
  lat_max <- max(lat) + ((max(lat) - min(lat)) / (2 * (length(lat) - 1)))
  lat_min <- min(lat) - ((max(lat) - min(lat)) / (2 * (length(lat) - 1)))
  lon_max <- max(lon) + ((max(lon) - min(lon)) / (2 * (length(lon) - 1)))
  lon_min <- min(lon) - ((max(lon) - min(lon)) / (2 * (length(lon) - 1)))

  # Calculate Range of Bounding Box -----
  lat_range <- lat_max - lat_min
  lon_range <- lon_max - lon_min

  # Determine Sequence of Lat and Lon Gridlines -----
  g_size <- sqrt(n)
  lats <- seq(lat_min,lat_max, length.out = g_size)
  lons <- seq(lon_min,lon_max, length.out = g_size)

  # Create a Run Sequence for Each Incident of Grid Points -----
  run_seq <- expand.grid(lats, lons)
  names(run_seq) <- c("lats", "lons")

  if (buffer == TRUE) {
    # Calculate Incident Buffer Zone -----
    dat_nn <- cbind(lat,lon) # Extract only lat and lon columns
    nn_list <- RANN::nn2(dat_nn, dat_nn, k=2) # Find NNs
    nn <- nn_list$nn.idx # Extract NN pairs

    # Calculate Distances Between NN Pairs -----
    nn_d <- NULL
    jj <- 1
    for(i in 1:nrow(nn)){
      incid1 <- dat_nn[nn[i,1],]
      incid2 <- dat_nn[nn[i,2],]
      nn_d[jj] <- geosphere::distHaversine(p1 = c(incid1[2], incid1[1]),
                                            p2 = c(incid2[2], incid2[1]),
                                            r = 3958) # hold y (lat) constant
      jj <- jj+1
    }
    plat_zone <- mean(nn_d) #Canter et al. (2000)
    buf_zone <- (mean(nn_d)) / 2 #Canter et al. (2000)

    jj <- 1
    output <- data.frame()
    # Progress Bar
    pb = utils::txtProgressBar(min = 0, max = length(lat) * n, style = 3)
    tick <- 0

    for(i in 1:length(lat)){
      for(j in 1:nrow(run_seq)){
        tick <- tick + 1
        utils::setTxtProgressBar(pb, tick)
        xn <- lon[i]
        yn <- lat[i]
        xi <- run_seq$lons[j]
        yi <- run_seq$lats[j]
        d <- geosphere::distHaversine(p1 = c(xn, yn),
                                      p2 = c(xi, yi),
                                      r = 3958)
        if(d < buf_zone) {out <- 0}
        if(d >= buf_zone & d < plat_zone) {out <- 1}
        if(d > plat_zone) {out <- (a * exp(b * (d - buf_zone)))}
        output[jj,i] <- out
        jj <- jj+1
      }
      jj <- 1
    }
    } else{
    jj <- 1
    output <- data.frame()
    pb = txtProgressBar(min = 0, max = length(lat) * n, style = 3)
    tick <- 0

    for(i in 1:length(lat)){
      for(j in 1:nrow(run_seq)){
        tick <- tick + 1
        utils::setTxtProgressBar(pb, tick)
        xn <- lon[i]
        yn <- lat[i]
        xi <- run_seq$lons[j]
        yi <- run_seq$lats[j]
        d <- geosphere::distHaversine(p1 = c(xn, yn),
                                      p2 = c(xi, yi),
                                      r = 3958)
        output[jj,i] <- a * exp(b * d)
        jj <- jj+1
      }
      jj <- 1
    }
    }

  # Summation of Values for Each Grid Point -----
  sums <- rowSums(output, na.rm = TRUE)
  dat <- cbind(sums, run_seq)
  return(dat)
}
