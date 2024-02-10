## cgt_profile
## Jamie Spaulding

#' Criminal Geographic Targeting Model for Geographic Profiling (Rossmo Formula)
#' @description An implementation of the criminal geographic targeting model
#'      for serial crime analysis developed by DK Rossmo. This function
#'      applies Rossmo's distance decay formula to a series of suspected crime
#'      incidents for geographic profiling and prediction of perpetrator home
#'      base.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @param buffer the radius for the buffer zone assumed by the distance decay
#'     model.
#' @param f decay formula coefficient which changes the steepness of the decay
#'     curve after the buffer radius. If \code{NULL}, the default value for '*f*'
#'     is 1.2 as recommended by Rossmo (1995)
#' @param g decay formula coefficient which changes the steepness of the decay
#'     curve before the buffer radius. If \code{NULL}, the default value for '*g*'
#'     is 1.2 as recommended by Rossmo (1995)
#' @param n total number of cells within the spatial grid for the jeopardy surface.
#'     If \code{NULL}, the default value for '*n*' is 40,000.
#' @return A data frame of points depicting a spatial grid of the hunting area
#'     for the given incident locations. Also given are the resultant summed
#'     values (score) for each map point. A higher resultant score indicates
#'     a greater the probability that point contains the offender's anchor point.
#' @author Jamie Spaulding, Keith Morris
#' @references DK Rossmo (2000). \emph{Geographic profiling. Boca Raton, FL: CRC Press.}
#' @references DK Rossmo (1995). \emph{Geographic profiling: Target patterns of serial
#'     murderers.} Diss. Theses (School of Criminology)/Simon Fraser University.
#' @keywords spatial methods
#' @examples
#' \dontshow{
#' data(desalvo)
#' test <- cgt_profile(desalvo$lat, desalvo$lon, n = 4)
#' }
#' \donttest{
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' test <- cgt_profile(desalvo$lat, desalvo$lon)
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
cgt_profile <- function(lat, lon, buffer = NULL, f = NULL, g = NULL, n = NULL){
  # Set Defaults -----
  if (is.null(buffer)) {
    # Calculate Incident Buffer Zone -----
    dat_nn <- cbind(lat, lon) # Extract only lat and lon columns
    nn_list <- RANN::nn2(dat_nn, dat_nn, k = 2) # Find NNs using Manhattan distance
    nn <- nn_list$nn.idx # Extract NN pairs

    # Calculate Manhattan Distances Between NN Pairs -----
    nn_md <- NULL
    jj <- 1
    for(i in 1:nrow(nn)){
      incid1 <- dat_nn[nn[i,1], ]
      incid2 <- dat_nn[nn[i,2], ]
      dx <- geosphere::distHaversine(p1 = c(incid1[2], incid1[1]),
                                     p2 = c(incid2[2], incid1[1]),
                                     r = 3958) # hold y (lat) constant
      dy <- geosphere::distHaversine(p1 = c(incid1[2], incid1[1]),
                                     p2 = c(incid1[2], incid2[1]),
                                     r = 3958) # hold x (lon) constant
      nn_md[jj] <- dx + dy
      jj <- jj+1
    }
    buffer <- (mean(nn_md)) / 2} #default: 1/2 Mean NN Manhattan Distance
  c <- length(lat)
  if (is.null(f)) {f <- 1.9} #default: Rossmo (1995)
  if (is.null(g)) {g <- 1.9} #default: Rossmo (1995)
  if (is.null(n)) {n <- 40000} #default: Rossmo (1995)

  # Computation of Map Boundaries/ Hunting Area -----
  # Rossmo (2000)
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

  # CGT Distance Decay Function -----
  jj <- 1
  phi <- NULL
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
      dx <- geosphere::distHaversine(p1 = c(xn, yi),
                                     p2 = c(xi, yi),
                                     r = 3958) #hold y (lat) constant
      dy <- geosphere::distHaversine(p1 = c(xi, yn),
                                     p2 = c(xi, yi),
                                     r = 3958) #hold y (lat) constant
      if(dx + dy > buffer){phi <- 1} else {phi <- 0}
      output[jj,i] <- (phi / ((dx + dy) ^ f)) +
        (((1 - phi) * (buffer ^ (g - f))) /
           (((2 * buffer) - (dx - dy)) ^ g)) #Rossmo Formula
      jj <- jj + 1
    }
    jj <- 1
  }

  ## Summation of Values for Each Grid Point
  sums <- rowSums(output, na.rm = TRUE)
  dat <- cbind(sums, run_seq)
  return(dat)
}

