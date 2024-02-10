## trun_neg_exp_profile
## Jamie Spaulding

#' CrimeStat Truncated Negative Exponential Model for Geographic Profiling
#' @description An implementation of the truncated negative exponential decay
#'     model for serial crime analysis within 'CrimeStat'. This is a joint function
#'     composed of both the linear and the negative exponential. For distances
#'     proximal to the incidents, a positive linear function is defined from zero
#'     likelihood at distance zero to a location of peak likelihood. At the peak
#'     likelihood the function takes the form of a negative exponential, rapidly
#'     declining as distance increases.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @param dp radial distance for the peak likelihood (cutoff distance). If \code{NULL},
#'     the default value for 'dp' is 4.2 (Levine 2013)
#' @param peak_lh peak likelihood for the distance decay function. If \code{NULL},
#'     the default value for 'peak_lh' is 13.8 (Levine 2013)
#' @param c exponential constant for the negative exponential decay function. If
#'     \code{NULL}, the default value for 'c' is -0.06 (Levine 2013)
#' @param n total number of cells within the spatial grid for the jeopardy surface.
#'     If \code{NULL}, the default value for '*n*' is 40,000.
#' @return A data frame of points depicting a spatial grid of the hunting area
#'     for the given incident locations. Also given are the resultant summed
#'     values (score) for each map point. A higher resultant score indicates
#'     a greater the probability that point contains the offender's anchor point.
#' @author Jamie Spaulding, Keith Morris
#' @references Ned Levine, \emph{CrimeStat IV: A Spatial Statistics Program for the
#'     Analysis of Crime Incident Locations (version 4.0)}. Ned Levine & Associates,
#'     Houston, TX, and the National Institute of Justice, Washington, DC, June 2013.
#' @keywords spatial methods
#' @examples
#' \dontshow{
#' data(desalvo)
#' test <- trun_neg_exp_profile(desalvo$lat, desalvo$lon, n = 4)
#' }
#' \donttest{
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' test <- trun_neg_exp_profile(desalvo$lat, desalvo$lon)
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
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
trun_neg_exp_profile <- function(lat, lon, dp = NULL, peak_lh = NULL, c = NULL, n = NULL){
  # Set Defaults -----
  if (is.null(dp)) {dp <- 0.4} #default: Levine (2013)
  if (is.null(peak_lh)) {peak_lh <- 13.8} #default: Levine (2013)
  if (is.null(c)) {c <- -0.06} #default: Levine (2013)
  if (is.null(n)) {n <- 40000}
  B <- (peak_lh / dp) #slope of linear function (origin -> cutoff)

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

  # Linear Distance Decay Function -----
  jj <- 1
  output <- data.frame()
  # PROGRESS BAR FOR LOOP
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
      if(d <= dp){out <- (B * d)}
      if(d > dp){out <- (peak_lh * exp(c * (d - dp)))}
      output[jj,i] <- out
      jj <- jj + 1
    }
    jj <- 1
  }

  # Summation of Values for Each Grid Point -----
  sums <- rowSums(output, na.rm = TRUE)
  dat <- cbind(sums, run_seq)
  return(dat)
}
