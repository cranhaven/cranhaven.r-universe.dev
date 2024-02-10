## lognorm_profile
## Jamie Spaulding

#' CrimeStat Lognormal Model for Geographic Profiling
#' @description An implementation of the lognormal decay model for serial crime
#'     analysis within 'CrimeStat'. This model is very similar to the normal model
#'     except with more skew to either side. If there is reason to believe that
#'     the perpetrator's residence is closer to the incidents, this function can
#'     take the form of a very rapid increase near incident with a gradual decline
#'     from the peak likelihood.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @param a coefficient for the normal decay function. If \code{NULL}, the default
#'     value for 'a' is 8.6 (Levine 2013)
#' @param d_mean mean distance. If \code{NULL}, the default value for 'd_mean' is
#'     4.2 (Levine 2013)
#' @param sd standard deviation of the distances. If \code{NULL}, the default
#'     value for 'sd' is 4.6 (Levine 2013)
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
#' test <- lognorm_profile(desalvo$lat, desalvo$lon, n = 4)
#' }
#' \donttest{
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' test <- lognorm_profile(desalvo$lat, desalvo$lon)
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
lognorm_profile <- function(lat, lon, a = NULL, d_mean = NULL, sd = NULL, n = NULL){
  # Set Defaults -----
  if (is.null(a)) {a <- 8.6} #default: Levine (2013)
  if (is.null(d_mean)) {d_mean <- 4.2} #default: Levine (2013)
  if (is.null(sd)) {sd <- 4.6} #default: Levine (2013)
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

  # Normal Distance Decay Function -----
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
      output[jj,i] <- a * (1 / ((d ^ 2) * sd * (sqrt(2 * pi)))) *
        exp(-1 * ((log((d ^ 2) - d_mean)) ^ 2) /
              (2 * (sd ^ 2))) #Levine (2013) Eqn: 13.18
      jj <- jj + 1
    }
    jj <- 1
  }

  # Summation of Values for Each Grid Point -----
  sums <- rowSums(output, na.rm = TRUE)
  dat <- cbind(sums, run_seq)
  return(dat)
}
