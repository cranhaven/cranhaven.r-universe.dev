#' Create default study areas for use with `SpaDES` modules
#'
#' @param center `SpatialPoints` object specifying a set of coordinates and
#'               a projection. Default is an area in southern Alberta, Canada.
#'
#' @param size   Numeric specifying the approximate size of the area in m^2.
#'               Default `1e4`.
#'
#' @param seed   Numeric indicating the random seed to set internally
#'               (useful for ensuring the same study area is produced each time).
#'
#' @return `SpatVector`
#'
#' @export
#' @importFrom terra vect crs values<-
#'
#' @examples
#' a <- randomStudyArea(seed = 123)
#' if (interactive()) {
#'   terra::plot(a)
#' }
randomStudyArea <- function(center = NULL, size = 1e4, seed = NULL) {
  if (is.null(center)) {
    center <- vect(cbind(-1349980, 6986895))
    crs(center) <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  }

  if (!exists(".Random.seed", envir = .GlobalEnv)) set.seed(NULL)
  prevSeed <- get(".Random.seed", envir = .GlobalEnv)

  if (!is.null(seed))
    set.seed(seed)
  studyArea <- randomPolygon(x = center, area = size)
  if (!is.null(seed))
    set.seed(prevSeed)

  dfData <- if (is.null(rownames(studyArea))) {
    data.frame("field" = as.character(seq_along(length(studyArea))))
  } else {
    data.frame("field" = rownames(studyArea))
  }

  values(studyArea) <- dfData
  studyArea
}
