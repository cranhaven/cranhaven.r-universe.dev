#' # package lidaRtRee
#' # Copyright INRAE
#' # Author(s): Jean-Matthieu Monnet
#' # Licence: LGPL-3
#' ###################### FUNCTIONS FOR TREE-LEVEL METRICS COMPUTATION #################
#' #
#' ##############################
#' #' segment-wise computation of point metrics
#' #'
#' #' @param p A data frame with point attributes, including the segment ID field
#' #' @param columnId A string: name of the field containing the segment ID
#' #' @param FUN Function to compute for each segment ID. Default: number of points
#' #' @return A vector of values obtainted by applying the function to the point cloud in each segment
#' #' @export
#' computeMetric <- function(p,columnId,FUN=nrow)
#' {
#'   by(p,as.factor(p[,columnId]),FUN)
#' }
#' ##############################
#' #' replace values in a raster based on segment ID and tree attributes
#' #'
#' #' @param r.dem.w a raster with segment ID, integers starting from 0 to max(r.dem.w)
#' #' @param segms A data frame with tree attributes
#' #' @param attr the attribute to use for rast
#' #' @return a raster
#' #' @export
#' rasterValueFromSegment <- function(r.dem.w, segms, attr)
#' {
#'   dummy <- r.dem.w
#'   # creer le tableau de correspondance id / attribut
#'   # tableau avec autant de lignes que de segments 'max + 1' en comptant le 0
#'   fh <- matrix(nrow=max(raster::values(r.dem.w))+1,ncol=1)
#'   # mettre la hauteur correspondant aux id
#'   fh[segms$id+1] <- segms[,attr]
#'   # convertir id en h dans segmentation
#'   raster::values(dummy) <- as.vector(fh[raster::values(dummy)+1])
#'   dummy
#' }
#' ###############################
#' #' add trunk altitude information
#' #'
#' #' @param segms a data.frame of segment id
#' #' @param dtm a raster with terrain altitude
#' #' @param p a point cloud data.frame
#' #' @return a list with two elements: the segments with additional attribute (terrain altitude in the cell below the maximum) and the point cloud with additional attribute (point height relatively to terrain altitude below maximum)
#' #' @export
#' pointHeightAboveTrunk <- function(segms, dtm, p)
#' {
#'   # extract terrain altitude
#'   segms$alt.dtm <- pointsInSegments(segms[,c("x","y")],dtm)
#'   dummy <- merge(p,segms[,c("id","alt.dtm")],by.x="seg.id",by.y="id",all.x=TRUE)
#'   dummy$h.trunk <- dummy$z-dummy$alt.dtm
#'   list(segms,dummy)
#' }