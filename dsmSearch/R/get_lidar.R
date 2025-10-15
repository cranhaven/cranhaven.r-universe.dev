#' get_lidar
#' @description Search for and download LiDAR data based on coordinates
#' of a spatial point with a given distance or a bounding box. The maximum distance is 1000m.
#' Different dataset could be found and the function automatically downloads
#' the latest dataset.
#' To get more details of data on a larger scale, please use viewscape::lidar_search.
#'
#' @param x numeric, indicating Longtitude degree of the center point.
#' @param y numeric, indicating latitude degree of the center point.
#' @param r numeric, indicating search distance for LiDAR data.
#' The maximum distance is 1000m (3281ft).
#' If r > 1000m, it will be reset to 1000m.
#' @param epsg numeric, the EPSG code specifying the coordinate reference system.
#' @param bbox vector, a bounding box defining the geographical area for downloading data.
#' @param max_return numeric, indicating the maximum of returns.
#' @param folder string (optional), indicating a path for downloading the LiDAR data
#'
#' @return lidR LAS object.
#'
#' @references Jean-Romain Roussel and David Auty (2022).
#' Airborne LiDAR Data Manipulation and Visualization for
#' Forestry Applications. R package version 4.0.1. https://cran.r-project.org/package=lidR
#'
#' @examples
#' \donttest{
#' las <- dsmSearch::get_lidar(x = -83.741289, y = 42.270146, r = 1000, epsg = 2253)
#' las <- dsmSearch::get_lidar(bbox = c(-83.742282,42.273389,-83.733442,42.278724), epsg = 2253)
#'
#' if (inherits(las, "LAS")) {
#'   can <- lidR::rasterize_canopy(las, 10, lidR::dsmtin())
#'   terra::plot(can)
#' } else {
#'   message("No LAS object returned. Skipping rasterization.")
#' }
#' }
#'
#' @seealso [lidar_search()]
#'
#' @importFrom dplyr "%>%"
#' @importFrom lidR readLAScatalog
#' @importFrom lidR clip_rectangle
#' @importFrom lidR writeLAS
#' @importFrom lidR plot
#' @importFrom sp SpatialPoints
#' @importFrom sp CRS
#' @importFrom sp spTransform
#'
#' @export

get_lidar <- function(x,
                      y,
                      r,
                      epsg,
                      bbox,
                      max_return=1000,
                      folder) {
  if (missing(epsg)) {
    stop("epsg is missing. Please set epsg code")
  }
  proj <- sp::CRS(paste0("+init=epsg:", epsg))
  longlat <- sp::CRS("+proj=longlat")
  # create bbox
  if (missing(bbox)) {
    if (missing(x) || missing(y) || missing(r)) {
      stop("please specify x, y, and r, or bbox")
    } else {
      # check searching distance
      unit <- sub(".no_defs", "", sub(".*=", "", proj@projargs))
      if (r > 1000 && unit == "m ") {
        r <- 1000
      } else if (r > 3281 && unit == "us-ft " ) {
        r <- 3281
      }
      bbox <- pt2bbox(x, y, r, proj, longlat)
    }
  } else {
    bbox <- convertBbox(bbox, proj, longlat)
  }
  original_timeout <- getOption('timeout')
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout=9999)
  result <- 0
  # get response using API
  tryCatch(
    result <- return_response(bbox[[1]], max_return),
    error = function(e) {
      return(e)
    }
  )
  if(is.numeric(result)){
    return('API error. Please try again later.')
  }
  # filter overlapping files
  lastYear <- max(result$startYear)
  result <- result[which(result$startYear == lastYear),]
  num <- length(result[,1])
  title <- result$titles
  download <- result$downloadLazURL
  # download data
  files <- c()
  destination <- ""
  # if (isTRUE(Sys.info()[1]=="Windows") == FALSE){
  #   m <- "curl"
  # }else if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
  #   m <- "auto"
  # }
  for (i in 1:num) {
    if (missing(folder)) {
      destination <- tempfile(fileext = ".laz")
    } else {
      destination <- paste0(folder, "/", title[i], ".laz")
    }
    isDone <- retry_download(download[i], destination)
    # try(download.file(download[i],
    #                   destination,
    #                   method = m,
    #                   quiet = TRUE))
    if (isDone == 1) {
      files <- c(files, destination)
    }
  }
  options(timeout=original_timeout)
  # clip and merge
  suppressWarnings(
    lasc <- lidR::readLAScatalog(files, progress = FALSE)
  )
  suppressWarnings(
    las <- lidR::clip_rectangle(lasc,
                                xleft = bbox[[2]][1],
                                xright = bbox[[2]][3],
                                ybottom = bbox[[2]][2],
                                ytop = bbox[[2]][4])
  )
  # save
  if (!missing(folder)) {
    lidR::writeLAS(las, paste0(folder, "/", Sys.time(), ".laz"))
  }
  rm(lasc)
  # delete other laz data
  unlink(files)
  return(las)
}
