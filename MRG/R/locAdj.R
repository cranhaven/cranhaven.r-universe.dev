#' Function that modifies the observation locations, to reduce the risk that 
#' they are on grid cell boundaries
#' 
#' @eval MRGparam("ifg")
#' @eval MRGparam("locAdj")
#' @eval MRGparam("ress") 
#' 
#' @details
#' This can be used as a pre-processing step before creating a multi-resolution grid.
#' The gridding procedure will have problems if the points are located exactly on grid cell
#' boundaries. The locations should therefore be slightly modified, to better control 
#' to which grid cells they are associated. This can either be a systematic modification,
#' or a random modification. 
#' 
#' In the case of FSS data, the coordinates have been reported as the lower left
#' corner of a 1 km grid.  
#' 
#' @returns An \code{\link[sf]{sf}}-object with slightly modified locations for the 
#' survey or census data, according to the \code{locAdj}-parameter  
#' 
#' @examples
#' data(ifs_dk)
#' 
#' ifg = fssgeo(ifs_dk, locAdj = FALSE)
#' ifg = locAdjFun(ifg, "LL")
#' 
#' @export
locAdjFun = function(ifg, locAdj, ress) {
  #' @importFrom sf st_jitter st_set_crs st_geometry st_crs st_set_geometry st_bbox
  if (missing(ress)) {
    bb = st_bbox(ifg)
    aa = (bb[3]-bb[1]) / sqrt(dim(ifg)[1]) / 100
  } else {
    aa = ress[1]/100
  }
  if (locAdj == "jitter") {
    ifg = st_jitter(ifg, amount = aa)
  } else {
    geom = st_geometry(ifg)
    if (locAdj == "LL") {
      geom = st_set_crs(geom + c(aa,aa), st_crs(ifg))
    }
    if (locAdj == "LR") {
      geom = st_set_crs(geom + c(-aa, aa), st_crs(ifg))
    }
    if (locAdj == "UL") {
      geom = st_set_crs(geom + c(aa, -aa), st_crs(ifg))
    }
    if (locAdj == "UR") {
      geom = st_set_crs(geom + c(-aa, -aa), st_crs(ifg))
    }
    ifg = st_set_geometry(ifg, geom)
  }
  ifg
}
