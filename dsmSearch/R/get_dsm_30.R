#' get_dsm_30
#' @description Search for and download Digital Surface Model
#' based on coordinates of a spatial point with a given distance or bounding box.
#' The resolution of AW3D30 (ALOS World 3D 30m)
#' and SRTMGL1 (SRTM GL1 30m) raster is 30 meter.
#' The raster resolutions of USGS datasets are 10m and 1m.
#' Forest canopy height maps (CHM) include the ETH Global Sentinel-2 10m Canopy Height (2020) and
#' the Meta High Resolution 1m Global Canopy Height Map
#'
#' @param x numeric, indicating Longtitude degree of the center point. (bbox is specified, this argument is ignored)
#' @param y numeric, indicating latitude degree of the center point. (bbox is specified, this argument is ignored)
#' @param r numeric, indicating search distance (meter or feet) for LiDAR data. (bbox is specified, this argument is ignored)
#' @param epsg numeric, the EPSG code specifying the coordinate reference system. (bbox is specified, this argument is ignored)
#' @param bbox vector, a bounding box defining the geographical area for downloading data.
#' @param datatype character, dataset names including "AW3D30", "SRTMGL1", "USGS1m", "USGS10m", "metaCHM", and "ethCHM".
#' @param key character, API key of OpenTopography. (not required for "metaCHM", and "ethCHM")
#'
#' @return SpatRaster raster
#'
#' @details To request an API key of OpenTopography,
#' [online registeration](https://portal.opentopography.org/login?redirect=%2FrequestService%3Fservice%3Dapi) is needed.
#'
#' To get CHMs with more customized arguments please use `fd_canopy_height` from the `forestdata` package.
#'
#' Data may be freely used for academic purposes, but be cited appropriately (see references below).
#'
#' @references
#' Japan Aerospace Exploration Agency (2021). ALOS World 3D 30 meter DEM. V3.2, Jan 2021.
#' Distributed by OpenTopography.
#' https://doi.org/10.5069/G94M92HB. Accessed: 2025-06-04
#'
#' United States Geological Survey (2021). United States Geological Survey 3D Elevation Program 1 meter Digital Elevation Model.
#' Distributed by OpenTopography. https://doi.org/10.5069/G9NP22NT. Accessed: 2025-06-04
#'
#' United States Geological Survey (2021). United States Geological Survey 3D
#' Elevation Program 1/3 arc-second Digital Elevation Model.
#' Distributed by OpenTopography. https://doi.org/10.5069/G98K778D. Accessed: 2025-06-04
#'
#' United States Geological Survey (2021). United States Geological Survey 3D Elevation Program 1 arc-second Digital Elevation Model.
#' Distributed by OpenTopography. https://doi.org/10.5069/G9HX19WN. Accessed: 2025-06-04
#'
#' Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk Wegner.
#' "A high-resolution canopy height model of the Earth." arXiv preprint arXiv:2204.08322 (2022).
#'
#' Tolan, J., Yang, H.I., Nosarzewski, B., Couairon, G., Vo, H.V., Brandt, J., Spore, J., Majumdar, S.
#' , Haziza, D., Vamaraju, J. and Moutakanni, T., 2024. Very high resolution canopy height maps from
#' RGB imagery using self-supervised vision transformer and convolutional decoder trained on aerial lidar.
#' Remote Sensing of Environment, 300, p.113888.
#'
#' @examples
#' \dontrun{
#' data <- dsmSearch::get_dsm_30(bbox = c(-83.783557,42.241833,-83.696525,42.310420),
#'                               key = "API key")
#' data <- dsmSearch::get_dsm_30(x = -83.741289,
#'                               y = 42.270146,
#'                               r = 1000,
#'                               epsg = 2253,
#'                               key = "API key")
#' }
#'
#' @importFrom terra rast
#' @importFrom terra as.matrix
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom nominatimlite bbox_to_poly
#' @importFrom forestdata fd_canopy_height
#' @importFrom httr2 resp_body_raw
#'
#' @export

get_dsm_30 <- function(x, y, r, epsg, bbox,
                       datatype='AW3D30',
                       key= "") {
  if (key == "" && datatype %in% c('AW3D30', "SRTMGL1", "USGS1m", "USGS10m")) {
    stop("key is missing.")
  }
  # create bbox
  if (missing(bbox)) {
    if (missing(epsg)) {
      stop("epsg is missing. Please set epsg code")
    }
    if (missing(x) || missing(y) || missing(r)) {
      stop("please specify x, y, and r, or bbox")
    } else {
      proj <- sp::CRS(paste0("+init=epsg:", epsg))
      longlat <- sp::CRS("+proj=longlat")
      bbox <- pt2bbox(x, y, r, proj, longlat)[[1]]
    }
  }
  # request data
  if (datatype != 'metaCHM' && datatype != 'ethCHM') {
    response <- return_response2(bbox, key, datatype)
    # Store the original 'timeout' option and ensure it's reset upon function exit
    original_timeout <- getOption('timeout')
    on.exit(options(timeout = original_timeout), add = TRUE)
    options(timeout=9999)
    # download data
    if (response$status_code == 200) {
      temp_file <- tempfile(fileext = ".tif")
      writeBin(httr2::resp_body_raw(response), temp_file)
      out <- terra::rast(temp_file)
      out_m <- terra::as.matrix(out, wide=TRUE)
      out_ext <- terra::ext(out)
      out_crs <- terra::crs(out)
      new_rast <- terra::rast(out_m, extent = out_ext, crs = out_crs)
      unlink(temp_file)
      return(new_rast)
    }
  } else if (datatype == 'metaCHM' || datatype == 'ethCHM') {
    model_ <- gsub('CHM', '', datatype)
    mask_ <- nominatimlite::bbox_to_poly(bbox=bbox)
    canopy <- forestdata::fd_canopy_height(x = mask_, model = model_,
                                 layer = "chm", crop = TRUE,
                                 merge = TRUE)
    return(canopy)
  }
}

