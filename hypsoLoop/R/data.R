#' Digital Elevation Model (DEM) of Yanze Watershed
#'
#' This data consists of a 30X30m-resolution digital elevation model (DEM) of Yanze, a small watershed located in north-west of Kigali City, Rwanda.
#' The spatial reference system  used by the coordinates of DEM is GCS_WGS_1984 with a D_WGS_1984 datum.
#' The cell values are expressed in Meters with a minimum 1370m  of and a maximum of 2241.
#'
#' @name Yanze_DEM
#' @keywords DEM
#' @docType data
#' @format An S4 object of class stars
#' @import stars
#' @details
#'     extent: 29.91865, 30.03865, -1.941896, -1.755507  (xmin, xmax, ymin, ymax)
#'     dimensions : 671, 432, 289872  (nrow, ncol, ncell)
#'
"DEM"

#' Yanze sub-catchments boundaries
#'
#' This data consists of the boundaries of sub-catchments of Yanze watershed, a small watershed located in north-west of Kigali City, Rwanda.
#' The spatial reference system  used by the coordinates of this SpatialPolygonsDataFrame is GCS_WGS_1984 with a D_WGS_1984 datum.
#'
#' @format An S4 object of class simple features
#' @name Yanze_sub-catchments
#' @docType data
#' @import sf
#' @details
#'     extent: 29.91865, 30.03858, -1.755426, -1.941896 (xmin, xmax, ymin, ymax)
#' \describe{
#'   \item{gridcode}{The id of each polygon}
#'   \item{gridcode}{The name of each polygon}
#' }
#'
"watersheds"

#' Land Use - Land Cover map of Yanze
#'
#' This data consists of a raster representing the landuse in Yanze watershed,
#' a small watershed located in north-west of Kigali City, Rwanda.
#' he spatial reference system  used by the coordinates of DEM is the WGS 84 UTM zone 35S - EPSG:32735.
#' Each land use/land cover class is represented by its code as follows:
#' 1 = Forest
#' 2 = Open Areas or Grass
#' 3 = Agriculture (Seasonal)
#' 5 = Settlements and Buildings
#' 6 = Water
#' 10 = Sparse Forest
#' 11 = Agriculture (Perennial)
#'
#' @format An S4 object of class stars
#' @name Yanze_LULC
#' @docType data
#' @import stars
#' @details
#'     extent: 824728, 838128, -214942.7, -194282.7 (xmin, xmax, ymin, ymax)
#'
"lulcYanze"

#' Yanze sub-catchments table
#'
#' This data consists of a table containing the attributes of sub-catchment of Yanze watersehd.
#' @format a data frame.
#' @name Yanze_sub-catchments-table
#' @docType data
#' @details
#'    Each rows represents a sub-catchment in yanze watershed, namely: Mulindi, cyonyonyo, and Yanze downstream.
#' \describe{
#'   \item{gridcode}{The name of each sub-catchment}
#' }
#'
"watersheds_df"
