#' MLMS Wells
#'
#' @description A spatial dataset describing the location
#'   of wells equipped with a Multilevel Monitoring System (MLMS)
#'   in the U.S. Geological Survey (USGS) aquifer-monitoring network,
#'   Idaho National Laboratory (INL), Idaho.

#'
#' @format A spatial feature data frame with the following variables:
#'   \describe{
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`coord_acy_va`}{Accuracy of latitude/longitude value, in seconds.}
#'     \item{`alt_va`}{Altitude of the land surface reference point (also known as the brass cap),
#'       in feet above the North American Vertical Datum of 1988 (NAVD 88).}
#'     \item{`alt_acy_va`}{Accuracy of the altitude value, in feet.}
#'     \item{`etape_cf_va`}{E-tape correction factor determined through calibration, in feet.}
#'     \item{`deviation_cf_va`}{Borehole deviation correction factor, in feet.}
#'     \item{`stickup_va`}{Stick-up distance,
#'       the height of the well casing that extends above the land surface measurement point, in feet.}
#'     \item{`mp_b_va`}{Distance between the top and bottom of an inflated packer seal, in feet.}
#'     \item{`mp_c_va`}{Distance between the transducer plane and
#'       the measurement port inlet valve, in feet.}
#'     \item{`hole_depth_va`}{Total depth to which the hole is drilled,
#'       measured as the distance below the land surface reference point,
#'       in feet below land surface (ft bls).}
#'     \item{`well_depth_va`}{Depth of the finished well, in ft bls.}
#'     \item{`construction_dt`}{Date the well was completed.}
#'     \item{`install_dt`}{Date the MLMS was installed.}
#'     \item{`system_tp`}{Multiport monitoring system type.
#'       Two versions of the Westbay System were utilized in this study: the "MP55" and the "MP38".}
#'     \item{`nzones`}{Number of depth-discrete monitoring zones.}
#'     \item{`nports`}{Number of measurement ports.}
#'     \item{`nvisits`}{Count of field visits for pressure profiling.}
#'     \item{`geometry`}{Zero-dimensional geometry containing a single point.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office
#'   and underwent processing using the [`read_mlms_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(wells)

"wells"
