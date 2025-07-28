#' Field Visits
#'
#' @description This dataset provides detailed information about the context and specifics of field visits.
#'   It documents field visits to Multilevel Monitoring System (MLMS) wells in the U.S. Geological Survey (USGS)
#'   aquifer-monitoring network, Idaho National Laboratory (INL), Idaho.
#'   These visits involve measuring fluid pressure at various depths within an MLMS well,
#'   typically completed within a few hours.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`stime_dt`}{Start time for the field visit.}
#'     \item{`etime_dt`}{End time for the field visit.}
#'     \item{`baro_id`}{Identifier for the barometer used to measure atmospheric pressure.}
#'     \item{`baro_start_va`}{Atmospheric pressure measured at the beginning of the field visit,
#'       in pounds per square inch (psi).}
#'     \item{`baro_end_va`}{Atmospheric pressure measured at the visits end, in psi.}
#'     \item{`sensor_id`}{Identifier for the pressure sensor.}
#'     \item{`press_start_va`}{Absolute pressure of fluid measured at the beginning of the field visit, in psi.}
#'     \item{`press_end_va`}{Absolute pressure of fluid measured at the visits end, in psi.}
#'     \item{`temp_start_va`}{Temperature of fluid measured at the beginning of the field visit,
#'       in degree Celsius.}
#'     \item{`temp_end_va`}{Temperature of fluid measured at the visits end,
#'       in degree Celsius.}
#'     \item{`operators`}{Initials of field operators.}
#'     \item{`sheet_version_tx`}{Version of field sheet.}
#'     \item{`weather`}{Weather conditions during the field visit.}
#'     \item{`comment_tx`}{Comments.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office
#'   and underwent processing using the [`read_field_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(visits)

"visits"
