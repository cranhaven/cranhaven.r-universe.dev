#' Sensor Calibrations
#'
#' @description Dataset of electronic sensor calibration measurements
#'   in the Multilevel Monitoring System (MLMS) wells of the
#'   U.S. Geological Survey (USGS) aquifer-monitoring network,
#'   Idaho National Laboratory (INL), Idaho.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`sensor_id`}{Identifier for the integrated sensor,
#'       see [`sensors`] dataset for a description of each sensor.}
#'     \item{`cal_dt`}{Calendar date of sensor calibration.}
#'     \item{`cal_tp`}{Calibration type.}
#'     \item{`ref_temp_va`}{Reference temperature, in degree Celsius.}
#'     \item{`ts_dt`}{Date.}
#'     \item{`lab_standard`}{Laboratory standard.}
#'     \item{`r2`}{R-squared, coefficient of determination.}
#'     \item{`p_value`}{p-value.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office
#'   and underwent processing using the [`read_sensors_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(calibrations)

"calibrations"
