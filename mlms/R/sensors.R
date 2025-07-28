#' Sensors
#'
#' @description Dataset of sensors used to make measurements at port couplings
#'   in the Multilevel Monitoring System (MLMS) wells of the
#'   U.S. Geological Survey (USGS) aquifer-monitoring network,
#'   Idaho National Laboratory (INL), Idaho.
#'   Sensor calibration measurements are stored in the [`calibrations`] dataset.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`sensor_id`}{Identifier for the integrated sensor.}
#'     \item{`sensor_nu`}{Serial number for the sensor.}
#'     \item{`sensor_brand_nm`}{Brand name.}
#'     \item{`sensor_model_cd`}{Model code.}
#'     \item{`press_unit_cd`}{Pressure unit code.}
#'     \item{`temp_unit_cd`}{Temperature unit code.}
#'     \item{`sensor_tp`}{Integrated sensor types.}
#'     \item{`temp_lower_va`}{Lower temperature range, in degree Celsius.}
#'     \item{`temp_upper_va`}{Upper temperature range, in degree Celsius.}
#'     \item{`press_lower_va`}{Lower fluid pressure range, in pounds per square inch (psi).}
#'     \item{`press_upper_va`}{Upper fluid pressure range, in psi.}
#'     \item{`press_res_va`}{Pressure resolution, in psi.}
#'     \item{`press_acc_va`}{Pressure accuracy, in psi.}
#'     \item{`press_repeat_acc_va`}{Repeated pressure accuracy, in psi.}
#'     \item{`press_hyster_acc_va`}{Hysteresis error of the pressure sensor, in psi.}
#'     \item{`response_time_tx`}{Response time description.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office
#'   and underwent processing using the [`read_sensors_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(sensors)

"sensors"
