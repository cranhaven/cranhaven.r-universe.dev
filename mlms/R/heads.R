#' Hydraulic Heads
#'
#' @description Dataset of depth-discrete measurements of fluid pressure, fluid temperature,
#'   atmospheric pressure, and calculated hydraulic head in the Multilevel Monitoring System (MLMS)
#'   wells in the U.S. Geological Survey (USGS) aquifer-monitoring network,
#'   Idaho National Laboratory (INL), Idaho.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`site_nm`}{Local site name for an MLMS well.}
#'     \item{`port_nu`}{Identifier for the valved measurement port.}
#'     \item{`site_no`}{USGS site identification number.}
#'     \item{`stime_dt`}{Start time for field visit.}
#'     \item{`press_dt`}{Time at which measurements were measured outside the multiport casing.}
#'     \item{`temp_va`}{Fluid temperature measured inside the multiport casing
#'       from the bridge of the pressure transducer, in degree Celsius.}
#'     \item{`baro_va`}{Atmospheric pressure measured at the time of the port measurement,
#'       in pounds per square inch absolute (psi).}
#'     \item{`press_va`}{Absolute pressure of fluid measured outside the multiport casing, in psi.}
#'     \item{`press_head_va`}{Pressure head outside the multiport casing, in feet.}
#'     \item{`total_head_va`}{Hydraulic head outside the multiport casing,
#'       in feet above the North American Vertical Datum of 1988 (NAVD 88).}
#'     \item{`press_in_1_va`}{Fluid pressure measured inside the multiport casing
#'       before the outside pressure measurement was taken, in psi.}
#'     \item{`press_in_2_va`}{Fluid pressure measured inside the multiport casing
#'       after the outside pressure measurement was taken, in psi.}
#'     \item{`press_in_diff_va`}{Difference in the fluid pressure measurements
#'       taken inside the multiport casing, in psi.}
#'     \item{`replicate_fl`}{Whether the measurement is a replicate for quality-control purposes.}
#'     \item{`comment_tx`}{Comments.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office and underwent processing
#'   using the [`read_field_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(heads)

"heads"
