#' Measurement Ports
#'
#' @description Dataset describing the locations of measurement ports within the
#'   multilevel completions of wells in the U.S. Geological Survey (USGS) aquifer-monitoring network,
#'   Idaho National Laboratory (INL), Idaho.
#'   These valved measurement ports enable monitor groundwater outside the
#'   multiport casing and within a monitoring zone.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`port_nu`}{Identifier for the valved measurement port.}
#'     \item{`site_no`}{USGS site identification number.}
#'     \item{`mp_a_va`}{Distance between the transducer plane and the bottom of the adjacent upper packer
#'       (top of the monitoring zone), in feet, as documented in the system log.
#'       A value of not applicable (`NA`) indicates a optional lower port coupling within the monitoring zone.}
#'     \item{`wl_depth_va`}{Depth to water inside the multiport casing measured using an electric tape (e-tape),
#'       in feet below the top of the well casing.}
#'     \item{`baro_compl_va`}{Atmospheric pressure measured at land surface, in pounds per square inch (psi).}
#'     \item{`temp_compl_va`}{Fluid temperature measured inside the multiport casing at the pressure transducer bridge,
#'       in degrees Celsius (°C).}
#'     \item{`press_compl_va`}{Absolute pressure of fluid inside the multiport casing, in psi.}
#'     \item{`tp_depth_va`}{Depth to the transducer plane in the measurement port coupling,
#'       in feet below land surface.}
#'     \item{`port_depth_va`}{Depth to the measurement port inlet valve of a port coupling,
#'       in ft bls.}
#'     \item{`port_alt_va`}{Altitude of the measurement port inlet valve of a port coupling,
#'       in feet above the North American Vertical Datum of 1988 (NAVD 88).}
#'     \item{`zone_nu`}{Identifier for the depth-discrete monitoring zone,
#'       where groundwater in this zone is vertically isolated between upper and lower packers.}
#'     \item{`npress`}{Number of pressure and temperature measurements collected at the port.}
#'     \item{`nsamples`}{Number of water-quality samples collected at the port.}
#'   }
#'
#' @source The dataset originates from the USGS INL Project Office
#'   and underwent processing using the [`read_mlms_json`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(ports)

"ports"
