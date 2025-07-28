#' Water-Quality Samples
#'
#' @description Depth-discrete chemical and physical data for groundwater collected from
#'   Multilevel Monitoring System (MLMS) wells in the U.S. Geological Survey (USGS)
#'   aquifer-monitoring network, Idaho National Laboratory (INL), Idaho.
#'
#' @format A data frame with the following variables:
#'   \describe{
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`port_nu`}{Identifier for the valved measurement port.}
#'     \item{`site_no`}{USGS site identification number for a MLMS measurement port.}
#'     \item{`stime_dt`}{Start time for the pressure profiling event.}
#'     \item{`sample_dt`}{Date and time the water-quality sample was collected, in "America/Denver" time zone.
#'       Missing values of time were substituted with "12:00".}
#'     \item{`parm_short_nm`}{Parameter short name assigned by the USGS, such as "pH";
#'       followed by 'wu' water, unfiltered or 'wf' water, filtered.}
#'     \item{`unit_cd`}{Units of measurement.}
#'     \item{`pcode`}{USGS 5-digit parameter code. For example, the parameter code for Tritium is "07000".}
#'     \item{`remark_cd`}{Remark code (result level) used to qualify the parameter value.
#'       The codes and their meanings are as follows: [`NA`] (missing value) is a quantified value;
#'       "<" is where the actual value is known to be less than the value reported, that is,
#'       the measured concentration is below the reporting limit (RL) and represented as
#'       a censored (or nondetection) value. For censored values, the value reported is the RL;
#'       and "E" is an estimated value, that is, the actual value is greater than the
#'       minimum detection limit (MDL) and less than the laboratory reporting level (LRL).
#'     }
#'     \item{`result_va`}{Parameter value.}
#'     \item{`lab_li_va`}{Lower confidence interval of the result value based on laboratory analysis.}
#'     \item{`lab_ui_va`}{Upper confidence interval of the result value based on laboratory analysis.}
#'     \item{`dqi_cd`}{Data quality indicator code that indicates the review status of a result.
#'       The codes and their meanings are as follows: "R" reviewed and accepted,
#'       and "S" provisional (presumed satisfactory).}
#'     \item{`sample_type_cd`}{Sample type code that identifies the quality-assurance (QA) type of a sample.
#'       The codes and their meanings are as follows:
#'       "2" is a blank sample;
#'       "7" is a replicate sample taken from the environment; and
#'       "9" is a regular sample taken from the environment.}
#'   }
#'
#' @source Sample data retrieved from the \pkg{inldata} package using the [`get_samples`] function.
#'
#' @keywords datasets
#'
#' @examples
#' str(samples)

"samples"
