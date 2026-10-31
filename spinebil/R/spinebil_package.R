#' spinebil
#'
#' Functions to evaluate the performance of projection pursuit
#'    index functions using tour methods.
#'
#' @seealso
#'
#' The main functions are:
#' \itemize{
#'   \item [get_trace()]
#'   \item [profile_rotation()]
#'   \item [compare_smoothing()]
#'   \item [time_sequence()]
#'   \item [squint_angle_estimate()]
#' }
#'
#' @keywords internal
"_PACKAGE"


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
globalVariables(c("alpha",
                  "value",
                  "angle",
                  "method",
                  "index_mean"))
