#' @keywords internal
"_PACKAGE"
#' @useDynLib uqsa, .registration = TRUE
## usethis namespace: start
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply
#' @importFrom errors errors errors<- as.errors set_errors
#' @importFrom grDevices contourLines hcl.colors rgb
#' @importFrom graphics arrows axis hist image legend lines matplot pairs par polygon text rect
#' @importFrom methods as is show
#' @importFrom stats acf approx cor cov dnorm dunif median na.omit quantile rnorm runif sd spline var IQR
#' @importFrom utils flush.console head read.delim tail
#' @importFrom cli cli_progress_bar cli_progress_update
## usethis namespace: end
NULL
