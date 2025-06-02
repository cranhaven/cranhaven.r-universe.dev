#'  Title t.baseline
#' @description a wrapper for the baseline.rdfbaseline function in the package baseline in order to have the output in the same format as the input
#' @param y  baseline correction on y
#' @return  y.baseline returns the corrected y
#' @export
#' @importFrom  baseline baseline
#' @examples \donttest{
#' y.baseline <- t_baseline(y)
#' }

t_baseline <- function(y) {
  y.mat <- as.matrix(t(y))
  y.baseline <- baseline(y.mat, method="rfbaseline")
  y.baseline <- as.numeric(t(y.baseline$corrected))
  return(y.baseline)
}
