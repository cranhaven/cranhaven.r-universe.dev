#' @useDynLib WpProj, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom doRNG %dorng%
#' @importFrom foreach %:% 
#' @import ROI.plugin.ecos
#' @import ROI.plugin.lpsolve
#' @keywords internal
#' @aliases WpProj-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom magrittr %>%
## usethis namespace: end
NULL



dummy_fun <- function() {
  rqPen::rq.group.pen()
  lifecycle::badge("experimental")
}
