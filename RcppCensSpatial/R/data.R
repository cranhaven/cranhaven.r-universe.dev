#' TCDD concentration data
#'
#' The level of dioxin (2,3,7,8-tetrachlorodibenzo-p-dioxin or TCDD) data was collected
#' in November 1983 by the U.S. Environmental Protection Agency (EPA) in several areas
#' of a highway in Missouri, USA. The TCDD measurement was subject to a limit of
#' detection (\code{cens}); thereby, the \code{TCDD} data is left-censored. Only the
#' locations used in the geostatistical analysis by \insertCite{zirschky1986geostatistical;textual}{RcppCensSpatial} are shown.
#'
#' @usage data("Missouri")
#'
#' @format A data frame with 127 observations and five variables:
#' \describe{
#'   \item{xcoord}{x coordinate of the start of each transect (ft).}
#'   \item{ycoord}{y coordinate of the start of each transect (ft).}
#'   \item{TCDD}{TCDD concentrations (mg/kg).}
#'   \item{transect}{transect length (ft).}
#'   \item{cens}{indicator of censoring (left-censored observations).}
#' }
#'
#' @source \insertRef{zirschky1986geostatistical}{RcppCensSpatial}
#'
#' @seealso \code{\link{EM.sclm}}, \code{\link{MCEM.sclm}}, \code{\link{SAEM.sclm}}
#'
#' @examples
#' \donttest{data("Missouri")
#' y = log(Missouri$TCDD)
#' cc = Missouri$cens
#' coord = cbind(Missouri$xcoord/100, Missouri$ycoord)
#' x = matrix(1, length(y), 1)
#' lcl = rep(-Inf, length(y))
#' ucl = y
#'
#' ## SAEM fit
#' set.seed(83789)
#' fit1 = SAEM.sclm(y, x, cc, lcl, ucl, coord, 5, 1, lower=c(1e-5,1e-5),
#'                  upper=c(50,50))
#' fit1$tab
#'
#' ## MCEM fit
#' fit2 = MCEM.sclm(y, x, cc, lcl, ucl, coord, 5, 1, lower=c(1e-5,1e-5),
#'                  upper=c(50,50), MaxIter=300, nMax=1000)
#' fit2$tab
#'
#' ## Imputed values
#' cbind(fit1$EY, fit2$EY)[cc==1,]}
"Missouri"
