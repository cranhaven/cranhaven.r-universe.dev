#' evbsreg: Local Influence Diagnostics for the EVBS Regression Model
#'
#' Implements local influence diagnostics for the Extreme-Value
#' Birnbaum-Saunders (EVBS) regression model: joint maximum likelihood
#' estimation, conformal normal curvature diagnostics under three
#' perturbation schemes (case-weight, response variable, and explanatory
#' variable), randomized quantile residuals with simulation envelope,
#' Monte Carlo simulation utilities, and publication-quality density and
#' diagnostic plots.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{evbsreg.fit}}}{Joint MLE of the EVBS regression model.}
#'   \item{\code{\link{cnc_diagnostics}}}{Conformal normal curvature
#'     diagnostics.}
#'   \item{\code{\link{plot_cnc}}}{Two-panel diagnostic figure.}
#'   \item{\code{\link{rqrandomized}}, \code{\link{rcoxsnell}},
#'     \code{\link{envelope_qq}}}{Residuals and envelope.}
#'   \item{\code{\link{revbs}}, \code{\link{evbsreg.fit.mc}}}{Random
#'     generation and Monte Carlo study.}
#' }
#'
#' @references
#' Ospina, R., Lima, J. I. C., Barros, M., and Macedo, A. M. S. (2026).
#' Local influence diagnostics for the extreme-value Birnbaum-Saunders
#' regression model: methodology, validation, and application to anomalous
#' wind gusts. \emph{Submitted}.
#'
#' @keywords internal
"_PACKAGE"
