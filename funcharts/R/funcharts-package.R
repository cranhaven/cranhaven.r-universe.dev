#' @keywords internal
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2 emissions based
#' on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500.
#' <doi:10.1002/asmb.2507>
#'
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294, <doi:10.1080/00401706.2020.1753581>
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' Capezza, C., Capizzi, G., Centofanti, F., Lepore, A., Palumbo, B. (2025)
#' An Adaptive Multivariate Functional EWMA Control Chart.
#' \emph{Journal of Quality Technology},  57(1):1--15,
#' <doi:10.1080/00224065.2024.2383674>.
#'
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' <doi:10.1080/00224065.2024.2430978>.
#'
"_PACKAGE"


## usethis namespace: start
#' @importFrom dplyr "%>%"
#' @importFrom Rcpp sourceCpp
#' @importFrom robustbase Mwgt
#' @useDynLib funcharts, .registration = TRUE
## usethis namespace: end
NULL
