#' Incubate package for parametric time-to-event analysis with delay
#'
#' Estimation and statistical tests on parameters in parametric time-to-event analyses with delay.
#'
#' @useDynLib incubate, .registration = TRUE
#' @importFrom future plan
#' @importFrom future.apply future_apply
#' @importFrom glue glue
#' @importFrom MASS boxcox
#' @importFrom purrr chuck
#' @importFrom rlang .data `%||%`
#' @importFrom survival Surv is.Surv
#' @importFrom stats coef simulate update
#' @importFrom tibble tibble
#' @name incubate
#' @docType package
#' @keywords internal
"_PACKAGE"
