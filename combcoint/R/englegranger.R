#' Cointegration Tests
#'
#' Executes common cointegration tests, which serve as underlying tests for the Bayer Hanck Test statistic.
#'
#' @param formula An object of class \code{\link[stats]{formula}} to describe the model.
#' @param data An optional data frame containing the variables in the model.
#' @param lags Number of lags to be included.
#' @param trend Type of deterministic component to be inlcuded, "none" for no deterministics,
#' "const" for a constant and "trend" for a constant plus trend.
#' @param type Test to be conducted, either "eigen" or "trace".
#'
#' @return Returns an object of class \code{"list"}.
#' @export
#'
#' @references Engle, R. and Granger, C. (1987), Co-integration and Error Correction: Representation, Estimation, and Testing, Econometrica 55(2), 251-76.
#'
#' Johansen, S. (1988), Statistical analysis of cointegration vectors, Journal of Economic Dynamics and Control 12(2-3), 231-254.
#'
#' Banerjee, A., Dolado, J. J. and Mestre, R. (1998), Error-correction Mechanism Tests for Cointegration in a Single-equation Framework, Journal of Times Series Analysis 19(3), 267-283.
#'
#' Boswijk, H. P. (1994), Testing for an unstable root in conditional and structural error correction models, Journal of Econometrics 63(1), 37-60.
#'
#' @examples
#' # Luetkepohl (2007): Economic data from West Germany
#' data(lutkepohl_e1)
#' englegranger(linvestment ~ lincome + lconsumption, data = lutkepohl_e1)
#'
#' #' @examplesIf requireNamespace("MTS", quietly = TRUE)
#' # World Almanac and Book of Facts (1975): Monthly simple returns of the stocks of IBM,
#' # Coca Cola and S&P Composite index
#' try({
#'   data("mts-examples", package = "MTS")
#'   englegranger(sp ~ ibm + ko, data = ibmspko)
#' }, silent = TRUE)
#'
#'
englegranger <- function(formula, data, lags = 1, trend = "const"){

  # ---- Check Syntax ----
  mf <- match.call()
  m <- match(c("formula", "data"), names(mf), 0L)
  if (is.null(data) | is.null(formula))
    stop()
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mf <- stats::na.omit(mf)
  y <- stats::model.response(mf, "numeric")
  trend <- match.arg(trend,
                     choices = c("none", "const", "trend"))


  # ---- Trend Specification ----
  if (identical(trend, "none")){
    eg_lm <- stats::lm(stats::update(formula, ~. -1), data = data, na.action = stats::na.omit)
  } else if (identical(trend, "const")) {
    eg_lm <- stats::lm(formula, data = data, na.action = stats::na.omit)
  } else if (identical(trend, "trend")){
    tr <- seq_along(y)
    data$tr <- tr
    eg_lm <- stats::lm(stats::update(formula, ~. + tr), data = data, na.action = stats::na.omit)
    data <- data[, -which(colnames(data) == "tr")]
  }


  # ---- Engle Granger Test ----
  eg_res <- eg_lm$residuals
  eg_adf <- urca::ur.df(eg_res, lags = lags)
  test.stat <- as.numeric(eg_adf@teststat)

  out <- list(test.stat = test.stat,
              lags = lags,
              trend = trend,
              test = "Engle-Granger",
              formula = formula)
  return(out)
}
