#' Joint Test-Statistic for the Null of Non-Cointegration
#'
#' Produces a joint test-statistic for the null of non-cointegration, aggregating
#' various cointegration tests.
#'
#' @param formula An object of class \code{\link[stats]{formula}} to describe the model.
#' @param data An optional data frame containing the variables in the model.
#' @param lags Number of lags to be included.
#' @param trend Type of deterministic component to be included. "none" for no deterministic,
#' "const" for a constant and "trend" for a constant plus trend.
#' @param test Selection of tests to choose from. Choices are either "ej", for \code{\link{englegranger}}
#' and \code{\link{johansen}}, or "all", for \code{\link{englegranger}}, \code{\link{johansen}},
#' \code{\link{banerjee}} and \code{\link{boswijk}}.
#'
#' @return \code{bayerhanck} returns an object of classes \code{"bhtest"} and \code{"list"}.
#'
#' The function \code{summary} is used to print a summary, which includes the test statistics and p-values
#' for the underlying tests, as well as the test statistic and p-value for the Bayer Hanck Test.
#'
#' An object of class \code{"bhtest"} is a \code{"list"} containing, inter alia, the components:
#'
#'\item{bh.test}{the test statistic of the Bayer Hanck Test.}
#'\item{bh.pval}{the p-Value of the Bayer Hanck Test.}
#'\item{test.stat}{the test statistics of the underlying tests.}
#'\item{pval.stat}{the p-values of the underlying tests.}
#'
#'
#' @export
#'
#' @references
#' Bayer, C. and Hanck, C. (2013). Combining non-cointegration tests. \emph{Journal of Time Series Analysis}, 34(1), 83 -- 95. \doi{10.1111/j.1467-9892.2012.00814.x}
#'
#' @examples
#' # Luetkepohl (2007): Economic data from West Germany
#' data(lutkepohl_e1)
#' bayerhanck(linvestment ~ lincome + lconsumption, data = lutkepohl_e1)
#' bayerhanck(linvestment ~ lincome + lconsumption, data = lutkepohl_e1, lags = 4)
#'
#'
#' @examplesIf requireNamespace("MTS", quietly = TRUE)
#' # World Almanac and Book of Facts (1975): Monthly simple returns of the stocks of IBM,
#' # Coca Cola and S&P Composite index
#' try({
#'   data("mts-examples", package = "MTS")
#'   bayerhanck(sp ~ ibm + ko, data = ibmspko)
#' }, silent = TRUE)
#'
#'
#'
bayerhanck <- function(formula, data, lags = 1, trend = "const", test = "all") {


  # ---- Check Syntax ----
  mf <- match.call()
  m <- match(c("formula", "data"), names(mf), 0L)
  if (is.null(data) | is.null(formula))
    stop()
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "numeric")
  x <- stats::model.matrix(mt, mf)[, -1]
  nvar <- ncol(cbind(y, x))
  trend <- match.arg(trend,
                     choices = c("none", "const", "trend"))
  test <- match.arg(test,
                    choices = c("ej", "all"))


  lag <- lags
  if (lag < 0)
    stop("Lags must be set to a non-negative value.")


  # ---- Code trendtypes ----
  if (identical(trend, "none")){
    ending = -1
    trendtype = 1
  } else if (identical(trend, "const")){
    ending = ""
    trendtype = 2
  } else if (identical(trend, "trend")){
    ending = "FORMULAR"
    trendtype = 3
  }


  # ---- Call Tests ----
  test.stat <- rep(NA, 4)
  names(test.stat) <- c("Engle-Granger", "Johansen", "Banerjee", "Boswijk")

  invisible(utils::capture.output(
  if (identical(test, "ej"))
    test.stat[1:2] <- c(englegranger(formula = formula, data = data, lags = lags, trend = trend)$test.stat,
                        johansen(formula = formula, data = data, lags = lags, trend = trend)$test.stat)
  ))
  invisible(utils::capture.output(
  if (identical(test, "all"))
    test.stat[1:4] <- c(englegranger(formula = formula, data = data, lags = lags, trend = trend)$test.stat,
                        johansen(formula = formula, data = data, lags = lags, trend = trend)$test.stat,
                        banerjee(formula = formula, data = data, lags = lags, trend = trend)$test.stat,
                        boswijk(formula = formula, data = data, lags = lags, trend = trend)$test.stat)
  ))

  test.stat <- test.stat
  pval.stat <- test.stat


  # ---- Obtain P-Values ----
  N <- nrow(null_dist)

  basecase <- 44 * (trendtype - 1) + 4 * (nvar - 2)

  for (i in 1:4) {
    case = basecase + i
    if (i %in% c(1, 3)) {
      n <- sum(pval.stat[i] > null_dist[, case])
      pval.stat[i] <-  (n/N) + .000000000001
    } else {
      if (i %in% c(2, 4)) {
        n <- sum(pval.stat[i] < null_dist[, case])
        pval.stat[i] <-  (n/N) + .000000000001
      }
    }
  }


  # ---- Calculate Bayer-Hanck Fisher Statistics ----
  #### compute statistics ####
  if (identical(test, "ej"))
    bh.test <- -2*sum(log(pval.stat[1:2]))
  if (identical(test, "all"))
    bh.test <- -2*sum(log(pval.stat[1:4]))

  # degrees of freedom
  k <- nvar - 1

  # Estimate p-Value
  if(identical(test, "ej")) test <- "E_J"

  bh.pval <- get_p_value(bh.test = bh.test,
                         trendtype = trendtype,
                         test.type = test,
                         k = k)

  # ---- Display Results ----
  out <- list(bh.test = bh.test,
              bh.pval = bh.pval,
              test.stat = test.stat[stats::complete.cases(test.stat)],
              pval.stat = pval.stat[stats::complete.cases(test.stat)],
              formula = formula,
              lags = lags,
              trend = trend,
              test.type = test,
              k = k)
  class(out) <- c("bhtest", "list")
  return(out)
}
