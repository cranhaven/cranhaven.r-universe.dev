#'Mc.Leod.Li nonlinearity test
#'
#' This function allows you to make Mc.Leod.Li nonlinearity test
#' @param y series name,
#' @param lag lag parameter,
#' @keywords nonlinearity test
#' @return "lag stat pvalue" the lag order, the value of the test statistic and the probability of test statistic, respectively.
#' @references
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#'
#' @export
#' @importFrom stats acf pchisq
#' @examples
#' x <- rnorm(1000)
#' Mc.Leod.Li(x, 10)
#'
#'
#' data(IBM)
#' Mc.Leod.Li(IBM,4)
#'
#'

Mc.Leod.Li = function (y,lag)
{

    acfs = acf(y, lag.max = lag, plot = FALSE, demean = FALSE)

    n = sum(!is.na(y))
    goz = acfs$acf[1:lag]
    stat = n*(n+2)*sum(1/seq.int(n-1, n-lag)*goz^2)
    pvalue = 1 - pchisq(stat, lag)



  cbind(lag,stat,pvalue)

}
