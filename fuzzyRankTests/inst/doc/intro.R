## ----options-width,include=FALSE,echo=FALSE-----------------------------------
options(keep.source = TRUE, width = 80)

## ----libraries----------------------------------------------------------------
library(fuzzyRankTests)

## ----beak-data-and-test-------------------------------------------------------
z <- c(-0.8, 7.5, 46.9, 17.6, -4.6, 54.0, 48.3, 3.9, 16.7,
    19.7, -8.5, 7.1, 40.7, 23.8, 14.8, 20.6, 25.0, 24.7,
    -1.8, 21.9, 4.7, 24.7, 52.8, 8.5, 1.9)
fuzzy.sign.test(z, alternative = "greater")

## ----beak-ci, align="center", fig.cap="95\\% fuzzy confidence interval for Example 3.5 of Hollander et al. (2014).\\@  Interval dual to sign test."----
fuzzy.sign.ci(z) |> plot()

## ----sign.test.with.zeroes----------------------------------------------------
z <- c(-1.3, -0.4, 0.0, 0.0, 0.3, 0.5, 0.9, 1.1, 1.1, 1.1, 2.3,
    2.5, 3.1, 4.5, 5.5)
fuzzy.sign.test(z)

## ----sign.test.with.zeroes.plot.pdf, align="center", fig.cap="PDF of Fuzzy P-value."----
fuzzy.sign.test(z) |> plot()

## ----sign.test.with.zeroes.plot.cdf, align="center", fig.cap="CDF of Fuzzy P-value."----
fuzzy.sign.test(z) |> plot(type = "cdf")

## ----sign.ci.with.zeroes, align="center", fig.cap="95\\% fuzzy confidence interval for made-up data with ties.\\@  Interval dual to sign test."----
fuzzy.sign.ci(z) |> plot()

## ----signed.rank.pdf, align="center", fig.cap="Signed rank test for made-up data."----
z <- c(-2.2, -1.3, -0.3, 0.0, 0.0, 0.3, 0.5, 0.9, 1.1, 1.3,
    1.3, 2.3, 2.5, 3.1, 4.5, 5.5)
fuzzy.signrank.test(z) |> plot()

## ----signed.rank.cdf, align="center", fig.cap="Signed rank test for made-up data."----
fuzzy.signrank.test(z) |> plot(type = "cdf")

## ----signed.rank.ci, align="center", fig.cap="95\\% Signed rank confidence interval for made-up data."----
fuzzy.signrank.ci(z) |> plot()

## ----rank.sum.pdf, align="center", fig.cap="Rank sum test for made-up data."----
x <- c(1, 2, 3, 4, 4, 4, 5, 6, 7)
y <- c(4, 5, 7, 7, 8, 9, 10, 11)
fuzzy.ranksum.test(x, y) |> plot()

## ----rank.sum.ci, align="center", fig.cap="95\\% rank sum confidence interval for made-up data."----
fuzzy.ranksum.ci(x, y) |> plot()

