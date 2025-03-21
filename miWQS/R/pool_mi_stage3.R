#' Pooling Multiple Imputation Results
# Log: \Sent to DW - July 9. Final Version.

#' @keywords pool

#' @description
#' Combines multiple parameter estimates (as used in MI) across the K imputed datasets using Rubin 1996 / 1987 formulas, including: calculating a pooled mean, standard error, missing data statistics, confidence intervals, and p-values.
#'
#' @details
#' Stage 3 of Multiple Imputation: We assume that each complete-data estimate is normally distributed. But, given incomplete data, we assume a t-distribution, which forms the basis for confidence intervals and hypothesis tests.
#'
#' The input is an array with p rows referring to the number of parameters to be combined. An estimate and within standard error forms the two columns of the array, which can be easily be taken as the first two columns of the coefficients element of the summary of a glm/lm object. The last dimension is the number of imputations, K. See dataset \code{wqs.pool.test} as an example.
#'
#' Uses Rubin's rules to calculate the statistics of an imputed dataset including: the pooled mean, total standard error, a relative increase in variance, the fraction of missing information, a (1-alpha)% confidence interval, and a p-value.
#'
#' @note
#' Modified the \code{\link[mice]{pool.scalar}} (version R 3.4) in the \pkg{mice} package to handle multiple parameters at once in an array and combine them. Similar to \code{\link[norm]{mi.inference}} in the \pkg{norm} package, but the small-sample adjustment is missing.
#'
#' @references
# APA format
#' Rubin, D. B. (1987). Multiple Imputation for nonresponse in surveys. New York: Wiley.
#'
#' Rubin, D. B. (1996). Multiple Imputation After 18+ Years. Journal of the American Statistical Association, 91(434), 473–489. https://doi.org/10.2307/2291635.
#'
#' Barnard, J., & Rubin, D. B. (1999). Small-Sample Degrees of Freedom with Multiple Imputation. Biometrika, 86(4), 948–955.

#' @param to.pool  An array of p x 2 x K, where p is the number of parameters to be pooled,
#'                 2 refers to the parameters of mean and standard deviation, and K imputation draws.
#'                 The rownames of to.pool are kept in the results.
#' @param n        A number providing the sample size, which is used in calculating the degrees of freedom. If nothing is specified, a large sample is assumed. Has no effect if \code{K} = 1.
#' @param method A string to indicate the method to calculate the degrees of freedom, df.t.
#'    If method = "smallsample" (the default) then the Barnard-Rubin adjustment for small
#'    degrees of freedom is used. Otherwise, the method from Rubin (1987) is used.
#' @param alpha  Type I error used to form the confidence interval. Default: 0.05.
#' @param  prt	 Boolean variable for printing out the standard output. If TRUE, selective parts of a pool.mi object are printed to the screen in an understandable fashion.
#' @inheritParams estimate.wqs


#' @return A data-frame is returned with the following columns: \describe{
#' \item{pooled.mean}{The pooled univariate estimate, Qbar, formula (3.1.2) Rubin (1987).}
#' \item{pooled.total.se}{The total standard error of the pooled estimate, formula (3.1.5) Rubin (1987).}
#' \item{pooled.total.var}{The total variance of the pooled estimate, formula (3.1.5) Rubin (1987).}
#' \item{se.within}{The standard error of mean of the variances (i.e. the pooled within-imputation variance), formula (3.1.3) Rubin (1987).}
#' \item{se.between}{The between-imputation standard error, square root of formula (3.1.4) Rubin (1987).}
#' \item{relative.inc.var(r)}{The relative increase in variance due to nonresponse, formula (3.1.7) Rubin (1987).}
#' \item{proportion.var.missing(lambda)}{The proportion of variation due to nonresponse, formula (2.24) Van Buuren (2012).}
#' \item{frac.miss.info}{The fraction missing information due to nonresponse, formula (3.1.10) Rubin (1987).}
#' \item{df.t}{The degrees of freedom for the reference t-distribution, formula (3.1.6) Rubin (1987) or method of Barnard-Rubin (1999) (if method = "smallsample" (default)).}
#' \item{CI}{The (1-alpha)\% confidence interval (CI) for each pooled estimate.}
#' \item{p.value}{The p-value used to test significance.}
#' }

#' @examples
#' #### Example 1: Sample Dataset 87, using 10% BDL Scenario
#' data(wqs.pool.test)
#' # Example of the `to.pool` argument
#' head(wqs.pool.test)
#'
#' # Pool WQS results and decrease in order of weights.
#' wqs.results.pooled <-   pool.mi(wqs.pool.test, n = 1000)
#' weight.dec <- c(order(wqs.results.pooled$pooled.mean[1:14], decreasing = TRUE), 15:16)
#' wqs.results.pooled <-  wqs.results.pooled[weight.dec, ]
#' wqs.results.pooled
#'
#'
#' # When there is 1 estimate (p = 1)
#' a <- pool.mi(wqs.pool.test[1, , , drop = FALSE], n = 1000)
#' a
#' # wqs.results.pooled["dieldrin", ]
#'
#' # For single imputation (K = 1):
#' b <- pool.mi(wqs.pool.test[, , 1, drop = FALSE], n = 1000)
#' b
#'
#' # Odds ratio and 95% CI using the CLT.
#' odds.ratio <- exp(wqs.results.pooled[15:16, c("pooled.mean", "CI.1", "CI.2")])
#' ## makeJournalTables :: format.CI(odds.ratio, trim = TRUE, digits = 2, nsmall = 2)
#' odds.ratio
#'
#' #  The mice package is suggested for the examples, but not needed for the function.
#' @importFrom Hmisc format.pval
#' @export pool.mi


pool.mi <- function(to.pool, n = 999999, method = c("smallsample", "rubin"), alpha = 0.05, prt = TRUE, verbose = FALSE) {
  method <- tolower(method)
  method <- match.arg(method)

  ## Information
  p <- dim(to.pool)[[1]]  # Number of parameters
  if (dim(to.pool)[[2]] != 2) {
    stop("There are more than two statistics. Please reformat to put mean and standard error in order.")
  }
  K <- dim(to.pool)[[3]]  # Number of imputations

  ## If there is a single imputation, orginal estimates are used.
  if (K == 1) {
    message("There is one imputation; so the orginal estimates are used. The degrees of freedom are assumed to be 1E-9 so test used is normal (see glm2).")
    pooled.mean <- Qbar <-  to.pool[, 1, 1]
    pooled.total.se <- to.pool[, 2, 1]
    var.within <-  (to.pool[, 2, 1])^2
    var.between <- 0
    relative.inc.var <- r <- 0
    # proportion.var.missing <- 0
    frac.miss.info <- 0
    df.t <- 1E9;  # df.

  }  else {  ## We can pool with multiple imputations
    cat("#> Pooling estimates from ", K, " imputed analyses for ", p, " parameters. \n", sep = "")

    ## Overall Mean
    pooled.mean <- Qbar <- apply(to.pool[, 1, , drop = FALSE], 1, mean)

    ## Variances
    subset.mean <-  to.pool[, 1, , drop = FALSE]
    var.between <- apply(subset.mean, 1:2, var)  # denoted B in Rubin notes
    colnames(var.between) <- NULL

    subset <- to.pool[, 2, , drop = FALSE]^2
    dimnames(subset)[[2]] <- "Variance"
    var.within <- apply(subset, 1:2, mean)  # denoted Ubar in Rubin notes, Ubar in scalar.pool.mi
    colnames(var.within) <- NULL

    ## Total Variance
    pooled.total.var <- t <- var.within + (K + 1) / K * var.between
    pooled.total.se <- sqrt(t)

    ## Variance Increasing Statistics
    # rel.inc.var.miss(r) is the relative increase in variance due to nonresponse, formula (3.1.7) Rubin (1987).
    r <- (1 + 1 / K) * var.between / var.within
    # prop.var.miss(lambda) is the proportion of variation due to nonresponse, formula (2.24) Van Buuren (2012).
    lambda <- (1 + 1 / K) * var.between / t
    #  Component df.t is the degrees of freedom for t reference distribution depending on which method is selected.
    df.t <- mice.df(K, lambda, n - p, method)
    # frac.miss.info is the fraction missing information due to nonresponse, formula (3.1.10) Rubin (1987).
    frac.miss.info <- (r + 2 / (df.t + 3)) / (r + 1)
  }

  # confidence interval: Formula 3.1.8 from Rubin 1987
  width <- pooled.total.se * qt(alpha / 2, df.t, lower.tail = FALSE)
  CI <- pooled.mean + cbind(-width, width)
  colnames(CI) <- NULL

  # P-value:  #equivalent to Formula 3.1.9 from Rubin 1987
  sig.level <- 2 * pt(abs(pooled.mean) / pooled.total.se, df = df.t, lower.tail = FALSE)

  if (verbose) {
    cat("\n width:", width)
    cat("\n CI \n"); print(CI)
    sig.level2 <- pf(pooled.mean^2 / pooled.total.var,  df1 = 1, df2 = df.t,  lower.tail = FALSE)
    cat("\n Sig.level \n"); print(sig.level)
  }

  # Return
  fit <-  data.frame(
    pooled.mean = Qbar, pooled.total.se = pooled.total.se, se.within = sqrt(var.within),
    se.between = sqrt(var.between), relative.inc.var = r, frac.miss.info = frac.miss.info,
    CI = CI, p.value = sig.level
  )

  if (prt) {
    P.value <- Hmisc::format.pval(fit$p.value, digits = 3, eps = 0.001)
    print(cbind(round(fit[, c(1:2, 6:8)], digits = 3), P.value))
  }

  return(fit)
}

# All the columns to be returned if verbose = TRUE
# fit <-  data.frame(pooled.mean = Qbar, pooled.total.se = sqrt(t),  #Essentials
#                   var.within = within, var.between = between,
#                   relative.inc.var = r, proportion.var.missing = lambda,
#                   frac.miss.info =frac.miss.info ,  df.t = df.t,
#                   CI = CI, p.value = sig.level,  p.value2 = sig.level2)

###############################################################################
#' Calculating Degrees of Freedom
#' @description Used as helper function in \link{mice} and calculates the degrees of freedom
#' using method. If method = "smallsample" (the default) then the Barnard-Rubin adjustment for
#' small degrees of freedom is used. Otherwise, the method from Rubin (1987) is used.
#' @param m A natural number of imputed datasets to generate.
#' @param lambda Or, prop.var.miss is the proportion of variation due to nonresponse, formula (2.24) Van Buuren (2012).
#' @param dfcom The complete degree of freedom.
#' @return Degree of freedom calculated from method.
#' @noRd
#' @note Used as an accessory function in \code{\link[mice]{pool.scalar}} (version R 3.4) from the \pkg{mice} package. Made it clearer which method calculates which degrees of freedom.

mice.df <- function(m, lambda, dfcom, method) {
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2  # rubin
  # Or, in terms of r (rel increase in variance):
  # dfold <- (m - 1) * (1 + (1/r) )^2
  if (method == "rubin") {
    df <- dfold
  } else if (method == "smallsample") {
    dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    # or  (dfcom + 1)/(dfcom + 3) * dfcom * 1/(r+1) )
    df <- dfold * dfobs / (dfold + dfobs)
    # df2 <- (1/dfold + 1/dfobs)^-1  #alternative representation
  }
  return(df)
}
