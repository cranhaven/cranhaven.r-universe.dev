## GOFtest =====================================================================
#' Goodness-of-Fit Test for Count Models
#'
#' Performs the Manjon and Martinez (2014) chi-squared goodness-of-fit test
#' for count data models.
#'
#' @param object An object of class `"mlmodel.count"` (typically from 
#'   `ml_poisson()` or `ml_negbin()`).
#' @param bins Integer vector. Defines the boundaries of the bins used
#'   to group counts. Default is `0:5`.
#'
#' @details
#' The test compares the observed frequencies with the expected frequencies
#' predicted by the model across different count bins. It produces both a
#' binned comparison table and an overall regression-based chi-squared test
#' statistic.
#'
#' A low p-value indicates that the model's predicted probabilities do not
#' adequately match the observed count distribution (model misspecification).
#'
#' @return An object of class `"GOFtest.mlmodel"` with components:
#' \describe{
#'   \item{model}{Description of the fitted model.}
#'   \item{matrix}{A table with observed and predicted frequencies, proportions,
#'     absolute differences, and Pearson contributions per bin.}
#'   \item{test}{A list containing `teststat`, `df`, and `pval` for the overall
#'     goodness-of-fit test.}
#' }
#'
#' @author Alfonso Sanchez-Penalver
#' 
#' @references
#' Manjon, M., & Martinez, O. (2014). 'The chi-squared goodness-of-fit test 
#' for count-data models.' *The Stata Journal*, 14(4), 798-816.
#' \doi{10.1177/1536867X1401400406}
#'
#' @examples
#' 
#' # Poisson model
#' fit_pois <- ml_poisson(docvis ~ private + medicaid + age + I(age^2) + 
#'                        educyr + actlim + totchr, data = docvis)
#' 
#' GOFtest(fit_pois, bins = 0:5)
#' 
#' # Negative binomial model
#' fit_nb2 <- ml_negbin(docvis ~ private + medicaid + age + I(age^2) + 
#'                      educyr + actlim + totchr, data = docvis)
#' 
#' GOFtest(fit_nb2)
#'
#' @export
GOFtest <- function(object, bins = 0:5) UseMethod("GOFtest")

#' @rdname GOFtest
#' @export
GOFtest.mlmodel <- function(object, bins = 0:5)
{
  if(!inherits(object, "mlmodel.count"))
    cli::cli_abort("`object` needs to be of class 'mlmodel.count'", call = NULL)
  
  # 1. Type validation
  if (!is.numeric(bins)) {
    cli::cli_abort("{.arg bins} must be a numeric vector.", call = NULL)
  }
  
  # 2. Structure cleaning
  bins <- sort(unique(floor(bins)))
  
  # 3. Domain validation
  if (any(bins < 0)) {
    cli::cli_abort("All values in {.arg bins} must be non-negative.", call = NULL)
  }
  
  # 4. If the model is not truncated add 0 to the bins if it's not there.
  if(!inherits(object, "mlmodel.truncated") && !(0 %in% bins))
      bins <- c(0, bins)
  
  n_bins <- length(bins)
  
  # Get reduced y (only observations used in estimation)
  y <- as.vector(object$model$value$outcomes[[1]])
  n_obs <- length(y)
  
  # Get logical vector of used observations
  sample_idx <- object$model$sample
  
  # Safety check: make sure the reduced y matches the sample size
  if (sum(sample_idx) != n_obs) {
    cli::cli_abort("Length mismatch between stored outcomes and sample index.", call = NULL)
  }
  
  # Regression matrix: rows: # observations, cols: # bins.
  R <- matrix(0, nrow = n_obs, ncol = n_bins)
  
  # Display matrix: rows # bins + 1, 
  D <- matrix(0, nrow = n_bins + 1, ncol = 5)
  
  row_names <- character(n_bins + 1)
  # need to loop through each value in bins.
  for(i in seq_len(length(bins)))
  {
    # now the bin we want will be formed by the current value and the next one
    # except for the last bin
    if (i == length(bins))
    {
      row_names[i] <- paste(bins[i], "-", bins[i])
      # We're on the last bin so it's probability equal to that value. 
      d_j <- (y == bins[i])
      p_j <- predict(object, type = paste0("P(", bins[i], ")"))
    }
    else
    {
      low <- bins[i]
      high <- bins[i+1] - 1
      row_names[i] <- paste(low, "-", high)
      if (low == high)
      {
        # exact probability
        d_j <- (y == low)
        p_j <- predict(object, type = paste0("P(", low, ")"))
      }
      else
      {
        # interval
        d_j <- (y >= low & y <= high)
        p_j <- predict(object, type = paste0("P(", low, ",", high, ")"))
      }
    }
    # Extract the actual prediction from the new class object
    p_j <- p_j$fit
    # Keep only the observations used in estimation, so it's clean of NAs and
    # has the same length as y.
    p_j <- p_j[sample_idx]
    R[, i] <- d_j - p_j
    D[i, ] <- rbind(
      sum(d_j, na.rm = TRUE),                                # Frequency
      mean(d_j, na.rm = TRUE),                               # Proportion
      mean(p_j, na.rm = TRUE),                               # Probability
      abs(mean(d_j, na.rm = TRUE) - mean(p_j, na.rm = TRUE)),              # |Difference|
      n_obs * ((mean(d_j, na.rm = TRUE)) - mean(p_j, na.rm = TRUE))^2 / mean(p_j, na.rm = TRUE)  # Pearson Chisq.
    )
  }
  
  # We need to add the last one to the display, which is the probability of
  # greater than the last value.
  k <- bins[n_bins] + 1
  d_j <- y >= k
  p_j <- predict(object, type = paste0("P(", k, ",)"))$fit
  # Only estimation observations (same length as y and d_j)
  p_j <- p_j[sample_idx]
  D[nrow(D), ] <- rbind(
    sum(d_j, na.rm = TRUE),                                # Frequency
    mean(d_j, na.rm = TRUE),                               # Proportion
    mean(p_j, na.rm = TRUE),                               # Probability
    abs(mean(d_j, na.rm = TRUE) - mean(p_j, na.rm = TRUE)),              # |Difference|
    n_obs * ((mean(d_j, na.rm = TRUE)) - mean(p_j, na.rm = TRUE))^2 / mean(p_j, na.rm = TRUE)  # Pearson Chisq.
  )
  row_names[nrow(D)] <- paste(k,"+")
  
  colnames(D) <- c("Frequency",
                   "Proportion",
                   "Probability",
                   "|Difference|",
                   "Pearson")
  rownames(D) <- row_names
  
  # Regression.
  ones <- rep(1, length(y))
  x <- cbind(R, object$gradientObs)
  ols <- .lm.fit(x, ones)
  teststat <- n_obs - sum(ols$residuals^2)
  df <- n_bins
  pval <- pchisq(teststat, df, lower.tail = FALSE)
  
  out <- list(
    model = object$model$description,
    matrix = D,
    test = list(
      teststat = teststat,
      df = df,
      pval = pval
    )
  )
  class(out) <- "GOFtest.mlmodel"
  return(out)
}

#' @export
print.GOFtest.mlmodel <- function(x, ...) {
  if(!inherits(x, "GOFtest.mlmodel"))
    cli::cli_abort("`x` must be of class `GOFtest`.",
                   call = NULL)
  
  cat("\nGoodness-of-fit test for count models\n")
  if(!is.null(x$model))
    cat("   Model:", x$model)
  cat("\n--------------------------------------------------\n")
  cat("Manjon & Martinez (2014) Score Test\n\n")
  
  # Print the Comparison Table
  print(round(x$matrix, 4))
  
  cat("\n--------------------------------------------------\n")
  cat(sprintf("  Chisq(%d):             %0.4f\n", x$test$df, x$test$teststat))
  cat(sprintf("  p-value:               %0.4f\n", x$test$pval))
  cat("--------------------------------------------------\n")
  
  invisible(x)
}

## OVDtest =====================================================================
#' Overdispersion Tests for Count Models
#'
#' Performs Cameron and Trivedi's (1990) regression-based tests for overdispersion
#' in count models.
#'
#' @param object An object of class `"mlmodel.count"` (fitted with 
#'   `ml_poisson()` or `ml_negbin()`).
#'
#' @details
#' These tests evaluate the null hypothesis that the conditional variance equals
#' the conditional mean (the Poisson assumption). Rejection indicates overdispersion
#' and suggests that a negative binomial model may be more appropriate.
#'
#' When the input object is not a Poisson model, a Poisson regression is fitted
#' internally using the value (mean) equation specification from `object` in order
#' to perform the test.
#'
#' @return A list containing the results of the tests against NB1 and NB2 
#'   alternatives, with coefficient estimates, t-statistics, and p-values.
#'
#' @references
#' Cameron, A. C., & Trivedi, P. K. (1990). 'Regression-based tests for overdispersion 
#' in the Poisson model.' *Journal of Econometrics*, 46(3), 347-364.
#' \doi{10.1016/0304-4076(90)90014-K}
#'
#' Cameron, A. C., & Trivedi, P. K. (2013). *Regression Analysis of Count Data* (2nd ed.).
#' Cambridge University Press. \doi{10.1017/CBO9781139013567}
#'
#' @seealso [IMtest()], [GOFtest()]
#'
#' @examples
#' 
#' # Poisson model
#' fit_pois <- ml_poisson(docvis ~ private + medicaid + age + I(age^2) + 
#'                        educyr + actlim + totchr, data = docvis)
#' OVDtest(fit_pois)
#' 
#' # Negative binomial model (the test still fits a Poisson internally using 
#' # only the value equation, so results are identical)
#' fit_nb2 <- ml_negbin(docvis ~ private + medicaid + age + I(age^2) + 
#'                      educyr + actlim + totchr, data = docvis)
#' OVDtest(fit_nb2)
#' 
#' @author Alfonso Sanchez-Penalver
#'
#' @export
OVDtest <- function(object)
{
  if(!inherits(object, "mlmodel.count"))
    cli::cli_abort("`object` needs to be of class 'mlmodel.count'", call = NULL)
  
  # No matter what model, we need to pull y from the value object
  y <- as.vector(object$model$value$outcomes[[1]])
  if(inherits(object, "ml_poisson"))
  {
    # Poisson already estimated fitted values and residuals.
    
    u <- object$model$residuals
    mu <- object$model$fitted.values
  }
  else
  {
    # NB1 or NB2. Need to estimate a poisson. Need x and w.
    x <- as.matrix(object$model$value$predictors)
    w <- object$model$weights
    
    pois_fit <- suppressWarnings({
      .ml_poisson.fit(y,x,w)
    })
    
    # this returns the maxLik object.
    beta <- pois_fit$estimate
    mu <- as.vector(exp(x %*% beta))
    u <- y - mu
  }
  
  index <- (u^2 - y) / mu
  df <- length(index) - 1
  
  nb2_reg <- .lm.fit(as.matrix(mu), index)
  nb1_reg <- .lm.fit(matrix(1, nrow = length(index)), index)
  
  # okay have to do the t-tests myself. 
  nb2_alpha <- nb2_reg$coefficients
  nb2_resid <- nb2_reg$residuals
  nb2_mse <- sum(nb2_resid^2) / df
  nb2_se  <- sqrt(nb2_mse / sum(mu^2))
  nb2_t <- nb2_alpha / nb2_se
  nb2_pval <- pt(nb2_t, df = df, lower.tail = FALSE)
  
  nb1_alpha <- nb1_reg$coefficients
  nb1_resid <- nb1_reg$residuals
  nb1_mse <- sum(nb1_resid^2) / df
  nb1_se  <- sqrt(nb1_mse / length(index))
  nb1_t <- nb1_alpha / nb1_se
  nb1_pval <- pt(nb1_t, df = df, lower.tail = FALSE)
  
  test <- list(nb2 = list(
    alpha = nb2_alpha,
    se = nb2_se,
    tstat = nb2_t,
    pval = nb2_pval
  ), nb1 = list(
    alpha = nb1_alpha,
    se = nb1_se,
    tstat = nb1_t,
    pval = nb1_pval
  ), nobs = length(y))
  
  class(test) <- "OVDtest"
  return(test)
}

#' @export
print.OVDtest <- function(x, digits = 4, ...)
{
  if(!inherits(x, "OVDtest"))
    cli::cli_abort("`x` needs to be of class `'OVDtest'`")
  
  cat("\nCameron and Trivedi (1990) Overdispersion Test:",
      "--------------------------------------",
      "  H0: Poisson (alpha = 0)",
      "  H1: Overdispersion (alpha > 0)",
      "--------------------------------------",
      sep = "\n")
  
  # NB2
  res <- x$nb2
  
  stars <- cut(res$pval,
               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
               labels = c("***", "**", "*", ".", " "))
  
  nb2_df <- data.frame(
    Estimate = format(round(res$alpha, digits), nsmall = digits),
    `t-stat`  = format(round(res$tstat, digits), nsmall = digits),
    `p-value` = format(round(res$pval, digits), nsmall = digits),
    ` `      = as.character(stars),
    check.names = FALSE
  )
  
  res <- x$nb1
  
  stars <- cut(res$pval,
               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
               labels = c("***", "**", "*", ".", " "))
  
  nb1_df <- data.frame(
    Estimate = format(round(res$alpha, digits), nsmall = digits),
    `t-stat`  = format(round(res$tstat, digits), nsmall = digits),
    `p-value` = format(round(res$pval, digits), nsmall = digits),
    ` `      = as.character(stars),
    check.names = FALSE
  )
  
  out <- rbind(nb2_df, nb1_df)
  rownames(out) <- c("NB2", "NB1")
  
  print(out, quote = FALSE, right = TRUE)
  
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("Observations:", x$nobs, "\n")
  cat("Note: P-values are based on a one-tailed t-test (Right Tail).\n\n")
  
  invisible(x)
}