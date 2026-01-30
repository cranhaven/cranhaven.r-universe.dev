# Only for linear models -------------------------------------------------------

#' Maximum Likelihood 
#' 
#' @param y vector of response variables
#' @param x matrix of regressors
#' @param lambda transformation parameter
#' @param trafo determines the chosen transformation
#' @param custom_func_std a function that determines a customized standard
#' transformation.
#' @return log-likelihood
#' @keywords internal

ML <- function(y, x, lambda, trafo, custom_func_std){
  qr <- qr(x)
  n <- length(y)
  yt <- rep(NA, n)
  
  lglike <- -lglike(lambda = lambda, y = y, qr = qr, n = n, trafo = trafo,
                    custom_func_std = custom_func_std)
}

# Log-likelihood function for ML method
lglike <- function(lambda, y, qr, n, trafo, custom_func_std, ...) {
  
  # if (abs(lambda) != 0) {
  #   yt <- (y^lambda - 1)/lambda
  #   
  # }
  # else {
  #   yt <- log(y) 
  # }

  
  # Wrapper with standardized tranformations (Done!)
  zt <- if (trafo == "boxcoxshift") {
    box_cox_shift_std(y = y, lambda = lambda)
  } else if (trafo == "boxcox") {
    box_cox_std(y = y, lambda = lambda)
  } else if (trafo == "modulus") {
    modul_std(y = y, lambda = lambda)
  } else if (trafo == "bickeldoksum") {
    Bick_dok_std(y = y, lambda = lambda)
  } else if (trafo == "manly") {
    Manly_std(y = y, lambda = lambda)
  } else if (trafo == "dual") {
    Dual_std(y = y, lambda = lambda)
  } else if (trafo == "yeojohnson") {
    Yeo_john_std(y = y, lambda = lambda)
  } else if (trafo == "logshiftopt") {
    log_shift_opt_std(y = y, lambda = lambda)
  } else if (trafo == "sqrtshift") {
    sqrt_shift_std(y = y, lambda = lambda)
  } else if (trafo == "gpower") {
    gPower_std(y = y, lambda = lambda)
  } else if (trafo == "custom") {
    custom_func_std(y = y, lambda = lambda)
  }
  
  # zt <- yt/exp((lambda - 1)*mean(log(y)))
  
  if (any(is.nan(abs(zt))) | any(is.infinite(zt))) {
    llike <- -Inf
  } else {
    llike <- -n/2 * log((sum(qr.resid(qr, zt)^2))/n)
  }
  
  llike
}

# Only for linear mixed models -------------------------------------------------


# Restricted maximum likelihood 
restricted_ML <- function(y = y,
                 formula = formula, 
                 lambda,
                 data = data,
                 rand_eff = rand_eff,
                 trafo, 
                 custom_func_std) {
  
  lme <- NULL
  VarCorr <- NULL
  random.effects <- NULL
  ranef <- NULL
  
  # Wrapper for other standardized transformations (Done!)
  zt <- if (trafo == "boxcoxshift") {
    box_cox_shift_std(y = y, lambda = lambda)
  } else if (trafo == "boxcox") {
    box_cox_std(y = y, lambda = lambda)
  } else if (trafo == "modulus") {
    modul_std(y = y, lambda = lambda)
  } else if (trafo == "bickeldoksum") {
    Bick_dok_std(y = y, lambda = lambda)
  } else if (trafo == "manly") {
    Manly_std(y = y, lambda = lambda)
  } else if (trafo == "dual") {
    Dual_std(y = y, lambda = lambda)
  } else if (trafo == "yeojohnson") {
    Yeo_john_std(y = y, lambda = lambda)
  } else if (trafo == "logshiftopt") {
    log_shift_opt_std(y = y, lambda = lambda)
  } else if (trafo == "sqrtshift") {
    sqrt_shift_std(y = y, lambda = lambda)
  } else if (trafo == "gpower") {
    gPower_std(y = y, lambda = lambda)
  } else if (trafo == "custom") {
    custom_func_std(y = y, lambda = lambda)
  }
  

  data[paste(formula[2])] <- zt
  tdata <- data

  
  model_REML <- NULL
  try(model_REML <- lme(fixed     = formula,
                        data      = tdata,
                        random    = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                        method    = "REML",
                        keep.data = FALSE, 
                        na.action = na.omit), silent = TRUE)
  if (is.null(model_REML)) {
    stop("For some lambda in the lambdarange, the likelihood does not converge.
         Choose another lambdarange.")
  } else {
    model_REML <- model_REML
  }
  
  log_likelihood <- -logLik(model_REML)
  
  return(log_likelihood)
}


# Pooled skewness by Rojas-Perilla
pooled_skewness_min <- function(model, res) {
  
  random.effects <- NULL
  VarCorr <- NULL
  
  skew_resid <- skewness(res)
  random_effect <- as.matrix(random.effects(model))[,1]
  skew_random_effect <- skewness(random_effect)
  sigmae2est <- model$sigma^2
  sigmau2est <- as.numeric(VarCorr(model)[1,1])
  w <- sigmae2est / (sigmae2est + sigmau2est)
  
  pooled_skew <- w * abs(skew_resid) + (1 - w) * abs(skew_random_effect)
  
  return(pooled_skew)
}


# For linear and linear mixed models -------------------------------------------

#' Skewness minimization by Molina 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return absolute value of the skewness of the residuals
#' @import moments
#' @keywords internal

skewness_min <- function(res = res) {
  skew_resid <- skewness(res)
  
  absolute_skew <- abs(skew_resid)
  
  return(absolute_skew)
}


#' Kurtosis 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return absolute value of the skewness of the residuals
#' @import moments
#' @keywords internal

kurtosis_min <- function(res = res) {
  result <- abs(kurtosis(res) - 3)
  return(result)
}


#' Divergence minimization by Kolmogorov Smirnov
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return differences of supremum
#' @keywords internal

divergence_min_KS <- function(res = res) {
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs
  
  supremum_diff <- max(abs(difs))
  
  return(supremum_diff)
}


#' Divergence minimization by Cramer von Mises 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return sum of squared differences
#' @keywords internal

divergence_min_CvM <- function(res = res) {
  
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs
  
  sum_sq_diff <- sum((difs)^2)
  
  return(sum_sq_diff)
}


#' Divergence minimization by Kullback-Leibler
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return median of Kullback-Leibler divergence
#' @importFrom FNN KL.divergence
#' @keywords internal

divergence_min_KL <- function(res = res) {
  
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.probs <- eval.probs[-c(1,length(eval.probs))]
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <-  quantile(res, probs = eval.probs)
  divergence_KL <- KL.divergence(eval.probs, test.probs, k = 5)
  
  median_divergence_KL <- median(divergence_KL)
  
  return(median_divergence_KL)
}
