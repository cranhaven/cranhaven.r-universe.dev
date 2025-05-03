#'Random Walk test for skill differences
#'
#'Forecast comparison of the skill obtained with 2 forecasts (with respect to a 
#'common observational reference) based on Random Walks (DelSole and Tippett,
#'2016).
#'
#'@param skill_A A numerical array of the time series of the scores obtained
#'  with the forecaster A.
#'@param skill_B A numerical array of the time series of the scores obtained
#'  with the forecaster B. The dimensions should be identical as parameter
#'  'skill_A'.
#'@param time_dim A character string indicating the name of the dimension along
#'  which the tests are computed. The default value is 'sdate'.
#'@param test.type A character string indicating the type of significance test. 
#'  It can be "two.sided.approx" (to assess whether forecaster A and forecaster
#'  B are significantly different in terms of skill with a two-sided test using  
#'  the approximation of DelSole and Tippett, 2016), "two.sided" (to assess 
#'  whether forecaster A and forecaster B are significantly different in terms
#'  of skill with an exact two-sided test), "greater" (to assess whether 
#'  forecaster A shows significantly better skill than forecaster B with a 
#'  one-sided test for negatively oriented scores), or "less" (to assess whether
#'  forecaster A shows significantly better skill than forecaster B with a 
#'  one-sided test for positively oriented scores). The default value is 
#'  "two.sided.approx".
#'@param alpha A numeric of the significance level to be used in the statistical
#'  significance test (output "sign"). The default value is 0.05.
#'@param pval A logical value indicating whether to return the p-value of the
#'  significance test. The default value is TRUE.
#'@param sign A logical value indicating whether to return the statistical
#'  significance of the test based on 'alpha'. The default value is FALSE.
#'@param N.eff Effective sample size to be used in the statistical significance
#'  test. It can be FALSE (and the length of the time series will be used), a 
#'  numeric (which is used for all cases), or an array with the same dimensions
#'  as "skill_A" except "time_dim" (for a particular N.eff to be used for each 
#'  case). The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A list with:
#'\item{$score}{
#'  A numerical array with the same dimensions as the input arrays except 
#'  'time_dim'. The number of times that forecaster A has been better than 
#'  forecaster B minus the number of times that forecaster B has been better 
#'  than forecaster A (for skill negatively oriented, i.e., the lower the 
#'  better). If $score is positive, forecaster A has been better more times
#'  than forecaster B. If $score is negative, forecaster B has been better more
#'  times than forecaster A.
#'}
#'\item{$sign}{
#'  A logical array of the statistical significance with the same dimensions
#'  as the input arrays except "time_dim". Returned only if "sign" is TRUE.
#'}
#'\item{$p.val}{
#'  A numeric array of the p-values with the same dimensions as the input arrays
#'  except "time_dim". Returned only if "pval" is TRUE.
#'}
#'
#'@details 
#' Null and alternative hypothesis for "two-sided" test (regardless of the 
#' orientation of the scores):\cr
#' H0: forecaster A and forecaster B are not different in terms of skill\cr
#' H1: forecaster A and forecaster B are different in terms of skill
#' 
#' Null and alternative hypothesis for one-sided "greater" (for negatively 
#' oriented scores, i.e., the lower the better) and "less" (for positively 
#' oriented scores, i.e., the higher the better) tests:\cr
#' H0: forecaster A is not better than forecaster B\cr
#' H1: forecaster A is better than forecaster B
#' 
#' Examples of negatively oriented scores are the RPS, RMSE and the Error, while
#' the ROC score is a positively oriented score.
#' 
#' DelSole and Tippett (2016) approximation for two-sided test at 95% confidence 
#' level: significant if the difference between the number of times that 
#' forecaster A has been better than forecaster B and forecaster B has been
#' better than forecaster A is above 2sqrt(N) or below -2sqrt(N).
#'
#'@references 
#'DelSole and Tippett (2016): https://doi.org/10.1175/MWR-D-15-0218.1
#'
#'@examples
#' fcst_A <- array(data = 11:50, dim = c(sdate = 10, lat = 2, lon = 2))
#' fcst_B <- array(data = 21:60, dim = c(sdate = 10, lat = 2, lon = 2))
#' reference <- array(data = 1:40, dim = c(sdate = 10, lat = 2, lon = 2))
#' scores_A <- abs(fcst_A - reference)
#' scores_B <- abs(fcst_B - reference)
#' res1 <- RandomWalkTest(skill_A = scores_A, skill_B = scores_B, pval = FALSE, sign = TRUE)
#' res2 <- RandomWalkTest(skill_A = scores_A, skill_B = scores_B, test.type = 'greater')
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
RandomWalkTest <- function(skill_A, skill_B, time_dim = 'sdate',  
                           test.type = 'two.sided.approx', alpha = 0.05, pval = TRUE,
                           sign = FALSE, N.eff = FALSE, ncores = NULL) {
  
  # Check inputs
  ## skill_A and skill_B
  if (is.null(skill_A) | is.null(skill_B)) {
    stop("Parameters 'skill_A' and 'skill_B' cannot be NULL.")
  }
  if (!is.numeric(skill_A) | !is.numeric(skill_B)) {
    stop("Parameters 'skill_A' and 'skill_B' must be a numerical array.")
  }
  if (!identical(dim(skill_A), dim(skill_B))) {
    stop("Parameters 'skill_A' and 'skill_B' must have the same dimensions.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(skill_A)) | !time_dim %in% names(dim(skill_B))) {
    stop("Parameter 'time_dim' is not found in 'skill_A' or 'skill_B' dimensions.")
  }
  ## alpha
  if (any(!is.numeric(alpha) | alpha <= 0 | alpha >= 1 | length(alpha) > 1)) {
    stop("Parameter 'alpha' must be a number between 0 and 1.")
  }
  ## test.type
  if (!test.type %in% c('two.sided.approx', 'two.sided', 'greater', 'less')) {
    stop("Parameter 'test.type' must be 'two.sided.approx', 'two.sided', 'greater', or 'less'.")
  }
  if (test.type == 'two.sided.approx') {
    if (alpha != 0.05) {
      .warning("DelSole and Tippett (2016) aproximation is valid for alpha ",
              "= 0.05 only. Returning the significance at the 0.05 significance level.")
    } 
    if (pval) {
      .warning("p-value cannot be returned with the DelSole and Tippett (2016) ",
               "aproximation. Returning the significance at the 0.05 significance level.")
    }
    sign <- TRUE
  }
  ## N.eff
  if (is.array(N.eff)) {
    if (!is.numeric(N.eff)) stop("Parameter 'N.eff' must be numeric.")
    if (!all(names(dim(N.eff)) %in% names(dim(skill_A))) |
        any(dim(skill_A)[match(names(dim(N.eff)), names(dim(skill_A)))] != dim(N.eff))) {
      stop('If parameter "N.eff" is provided with an array, it must ',
           'have the same dimensions as "skill_A" except "time_dim".')
    }
  } else if (any((!isFALSE(N.eff) & !is.numeric(N.eff)) | length(N.eff) != 1)) {
    stop('Parameter "N.eff" must be FALSE, a numeric, or an array with ',
         'the same dimensions as "skill_A" except "time_dim".')
  }
  if (!isFALSE(N.eff) & test.type=='two.sided.approx'){
    warning('"N.eff" will not be used if "test.type" is "two.sided.approx".')
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  
  ## Compute the Random Walk Test
  if (is.array(N.eff)) {
    res <- Apply(data = list(skill_A = skill_A,
                             skill_B = skill_B,
                             N.eff = N.eff),
                 target_dims = list(skill_A = time_dim,
                                    skill_B = time_dim,
                                    N.eff = NULL),
                 fun = .RandomWalkTest,
                 test.type = test.type,
                 alpha = alpha, pval = pval, sign = sign,
                 ncores = ncores)
  } else {
    res <- Apply(data = list(skill_A = skill_A,
                             skill_B = skill_B),
                 target_dims = list(skill_A = time_dim,
                                    skill_B = time_dim),
                 fun = .RandomWalkTest,
                 test.type = test.type,
                 alpha = alpha, pval = pval, sign = sign,
                 N.eff = N.eff, ncores = ncores)
  }
 
  return(res)
}

.RandomWalkTest <- function(skill_A, skill_B, test.type = 'two.sided.approx', 
                            alpha = 0.05, pval = TRUE, N.eff = FALSE, sign = FALSE) {
  #skill_A and skill_B: [sdate]
  
  A_better <- sum(skill_B > skill_A)
  B_better <- sum(skill_B < skill_A)
  
  output <- NULL
  output$score <- A_better - B_better
  
  if (test.type == 'two.sided.approx') {
    output$sign <- abs(output$score) > (2 * sqrt(length(skill_A)))

  } else {
    
    if (isFALSE(N.eff)){N.eff <- length(skill_A)}
    
    if (!is.na(output$score) & N.eff >= A_better) {
      p.val <- binom.test(x = A_better, n = floor(N.eff), p = 0.5, 
                          conf.level = 1 - alpha, 
                          alternative = test.type)$p.value
    } else {
      p.val <- NA
    }
    
    if (pval) {
      output$p.val <- p.val
    } 
    if (sign) {
      output$sign <- !is.na(p.val) & p.val <= alpha
    }
    
  }
  
  return(output)
}
