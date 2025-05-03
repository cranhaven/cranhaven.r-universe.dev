#'Compute the residual correlation and its significance
#'
#'The residual correlation assesses whether a forecast captures any of the 
#'observed variability that is not already captured by a reference forecast 
#'(Smith et al., 2019; https://doi.org/10.1038/s41612-019-0071-y.).
#'The procedure is as follows: the residuals of the forecasts and observations 
#'are computed by linearly regressing out the reference forecast's ensemble mean
#'from the forecasts' ensemble mean and observations, respectively. Then, the 
#'residual correlation is computed as the correlation between both residuals. 
#'Positive values of the residual correlation indicate that the forecast capture
#'more observed variability than the reference forecast, while negative values  
#'mean that the reference forecast capture more. The significance of the 
#'residual correlation is computed with a two-sided t-test
#'(Wilks, 2011; https://doi.org/10.1016/B978-0-12-385022-5.00008-7) using an 
#'effective degrees of freedom to account for the time series' autocorrelation 
#'(von Storch and Zwiers, 1999; https://doi.org/10.1017/CBO9780511612336).
#'
#'@param exp A named numerical array of the forecast with at least time 
#'  dimension. 
#'@param obs A named numerical array of the observations with at least time 
#'  dimension. The dimensions must be the same as "exp" except 'memb_dim'.
#'@param ref A named numerical array of the reference forecast data with at 
#'  least time dimension. The dimensions must be the same as "exp" except 
#'  'memb_dim'.
#'@param N.eff Effective sample size to be used in the statistical significance
#'  test. It can be NA (and it will be computed with the s2dv:::.Eno), a 
#'  numeric (which is used for all cases), or an array with the same dimensions
#'  as "obs" except "time_dim" (for a particular N.eff to be used for each case)
#'  . The default value is NA.
#'@param time_dim A character string indicating the name of the time dimension.
#'  The default value is 'year'.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the ensemble mean of the forecast and reference forecast. If it 
#'  is NULL, the ensemble mean should be provided directly to the function. The
#'  default value is NULL.
#'@param method A character string indicating the correlation coefficient to be
#'  computed ("pearson", "kendall", or "spearman"). The default value is 
#'  "pearson".
#'@param alpha A numeric of the significance level to be used in the statistical
#'  significance test (output "sign"). The default value is 0.05.
#'@param handle.na A charcater string indicating how to handle missing values.
#'  If "return.na", NAs will be returned for the cases that contain at least one
#'  NA in "exp", "ref", or "obs". If "only.complete.triplets", only the time 
#'  steps with no missing values in all "exp", "ref", and "obs" will be used. If
#'  "na.fail", an error will arise if any of "exp", "ref", or "obs" contains any
#'  NA. The default value is "return.na".
#'@param pval A logical value indicating whether to return the p-value of the
#'  significance test Ho: DiffCorr = 0. The default value is TRUE.
#'@param sign A logical value indicating whether to return the statistical
#'  significance of the test Ho: DiffCorr = 0 based on 'alpha'. The default
#'  value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A list with:
#'\item{$res.corr}{
#'  A numerical array of the residual correlation with the same dimensions as 
#'  the input arrays except "time_dim" (and "memb_dim" if provided).
#'}
#'\item{$sign}{
#'  A logical array indicating whether the residual correlation is statistically 
#'  significant or not with the same dimensions as the input arrays except "time_dim"
#'  (and "memb_dim" if provided). Returned only if "sign" is TRUE.
#'}
#'\item{$p.val}{
#'  A numeric array of the p-values with the same dimensions as the input arrays
#'  except "time_dim" (and "memb_dim" if provided). Returned only if "pval" is
#'  TRUE.
#'}
#'
#'@examples
#' exp <- array(rnorm(1000), dim = c(lat = 3, lon = 2, member = 10, sdate = 50))
#' obs <- array(rnorm(1000), dim = c(lat = 3, lon = 2, sdate = 50))
#' ref <- array(rnorm(1000), dim = c(lat = 3, lon = 2, member = 5, sdate = 50))
#' res <- ResidualCorr(exp = exp, obs = obs, ref = ref, memb_dim = 'member')
#'
#'@import multiApply
#'@export
ResidualCorr <- function(exp, obs, ref, N.eff = NA, time_dim = 'sdate',
                         memb_dim = NULL, method = 'pearson', alpha = 0.05, 
                         handle.na = 'return.na', pval = TRUE, sign = FALSE, ncores = NULL) {

  # Check inputs
  ## exp, ref, and obs (1)
  if (!is.array(exp) | !is.numeric(exp))
    stop('Parameter "exp" must be a numeric array.')
  if (!is.array(obs) | !is.numeric(obs))
    stop('Parameter "obs" must be a numeric array.')
  if (!is.array(ref) | !is.numeric(ref))
    stop('Parameter "ref" must be a numeric array.')

  ## N.eff
  if (is.array(N.eff)) {
    if (!is.numeric(N.eff)) stop("Parameter 'N.eff' must be numeric.")
    if (!all(names(dim(N.eff)) %in% names(dim(obs))) |
        any(dim(obs)[match(names(dim(N.eff)), names(dim(obs)))] != dim(N.eff))) {
      stop('If parameter "N.eff" is provided with an array, it must ',
           'have the same dimensions as "obs" except "time_dim".')
    }
  } else if (any((!is.na(N.eff) & !is.numeric(N.eff)) | length(N.eff) != 1)) {
    stop('Parameter "N.eff" must be NA, a numeric, or an array with ',
         'the same dimensions as "obs" except "time_dim".')
  }
  
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1)
    stop('Parameter "time_dim" must be a character string.')
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs)) | 
      !time_dim %in% names(dim(ref))) {
    stop("Parameter 'time_dim' is not found in 'exp', 'obs', or 'ref' dimension.")
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp)) | !memb_dim %in% names(dim(ref))) {
      stop("Parameter 'memb_dim' is not found in 'exp' or 'ref' dimension.")
    }
  }
  ## exp, ref, and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  name_ref <- sort(names(dim(ref)))
  if (!is.null(memb_dim)) {
    name_exp <- name_exp[-which(name_exp == memb_dim)]
    name_ref <- name_ref[-which(name_ref == memb_dim)]
  }
  if (length(name_exp) != length(name_obs) | 
      length(name_exp) != length(name_ref) |
      any(dim(exp)[name_exp] != dim(obs)[name_obs]) |
      any(dim(exp)[name_exp] != dim(ref)[name_ref])) {
    stop("Parameter 'exp', 'obs', and 'ref' must have same length of ",
         "all dimensions except 'memb_dim'.")
  }
  ## method
  if (!method %in% c("pearson", "kendall", "spearman")) {
    stop('Parameter "method" must be "pearson", "kendall", or "spearman".')
  }
  ## alpha
  if (sign & any(!is.numeric(alpha) | alpha <= 0 | alpha >= 1 | length(alpha) > 1)) {
    stop('Parameter "alpha" must be a number between 0 and 1.')
  }
  ## handle.na
  if (!handle.na %in% c('return.na', 'only.complete.triplets', 'na.fail')) {
    stop('Parameter "handle.na" must be "return.na", "only.complete.triplets" or "na.fail".')
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (any(!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
        length(ncores) > 1)) {
      stop('Parameter "ncores" must be either NULL or a positive integer.')
    }
  }

  ###############################

  # NA check: na.fail
  if (handle.na == "na.fail" & (anyNA(exp) | anyNA(obs) | anyNA(ref)))
    stop('The data contain NAs.')

  # Calculate ensemble mean
  dim_exp <- dim(exp)
  dim_ref <- dim(ref)

  if (!is.null(memb_dim)) {
    exp_memb_dim_ind <- which(names(dim_exp) == memb_dim)
    ref_memb_dim_ind <- which(names(dim_ref) == memb_dim)
    exp <- apply(exp, c(seq_along(dim_exp))[-exp_memb_dim_ind], mean, na.rm = FALSE)
    ref <- apply(ref, c(seq_along(dim_ref))[-ref_memb_dim_ind], mean, na.rm = FALSE)
    if (is.null(dim(exp))) exp <- array(exp, dim = c(dim_exp[time_dim]))
    if (is.null(dim(ref))) ref <- array(ref, dim = c(dim_ref[time_dim]))
  }

  # output_dims
  output_dims <- list(res.corr = NULL)
  if (pval) {
    output_dims <- c(output_dims, list(p.val = NULL))
  }
  if (sign) {
    output_dims <- c(output_dims, list(sign = NULL))
  } 

  # Residual correlation
  if (is.array(N.eff)) {
    output <- Apply(data = list(exp = exp, obs = obs, ref = ref,
                                N.eff = N.eff),
                    target_dims = list(exp = time_dim, obs = time_dim,
                                       ref = time_dim, N.eff = NULL),
                    output_dims = output_dims,
                    fun = .ResidualCorr, method = method,
                    alpha = alpha, handle.na = handle.na, pval = pval, sign = sign,
                    ncores = ncores)
  } else { 
    output <- Apply(data = list(exp = exp, obs = obs, ref = ref),
                    target_dims = list(exp = time_dim, obs = time_dim, 
                                       ref = time_dim), 
                    output_dims = output_dims, N.eff = N.eff,
                    fun = .ResidualCorr, method = method, 
                    alpha = alpha, handle.na = handle.na, pval = pval, sign = sign,
                    ncores = ncores)
  }

  return(output)
}

.ResidualCorr <- function(exp, obs, ref, N.eff = NA, method = 'pearson', alpha = 0.05, 
                          handle.na = 'return.na', pval = TRUE, sign = FALSE) {
  # exp and ref and obs: [time]
  .residual.corr <- function(exp, obs, ref, N.eff = NA, method = 'pearson', alpha = 0.05,
                             pval = TRUE, sign = FALSE) {

    # Residuals of 'exp' and 'obs' (regressing 'ref' out in both 'exp' and 'obs')
    exp_res <- lm(formula = y ~ x, data = list(y = exp, x = ref), na.action = NULL)$residuals
    obs_res <- lm(formula = y ~ x, data = list(y = obs, x = ref), na.action = NULL)$residuals
    
    # Residual correlation (and significance)
    output <- NULL
    output$res.corr <- cor(x = exp_res, y = obs_res, method = method)
    
    # Effective degrees of freedom
    if (is.na(N.eff)) {
      N.eff <- .Eno(x = obs_res, na.action = na.pass)
    }  
    t <- abs(output$res.corr) * sqrt(N.eff - 2) / sqrt(1 - output$res.corr^2)
    
    if (pval | sign) { # p-value
      p.value <- pt(q = t, df = N.eff - 2, lower.tail = FALSE)
    } 
    if (pval) {
      output$p.val <- p.value
    }
    if (sign) {
      t_alpha2_n2 <- qt(p = alpha / 2, df = N.eff - 2, lower.tail = FALSE)
      if (!anyNA(c(t, t_alpha2_n2)) & t >= t_alpha2_n2) {
        output$sign <- TRUE
      } else {
        output$sign <- FALSE
      }
    }
    
    return(output)
  }


  #==================================================
  
  if (anyNA(exp) | anyNA(obs) | anyNA(ref)) { ## There are NAs 
    if (handle.na == 'only.complete.triplets') {
      nna <- is.na(exp) | is.na(obs) | is.na(ref) # A vector of T/F
      if (all(nna)) stop("There is no complete set of forecasts and observations.")
      # Remove the incomplete set
      exp <- exp[!nna]
      obs <- obs[!nna]
      ref <- ref[!nna]

      output <- .residual.corr(exp = exp, obs = obs, ref = ref, method = method, 
                               N.eff = N.eff, alpha = alpha, pval = pval, sign = sign)
      
    } else if (handle.na == 'return.na') {
      # Data contain NA, return NAs directly without passing to .residual.corr
      output <- list(res.corr = NA)
      if (pval) {
        output <- c(output, list(p.val = NA))
      }
      if (sign) {
        output <- c(output, list(sign = NA))
      }
    }
    
  } else { ## There is no NA  
    output <- .residual.corr(exp = exp, obs = obs, ref = ref, method = method, 
                             N.eff = N.eff, alpha = alpha, pval = pval, sign = sign)
  }

  return(output)
}
