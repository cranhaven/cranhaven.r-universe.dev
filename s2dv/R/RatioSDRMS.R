#'Compute the ratio between the ensemble spread and RMSE
#'
#'Compute the ratio between the standard deviation of the members around the 
#'ensemble mean in experimental data and the RMSE between the ensemble mean of 
#'experimental and observational data. The p-value is provided by a one-sided 
#'Fisher's test.
#'
#'@param exp A named numeric array of experimental data with at least two 
#'  dimensions 'memb_dim' and 'time_dim'.
#'@param obs A named numeric array of observational data with at least two 
#'  dimensions 'memb_dim' and 'time_dim'. It should have the same dimensions as
#'  parameter 'exp' except along 'dat_dim' and 'memb_dim'.
#'@param dat_dim A character string indicating the name of dataset (nobs/nexp) 
#'  dimension. The default value is NULL (no dataset).
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. It must be one dimension in 'exp' and 'obs'. The default value 
#'  is 'member'.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the ratio is computed. The default value is 'sdate'.
#'@param pval A logical value indicating whether to compute or not the p-value 
#'  of the test Ho : SD/RMSE = 1 or not. The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A list of two arrays with dimensions c(nexp, nobs, the rest of 
#'  dimensions of 'exp' and 'obs' except memb_dim and time_dim), which nexp is
#'  the length of dat_dim of 'exp' and nobs is the length of dat_dim of 'obs'. 
#'  If dat_dim is NULL, nexp and nobs are omitted. \cr
#'\item{$ratio}{
#'  The ratio of the ensemble spread and RMSE.
#'}
#'\item{$p_val}{
#'  The p-value of the one-sided Fisher's test with Ho: SD/RMSE = 1. Only present
#'  if \code{pval = TRUE}.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'rsdrms <- RatioSDRMS(sampleData$mod, sampleData$obs, dat_dim = 'dataset')
#'# Reorder the data in order to plot it with PlotVsLTime
#'rsdrms_plot <- array(dim = c(dim(rsdrms$ratio)[1:2], 4, dim(rsdrms$ratio)[3]))
#'rsdrms_plot[, , 2, ] <- rsdrms$ratio
#'rsdrms_plot[, , 4, ] <- rsdrms$p.val
#'\dontrun{
#'PlotVsLTime(rsdrms_plot, toptitle = "Ratio ensemble spread / RMSE", ytitle = "", 
#'            monini = 11, limits = c(-1, 1.3), listexp = c('CMIP5 IC3'), 
#'            listobs = c('ERSST'), biglab = FALSE, siglev = TRUE) 
#'}
#'
#'@import multiApply
#'@export
RatioSDRMS <- function(exp, obs, dat_dim = NULL, memb_dim = 'member', 
                       time_dim = 'sdate', pval = TRUE, ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must be array with as least two ",
         "dimensions memb_dim and time_dim.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }
  ## memb_dim
  if (!is.character(memb_dim) | length(memb_dim) > 1) {
    stop("Parameter 'memb_dim' must be a character string.")
  }
  if (!memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
    stop("Parameter 'memb_dim' is not found in 'exp' nor 'obs' dimension. ",
         "Set it as NULL if there is no member dimension.")
  }
  # Add [member = 1] 
  if (memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
    dim(obs) <- c(dim(obs), 1)
    names(dim(obs))[length(dim(obs))] <- memb_dim
  }
  if (!memb_dim %in% names(dim(exp)) & memb_dim %in% names(dim(obs))) {
    dim(exp) <- c(dim(exp), 1)
    names(dim(exp))[length(dim(exp))] <- memb_dim
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## exp and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  name_exp <- name_exp[-which(name_exp == memb_dim)]
  name_obs <- name_obs[-which(name_obs == memb_dim)]
  if (!identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all the dimensions except 'dat_dim' and 'memb_dim'.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 


  ###############################
  # Calculate RatioSDRMS

  # If dat_dim = NULL, insert dat dim
  remove_dat_dim <- FALSE
  if (is.null(dat_dim)) {
    dat_dim <- 'dataset'
    exp <- InsertDim(exp, posdim = 1, lendim = 1, name = 'dataset')
    obs <- InsertDim(obs, posdim = 1, lendim = 1, name = 'dataset')
    remove_dat_dim <- TRUE
  }

  res <- Apply(list(exp, obs), 
               target_dims = list(c(dat_dim, memb_dim, time_dim), 
                                  c(dat_dim, memb_dim, time_dim)),
               pval = pval,  
               fun = .RatioSDRMS, 
               ncores = ncores)

  if (remove_dat_dim) {
    if (length(dim(res[[1]])) > 2) {
      res <- lapply(res, Subset, c('nexp', 'nobs'), list(1, 1), drop = 'selected')
    } else {
      res <- lapply(res, as.numeric)
    }
  }

  return(res)
}

.RatioSDRMS <- function(exp, obs, pval = TRUE) {

  # exp: [dat_exp, member, sdate]
  # obs: [dat_obs, member, sdate]
  nexp <- dim(exp)[1]
  nobs <- dim(obs)[1]

  # ensemble mean
  ens_exp <- MeanDims(exp, 2, na.rm = TRUE) # [dat, sdate]
  ens_obs <- MeanDims(obs, 2, na.rm = TRUE)

  dif <- exp - InsertDim(ens_exp, 2, dim(exp)[2]) # [nexp, member, sdate]
  std <- apply(dif, 1, sd, na.rm = TRUE) # [nexp]
  enosd <- rowSums(Eno(dif, names(dim(exp))[3]), na.rm = TRUE)
  
  # Create empty arrays
  ratiosdrms <- array(dim = c(nexp = as.numeric(nexp), nobs = as.numeric(nobs)))  # [nexp, nobs]
  p.val <- array(dim = c(nexp = as.numeric(nexp), nobs = as.numeric(nobs)))  # [nexp, nobs]

  for (jexp in 1:nexp) {
    for (jobs in 1:nobs) {
      dif <- ens_exp[jexp, ] - ens_obs[jobs, ]
      rms <- mean(dif^2, na.rm = TRUE)^0.5
      enorms <- Eno(dif)
      ratiosdrms[jexp, jobs] <- std[jexp] / rms

      if (pval) {
        F <- (enosd[jexp] * std[jexp]^2 / (enosd[jexp] - 1)) / (enorms * rms^2 / (enorms - 1))
        if (!is.na(F) & !is.na(enosd[jexp]) & !is.na(enorms) & any(enosd > 2) & enorms > 2) {
          p.val[jexp, jobs] <- 1 - pf(F, enosd[jexp] - 1, enorms - 1)
        } else {
          ratiosdrms[jexp, jobs] <- NA
        }
      }
    }
  }

  if (pval) {
    return(invisible(list(ratio = ratiosdrms, p.val = p.val)))
  } else {
    return(invisible(list(ratio = ratiosdrms)))
  }
}
