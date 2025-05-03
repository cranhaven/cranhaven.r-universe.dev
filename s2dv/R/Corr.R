#'Compute the correlation coefficient between an array of forecast and their
#'corresponding observation
#'
#'Calculate the correlation coefficient (Pearson, Kendall or Spearman) for 
#'an array of forecast and an array of observation. The correlations are 
#'computed along 'time_dim' that usually refers to the start date dimension. If
#''comp_dim' is given, the correlations are computed only if obs along comp_dim
#'dimension are complete between limits[1] and limits[2], i.e., there is no NA 
#'between limits[1] and limits[2]. This option can be activated if the user 
#'wants to account only for the forecasts which the corresponding observations 
#'are available at all leadtimes.\cr 
#'The confidence interval is computed by the Fisher transformation and the 
#'significance level relies on an one-sided student-T distribution.\cr 
#'The function can calculate ensemble mean before correlation by 'memb_dim' 
#'specified and 'memb = F'. If ensemble mean is not calculated, correlation will
#'be calculated for each member.
#'If there is only one dataset for exp and obs, you can simply use cor() to 
#'compute the correlation.
#'
#'@param exp A named numeric array of experimental data, with at least dimension
#'  'time_dim'.
#'@param obs A named numeric array of observational data, same dimensions as  
#'  parameter 'exp' except along 'dat_dim' and 'memb_dim'.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the correlations are computed. The default value is 'sdate'.
#'@param dat_dim A character string indicating the name of dataset (nobs/nexp) 
#'  dimension. The default value is NULL (no dataset).
#'@param comp_dim A character string indicating the name of dimension along which
#'  obs is taken into account only if it is complete. The default value
#'  is NULL.
#'@param limits A vector of two integers indicating the range along comp_dim to 
#'  be completed. The default is c(1, length(comp_dim dimension)).
#'@param method A character string indicating the type of correlation: 
#'  'pearson', 'spearman', or 'kendall'. The default value is 'pearson'.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. It must be one dimension in 'exp' and 'obs'. If there is no 
#'  member dimension, set NULL. The default value is NULL.
#'@param memb A logical value indicating whether to remain 'memb_dim' dimension
#'  (TRUE) or do ensemble mean over 'memb_dim' (FALSE). Only functional when 
#'  'memb_dim' is not NULL. The default value is TRUE.
#'@param pval A logical value indicating whether to return or not the p-value 
#'  of the test Ho: Corr = 0. The default value is TRUE.
#'@param conf A logical value indicating whether to return or not the confidence 
#'  intervals. The default value is TRUE.
#'@param sign A logical value indicating whether to retrieve the statistical
#'  significance of the test Ho: Corr = 0 based on 'alpha'. The default value is
#'  FALSE.
#'@param alpha A numeric indicating the significance level for the statistical
#'  significance test. The default value is 0.05.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing the numeric arrays with dimension:\cr 
#'  c(nexp, nobs, exp_memb, obs_memb, all other dimensions of exp except 
#'  time_dim and memb_dim).\cr
#'nexp is the number of experiment (i.e., 'dat_dim' in exp), and nobs is the 
#'number of observation (i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp and 
#'nobs are omitted. exp_memb is the number of member in experiment (i.e., 
#''memb_dim' in exp) and obs_memb is the number of member in observation (i.e.,
#''memb_dim' in obs). If memb = F, exp_memb and obs_memb are omitted.\cr\cr
#'\item{$corr}{
#'  The correlation coefficient. 
#'}
#'\item{$p.val}{
#'  The p-value. Only present if \code{pval = TRUE}.
#'}
#'\item{$conf.lower}{
#'  The lower confidence interval. Only present if \code{conf = TRUE}.
#'}
#'\item{$conf.upper}{
#'  The upper confidence interval. Only present if \code{conf = TRUE}.
#'}
#'\item{$sign}{
#'  The statistical significance. Only present if \code{sign = TRUE}.
#'}
#'
#'@examples
#'# Case 1: Load sample data as in Load() example: 
#'example(Load) 
#'clim <- Clim(sampleData$mod, sampleData$obs) 
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp) 
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs) 
#'runmean_months <- 12 
#'
#'# Smooth along lead-times   
#'smooth_ano_exp <- Smoothing(ano_exp, runmeanlen = runmean_months) 
#'smooth_ano_obs <- Smoothing(ano_obs, runmeanlen = runmean_months) 
#'required_complete_row <- 3  # Discard start dates which contain any NA lead-times 
#'leadtimes_per_startdate <- 60 
#'corr <- Corr(MeanDims(smooth_ano_exp, 'member'),              
#'             MeanDims(smooth_ano_obs, 'member'),              
#'             comp_dim = 'ftime', dat_dim = 'dataset', 
#'             limits = c(ceiling((runmean_months + 1) / 2),                         
#'             leadtimes_per_startdate - floor(runmean_months / 2))) 
#'
#'# Case 2: Keep member dimension
#'corr <- Corr(smooth_ano_exp, smooth_ano_obs, memb_dim = 'member', dat_dim = 'dataset')
#'# ensemble mean
#'corr <- Corr(smooth_ano_exp, smooth_ano_obs, memb_dim = 'member', memb = FALSE,
#'             dat_dim = 'dataset')
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@importFrom stats cor pt qnorm 
#'@export
Corr <- function(exp, obs, time_dim = 'sdate', dat_dim = NULL, 
                 comp_dim = NULL, limits = NULL, method = 'pearson', 
                 memb_dim = NULL, memb = TRUE,
                 pval = TRUE, conf = TRUE, sign = FALSE,
                 alpha = 0.05, ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must be at least two dimensions ",
         "containing time_dim and dat_dim.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string or NULL.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.",
           " Set it as NULL if there is no dataset dimension.")
    }
  }
  ## comp_dim
  if (!is.null(comp_dim)) {
    if (!is.character(comp_dim) | length(comp_dim) > 1) {
      stop("Parameter 'comp_dim' must be a character string.")
    }
    if (!comp_dim %in% names(dim(exp)) | !comp_dim %in% names(dim(obs))) {
      stop("Parameter 'comp_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }
  ## limits
  if (!is.null(limits)) {
    if (is.null(comp_dim)) {
      stop("Paramter 'comp_dim' cannot be NULL if 'limits' is assigned.")
    }
    if (!is.numeric(limits) | any(limits %% 1 != 0) | any(limits < 0) | 
        length(limits) != 2 | any(limits > dim(exp)[comp_dim])) {
      stop("Parameter 'limits' must be a vector of two positive ",
           "integers smaller than the length of paramter 'comp_dim'.")
    }
  }
  ## method
  if (!(method %in% c("kendall", "spearman", "pearson"))) {
    stop("Parameter 'method' must be one of 'kendall', 'spearman' or 'pearson'.")
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
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
  }
  ## memb
  if (!is.logical(memb) | length(memb) > 1) {
    stop("Parameter 'memb' must be one logical value.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## alpha
  if (!is.numeric(alpha) | alpha < 0 | alpha > 1 | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 
  ## exp and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!is.null(memb_dim)) {
    name_exp <- name_exp[-which(name_exp == memb_dim)]
    name_obs <- name_obs[-which(name_obs == memb_dim)]
  }
  if (!identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all dimension except 'dat_dim' and 'memb_dim'.")
  }
  if (dim(exp)[time_dim] < 3) {
    stop("The length of time_dim must be at least 3 to compute correlation.")
  }


  ###############################
  # Sort dimension
  name_exp <- names(dim(exp))
  name_obs <- names(dim(obs))
  order_obs <- match(name_exp, name_obs)
  obs <- Reorder(obs, order_obs)


  ###############################
  # Calculate Corr

  # Remove data along comp_dim dim if there is at least one NA between limits
  if (!is.null(comp_dim)) {
    pos <- which(names(dim(obs)) == comp_dim)
    if (is.null(limits)) {
      obs_sub <- obs
    } else {
      obs_sub <- ClimProjDiags::Subset(obs, pos, list(limits[1]:limits[2]))
    }
    outrows <- is.na(MeanDims(obs_sub, pos, na.rm = FALSE))
    outrows <- InsertDim(outrows, pos, dim(obs)[comp_dim])
    obs[which(outrows)] <- NA
    rm(obs_sub, outrows)
  }
  if (!is.null(memb_dim)) {
    if (!memb) { #ensemble mean
      exp <- MeanDims(exp, memb_dim, na.rm = TRUE)
      obs <- MeanDims(obs, memb_dim, na.rm = TRUE)
#      name_exp <- names(dim(exp))
#      margin_dims_ind <- c(1:length(name_exp))[-which(name_exp == memb_dim)]
#      exp <- apply(exp, margin_dims_ind, mean, na.rm = TRUE) #NOTE: remove NAs here 
#      obs <- apply(obs, margin_dims_ind, mean, na.rm = TRUE)
      memb_dim <- NULL
    }
  }

  res <- Apply(list(exp, obs),
                   target_dims = list(c(time_dim, dat_dim, memb_dim),
                                      c(time_dim, dat_dim, memb_dim)),
                   fun = .Corr,
                   dat_dim = dat_dim, memb_dim = memb_dim,
                   time_dim = time_dim, method = method,
                   pval = pval, conf = conf, sign = sign, alpha = alpha,
                   ncores = ncores)

 return(res)
}

.Corr <- function(exp, obs, dat_dim = NULL, memb_dim = 'member',
		  time_dim = 'sdate', method = 'pearson',
                  conf = TRUE, pval = TRUE, sign = FALSE, alpha = 0.05) {

  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
  }

  if (is.null(memb_dim)) {
    CORR <- array(dim = c(nexp = nexp, nobs = nobs))

    if (is.null(dat_dim)) {
      # exp: [sdate]
      # obs: [sdate]
      if (!all(is.na(exp)) && sum(!is.na(obs)) > 2) {
        CORR[, ] <- cor(exp, obs, use = "pairwise.complete.obs", method = method)
      }
    } else {
      # exp: [sdate, dat_exp]
      # obs: [sdate, dat_obs]
      for (j in 1:nobs) {
        for (y in 1:nexp) {            
          if (!all(is.na(exp[, y])) && sum(!is.na(obs[, j])) > 2) {
            CORR[y, j] <- cor(exp[, y], obs[, j],
                              use = "pairwise.complete.obs",
                              method = method)
          } 
        }
      }
#----------------------------------------
# Same as above calculation. 
#TODO: Compare which is faster.
#    CORR <- sapply(1:nobs, function(i) {
#              sapply(1:nexp, function (x) {
#                if (any(!is.na(exp[, x])) && sum(!is.na(obs[, i])) > 2) {
#                  cor(exp[, x], obs[, i],
#                      use = "pairwise.complete.obs",
#                      method = method)
#                } else {
#                  NA 
#                }
#              })
#            })
#-----------------------------------------
    }

  } else {  # memb_dim != NULL
    exp_memb <- as.numeric(dim(exp)[memb_dim]) # memb_dim
    obs_memb <- as.numeric(dim(obs)[memb_dim])

    CORR <- array(dim = c(nexp = nexp, nobs = nobs, exp_memb = exp_memb, obs_memb = obs_memb))

    if (is.null(dat_dim)) {
      # exp: [sdate, memb_exp]
      # obs: [sdate, memb_obs]
      for (j in 1:obs_memb) {
        for (y in 1:exp_memb) {
              
          if (!all(is.na(exp[, y])) && sum(!is.na(obs[, j])) > 2) {
            CORR[, , y, j] <- cor(exp[, y], obs[, j],
                                  use = "pairwise.complete.obs",
                                  method = method)
          }

        }
      }
    } else {
      # exp: [sdate, dat_exp, memb_exp]
      # obs: [sdate, dat_obs, memb_obs]
      for (j in 1:obs_memb) {
        for (y in 1:exp_memb) {
          CORR[, , y, j] <- sapply(1:nobs, function(i) {
                              sapply(1:nexp, function (x) {
            if (!all(is.na(exp[, x, y])) && sum(!is.na(obs[, i, j])) > 2) {
              cor(exp[, x, y], obs[, i, j],
                  use = "pairwise.complete.obs",
                  method = method)
            } else {
              NA
            }
            })
          })

        }
      }
    }

  }


#  if (pval) {
#    for (i in 1:nobs) {
#      p.val[, i] <- try(sapply(1:nexp,
#                           function(x) {(cor.test(exp[, x], obs[, i],
#                                         use = "pairwise.complete.obs",
#                                         method = method)$p.value)/2}), silent = TRUE)
#      if (class(p.val[, i]) == 'character') {
#        p.val[, i] <- NA
#      }
#    }
#  }

  if (pval || conf || sign) {
    if (method == "kendall" | method == "spearman") {
      if (!is.null(dat_dim) | !is.null(memb_dim)) {
        tmp <- apply(obs,
                     c(seq_along(dim(obs)))[-1],
                     rank) # for memb_dim = NULL, 2; for memb_dim, c(2, 3)
        names(dim(tmp))[1] <- time_dim
        eno <- Eno(tmp, time_dim)
      } else {
        tmp <- rank(obs)
        tmp <- array(tmp)
        names(dim(tmp)) <- time_dim
        eno <- Eno(tmp, time_dim)
      }
    } else if (method == "pearson") {
      eno <- Eno(obs, time_dim)  
    }

    if (is.null(memb_dim)) {
      eno_expand <- array(dim = c(nexp = nexp, nobs = nobs))
      for (i in 1:nexp) {
        eno_expand[i, ] <- eno
      }
    } else {  #member
      eno_expand <- array(dim = c(nexp = nexp, nobs = nobs,
                                  exp_memb = exp_memb, obs_memb = obs_memb))
      for (i in 1:nexp) {
        for (j in 1:exp_memb) {
          eno_expand[i, , j, ] <- eno
        }
      }
    }

  }

#############old#################
#This doesn't return error but it's diff from cor.test() when method is spearman and kendall
  if (pval || sign) {
    t <- sqrt(CORR * CORR * (eno_expand - 2) / (1 - (CORR ^ 2)))
    p.val <- pt(t, eno_expand - 2, lower.tail = FALSE)
    if (sign) signif <- !is.na(p.val) & p.val <= alpha
  } 
###################################
  if (conf) {
    conf.lower <- alpha / 2
    conf.upper <- 1 - conf.lower
    suppressWarnings({
    conflow <- tanh(atanh(CORR) + qnorm(conf.lower) / sqrt(eno_expand - 3))
    confhigh <- tanh(atanh(CORR) + qnorm(conf.upper) / sqrt(eno_expand - 3))
    })
  }

###################################  
  # Remove nexp and nobs if dat_dim = NULL
  if (is.null(dat_dim)) {
#  if (is.null(dat_dim) & !is.null(memb_dim)) {

    if (length(dim(CORR)) == 2) {
      dim(CORR) <- NULL
      if (pval) {
        dim(p.val) <- NULL
      }
      if (conf) {
        dim(conflow) <- NULL
        dim(confhigh) <- NULL
      }
      if (sign) {
        dim(signif) <- NULL
      }
    } else {
      dim(CORR) <- dim(CORR)[3:length(dim(CORR))]
      if (pval) {
        dim(p.val) <- dim(p.val)[3:length(dim(p.val))]
      }
      if (conf) {
        dim(conflow) <- dim(conflow)[3:length(dim(conflow))]
        dim(confhigh) <- dim(confhigh)[3:length(dim(confhigh))]
      }
      if (sign) {
        dim(signif) <- dim(signif)[3:length(dim(signif))]
      }
    }
  }

###################################

  res <- list(corr = CORR)
  if (pval) {
    res <- c(res, list(p.val = p.val))
  }
  if (conf) {
    res <- c(res, list(conf.lower = conflow, conf.upper = confhigh))
  }
  if (sign) {
    res <- c(res, list(sign = signif))
  }

  return(res)

}
