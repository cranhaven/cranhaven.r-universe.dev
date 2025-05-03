#'Compute Brier scores
#'
#'Interface to compute probabilistic scores (Brier Score, Brier Skill Score) 
#'from the forecast and observational data anomalies. It provides six types
#'to choose.
#'
#'@param exp A numeric array of forecast anomalies with named dimensions that
#'  at least include 'memb_dim', and 'time_dim'. It can be provided
#'  by \code{Ano()}. 
#'@param obs A numeric array of observational reference anomalies with named
#'  dimensions that at least include 'time_dim'. If it has 
#'  'memb_dim', the length  must be 1. The dimensions should be consistent with
#'  'exp' except 'dat_dim' and 'memb_dim'. It can be provided by \code{Ano()}. 
#'@param dat_dim A character string indicating the name of the dataset 
#'  dimension in 'exp' and 'obs'. The default value is NULL (no dataset).
#'  dimension, set NULL.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension in 'exp' (and 'obs') for ensemble mean calculation. The default
#'  value is 'member'.
#'@param time_dim A character string indicating the dimension along which to 
#'  compute the probabilistic scores. The default value is 'sdate'.
#'@param quantile A logical value to decide whether a quantile (TRUE) or a 
#'  threshold (FALSE) is used to estimate the forecast and observed 
#'  probabilities. If 'type' is 'FairEnsembleBS' or 'FairEnsembleBSS', it must
#'  be TRUE. The default value is TRUE.
#'@param thr A numeric vector to be used in probability calculation (for 'BS', 
#'  'FairStartDatesBS', 'BSS', and 'FairStartDatesBSS') and binary event 
#'  judgement (for 'FairEnsembleBS' and 'FairEnsembleBSS'). It is as
#'  quantiles if 'quantile' is TRUE or as thresholds if 'quantile' is FALSE. 
#'  The default value is \code{c(0.05, 0.95)} for 'quantile = TRUE'.
#'@param type A character string of the desired score type. It can be the 
#'  following values:
#'\itemize{
#'  \item{'BS': Simple Brier Score. Use SpecsVerification::BrierDecomp inside.}
#'  \item{'FairEnsembleBS': Corrected Brier Score computed across ensemble 
#'    members. Use SpecsVerification::FairBrier inside.}
#'  \item{'FairStartDatesBS': Corrected Brier Score computed across starting 
#'    dates. Use s2dv:::.BrierScore inside.}
#'  \item{'BSS': Simple Brier Skill Score. Use s2dv:::.BrierScore inside.}
#'  \item{'FairEnsembleBSS': Corrected Brier Skill Score computed across 
#'    ensemble members. Use SpecsVerification::FairBrierSs inside.}
#'  \item{'FairStartDatesBSS': Corrected Brier Skill Score computed across 
#'    starting dates. Use s2dv:::.BrierScore inside.}
#'}
#'  The default value is 'BS'.
#'@param decomposition A logical value to determine whether the decomposition 
#'  of the Brier Score should be provided (TRUE) or not (FALSE). It is only 
#'  used when 'type' is 'BS' or 'FairStartDatesBS'. The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'If 'type' is 'BS' or 'FairStartDatesBS' and 'decomposition' is TRUE, the 
#'output is a list of 4 arrays (see details below.) In other cases, the output 
#'is an array of Brier scores or Brier skill scores. All the arrays have the 
#'same dimensions:
#'c(nexp, nobs, no. of bins, the rest dimensions of 'exp' except 'time_dim' and
#''memb_dim'). 'nexp' and 'nobs' is the length of dataset dimension in 'exp'
#'and 'obs' respectively. If dat_dim is NULL, nexp and nobs are omitted.\cr
#'The list of 4 includes: 
#'  \itemize{
#'    \item{$bs: Brier Score}
#'    \item{$rel: Reliability component}
#'    \item{$res: Resolution component}
#'    \item{$unc: Uncertainty component}
#'  }
#'
#'@examples
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                     leadtimemin = 1,
#'                                     leadtimemax = 4,
#'                                     output = 'lonlat',
#'                                     latmin = 27, latmax = 48,
#'                                     lonmin = -12, lonmax = 40)
#'  }
#'sampleData$mod <- Season(sampleData$mod, monini = 11, moninf = 12, monsup = 2)
#'sampleData$obs <- Season(sampleData$obs, monini = 11, moninf = 12, monsup = 2)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'exp <- Ano(sampleData$mod, clim$clim_exp)
#'obs <- Ano(sampleData$obs, clim$clim_obs)
#'bs <- UltimateBrier(exp, obs, dat_dim = 'dataset')
#'bss <- UltimateBrier(exp, obs, type = 'BSS', dat_dim = 'dataset')
#'
#'@import SpecsVerification plyr multiApply
#'@export
UltimateBrier <- function(exp, obs, dat_dim = NULL, memb_dim = 'member', time_dim = 'sdate', 
                          quantile = TRUE, thr = c(5/100, 95/100), type = 'BS', 
                          decomposition = TRUE, ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a vector or a numeric array.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string or NULL.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }

  ## memb_dim
  if (!is.character(memb_dim) | length(memb_dim) > 1) {
    stop("Parameter 'memb_dim' must be a character string.")
  }
  if (!memb_dim %in% names(dim(exp))) {
    stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
  }
  if (!memb_dim %in% names(dim(obs))) {
    # Insert memb_dim into obs for the ease of later calculation
    obs <- InsertDim(obs, posdim = 2, lendim = 1, name = memb_dim)
  } else if (dim(obs)[memb_dim] != 1) {
    stop("The length of parameter 'memb_dim' in 'obs' must be 1.") 
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
  name_exp <- name_exp[-which(name_exp == memb_dim)]
  name_obs <- name_obs[-which(name_obs == memb_dim)]
  name_exp <- name_exp[-which(name_exp == dat_dim)]
  name_obs <- name_obs[-which(name_obs == dat_dim)]
  if (any(name_exp != name_obs)) {
    stop("Parameter 'exp' and 'obs' must have the same names and lengths ",
         "of all the dimensions except 'dat_dim' and 'memb_dim'.")
  }
  if (!all(dim(exp)[name_exp] == dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have the same names and lengths ",
         "of all the dimensions except 'dat_dim' and 'memb_dim'.")
  }
  ## quantile
  if (!is.logical(quantile) | length(quantile) > 1) {
    stop("Parameter 'quantile' must be one logical value.")
  }
  ## thr
  if (!is.numeric(thr) | !is.vector(thr)) {
    stop("Parameter 'thr' must be a numeric vector.")
  }
  if (quantile && !all(thr < 1 & thr > 0)) {
    stop("Parameter 'thr' must be between 0 and 1 when quantile is TRUE.")
  }
  if (!quantile & (type %in% c('FairEnsembleBSS', 'FairEnsembleBS'))) {
    stop("Parameter 'quantile' must be TRUE if 'type' is 'FairEnsembleBSS' or 'FairEnsembleBS'.")
  }
  ## type
  if (!(type %in% c("BS", "BSS", "FairEnsembleBS", "FairEnsembleBSS",
                    "FairStartDatesBS", "FairStartDatesBSS"))) {
    stop("Parameter 'type' must be one of 'BS', 'BSS', 'FairEnsembleBS', ",
         "'FairEnsembleBSS', 'FairStartDatesBS' or 'FairStartDatesBSS'.")
  }
  ## decomposition
  if (!is.logical(decomposition) | length(decomposition) > 1) {
    stop("Parameter 'decomposition' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Calculate UltimateBrier 

  if (type %in% c('FairEnsembleBSS', 'FairEnsembleBS')) {
    res <- Apply(list(exp, obs),
                 target_dims = list(c(time_dim, dat_dim, memb_dim),
                                    c(time_dim, dat_dim, memb_dim)),
                 fun = .UltimateBrier,
                 dat_dim = dat_dim, memb_dim = memb_dim,
                 thr = thr, type = type,
                 decomposition = decomposition,
                 ncores = ncores)$output1

  } else {
    # Calculate probablities by ProbBins() and ensemble mean first.
    # The first dim will become 'bin' and memb_dim is gone.
    exp <- MeanDims(
             ProbBins(exp, thr = thr, time_dim = time_dim, memb_dim = memb_dim,
                      quantile = quantile, ncores = ncores),
             memb_dim)
    obs <- MeanDims(
             ProbBins(obs, thr = thr, time_dim = time_dim, memb_dim = memb_dim,
                      quantile = quantile, ncores = ncores),
             memb_dim)

    res <- Apply(list(exp, obs),
                 target_dims = list(c(time_dim, dat_dim),
                                    c(time_dim, dat_dim)),
                 fun = .UltimateBrier,
                 dat_dim = dat_dim, memb_dim = memb_dim,
                 thr = thr, type = type,
                 decomposition = decomposition,
                 ncores = ncores)

    if (type %in% c('BSS', 'FairStartDatesBSS')) {
      res <- res$output1
    } else if (!decomposition) {
      res <- res$bs
    }
  }

  return(res)
}

.UltimateBrier <- function(exp, obs, dat_dim = NULL, memb_dim = 'member', thr = c(5/100, 95/100), 
                           type = 'BS', decomposition = TRUE) {
  # If exp and obs are probablistics
  # exp: [sdate, nexp]
  # obs: [sdate, nobs]
  # If exp and obs are anomalies
  # exp: [sdate, nexp, memb]
  # obs: [sdate, nobs, memb]
  
  #NOTE: 'thr' is used in 'FairEnsembleBSS' and 'FairEnsembleBS'. But if quantile = F and 
  #      thr is real value, does it work?
  if (type == 'FairEnsembleBSS') {
    if (is.null(dat_dim)) {
      obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
      exp <- InsertDim(exp, posdim = 2, lendim = 1, name = 'dataset')
    }
    size_ens_ref <- prod(dim(obs)[c(1, 3)])
    res <- array(dim = c(nexp = as.numeric(dim(exp)[2]), 
                         nobs = as.numeric(dim(obs)[2]), 
                         bin = length(thr) + 1))
    for (n_exp in seq_len(dim(exp)[2])) {
      for (n_obs in seq_len(dim(obs)[2])) {
        ens_ref <- matrix(obs[, n_obs, 1], size_ens_ref, size_ens_ref, byrow = TRUE)
        for (n_thr in seq_along(c(thr, 1))) {
          #NOTE: FairBreirSs is deprecated now. Should change to SkillScore (according to 
          #      SpecsVerification's documentation)
          res[n_exp, n_obs, n_thr] <- 
            SpecsVerification::FairBrierSs(exp[, n_exp, ] > c(thr, 1)[n_thr],
                                           ens_ref > c(thr, 1)[n_thr],
                                           obs[, n_obs, 1] > c(thr, 1)[n_thr])['skillscore']
        }
      }
    }
    if (is.null(dat_dim)) {
      dim(res) <- dim(res)[3:length(dim(res))]
    }

  } else if (type == 'FairEnsembleBS') {
    #NOTE: The calculation in s2dverification::UltimateBrier is wrong. In the final stage,
    #      the function calculates like 
    #      "take(result, 3, 1) - take(result, 3, 2) + take(result, 3, 3)",
    #      but the 3rd dim of result is 'bins' instead of decomposition. 'FairEnsembleBS' does
    #      not have decomposition.
    #      The calculation is fixed here.
    if (is.null(dat_dim)) {
      obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
      exp <- InsertDim(exp, posdim = 2, lendim = 1, name = 'dataset')
    }
    res <- array(dim = c(nexp = as.numeric(dim(exp)[2]), 
                         nobs = as.numeric(dim(obs)[2]), 
                         bin = length(thr) + 1))
    for (n_exp in seq_len(dim(exp)[2])) {
      for (n_obs in seq_len(dim(obs)[2])) {
        for (n_thr in seq_along(c(thr, 1))) {
          fb <- SpecsVerification::FairBrier(ens = exp[, n_exp, ] > c(thr, 1)[n_thr], 
                                             obs = obs[, n_obs, 1] > c(thr, 1)[n_thr])
          res[n_exp, n_obs, n_thr] <- mean(fb, na.rm = T)
        }
      }
    }
    if (is.null(dat_dim)) {
      dim(res) <- dim(res)[3:length(dim(res))]
    }
#    tmp <- res[, , 1] - res[, , 2] + res[, , 3]
#    res <- array(tmp, dim = c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2])))

  } else if (type == 'BS') {
    if (is.null(dat_dim)) {
      obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
      exp <- InsertDim(exp, posdim = 2, lendim = 1, name = 'dataset')
    }
    comp <- array(dim = c(nexp = as.numeric(dim(exp)[2]),
                          nobs = as.numeric(dim(obs)[2]),
                          comp = 3))
    for (n_exp in seq_len(dim(exp)[2])) {
      for (n_obs in seq_len(dim(obs)[2])) {
        #NOTE: Parameter 'bins' is default.
        comp[n_exp, n_obs, ] <- SpecsVerification::BrierDecomp(p = exp[, n_exp], 
                                                               y = obs[, n_obs])[1, ]
      }
    }
    if (decomposition) {
      rel <- comp[, , 1]
      res <- comp[, , 2]
      unc <- comp[, , 3]
      bs <- rel - res + unc
      if (is.null(dat_dim)) {
        dim(rel) <- NULL
        dim(res) <- NULL
        dim(unc) <- NULL
        dim(bs) <- NULL
      } else {
        dim(rel) <- c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2]))
        dim(res) <- c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2]))
        dim(unc) <- c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2]))
        dim(bs) <- c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2]))
      }
      res <- list(bs = bs, rel = rel, res = res, unc = unc)
    } else {
      bs <- comp[, , 1] - comp[, , 2] + comp[, , 3]
      if (is.null(dat_dim)) {
        dim(bs) <- NULL
      } else {
        dim(bs) <- c(nexp = as.numeric(dim(exp)[2]), nobs = as.numeric(dim(obs)[2]))
      }
      res <- list(bs = bs)
    }

  } else if (type == 'FairStartDatesBS') {
    #NOTE: parameter 'thresholds' is not specified.
    res <- .BrierScore(exp = exp, obs = obs)
    if (decomposition) {
      res <- list(bs = res$bs, rel = res$rel, res = res$res, unc = res$unc)
    } else {
      res <- list(bs = res$bs)
    }

  } else if (type == 'BSS') {
    #NOTE: parameter 'thresholds' is not specified.
    res <- .BrierScore(exp = exp, obs = obs)$bss_res

  } else if (type == 'FairStartDatesBSS') {
    #NOTE: parameter 'thresholds' is not specified.
    res <- .BrierScore(exp = exp, obs = obs)$bss_gres
  }
  
  return(res)

}

