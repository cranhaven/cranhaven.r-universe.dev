#'Compute anomalies in cross-validation mode
#'
#'Compute the anomalies from the arrays of the experimental and observational 
#'data output by subtracting the climatologies computed with a leave-one-out 
#'cross validation technique and a per-pair method (Garcia-Serrano and 
#'Doblas-Reyes, CD, 2012).
#'Per-pair climatology means that only the start dates covered by the 
#'whole experiments/observational datasets will be used. In other words, the 
#'startdates which do not all have values along 'dat_dim' dimension of both
#'the 'exp' and 'obs' are excluded when computing the climatologies.
#'
#'@param exp A named numeric array of experimental data, with at least 
#'  dimensions 'time_dim' and 'dat_dim'.
#'@param obs A named numeric array of observational data, same dimensions as
#'  parameter 'exp' except along 'dat_dim'.
#'@param time_dim A character string indicating the name of the time dimension.  
#'  The default value is 'sdate'.
#'@param dat_dim A character vector indicating the name of the dataset and 
#'  member dimensions. When calculating the climatology, if data at one 
#'  startdate (i.e., 'time_dim') is not complete along 'dat_dim', this startdate
#'  along 'dat_dim' will be discarded. If there is no dataset dimension, it can be NULL.
#' The default value is 
#'  "c('dataset', 'member')".
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. Only used when parameter 'memb' is FALSE. It must be one element
#'  in 'dat_dim'. The default value is 'member'.
#'@param memb A logical value indicating whether to subtract the climatology 
#'  based on the individual members (TRUE) or the ensemble mean over all
#'  members (FALSE) when calculating the anomalies. The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list of 2:
#'\item{$exp}{
#'  A numeric array with the same dimensions as 'exp'. The dimension order may
#'  change.
#'}
#'\item{$obs}{
#'  A numeric array with the same dimensions as 'obs'.The dimension order may
#'  change.
#'}
#'
#'@examples 
#'# Load sample data as in Load() example:
#'example(Load)
#'anomalies <- Ano_CrossValid(sampleData$mod, sampleData$obs)
#'\dontrun{
#'PlotAno(anomalies$exp, anomalies$obs, startDates, 
#'        toptitle = paste('anomalies'), ytitle = c('K', 'K', 'K'), 
#'        legends = 'ERSST', biglab = FALSE)
#'}
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
Ano_CrossValid <- function(exp, obs, time_dim = 'sdate', dat_dim = c('dataset', 'member'),
                           memb_dim = 'member', memb = TRUE, ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must have at least dimensions ",
         "time_dim and dat_dim.")
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
  ## memb
  if (!is.logical(memb) | length(memb) > 1) {
    stop("Parameter 'memb' must be one logical value.")
  }
  ## memb_dim
  if (!memb) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
      stop("Parameter 'memb_dim' is not found in 'exp' nor 'obs' dimension. ", 
           "Set it as NULL if there is no member dimension.")
    }
#    # Add [member = 1] 
#    if (memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
#      dim(obs) <- c(dim(obs), 1)
#      names(dim(obs))[length(dim(obs))] <- memb_dim
#    }
#    if (!memb_dim %in% names(dim(exp)) & memb_dim %in% names(dim(obs))) {
#      dim(exp) <- c(dim(exp), 1)
#      names(dim(exp))[length(dim(exp))] <- memb_dim
#    }
  }

  ## dat_dim
  reset_obs_dim <- reset_exp_dim <- FALSE
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim)) {
      stop("Parameter 'dat_dim' must be a character vector.")
    }
    if (!any(dat_dim %in% names(dim(exp))) & !any(dat_dim %in% names(dim(obs)))) {
      stop("Parameter 'dat_dim' is not found in 'exp' nor 'obs' dimension.",
           " Set it as NULL if there is no dataset dimension.")
    }
    # If dat_dim is not in obs, add it in
    if (!all(dat_dim %in% names(dim(obs)))) {
      reset_obs_dim <- TRUE
      ori_obs_dim <- dim(obs)
      dim(obs) <- c(dim(obs), rep(1, length(dat_dim[which(!dat_dim %in% names(dim(obs)))])))
      names(dim(obs)) <- c(names(ori_obs_dim), dat_dim[which(!dat_dim %in% names(dim(obs)))])
    }
    # If dat_dim is not in obs, add it in
    if (!all(dat_dim %in% names(dim(exp)))) {
      reset_exp_dim <- TRUE
      ori_exp_dim <- dim(exp)
      dim(exp) <- c(dim(exp), rep(1, length(dat_dim[which(!dat_dim %in% names(dim(exp)))])))
      names(dim(exp)) <- c(names(ori_exp_dim), dat_dim[which(!dat_dim %in% names(dim(exp)))])
    }
  }
  # memb_dim and dat_dim
  if (!memb && !memb_dim %in% dat_dim) {
    stop("Parameter 'memb_dim' must be one element in parameter 'dat_dim'.")
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
    for (i in seq_along(dat_dim)) {
      name_exp <- name_exp[-which(name_exp == dat_dim[i])]
      name_obs <- name_obs[-which(name_obs == dat_dim[i])]
    }
  }
  if (!identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have the same length of ",
         "all dimensions except 'dat_dim'.")
  }

  ###############################
  # Sort dimension
  name_exp <- names(dim(exp))
  name_obs <- names(dim(obs))
  order_obs <- match(name_exp, name_obs)
  obs <- Reorder(obs, order_obs)

  #-----------------------------------
  # Per-paired method: If any sdate along dat_dim is NA, turn all sdate points 
  # along dat_dim into NA.
  if (!is.null(dat_dim)) {
    pos <- rep(0, length(dat_dim))  # dat_dim: [dataset, member]
    for (i in seq_along(dat_dim)) {
        pos[i] <- which(names(dim(obs)) == dat_dim[i])
    }
    outrows_exp <- MeanDims(exp, pos, na.rm = FALSE) +
                    MeanDims(obs, pos, na.rm = FALSE)
    outrows_obs <- outrows_exp
#browser()
    for (i_pos in sort(pos)) {
      outrows_exp <- InsertDim(outrows_exp, i_pos, dim(exp)[i_pos])
      outrows_obs <- InsertDim(outrows_obs, i_pos, dim(obs)[i_pos])
    }
    exp_for_clim <- exp
    obs_for_clim <- obs
    exp_for_clim[which(is.na(outrows_exp))] <- NA
    obs_for_clim[which(is.na(outrows_obs))] <- NA
  } else {
    exp_for_clim <- exp
    obs_for_clim <- obs
  }


  #-----------------------------------
  res <- Apply(list(exp, obs, exp_for_clim, obs_for_clim),
                    target_dims = c(time_dim, dat_dim),
                    fun = .Ano_CrossValid, dat_dim = dat_dim, 
                    memb_dim = memb_dim, memb = memb,
                    ncores = ncores)

  # Remove dat_dim in obs if obs doesn't have at first place
  if (reset_obs_dim) {
     tmp <- match(names(dim(res$obs)), names(ori_obs_dim))
     dim(res$obs) <- ori_obs_dim[tmp[which(!is.na(tmp))]]
  }
  if (reset_exp_dim) {
     tmp <- match(names(dim(res$exp)), names(ori_exp_dim))
     dim(res$exp) <- ori_exp_dim[tmp[which(!is.na(tmp))]]
  }

#    res_obs_dim <- ori_obs_dim[-which(names(ori_obs_dim) == time_dim)]
#    if (!memb & memb_dim %in% names(res_obs_dim)) {
#      res_obs_dim <- res_obs_dim[-which(names(res_obs_dim) == memb_dim)]
#    }
#    if (is.integer(res_obs_dim) & length(res_obs_dim) == 0) {
#      res$obs <- as.vector(res$obs)
#    } else {
#      res$obs <- array(res$obs, dim = res_obs_dim)
#    }
#  }

  return(res)
}

.Ano_CrossValid <- function(exp, obs, exp_for_clim, obs_for_clim, dat_dim = c('dataset', 'member'),
                            memb_dim = 'member', memb = TRUE, ncores = NULL) {
  if (is.null(dat_dim)) {
    ini_dims_exp <- dim(exp)
    ini_dims_obs <- dim(obs)
    exp <- InsertDim(exp, posdim = 2, lendim = 1, name = 'dataset')
    exp_for_clim <- InsertDim(exp_for_clim, posdim = 2, lendim = 1, name = 'dataset')
    obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
    obs_for_clim <- InsertDim(obs_for_clim, posdim = 2, lendim = 1, name = 'dataset')
  }

  # exp: [sdate, dat_dim, memb_dim]
  # obs: [sdate, dat_dim, memb_dim]
  ano_exp_list <- vector('list', length = dim(exp)[1])  #length: [sdate]
  ano_obs_list <- vector('list', length = dim(obs)[1])  

  for (tt in seq_len(dim(exp)[1])) {  #[sdate]
    # calculate clim
    exp_sub <- ClimProjDiags::Subset(exp_for_clim, 1, seq_len(dim(exp)[1])[-tt])
    obs_sub <- ClimProjDiags::Subset(obs_for_clim, 1, seq_len(dim(obs)[1])[-tt])
    # Average out time_dim -> [dat, memb]
    clim_exp <- apply(exp_sub, seq_along(dim(exp))[-1], mean, na.rm = TRUE)
    clim_obs <- apply(obs_sub, seq_along(dim(obs))[-1], mean, na.rm = TRUE)

    # ensemble mean
    if (!memb) {
      if (is.null(dim(clim_exp)) | length(dim(clim_exp)) == 1) {   #dim: [member]
        clim_exp <- mean(clim_exp, na.rm = TRUE)  # a number
        clim_obs <- mean(clim_obs, na.rm = TRUE)
      } else {
        pos <- which(names(dim(clim_exp)) == memb_dim)
        pos <- seq_along(dim(clim_exp))[-pos]
        dim_name <- names(dim(clim_exp))
        dim_exp_ori <- dim(clim_exp)
        dim_obs_ori <- dim(clim_obs)

        clim_exp <- apply(clim_exp, pos, mean, na.rm = TRUE)
        clim_obs <- apply(clim_obs, pos, mean, na.rm = TRUE)
        if (is.null(names(dim(as.array(clim_exp))))) {
          clim_exp <- as.array(clim_exp)
          clim_obs <- as.array(clim_obs)
          names(dim(clim_exp)) <- dim_name[pos]
          names(dim(clim_obs)) <- dim_name[pos]
        }
      
        # Expand it back 
        clim_exp_tmp <- array(clim_exp, dim = c(dim_exp_ori[pos], dim_exp_ori[-pos]))
        clim_obs_tmp <- array(clim_obs, dim = c(dim_obs_ori[pos], dim_obs_ori[-pos]))
        # Reorder it back to dim(clim_exp)
        tmp <- match(dim_exp_ori, dim(clim_exp_tmp))
        clim_exp <- Reorder(clim_exp_tmp, tmp)
        clim_obs <- Reorder(clim_obs_tmp, tmp)
      }
    }
    # calculate ano
    ano_exp_list[[tt]] <- ClimProjDiags::Subset(exp, 1, tt, drop = 'selected') - clim_exp
    ano_obs_list[[tt]] <- ClimProjDiags::Subset(obs, 1, tt, drop = 'selected') - clim_obs
  }

  ano_exp <- array(unlist(ano_exp_list), dim = c(dim(exp)[-1], dim(exp)[1]))
  ano_exp <- Reorder(ano_exp, c(length(dim(exp)), 1:(length(dim(exp)) - 1)))
  ano_obs <- array(unlist(ano_obs_list), dim = c(dim(obs)[-1], dim(obs)[1]))
  ano_obs <- Reorder(ano_obs, c(length(dim(obs)), 1:(length(dim(obs)) - 1)))

  if (is.null(dat_dim)) {
    ano_exp <- array(ano_exp, dim = ini_dims_exp)
    ano_obs <- array(ano_obs, dim = ini_dims_obs)
  }

  return(list(exp = ano_exp, obs = ano_obs))
}
