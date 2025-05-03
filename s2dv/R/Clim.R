#'Compute Bias Corrected Climatologies
#'
#'This function computes per-pair climatologies for the experimental 
#'and observational data using one of the following methods:
#'\enumerate{
#'  \item{per-pair method (Garcia-Serrano and Doblas-Reyes, CD, 2012 
#'        https://doi.org/10.1007/s00382-012-1413-1)}
#'  \item{Kharin method (Kharin et al, GRL, 2012 https://doi.org/10.1029/2012GL052647)}
#'  \item{Fuckar method (Fuckar et al, GRL, 2014 https://doi.org/10.1002/2014GL060815)}
#'}
#'Per-pair climatology means that only the startdates covered by the 
#'whole experiments/observational dataset will be used. In other words, the 
#'startdates which are not all available along 'dat_dim' dimension of both
#'the 'exp' and 'obs' are excluded when computing the climatologies.
#'Kharin method is the linear trend bias correction method, and Fuckar method 
#'is the initial condition bias correction method. The two methods both do the
#'per-pair correction beforehand.
#'
#'@param exp A named numeric array of experimental data with at least dimension
#'  'time_dim'.
#'@param obs A named numeric array of observational data that has the same 
#'  dimension as 'exp' except 'dat_dim'.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the climatologies are computed. The default value is 'sdate'.
#'@param dat_dim A character vector indicating the name of the dataset and 
#'  member dimensions. If data at one startdate (i.e., 'time_dim') are not 
#'  complete along 'dat_dim', this startdate along 'dat_dim' will be discarded.
#'  If there is no dataset dimension, it can be NULL, however, it will be more 
#'  efficient to simply use mean() to do the calculation. The default value is 
#'  "c('dataset', 'member')".
#'@param method A character string indicating the method to be used. The 
#'  options include 'clim' (per-pair method), 'kharin' (Kharin method), and 
#'  'NDV' (Fuckar method). The default value is 'clim'.
#'@param ftime_dim A character string indicating the name of forecast time
#'  dimension. Only used when method = 'NDV'. The default value is 'ftime'.
#'@param memb A logical value indicating whether to remain 'memb_dim' dimension
#'  (TRUE) or do ensemble mean over 'memb_dim' (FALSE). The default value is 
#'  TRUE.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. Only used when parameter 'memb' is FALSE. It must be one element
#'  in 'dat_dim'. The default value is 'member'.
#'@param na.rm A logical value indicating whether to remove NA values along 
#'  'time_dim' when calculating climatology (TRUE) or return NA if there is NA 
#'  along 'time_dim' (FALSE). The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list of 2:
#'\item{$clim_exp}{
#'  A numeric array with the same dimensions as parameter 'exp' but 
#'  dimension 'time_dim' is moved to the first position. If parameter 'method'
#'  is 'clim', dimension 'time_dim' is removed. If parameter 'memb' is FALSE,
#'  dimension 'memb_dim' is also removed.
#'}
#'\item{$clim_obs}{
#'  A numeric array with the same dimensions as  parameter 'obs' 
#'  except dimension 'time_dim' is removed. If parameter 'memb' is FALSE,
#'  dimension 'memb_dim' is also removed.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'clim2 <- Clim(sampleData$mod, sampleData$obs, method = 'kharin', memb = FALSE)
#'\dontrun{
#'PlotClim(clim$clim_exp, clim$clim_obs, 
#'         toptitle = paste('sea surface temperature climatologies'), 
#'         ytitle = 'K', monini = 11, listexp = c('CMIP5 IC3'), 
#'         listobs = c('ERSST'), biglab = FALSE)
#'}
#'@importFrom abind adrop
#'@importFrom ClimProjDiags Subset
#'@import multiApply
#'@export
Clim <- function(exp, obs, time_dim = 'sdate', dat_dim = c('dataset', 'member'), 
                 method = 'clim', ftime_dim = 'ftime', memb = TRUE,
                  memb_dim = 'member', na.rm = TRUE, ncores = NULL) {

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
    if (!is.character(dat_dim)) {
      stop("Parameter 'dat_dim' must be a character vector.")
    }
    # Check if dat_dim is in exp
    if (!all(dat_dim %in% names(dim(exp)))) {
      stop("Parameter 'dat_dim' is not found in 'exp' dimensions.")
    }
    # If dat_dim is not in obs, add it in
    if (!all(dat_dim %in% names(dim(obs)))) {
      reset_obs_dim <- TRUE
      ori_obs_dim <- dim(obs)
      dim(obs) <- c(dim(obs), rep(1, length(dat_dim[which(!dat_dim %in% names(dim(obs)))])))
      names(dim(obs)) <- c(names(ori_obs_dim), dat_dim[which(!dat_dim %in% names(dim(obs)))])
    } else {
      reset_obs_dim <- FALSE
    }
  } else {
    reset_obs_dim <- FALSE
  } 
  ## method
  if (!(method %in% c("clim", "kharin", "NDV"))) {
    stop("Parameter 'method' must be one of 'clim', 'kharin' or 'NDV'.")
  }
  ## ftime_dim
  if (method == "NDV") {
    if (!is.character(ftime_dim) | length(ftime_dim) > 1) {
      stop("Parameter 'ftime_dim' must be a character string.")
    }
    if (!ftime_dim %in% names(dim(exp)) | !ftime_dim %in% names(dim(obs))) {
      stop("Parameter 'ftime_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }
  ## memb
  if (!is.logical(memb) | length(memb) > 1) {
    stop("Parameter 'memb' must be one logical value.")
  }
  ## memb_dim
  if (!is.null(memb_dim) & !memb) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp)) | !memb_dim %in% names(dim(obs))) {
      stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
    }
    if (!memb_dim %in% dat_dim)
      stop("Parameter 'memb_dim' must be one element in parameter 'dat_dim'.")
  } else if (is.null(memb_dim) & !memb) {
    memb <- TRUE
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
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
  if (!all(dim(exp)[name_exp] == dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have the same dimensions ",
         "except 'dat_dim'.")
  }

  ###############################
  # Sort dimension
  name_exp <- names(dim(exp))
  name_obs <- names(dim(obs))
  order_obs <- match(name_exp, name_obs)
  obs <- Reorder(obs, order_obs)
  

  ###############################
  # Calculate Clim

  #----------------------------------
  # Per-pair: Remove all sdate if not complete along dat_dim
  if (!is.null(dat_dim)) {
    pos <- rep(0, length(dat_dim))
    for (i in seq_along(dat_dim)) {  #[dat, sdate]
      ## dat_dim: [dataset, member]
      pos[i] <- which(names(dim(obs)) == dat_dim[i])
    }
    outrows_exp <- MeanDims(exp, pos, na.rm = FALSE) + 
                   MeanDims(obs, pos, na.rm = FALSE)
    outrows_obs <- outrows_exp

    for (i in seq_along(pos)) {
      outrows_exp <- InsertDim(outrows_exp, pos[i], dim(exp)[pos[i]])
      outrows_obs <- InsertDim(outrows_obs, pos[i], dim(obs)[pos[i]])
    }
    exp[which(is.na(outrows_exp))] <- NA
    obs[which(is.na(outrows_obs))] <- NA
  }

  #-----------------------------------

  if (method == 'clim') {
    clim <- Apply(list(exp, obs), 
                  target_dims = c(time_dim, dat_dim),
                  fun = .Clim,
                  method = method, time_dim = time_dim, 
                  dat_dim = dat_dim, memb_dim = memb_dim,
                  memb = memb, na.rm = na.rm, ncores_input = ncores,
                  ncores = ncores)

  } else if (method == 'kharin') {
    clim <- Apply(list(exp, obs),
                  target_dims = c(time_dim, dat_dim),
                  fun = .Clim,
                  method = method, time_dim = time_dim,
                  dat_dim = dat_dim, ftime_dim = ftime_dim, memb_dim = memb_dim,
                  memb = memb, na.rm = na.rm, ncores_input = ncores,
                  ncores = ncores)

  } else if (method == 'NDV') {
    clim <- Apply(list(exp, obs),
                  target_dims = c(time_dim, dat_dim, ftime_dim),
                  fun = .Clim,
                  method = method, time_dim = time_dim,
                  dat_dim = dat_dim, ftime_dim = ftime_dim, memb_dim = memb_dim,
                  memb = memb, na.rm = na.rm, ncores_input = ncores,
                  ncores = ncores)
  }

  # Remove dat_dim in obs if obs doesn't have at first place
  if (reset_obs_dim) {
    clim_obs_dim <- ori_obs_dim[-which(names(ori_obs_dim) == time_dim)]
    if (!memb & memb_dim %in% names(clim_obs_dim)) {
      clim_obs_dim <- clim_obs_dim[-which(names(clim_obs_dim) == memb_dim)]
    }
    if (is.integer(clim_obs_dim) & length(clim_obs_dim) == 0) {
     clim$clim_obs <- as.vector(clim$clim_obs)
    } else {
      clim$clim_obs <- array(clim$clim_obs, dim = clim_obs_dim)
    }
  } 

  return(clim)
}


.Clim <- function(exp, obs, method = 'clim',
                  time_dim = 'sdate', dat_dim = c('dataset', 'member'),
                  ftime_dim = 'ftime', memb_dim = 'member', memb = TRUE,
                  na.rm = TRUE, ncores_input = NULL) {

  if (method == 'clim') {
    if (!is.null(dat_dim)) {
    # exp: [sdate, dat_dim_exp]
    # obs: [sdate, dat_dim_obs]

      clim_exp <- apply(exp, which(names(dim(exp)) != time_dim),
                        mean, na.rm = na.rm) #average out time_dim
      clim_obs <- apply(obs, which(names(dim(obs)) != time_dim),
                        mean, na.rm = na.rm) #[dat_dim]
  
      if (is.null(dim(clim_exp))) {
        dim(clim_exp) <- length(clim_exp)
        names(dim(clim_exp)) <- dat_dim
        dim(clim_obs) <- length(clim_obs)
        names(dim(clim_obs)) <- dat_dim
      }
  
    ## ensemble mean
      if (!memb) {
        if (length(dim(clim_exp)) == 1) {   #dim: [member]
          clim_exp <- mean(clim_exp, na.rm = TRUE)    
          clim_obs <- mean(clim_obs, na.rm = TRUE)
        } else {
          dim_name <- names(dim(clim_exp))
          pos <- c(seq_along(dim(clim_exp)))[-which(dim_name == memb_dim)]
          clim_exp <- apply(clim_exp, pos, mean, na.rm = TRUE) 
          clim_obs <- apply(clim_obs, pos, mean, na.rm = TRUE) 
          if (is.null(dim(clim_exp))) {
            dim(clim_exp) <- length(clim_exp)
            dim(clim_obs) <- length(clim_obs)
            names(dim(clim_exp)) <- dim_name[pos]
            names(dim(clim_obs)) <- dim_name[pos]
          }
        }
      }
    } else {  #dat_dim = NULL
      clim_exp <- mean(exp)
      clim_obs <- mean(obs)
    }

  } else if (method == 'kharin') {
  # exp: [sdate, dat_dim_exp]
  # obs: [sdate, dat_dim_obs]

  # obs clim
    if (!is.null(dat_dim)) {
      clim_obs <- apply(obs, which(names(dim(obs)) != time_dim), 
                        mean, na.rm = na.rm) #[dat_dim]
      if (is.null(dim(clim_obs))) {
        dim(clim_obs) <- length(clim_obs)
        names(dim(clim_obs)) <- dat_dim
      }
    } else {
      clim_obs <- mean(obs)
    }

  # exp clim
  ##--- NEW trend ---##
    tmp_obs <- Trend(data = obs, time_dim = time_dim, interval = 1,
                     polydeg = 1, conf = FALSE, ncores = ncores_input)$trend 
    tmp_exp <- Trend(data = exp, time_dim = time_dim, interval = 1, 
                     polydeg = 1, conf = FALSE, ncores = ncores_input)$trend
    # tmp_exp: [stats, dat_dim]
    ##NOTE: Cannot use rowMeans here because tmp_obs may have only one dim
    tmp_obs_mean <- apply(tmp_obs, 1, mean)  #average out dat_dim (dat and member)
    #tmp_obs_mean: [stats = 2]
    if (!is.null(dat_dim)) {
      intercept_exp <- Subset(tmp_exp, 1, 1, drop = 'selected')  #[dat_dim]
      slope_exp <- Subset(tmp_exp, 1, 2, drop = 'selected')  #[dat_dim]
      intercept_obs <- array(tmp_obs_mean[1], dim = dim(exp)[-1])  #[dat_dim]
      slope_obs <- array(tmp_obs_mean[2], dim = dim(exp)[-1])  #[dat_dim]
    } else {
      intercept_exp <- tmp_exp[1]
      slope_exp <- tmp_exp[2]
      intercept_obs <- tmp_obs_mean[1]
      slope_obs <- tmp_obs_mean[2]
    }
    trend_exp <- list()
    trend_obs <- list()
    for (jdate in seq_len(dim(exp)[time_dim])) {
      trend_exp[[jdate]] <- intercept_exp + jdate * slope_exp
      trend_obs[[jdate]] <- intercept_obs + jdate * slope_obs
    }
    # turn list into array
    trend_exp <- array(unlist(trend_exp), dim = c(dim(exp)[-1], dim(exp)[1]))
    trend_obs <- array(unlist(trend_obs), dim = c(dim(exp)[-1], dim(exp)[1]))
    if (!is.null(dat_dim)) {
      len <- length(dim(exp))
      trend_exp <- Reorder(trend_exp, c(len, 1:(len - 1)))
      trend_obs <- Reorder(trend_obs, c(len, 1:(len - 1)))
    }

    # average out dat_dim, get a number
#    if (is.null(dim(clim_obs))) {
      clim_obs_mean <- mean(clim_obs)
#    } else {
#      clim_obs_mean <- mean(rowMeans(clim_obs))
#    }
    clim_obs_mean <- array(clim_obs_mean, dim = dim(exp)) #enlarge it for the next line
    clim_exp <- trend_exp - trend_obs + clim_obs_mean

    ## member mean
    if (!memb) {
      pos_exp <- c(seq_along(dim(clim_exp)))[-which(names(dim(clim_exp)) == memb_dim)]
      pos_obs <- c(seq_along(dim(clim_obs)))[-which(names(dim(clim_obs)) == memb_dim)]
      tmp_dim_exp <- dim(clim_exp)
      tmp_dim_obs <- dim(clim_obs)
      clim_exp <- apply(clim_exp, pos_exp, mean, na.rm = TRUE)
      if (is.integer(pos_obs) & length(pos_obs) == 0) {
        clim_obs <- mean(clim_obs)
      } else {
        clim_obs <- apply(clim_obs, pos_obs, mean, na.rm = TRUE)
      }
      if (is.null(dim(clim_exp))) {
        clim_exp <- as.array(clim_exp)
        dim(clim_exp) <- tmp_dim_exp[pos_exp]
      }
      if (is.null(dim(clim_obs)) & !(is.integer(pos_obs) & length(pos_obs) == 0)) {
        clim_obs <- as.array(clim_obs)
        dim(clim_obs) <- tmp_dim_obs[pos_obs]
      }
    }


  } else if (method == 'NDV') {
  # exp: [sdate, dat_dim, ftime]
  # obs: [sdate, dat_dim, ftime]

    # obs clim
    clim_obs <- apply(obs, which(names(dim(obs)) != time_dim),
                      mean, na.rm = na.rm) #[(dat_dim), ftime]

    if (is.null(dim(clim_obs))) {
      dim(clim_obs) <- length(clim_obs)
      names(dim(clim_obs)) <- c(dat_dim, ftime_dim)
    }

    # exp clim
    pos_ftime <- length(dim(exp))  #a number
    dim_ftime <- dim(exp)[pos_ftime]  #c(ftime = 4)
    if (!is.null(dat_dim)) {
      pos_dat <- 2:(length(dim(exp)) - 1)  #1 is sdate, last is ftime
      dim_dat <- dim(exp)[pos_dat]  #c(dataset = 1, member = 3)
    } else {
      dim_dat <- NULL
    }

    # Create initial data set (i.e., only first ftime)
    tmp <- Subset(exp, ftime_dim, 1, drop = 'selected')
    ini_exp <- InsertDim(tmp, pos_ftime, dim_ftime) #only first ftime
    tmp <- Subset(obs, ftime_dim, 1, drop = 'selected')
    ini_obs <- InsertDim(tmp, pos_ftime, dim_ftime) #only first ftime
    #ini_: [sdate, dat_dim, ftime]
    tmp_exp <- Regression(datay = exp, datax = ini_exp, reg_dim = time_dim,
                          na.action = na.omit,
                          pval = FALSE, conf = FALSE, ncores = ncores_input)$regression
    tmp_obs <- Regression(datay = obs, datax = ini_obs, reg_dim = time_dim, 
                          na.action = na.omit, 
                          pval = FALSE, conf = FALSE, ncores = ncores_input)$regression
    #tmp_: [stats = 2, dat_dim, ftime]
    #average out dat_dim (dat and member)
    tmp_obs_mean <- apply(tmp_obs, c(1, length(dim(tmp_obs))), mean)
    #tmp_obs_mean: [stats = 2, ftime]
    #average out dat_dim
    ini_obs_mean <- apply(ini_obs, c(1, length(dim(ini_obs))), mean)
    #ini_obs_mean: [sdate, ftime]

    # Find intercept and slope
    intercept_exp <- Subset(tmp_exp, 1, 1, drop = 'selected')  #[dat_dim, ftime]
    slope_exp <- Subset(tmp_exp, 1, 2, drop = 'selected')  #[dat_dim, ftime]
    intercept_obs <- array(tmp_obs_mean[1, ], 
                           dim = c(dim_ftime, dim_dat)) #[ftime, dat_dim] exp
    if (!is.null(dat_dim)) {
      intercept_obs <- Reorder(intercept_obs, 
                               c(2:length(dim(intercept_obs)), 1)) #[dat_dim, ftime] exp
    } 
    slope_obs <- array(tmp_obs_mean[2, ], dim = c(dim_ftime, dim_dat)) #[ftime, dat_dim] exp
    if (!is.null(dat_dim)) {
      slope_obs <- Reorder(slope_obs, 
                           c(2:length(dim(slope_obs)), 1)) #[dat_dim, ftime] exp
    }

    trend_exp <- list()
    trend_obs <- list()
    for (jdate in seq_len(dim(exp)[time_dim])) {
      tmp <- Subset(ini_exp, time_dim, jdate, drop = 'selected')  #[dat_dim, ftime]
      trend_exp[[jdate]] <- intercept_exp + tmp * slope_exp  #[dat_dim, ftime]

      tmp <- array(ini_obs_mean[jdate, ], dim = c(dim_ftime, dim_dat))  #[ftime, dat_dim]
      if (!is.null(dat_dim)) {
        tmp <- Reorder(tmp, c(2:length(dim(tmp)), 1))  #[dat_dim, ftime]
      }
      trend_obs[[jdate]] <- intercept_obs + tmp * slope_obs
    }
    # turn list into array
    trend_exp <- array(unlist(trend_exp), dim = c(dim(exp)[-1], dim(exp)[1]))
    trend_obs <- array(unlist(trend_obs), dim = c(dim(exp)[-1], dim(exp)[1]))
    #trend_: [dat_dim, ftime, sdate]
    len <- length(dim(exp))
    trend_exp <- Reorder(trend_exp, c(len, 1:(len - 1)))
    trend_obs <- Reorder(trend_obs, c(len, 1:(len - 1)))
    #trend_: [sdate, dat_dim, ftime]
    if (is.null(dim(clim_obs))) {
      clim_obs_mean <- clim_obs #[ftime]
    } else {
    clim_obs_mean <- apply(clim_obs, length(dim(clim_obs)), mean)  #average out dat_dim, [ftime]
    }
    clim_obs_mean <- array(clim_obs_mean, dim = c(dim_ftime, dim(exp)[1], dim_dat))
    #[ftime, sdate, dat_dim]
    len <- length(dim(clim_obs_mean))
    clim_obs_mean <- Reorder(clim_obs_mean, c(2:len, 1))
    #[sdate, dat_dim, ftime]

    clim_exp <- trend_exp - trend_obs + clim_obs_mean

    ## member mean
    if (!memb) {
      pos_exp <- c(seq_along(dim(clim_exp)))[-which(names(dim(clim_exp)) == memb_dim)]
      pos_obs <- c(seq_along(dim(clim_obs)))[-which(names(dim(clim_obs)) == memb_dim)]

      tmp_dim_exp <- dim(clim_exp)
      tmp_dim_obs <- dim(clim_obs)
      clim_exp <- apply(clim_exp, pos_exp, mean, na.rm = TRUE)
      if (is.integer(pos_obs) & length(pos_obs) == 0) {
        clim_obs <- mean(clim_obs)
      } else {
        clim_obs <- apply(clim_obs, pos_obs, mean, na.rm = TRUE)
      }
      if (is.null(dim(clim_exp))) {
        clim_exp <- as.array(clim_exp)
        dim(clim_exp) <- tmp_dim_exp[pos_exp]
      }
      if (is.null(dim(clim_obs)) & !(is.integer(pos_obs) & length(pos_obs) == 0)) {
        clim_obs <- as.array(clim_obs)
        dim(clim_obs) <- tmp_dim_obs[pos_obs]
      }
    }

  }

  return(list(clim_exp = clim_exp, clim_obs = clim_obs))
}
