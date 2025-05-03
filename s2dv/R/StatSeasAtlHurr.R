#'Compute estimate of seasonal mean of Atlantic hurricane activity
#'
#'Compute one of G. Villarini's statistically downscaled measure of mean 
#'Atlantic hurricane activity and its variance. The hurricane activity is 
#'estimated using seasonal averages of sea surface temperature anomalies over 
#'the tropical Atlantic (bounded by 10N-25N and 80W-20W) and the tropics at 
#'large (bounded by 30N-30S). The anomalies are for the JJASON season.\cr
#'The estimated seasonal average is either 1) number of hurricanes, 2) number 
#'of tropical cyclones with lifetime >=48h or 3) power dissipation index 
#'(PDI; in 10^11 m^3 s^(-2)).\cr
#'The statistical models used in this function are described in references.
#'
#'@param atlano A numeric array with named dimensions of Atlantic sea surface 
#'  temperature anomalies. It must have the same dimensions as 'tropano'.
#'@param tropano A numeric array with named dimensions of tropical sea surface 
#'  temperature anomalies. It must have the same dimensions as 'atlano'.
#'@param hrvar A character string of the seasonal average to be estimated. The
#'  options are either "HR" (hurricanes), "TC" (tropical cyclones with lifetime
#'  >=48h), or "PDI" (power dissipation index). The default value is 'HR'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A list composed of two arrays with the same dimensions as 'atlano' 
#'  and 'tropano'.
#'\item{$mean}{
#'  The mean of the desired quantity.
#'}
#'\item{$var}{
#'  The variance of that quantity.
#'}
#'
#'@references 
#'Villarini et al. (2010) Mon Wea Rev, 138, 2681-2705.\cr
#'Villarini et al. (2012) Mon Wea Rev, 140, 44-65.\cr
#'Villarini et al. (2012) J Clim, 25, 625-637.\cr
#'An example of how the function can be used in hurricane forecast studies 
#'  is given in\cr
#'Caron, L.-P. et al. (2014) Multi-year prediction skill of Atlantic hurricane 
#'  activity in CMIP5 decadal hindcasts. Climate Dynamics, 42, 2675-2690. 
#'  doi:10.1007/s00382-013-1773-1.
#'
#'@examples
#'# Let AtlAno represents 5 different 5-year forecasts of seasonally averaged 
#'# Atlantic sea surface temperature anomalies.
#'AtlAno <- array(runif(25, -1, 1), dim = c(sdate = 5, ftime = 5))
#'# Let TropAno represents 5 corresponding 5-year forecasts of seasonally 
#'# averaged tropical sea surface temperature anomalies.
#'TropAno <- array(runif(25, -1, 1), dim = c(sdate = 5, ftime = 5))
#'# The seasonal average of hurricanes for each of the five forecasted years, 
#'# for each forecast, would then be given by.
#'hr_count <- StatSeasAtlHurr(atlano = AtlAno, tropano = TropAno, hrvar = 'HR')
#'
#'@import multiApply
#'@export
StatSeasAtlHurr <- function(atlano, tropano, hrvar = "HR", ncores = NULL) {

  # Check inputs 
  ## atlano and tropano
  if (is.null(atlano) | is.null(tropano)) {
    stop("Parameter 'atlano' and 'tropano' cannot be NULL.")
  }
  if (!is.numeric(atlano) | !is.numeric(tropano)) {
    stop("Parameter 'atlano' and 'tropano' must be a numeric array.")
  }
  if (is.null(dim(atlano))) {  #is vector
    dim(atlano) <- c(length(atlano))
    names(dim(atlano)) <- 'dim1'
  }
  if (is.null(dim(tropano))) {  #is vector
    dim(tropano) <- c(length(tropano))
    names(dim(tropano)) <- 'dim1'
  }
  if (any(is.null(names(dim(atlano)))) | any(nchar(names(dim(atlano))) == 0) |
      any(is.null(names(dim(tropano)))) | any(nchar(names(dim(tropano))) == 0)) {
    stop("Parameter 'atlano' and 'tropano' must have dimension names.")
  }
  if (!all(names(dim(atlano)) %in% names(dim(tropano))) | 
      !all(names(dim(tropano)) %in% names(dim(atlano)))) {
    stop("Parameter 'atlano' and 'tropano' must have same dimension names.")
  }
  name_1 <- sort(names(dim(atlano)))
  name_2 <- sort(names(dim(tropano)))
  if (!all(dim(atlano)[name_1] == dim(tropano)[name_2])) {
    stop("Parameter 'atlano' and 'tropano' must have the same length of ",
         "all the dimensions.")
  }
  ## hrvar
  if (hrvar != "HR" & hrvar != "TC" & hrvar != "PDI") {
    stop("The parameter 'hrvar' must be either 'HR', 'TC', or 'PDI'.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 


  ###############################
  # Calculate StatSeasAtlHurr
  if (is.null(ncores)) {
    use_Apply <- FALSE
  } else if (ncores == 1) {
    use_Apply <- FALSE 
  } else {
    use_Apply <- TRUE
  }

  if (use_Apply) {
    res <- Apply(list(atlano, tropano), 
                 target_dims = list(c(names(which.max(dim(atlano)))), 
                                    c(names(which.max(dim(atlano))))),
                 fun = .StatSeasAtlHurr, 
                 hrvar = hrvar,
                 ncores = ncores)
  } else {

    # Get the values of the betas according to the hurricane
    # activity measure we specified.
    # ------------------------------------------------------
    if (hrvar == "HR") {
      # beta's are derived from Villarini et al. (2012), Mon Wea
      # Rev, 140, 44-65.  beta's are for corrected hurricane data +
      # ERSST with SBC criteria (table 2)
      beta0 <- 1.85
      betaAtl <- 1.05
      betaTrop <- -1.17
    } else if (hrvar == "TC") {
      # beta's are from Villarini et al. (2010), Mon Wea Rev, 138,
      # 2681-2705.  beta's are for corrected TC data (lifetime >=
      # 48h) + ERSST (table 5)
      beta0 <- 2.1
      betaAtl <- 1.02
      betaTrop <- -1.05
    } else if (hrvar == "PDI") {
      # beta's are from Villarini et al. (2012), J Clim, 25,
      # 625-637.  beta's are from ERSST, with SBC penalty criterion
      # (table 1)
      beta0 <- 0.76
      betaAtl <- 1.94
      betaTrop <- -1.78
    }
    # Create matrix of similar dimension as atlano for beta0.
    # -------------------------------------------------------
    intercept <- array(beta0, dim(atlano))
    # Compute statistical relationship b/w SSTAs and mean
    # hurricane activity.
    # ---------------------------------------------------
    atl <- betaAtl * atlano
    trop <- betaTrop * tropano
    # 
    temp <- intercept + atl + trop
    # 
    res <- list(mean = array(NA, dim(atl)), var = array(NA, dim(atl)))
    res$mean[] <- vapply(X = temp, FUN = exp, numeric(1))
    # Compute the variance of the distribution.  TC and HR follow
    # a Poisson distribution, so the variance is equal to the
    # mean.  PDI follows a gamma distribution, with sigma =
    # -0.57.  (variance = sigma^2 * mean^2).
    # -----------------------------------------------------------
    if (hrvar == "HR" | hrvar == "TC") {
      res$var <- res$mean
    } else {
      sigma <- -0.57
      res$var[] <- sigma^2 * vapply(X = res$mean, FUN = function(x) x^2, numeric(1))
    }

  }

 return(res)
}

.StatSeasAtlHurr <- function(atlano, tropano, hrvar = "HR") {

  # atlano and tropano: a vector with same length

  # Get the values of the betas according to the hurricane activity measure we 
  # specified.
  # ------------------------------------------------------
  if (hrvar == "HR") {
    # beta's are derived from Villarini et al. (2012), Mon Wea
    # Rev, 140, 44-65.  beta's are for corrected hurricane data +
    # ERSST with SBC criteria (table 2)
    beta0 <- 1.85
    betaAtl <- 1.05
    betaTrop <- -1.17
  } else if (hrvar == "TC") {
    # beta's are from Villarini et al. (2010), Mon Wea Rev, 138,
    # 2681-2705.  beta's are for corrected TC data (lifetime >=
    # 48h) + ERSST (table 5)
    beta0 <- 2.1
    betaAtl <- 1.02
    betaTrop <- -1.05
  } else if (hrvar == "PDI") {
    # beta's are from Villarini et al. (2012), J Clim, 25,
    # 625-637.  beta's are from ERSST, with SBC penalty criterion
    # (table 1)
    beta0 <- 0.76
    betaAtl <- 1.94
    betaTrop <- -1.78
  }

  # Compute statistical relationship b/w SSTAs and mean
  # hurricane activity.
  # ---------------------------------------------------
  atl <- betaAtl * atlano
  trop <- betaTrop * tropano
  temp <- beta0 + atl + trop
  stat_mean <- exp(temp)
  
  # Compute the variance of the distribution.  TC and HR follow
  # a Poisson distribution, so the variance is equal to the
  # mean.  PDI follows a gamma distribution, with sigma =
  # -0.57.  (variance = sigma^2 * mean^2).
  # -----------------------------------------------------------
  if (hrvar == "HR" | hrvar == "TC") {
    stat_var <- stat_mean
  } else {
    sigma <- -0.57
    stat_var <- sigma^2 * stat_mean^2
  }

  return(invisible(list(mean = stat_mean, var = stat_var)))
} 
