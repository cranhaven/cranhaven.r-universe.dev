#ROSE: Function that determines the ROSE and ROSE outer bound of an estimate at specified significance and power levels
#estimate: Numeric scalar
#SE: Numeric scalar > 0
#alpha: Numeric scalar strictly between 0 and 0.5; defaults to 0.05
#power_target: Numeric scalar strictly between 0.5 and 1; defaults to 0.8
#df: Numeric scalar > 0; defaults to NA

################
##### INIT #####
################

ROSE = function(estimate, se, alpha = 0.05, power_target = 0.8, df = NA) {

  ##################
  ##### ERRORS #####
  ##################

  #If any argument of the function is a list, data.frame, or matrix...
  if (is.list(estimate) | is.list(se) | is.list(df) | is.list(alpha) | is.list(power_target) | is.data.frame(estimate) | is.data.frame(se) | is.data.frame(df) | is.data.frame(alpha) | is.data.frame(power_target) | is.matrix(estimate) | is.matrix(se) | is.matrix(df) | is.matrix(alpha) | is.matrix(power_target)) {

    #... then stop the function
    stop("'tst' does not accept lists, dataframes, or matrices")

  }

  #If estimate is not a numeric scalar...
  if (!(is.numeric(estimate) & length(estimate) == 1)) {

    #... then stop the function
    stop("'estimate' must be a numeric scalar")

  }

  #If se is not a numeric scalar...
  if (!(is.numeric(se) & length(se) == 1)) {

    #... then stop the function
    stop("'se' must be a numeric scalar")

  }
  #If se is not greater than zero...
  if (se <= 0) {

    #... then stop the function
    stop("'se' must be strictly greater than 0")

  }

  #If alpha is not a numeric scalar...
  if (!(is.numeric(alpha) & length(alpha) == 1)) {

    #... then stop the function
    stop("'alpha' must be a numeric scalar")

  }
  #If alpha is not between 0 and 0.5...
  if (alpha <= 0 | alpha >= 0.5) {

    #... then stop the function
    stop("'alpha' must be strictly between 0 and 0.5")

  }

  #If power_target is not a numeric scalar...
  if (!(is.numeric(power_target) & length(power_target) == 1)) {

    #... then stop the function
    stop("'power_target' must be a numeric scalar")

  }
  #If power_target is not between 0.5 and 1...
  if (power_target <= 0.5 | power_target >= 1) {

    #... then stop the function
    stop("'power_target' must be strictly between 0.5 and 1")

  }

  ########################
  ##### SUB-ROUTINES #####
  ########################

  #If df is provided...
  if (!(is.na(df) | is.null(df))) {

    #If df is not a positive numeric scalar...
    if (!(is.numeric(df) & length(df) == 1) | df <= 0) {

      #... then stop the function
      stop("'df' must be a positive numeric scalar")

    }

    #Calculate the upper bound of the ROSE
    ROSEUB = estimate + ROSE_exact(alpha = alpha, power_target = power_target, df = df)*se
    #Calculate the lower bound of the ROSE
    ROSELB = estimate - ROSE_exact(alpha = alpha, power_target = power_target, df = df)*se

  }
  #If df is not provided...
  else {

    #Calculate the upper bound of the ROSE
    ROSEUB = estimate + ROSE_asymp(alpha = alpha, power_target = power_target)*se
    #Calculate the lower bound of the ROSE
    ROSELB = estimate - ROSE_asymp(alpha = alpha, power_target = power_target)*se

  }

    #Calculate the outer bound of the ROSE
    if (abs(ROSEUB) > abs(ROSELB)) {
      ROSEOB = ROSEUB
    }
    else {
      ROSEOB = ROSELB
    }
    #Calculate the outer bound magnitude of the ROSE
    ROSEOB_magnitude = abs(ROSEOB)
    #Store the ROSE
    ROSE = c(ROSELB, ROSEUB)
    names(ROSE) = c("Lower bound", "Upper bound")

    #Store output
    output = list(ROSE, ROSEOB, ROSEOB_magnitude)
    names(output) = c("ROSE", "ROSEOB", "ROSEOB_magnitude")

    return(output)

}
