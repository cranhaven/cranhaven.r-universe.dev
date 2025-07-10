### INPUTS ###
#estimate: The estimate of interest; numeric scalar
#se: The estimate of interest's standard error; numeric scalar greater than zero
#ROPE: Can either be a strictly positive numeric scalar (interpreted as the width of a symmetric ROPE around zero), or a vector of two different numeric scalars
#df: If added, must be a positive integer. If left blank, asymptotic normal approximations are reported. If provided, exact results are reported
#alpha: Defaults to 0.05. If provided, must be a numeric scalar strictly between 0 and 0.5
### OUTPUTS ###
#bounds: data.frame consisting of CI boundaries (asymptotic or exact, depending on whether degees of freedom are offered)
#test: data.frame consisting of the t-statistics and p-values for TST within the provided ROPE
#

tst = function(estimate, se, ROPE, df = NA, alpha = 0.05, plot = TRUE) {

  ##################
  ##### ERRORS #####
  ##################

  #If any argument of the function is a list, data.frame, or matrix...
  if (is.list(estimate) | is.list(se) | is.list(ROPE) | is.list(df) | is.list(alpha) | is.data.frame(estimate) | is.data.frame(se) | is.data.frame(ROPE) | is.data.frame(df) | is.data.frame(alpha) | is.matrix(estimate) | is.matrix(se) | is.matrix(ROPE) | is.matrix(df) | is.matrix(alpha)) {

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

  #If ROPE is not of length 1 or 2...
  if (!(length(ROPE) %in% c(1, 2))) {

    #... then stop the function
    stop("'ROPE' must be of length 1 or 2")

  }
  #If ROPE is of length 1...
  if (length(ROPE) == 1) {

    #If ROPE is not a numeric scalar...
    if (!(is.numeric(ROPE) & length(ROPE) == 1) | ROPE <= 0) {

      #... then stop the function
      stop("If 'ROPE' is of length 1, then it must be a numeric scalar")

    }
    #If ROPE is not greater than zero...
    if (ROPE <= 0) {

      #... then stop the function
      stop("If 'ROPE' is of length 1, then it must be strictly greater than zero")

    }

  }
  #If ROPE is of length 2...
  if (length(ROPE) == 2) {

    #If one of the two elements of ROPE is not of length 1...
    if (length(ROPE[1]) != 1 | length(ROPE[2]) != 1) {

      #... then stop the function
      stop("If 'ROPE' is of length 2, then its two elements must each be of length 1")

    }
    #If the first and second element of ROPE are equal to one another...
    if (ROPE[1] == ROPE[2]) {

      #... then stop the function
      stop("If 'ROPE' is of length 2, then its two elements must not be equal to one another")

    }
    #If one of the two elements of ROPE are non-numeric...

    if (!is.numeric(ROPE[1]) | !is.numeric(ROPE[2])) {

      #... then stop the function
      stop("All elements of 'ROPE' must be numeric")

    }

  }

  #If ROPE is a positive numeric scalar...
  if (length(ROPE) == 1) {

    #Generate a symmetric region of practical equivalence of length ROPE around zero
    ROPE = c(-ROPE, ROPE)

  }
  #If ROPE is a list of two different numeric scalars...
  if (length(ROPE) == 2) {

    #Generate a region of practical equivalence of [min(ROPE), max(ROPE)]
    ROPE = c(min(ROPE), max(ROPE))

  }

  #Generate bounds dataframe
  bounds = as.data.frame(matrix(nrow = 3, ncol = 2))
  colnames(bounds) = c("Lower Bound", "Upper Bound")
  rownames(bounds) = c(paste0(round((1 - alpha)*100, 3), "% TST confidence interval (for precision)"),
                       paste0(round((1 - alpha)*100, 3), "% equivalence confidence interval (for conclusions)"),
                       paste0(round((1 - alpha)*100, 3), "% classic confidence interval (for conclusions)"))

  #If the estimate is exactly midway between the lower and upper bounds of the ROPE...
  if (estimate == (ROPE[1] + ROPE[2])/2) {

    #... then select the upper boundary as the relevant bound
    bound = ROPE[2]

  }
  #... otherwise...
  else {

    #... the closer bound to the estimate is the relevant TOST bound
    bound = ROPE[which((c(abs(estimate - ROPE[1]), abs(estimate - ROPE[2])) == min(c(abs(estimate - ROPE[1]), abs(estimate - ROPE[2])))))]

  }

  ########################
  ##### SUB-ROUTINES #####
  ########################

  #If df is not provided...
  if ((is.na(df) | is.null(df))) {

    #Generate test dataframe
    test = as.data.frame(matrix(nrow = 3, ncol = 5))
    colnames(test) = c("ROPE Lower Bound", "ROPE Upper Bound", "z-statistic", "p-value", "Relevant")
    rownames(test) = c("Test: Estimate Bounded Above ROPE (Two-Sided)",
                       "Test: Estimate Bounded Within ROPE (TOST)",
                       "Test: Estimate Bounded Below ROPE (Two-Sided)")

    #Generate the bounds of the TST CI
    if (estimate < min(ROPE) + qnorm(p = 1 - alpha)*se) {

      bounds[1, 1] = estimate - qnorm(p = 1 - alpha)*se
      
    }
    if (estimate >= min(ROPE) + qnorm(p = 1 - alpha)*se & estimate <= min(ROPE) + qnorm(p = 1 - alpha/2)*se) {

      bounds[1, 1] = min(ROPE)
      
    }
    if (estimate > min(ROPE) + qnorm(p = 1 - alpha/2)*se & estimate < max(ROPE) + qnorm(p = 1 - alpha/2)*se) {

      bounds[1, 1] = estimate - qnorm(p = 1 - alpha/2)*se
      
    }
    if (estimate >= max(ROPE) + qnorm(p = 1 - alpha/2)*se) {

      bounds[1, 1] = max(ROPE)
      
    }
    if (estimate <= min(ROPE) - qnorm(p = 1 - alpha/2)*se) {

      bounds[1, 2] = min(ROPE)
      
    }
    if (estimate > min(ROPE) - qnorm(p = 1 - alpha/2)*se & estimate < max(ROPE) - qnorm(p = 1 - alpha/2)*se) {

      bounds[1, 2] = estimate + qnorm(p = 1 - alpha/2)*se
      
    }
    if (estimate >= max(ROPE) - qnorm(p = 1 - alpha/2)*se & estimate <= max(ROPE) - qnorm(p = 1 - alpha)*se) {

      bounds[1, 2] = max(ROPE)
      
    }
    if (estimate > max(ROPE) - qnorm(p = 1 - alpha)*se) {

      bounds[1, 2] = estimate + qnorm(p = 1 - alpha)*se
      
    }
    
    #Generate the bounds of the ECI
    bounds[2, 1] = estimate - qnorm(p = 1 - alpha)*se
    bounds[2, 2] = estimate + qnorm(p = 1 - alpha)*se

    #Generate the bounds of the classic CI
    bounds[3, 1] = estimate - qnorm(p = 1 - alpha/2)*se
    bounds[3, 2] = estimate + qnorm(p = 1 - alpha/2)*se

    #Store the ROPE
    test[, 1] = rep(ROPE[1], 3)
    test[, 2] = rep(ROPE[2], 3)

    #Store the z-statistic and p-value of the two-sided test for bounding above the ROPE
    test[1, 3] = (estimate - ROPE[2])/se
    test[1, 4] = min(pnorm(test[1, 3], lower.tail = FALSE)*2, 1)

    #If the lower bound of the ROPE is the relevant TOST bound...
    if (bound == ROPE[1]) {

      #Store the z-statistic as estimate - min(ROPE) in standard error units
      test[2, 3] = (estimate - ROPE[1])/se
      #Store the p-value of the one-sided test in the upper tail
      test[2, 4] = pnorm(test[2, 3], lower.tail = FALSE)

    }
    #If the upper bound of the ROPE is the relevant TOST bound...
    if (bound == ROPE[2]) {

      #Store the z-statistic as estimate - max(ROPE) in standard error units
      test[2, 3] = (estimate - ROPE[2])/se
      #Store the p-value of the one-sided test in the lower tail
      test[2, 4] = pnorm(test[2, 3], lower.tail = TRUE)

    }

    #Store the z-statistic and p-value of the two-sided test for bounding below the ROPE
    test[3, 3] = (estimate - ROPE[1])/se
    test[3, 4] = min(pnorm(test[3, 3], lower.tail = TRUE)*2, 1)

    #Determine relevant test
    if (estimate > ROPE[2]) {
      relevant = 1
    }
    if (estimate >= ROPE[1] & estimate <= ROPE[2]) {
      relevant = 2
    }
    if (estimate < ROPE[1]) {
      relevant = 3
    }

    #Mark test relevance
    test[relevant, 5] = "Y"
    test[setdiff(c(1:3), relevant), 5] = "N"

    #If no p-value is below alpha...
    if (min(test[, 4]) >= alpha) {

      #Report that the result is inconclusive
      conclusion = "The significance of the parameter is inconclusive."

    }
    #If the test bounding the estimate above the ROPE is significant...
    if (test[1, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded above the ROPE."

    }
    #If the TOST procedure is significant...
    if (test[2, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded within the ROPE."

    }
    #If the test bounding the estimate below the ROPE is significant...
    if (test[3, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded below the ROPE."

    }

    #If plot is desired...
    if (plot == TRUE) {

      #... then generate the plot
      plot(NA, ylim=c(0,1), 
           xlim=c(1.25*min(c(bounds[1, "Lower Bound"], bounds[3, "Lower Bound"], ROPE[1])), 1.25*max(c(bounds[1, "Upper Bound"], bounds[3, "Upper Bound"], ROPE[2]))), 
           bty="l", yaxt="n", ylab="",xlab="Estimate")
      abline(v = ROPE[2], lty=2)
      abline(v = ROPE[1], lty=2)
      abline(v = 0, lty = 2, col="grey")
      rect(bounds[1, "Lower Bound"], 
           0.52,  
           bounds[1, "Upper Bound"], 
           0.48, 
           col = "green", border = "green")
      rect(bounds[3, "Lower Bound"], 
           0.505,  
           bounds[3, "Upper Bound"], 
           0.495, 
           col = "red", border = "red")
      rect(bounds[2, "Lower Bound"], 
           0.505,  
           bounds[2, "Upper Bound"], 
           0.495, 
           col = "blue", border = "blue")
        points(x = estimate, y=0.5, pch=15, cex=1.5)
      
    }

  #Print citation disclaimer
  message("Asymptotically approximate equivalence confidence intervals and three-sided testing (TST) results reported")
  message("If using for academic/research purposes, please cite the papers underlying this program:")
  message("Fitzgerald, Jack (2025). The Need for Equivalence Testing in Economics. MetaArXiv. https://doi.org/10.31222/osf.io/d7sqr_v1.")
  message("Isager, P. & Fitzgerald, J. (2024). Three-Sided Testing to Establish Practical Significance: A Tutorial. PsyArXiv. https://doi.org/10.31234/osf.io/8y925.")
  #Store output
  output = list(bounds, test, conclusion)
  names(output) = c("bounds", "test", "conclusion")
  #Return bounds
  return(output)

  }

  #If df is provided...
  if (!(is.na(df) | is.null(df))) {

    #If df is not a positive numeric scalar...
    if (!(is.numeric(df) & length(df) == 1) | df <= 0) {

      #... then stop the function
      stop("'df' must be a positive numeric scalar")

    }

    #Generate test dataframe
    test = as.data.frame(matrix(nrow = 3, ncol = 5))
    colnames(test) = c("ROPE Lower Bound", "ROPE Upper Bound", "t-statistic", "p-value", "Relevant")
    rownames(test) = c("Test: Estimate Bounded Above ROPE (Two-Sided)",
                       "Test: Estimate Bounded Within ROPE (TOST)",
                       "Test: Estimate Bounded Below ROPE (Two-Sided)")

    #Generate the bounds of the TST CI
    if (estimate < min(ROPE) + qt(p = 1 - alpha, df = df)*se) {

      bounds[1, 1] = estimate - qt(p = 1 - alpha, df = df)*se
      
    }
    if (estimate >= min(ROPE) + qt(p = 1 - alpha, df = df)*se & estimate <= min(ROPE) + qt(p = 1 - alpha/2, df = df)*se) {

      bounds[1, 1] = min(ROPE)
      
    }
    if (estimate > min(ROPE) + qt(p = 1 - alpha/2, df = df)*se & estimate < max(ROPE) + qt(p = 1 - alpha/2, df = df)*se) {

      bounds[1, 1] = estimate - qt(p = 1 - alpha/2, df = df)*se
      
    }
    if (estimate >= max(ROPE) + qt(p = 1 - alpha/2, df = df)*se) {

      bounds[1, 1] = max(ROPE)
      
    }
    if (estimate <= min(ROPE) - qt(p = 1 - alpha/2, df = df)*se) {

      bounds[1, 2] = min(ROPE)
      
    }
    if (estimate > min(ROPE) - qt(p = 1 - alpha/2, df = df)*se & estimate < max(ROPE) - qt(p = 1 - alpha/2, df = df)*se) {

      bounds[1, 2] = estimate + qt(p = 1 - alpha/2, df = df)*se
      
    }
    if (estimate >= max(ROPE) - qt(p = 1 - alpha/2, df = df)*se & estimate <= max(ROPE) - qt(p = 1 - alpha, df = df)*se) {

      bounds[1, 2] = max(ROPE)
      
    }
    if (estimate > max(ROPE) - qt(p = 1 - alpha, df = df)*se) {

      bounds[1, 2] = estimate + qt(p = 1 - alpha, df = df)*se
      
    }
    
    #Generate the bounds of the ECI
    bounds[2, 1] = estimate - qt(p = 1 - alpha, df = df)*se
    bounds[2, 2] = estimate + qt(p = 1 - alpha, df = df)*se

    #Generate the bounds of the classic CI
    bounds[3, 1] = estimate - qt(p = 1 - alpha/2, df = df)*se
    bounds[3, 2] = estimate + qt(p = 1 - alpha/2, df = df)*se

    #Store the ROPE
    test[, 1] = rep(ROPE[1], 3)
    test[, 2] = rep(ROPE[2], 3)

    #Store the t-statistic and p-value of the two-sided test for bounding above the ROPE
    test[1, 3] = (estimate - ROPE[2])/se
    test[1, 4] = min(pt(test[1, 3], df = df, lower.tail = FALSE)*2, 1)

    #If the lower bound of the ROPE is the  TOST bound...
    if (bound == ROPE[1]) {

      #Store the t-statistic as estimate - min(ROPE) in standard error units
      test[2, 3] = (estimate - ROPE[1])/se
      #Store the p-value of the one-sided test in the upper tail
      test[2, 4] = pt(test[2, 3], df = df, lower.tail = FALSE)

    }
    #If the upper bound of the ROPE is the  TOST bound...
    if (bound == ROPE[2]) {

      #Store the t-statistic as estimate - max(ROPE) in standard error units
      test[2, 3] = (estimate - ROPE[2])/se
      #Store the p-value of the one-sided test in the lower tail
      test[2, 4] = pt(test[2, 3], df = df, lower.tail = TRUE)

    }

    #Store the t-statistic and p-value of the two-sided test for bounding below the ROPE
    test[3, 3] = (estimate - ROPE[1])/se
    test[3, 4] = min(pt(test[3, 3], df = df, lower.tail = TRUE)*2, 1)

    #Determine relevant test
    if (estimate > ROPE[2]) {
      relevant = 1
    }
    if (estimate >= ROPE[1] & estimate <= ROPE[2]) {
      relevant = 2
    }
    if (estimate < ROPE[1]) {
      relevant = 3
    }
    
    #Mark test relevance
    test[relevant, 5] = "Y"
    test[setdiff(c(1:3), relevant), 5] = "N"

    #If no p-value is below alpha...
    if (min(test[, 4]) >= alpha) {

      #Report that the result is inconclusive
      conclusion = "The significance of the parameter is inconclusive."

    }
    #If the test bounding the estimate above the ROPE is significant...
    if (test[1, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded above the ROPE."

    }
    #If the TOST procedure is significant...
    if (test[2, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded within the ROPE."

    }
    #If the test bounding the estimate below the ROPE is significant...
    if (test[3, 4] < alpha) {

      #Report the result
      conclusion = "The parameter is significantly bounded below the ROPE."

    }

    #If plot is desired...
    if (plot == TRUE) {

      #... then generate the plot
      plot(NA, ylim=c(0,1), 
           xlim=c(1.25*min(c(bounds[1, "Lower Bound"], bounds[3, "Lower Bound"], ROPE[1])), 1.25*max(c(bounds[1, "Upper Bound"], bounds[3, "Upper Bound"], ROPE[2]))), 
           bty="l", yaxt="n", ylab="",xlab="Estimate")
      abline(v = ROPE[2], lty=2)
      abline(v = ROPE[1], lty=2)
      abline(v = 0, lty = 2, col="grey")
      rect(bounds[1, "Lower Bound"], 
           0.52,  
           bounds[1, "Upper Bound"], 
           0.48, 
           col = "green", border = "green")
      rect(bounds[3, "Lower Bound"], 
           0.505,  
           bounds[3, "Upper Bound"], 
           0.495, 
           col = "red", border = "red")
      rect(bounds[2, "Lower Bound"], 
           0.505,  
           bounds[2, "Upper Bound"], 
           0.495, 
           col = "blue", border = "blue")
        points(x = estimate, y=0.5, pch=15, cex=1.5)
      
    }


    #Print citation disclaimer
    message("Exact (equivalence) confidence intervals (ECIs) and three-sided testing (TST) results reported")
    message("If using for academic/research purposes, please cite the papers underlying this program:")
    message("Fitzgerald, J. (2025). The Need for Equivalence Testing in Economics. MetaArXiv. https://doi.org/10.31222/osf.io/d7sqr_v1.")
    message("Isager, P. & Fitzgerald, J. (2024). Three-Sided Testing to Establish Practical Significance: A Tutorial. PsyArXiv. https://doi.org/10.31234/osf.io/8y925.")
    #Store output
    output = list(bounds, test, conclusion)
    names(output) = c("bounds", "test", "conclusion")
    #Return bounds
    return(output)

    }

  }
