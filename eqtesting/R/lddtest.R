lddtest = function(runvar, data, cutpoint, epsilon, alpha = 0.05, cluster = "", bootstrap = FALSE, breps = 1000, bin = NULL, bw = NULL, verbose = FALSE, plot = TRUE) {
  
  #If runvar is not a single string...
  if (!is.character(runvar) | length(runvar) != 1) {
    
    #... then stop the function
    stop("'runvar' must be a single character string")
    
  }
  #If cluster is not a single string...
  if (!is.character(runvar) | length(runvar) != 1) {
    
    #... then stop the function
    stop("'cluster' must be a single character string")
    
  }
  #If data is not a data.frame...
  if (!is.data.frame(data)) {
    
    #... then stop the function
    stop("'data' must be a data.frame")
    
  }
  #If alpha is not a numeric scalar...
  if (!(is.numeric(alpha) & length(alpha) == 1)) {
    
    #... then stop the function
    stop("'alpha' must be a numeric scalar")
    
  }
  #If alpha is not strictly between 0 and 0.5...
  if (alpha <= 0 | alpha >= 0.5) {
    
    #... then stop the function
    stop("'alpha' must be strictly between 0 and 0.5")
    
  }
  #If epsilon is not a numeric scalar...
  if (!(is.numeric(epsilon) & length(epsilon) == 1)) {
    
    #... then stop the function
    stop("'epsilon' must be a numeric scalar")
    
  }
  #If epsilon is not greater than one...
  if (epsilon <= 1) {
    
    #... then stop the function
    stop("'epsilon' must be strictly greater than 1")
    
  }
  #If breps is not a numeric scalar...
  if (!(is.numeric(breps) & length(breps) == 1)) {
    
    #... then stop the function
    stop("'breps' must be a numeric scalar")
    
  }
  #If breps is not greater than 1...
  if (breps <= 1) {
    
    #... then stop the function
    stop("'breps' must be strictly greater than 1")
    
  }
  #If breps is not at least 1/alpha...
  if (breps < 1/alpha) {
    
    #... then stop the function
    stop("'breps' must be at least as large as 1/alpha")
    
  }
  #If bootstrap repetitions are changed but bootstrap itself is not specified as TRUE...
  if (breps != 1000 & bootstrap == FALSE) {
    
    #... then stop the function
    stop("'breps' is changed from its default value but 'bootstrap' is specified as FALSE; change one or the other")
    
  }
  
  #Store total obserunvarations
  if (cluster != "") {
    
    N = nrow(data[which(!is.na(data[, runvar]) & !is.na(data[, cluster])), ])
      
  }
  if (cluster == "") {
    
    N = nrow(data[which(!is.na(data[, runvar])), ])
    
  }
  
  #Run DCdensity
  mccrary = DCdensity(data[, runvar], cutpoint, bin = bin, bw = bw, verbose = verbose, plot = plot, ext.out = TRUE, htest = FALSE)
  estimate = mccrary$theta
  se = mccrary$se
  
  #Store effective observations
  if (cluster != "") {
    
    N_eff = nrow(data[which(!is.na(data[, runvar]) & !is.na(data[, cluster]) & data[, runvar] >= cutpoint - mccrary$bw & data[, runvar] <= cutpoint + mccrary$bw), ])
    
  }
  if (cluster == "") {
    
    N_eff = nrow(data[which(!is.na(data[, runvar]) & data[, runvar] >= cutpoint - mccrary$bw & data[, runvar] <= cutpoint + mccrary$bw), ])
    
  }
  
  #Organize ROPE
  ROPE = c(-log(epsilon), log(epsilon))
  
  #Generate bounds dataframe
  bounds = as.data.frame(matrix(nrow = 1, ncol = 2))
  colnames(bounds) = c("Lower Bound", "Upper Bound")
  rownames(bounds) = c(paste0(round((1 - alpha)*100, 3), "% equivalence confidence interal (ECI)"))
  
  #If the estimate is exactly midway between the lower and upper bounds of the ROPE...
  if (estimate == (ROPE[1] + ROPE[2])/2) {
    
    #... then select the upper boundary as the relevant bound
    bound = ROPE[2]
    
  }
  #... otherwise...
  if (estimate != (ROPE[1] + ROPE[2])/2) {
    
    #... the closer bound to the estimate is the relevant TOST bound
    bound = ROPE[which((c(abs(estimate - ROPE[1]), abs(estimate - ROPE[2])) == min(c(abs(estimate - ROPE[1]), abs(estimate - ROPE[2])))))]
    
  }
  
  #Generate test dataframe
  test = as.data.frame(matrix(nrow = 1, ncol = 10))
  colnames(test) = c("N", "Effective N", "Epsilon Lower Bound", "Epsilon Upper Bound", "Theta", "SE", "ECI Lower Bound", "ECI Upper Bound", "Equivalence z-statistic", "p-value")
  
  #Store the sample sizes
  test[1, "N"] = N
  test[1, "Effective N"] = N_eff
  
  #Store the epsilon boundaries
  test[1, "Epsilon Lower Bound"] = ROPE[1]
  test[1, "Epsilon Upper Bound"] = ROPE[2]
  
  #Store the estimate
  test[1, "Theta"] = estimate
  
  #Create null bootstrap estimates vector
  boot_estimates = c()
  
  #If bootstrap is unnecessary...
  if (bootstrap == FALSE & cluster == "") {
    
    #Store the SE
    test[1, "SE"] = se
    
    #Store the ECI bounds
    test[1, "ECI Lower Bound"] = estimate - qnorm(1 - alpha)*se
    test[1, "ECI Upper Bound"] = estimate + qnorm(1 - alpha)*se
    
    #If the lower bound of the ROPE is the relevant TOST bound...
    if (bound == ROPE[1]) {
      
      #Store the z-statistic as estimate - min(ROPE) in standard error units
      test[1, "Equivalence z-statistic"] = (estimate - ROPE[1])/se
      #Store the p-value of the one-sided test in the upper tail
      test[1, "p-value"] = pnorm(test[1, "Equivalence z-statistic"], lower.tail = FALSE)
      
    }
    #If the upper bound of the ROPE is the relevant TOST bound...
    if (bound == ROPE[2]) {
      
      #Store the z-statistic as estimate - max(ROPE) in standard error units
      test[1, "Equivalence z-statistic"] = (estimate - ROPE[2])/se
      #Store the p-value of the one-sided test in the lower tail
      test[1, "p-value"] = pnorm(test[1, "Equivalence z-statistic"], lower.tail = TRUE)
      
    }
    
  }
  
  #If bootstrap is necessary...
  if (bootstrap == TRUE | cluster != "") {
    
    #Initialize bootstrap LDD list and count
    ldd_list = rep(NA, breps)
    i = 1
    
    #If conventional bootstrap is necessary...
    if (cluster == "") {
      
      #Define bootstrap data
      bdata = data[which(!is.na(data[, runvar])), ]
      
    }
    
    #If cluster bootstrap is necessary...
    if (cluster != "") {
      
      #Define bootstrap data
      bdata = data[which(!is.na(data[, runvar] & !is.na(data[, cluster]))), ]
      
    }
    
    #Set data.table
    setDT(bdata)
    
    #Until breps defined logarithmic density discontinuities are obtained...
    while (i <= breps) {
      
      #If conventional bootstrap is necessary...
      if (cluster == "") {
        
        #Resample rows of the dataset with replacement
        bsample <- bdata[sample(.N, replace = TRUE)]
        
        #If the cutpoint lies in the range of the resampled running variable...
        if (cutpoint > min(bsample[[runvar]]) & cutpoint < max(bsample[[runvar]])) {
        
          #Obtain the LDD estimate for the bootstrap sample, holding bin sizes and bandwidths constant
          bDCdensity = try(DCdensity(bsample[[runvar]], cutpoint, bw = mccrary$bw, bin = mccrary$binsize, ext.out = TRUE, plot = FALSE))
          
          #If the estimate is nonmissing...
          if (!inherits(bDCdensity, "try-error")) {
            if (!is.nan(bDCdensity$theta)) {
              
              #Store the estimate
              ldd_list[i] = bDCdensity$theta
              
              #Up the count
              i = i + 1
              
            }
          }
          
        }
        
      }
      
      #If cluster bootstrap is necessary...
      if (cluster != "") {
        
        #Identify unique clusters
        unique_clusters <- unique(bdata[[cluster]])
        
        #Obtain resampling clusters with replacement
        sampled_clusters <- sample(unique_clusters, replace = TRUE)
        
        #Obtain bootstrap sample
        bsample <- do.call(rbind, lapply(sampled_clusters, function(cl) {
          
          bdata[bdata[[cluster]] == cl]
          
        }))
        
        #If the cutpoint lies in the range of the resampled running variable...
        if (cutpoint > min(bsample[[runvar]]) & cutpoint < max(bsample[[runvar]])) {
        
          #Obtain the LDD estimate for the bootstrap sample, holding bin sizes and bandwidths constant
          bDCdensity = try(DCdensity(bsample[[runvar]], cutpoint, bw = mccrary$bw, bin = mccrary$binsize, ext.out = TRUE, plot = FALSE))
      
          #If the estimate is nonmissing...
          if (!inherits(bDCdensity, "try-error")) {
            if (!is.nan(bDCdensity$theta)) {
              
              #Store the estimate
              ldd_list[i] = bDCdensity$theta
              
              #Up the count
              i = i + 1
              
            }
          }
          
        }
        
      }
      
    }
    
    #Store bootstrap estimates
    boot_estimates = ldd_list
    
    #Compute the bootstrap standard error
    test[1, "SE"] = sd(ldd_list)
    
    #Compute the bootstrap ECI bounds
    test[1, "ECI Lower Bound"] = sort(ldd_list)[floor(alpha*breps)]
    test[1, "ECI Upper Bound"] = sort(ldd_list)[ceiling((1 - alpha)*breps)]
    
    #Compute the bootstrap p-value
    test[1, "p-value"] = sum(ifelse(ldd_list >= -log(epsilon) & ldd_list <= log(epsilon), 0, 1))/breps
    
  }
  
  #If the p-value is below alpha...
  if (test[1, "p-value"] <= alpha) {
    
    #... then conclude the LDD is significantly bounded
    conclusion = paste0("The running variable's density discontinuity at the cutpoint is significantly bounded beneath a ratio of ",
                        epsilon,
                        " at the ",
                        round(alpha*100, 3),
                        "% significance level.")
    
  }
  
  #Otherwise...
  if (test[1, "p-value"] > alpha) {
    
    #... then conclude the LDD is NOT significantly bounded
    conclusion = paste0("The running variable's density discontinuity at the cutpoint is NOT significantly bounded beneath a ratio of ",
                        epsilon,
                        " at the ",
                        round(alpha*100, 3),
                        "% significance level.")
    
  }
  
  #Print citation disclaimer
  message("Please cite the paper underlying this program:")
  message("Fitzgerald, Jack (2025). Manipulation Tests in Regression Discontinuity Design: The Need for Equivalence Testing. MetaArXiv. https://doi.org/10.31222/osf.io/2dgrp_v1.")
  #Store output
  output = list(test, conclusion, boot_estimates)
  names(output) = c("test", "conclusion", "boot_estimates")
  #Return bounds
  return(output)
  
}
