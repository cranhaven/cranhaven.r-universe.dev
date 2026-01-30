#' Internal diagnostic functions
#'
#' The function is called in diagnostics.trafo_lm and 
#' diagnostics.trafo_compare.
#' @keywords internal


diagnostics_internal <- function(modOne, modTwo) {
  
  
  formula <- NULL
  
  if (inherits(modOne, "lm")) {
    
    resid_One <- residuals(modOne, level = 0, type = "pearson")
    resid_Two <- residuals(modTwo, level = 0, type = "pearson")
    
    if (length(resid_One) > 3 & length(resid_One) < 5000) {
      
      # Original model or first transformed
      shapiroEst_One <- shapiro.test(resid_One)$statistic[[1]]
      shapiroP_One <- shapiro.test(resid_One)$p.value[[1]]
      
      # Transformed model or second transformed
      shapiroEst_Two <- shapiro.test(resid_Two)$statistic[[1]]
      shapiroP_Two <- shapiro.test(resid_Two)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_One <- NA
      shapiroP_One <- NA
      
      shapiroEst_Two <- NA
      shapiroP_Two <- NA
    }
    
    skewness_One <- skewness(resid_One)
    kurtosis_One <- kurtosis(resid_One)
    
    skewness_Two <- skewness(resid_Two)
    kurtosis_Two <- kurtosis(resid_Two)
    
    norm_resid <- data.frame(Skewness  = c(skewness_One, skewness_Two),
                             Kurtosis  = c(kurtosis_One, kurtosis_Two),
                             Shapiro_W = c(shapiroEst_One, shapiroEst_Two),
                             Shapiro_p = c(shapiroP_One, shapiroP_Two),
                             row.names = c(modOne$name, 
                                           modTwo$name))
    
    norm_ranef <- NULL
    
    
    breusch_pagan_One <- bptest(formula(modOne$terms), 
                                 data = modOne$model)
    breusch_pagan_Two <- bptest(formula(modTwo$terms), 
                                  data = modTwo$model)
    
    hetero <- data.frame(BreuschPagan_V = c(breusch_pagan_One$statistic,
                                            breusch_pagan_Two$statistic),
                         BreuschPagan_p = c(breusch_pagan_One$p.value,
                                            breusch_pagan_Two$p.value),
                         row.names = c(modOne$name, 
                                       modTwo$name))
    
    return(list(norm_resid = norm_resid, 
                norm_ranef = norm_ranef, 
                hetero = hetero))
    
    
  } else if (inherits(modOne, "lme")) {
    
    ranef <- NULL
    
    resid_One <- residuals(modOne, level = 0, type = "pearson")
    resid_Two <- residuals(modTwo, level = 0, type = "pearson")
    
    if (length(resid_One) > 3 & length(resid_One) < 5000) {
      shapiroEst_residOne <- shapiro.test(resid_One)$statistic[[1]]
      shapiroP_residOne <- shapiro.test(resid_One)$p.value[[1]]
      
      shapiroEst_residTwo <- shapiro.test(resid_Two)$statistic[[1]]
      shapiroP_residTwo <- shapiro.test(resid_Two)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_residOne <- NA
      shapiroP_residOne <- NA
      
      shapiroEst_residTwo <- NA
      shapiroP_residTwo <- NA
    }
    
    skewness_residOne <- skewness(resid_One)
    kurtosis_residOne <- kurtosis(resid_One)
    
    skewness_residTwo <- skewness(resid_Two)
    kurtosis_residTwo <- kurtosis(resid_Two)
    
    
    ranef_One <- ranef(modOne)$'(Intercept)'
    ranef_Two <- ranef(modTwo)$'(Intercept)'
    
    if (length(ranef_One) > 3 & length(ranef_One) < 5000) {
      shapiroEst_ranefOne <- shapiro.test(ranef_One)$statistic[[1]]
      shapiroP_ranefOne <- shapiro.test(ranef_One)$p.value[[1]]
      
      shapiroEst_ranefTwo <- shapiro.test(ranef_Two)$statistic[[1]]
      shapiroP_ranefTwo <- shapiro.test(ranef_Two)$p.value[[1]]
    }
    else{
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for random effects.")
      shapiroEst_ranefOne <- NA
      shapiroP_ranefOne <- NA
      
      shapiroEst_ranefTwo <- NA
      shapiroP_ranefTwo <- NA
    }
    
    skewness_ranefOne <- skewness(ranef_One)
    kurtosis_ranefOne <- kurtosis(ranef_One)
    
    skewness_ranefTwo <- skewness(ranef_Two)
    kurtosis_ranefTwo <- kurtosis(ranef_Two)
    
    norm_resid <- data.frame(Skewness  = c(skewness_residOne, skewness_residTwo),
                             Kurtosis  = c(kurtosis_residOne, kurtosis_residTwo),
                             Shapiro_W = c(shapiroEst_residOne, shapiroEst_residTwo),
                             Shapiro_p = c(shapiroP_residOne, shapiroP_residTwo),
                             row.names = c(modOne$name, 
                                           modTwo$name))
    
    norm_ranef <- data.frame(Skewness  = c(skewness_ranefOne, skewness_ranefTwo),
                             Kurtosis  = c(kurtosis_ranefOne, kurtosis_ranefTwo),
                             Shapiro_W = c(shapiroEst_ranefOne, shapiroEst_ranefTwo),
                             Shapiro_p = c(shapiroP_ranefOne, shapiroP_ranefTwo),
                             row.names = c(modOne$name, 
                                           modTwo$name))
    
    return(list(norm_resid = norm_resid, 
                norm_ranef = norm_ranef,
                hetero = NULL))
    
  }
}