

Factorial_Invariance <- function(model, data, group, estimator = 'ML', verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  if (!(estimator %in% c('ML', 'GLS', 'WLS', 'DWLS', 'ULS', 'DLS', 'PML', 'MLM', 
                         'MLMVS', 'MLMV', 'MLF', 'MLR', 'WLSM', 'WLSMVS', 'WLSMV', 
                         'ULSM', 'ULSMVS', 'ULSMV')))
    stop('The provided estimator, ', estimator, ', is not one of the possible estimators.')
  

  fit_coef_prefs <- c('rmsea','srmr','cfi','tli','aic','bic')
  
  # npar fmin chisq df pvalue baseline.chisq baseline.df baseline.pvalue 
  # cfi tli nnfi rfi nfi pnfi ifi rni logl unrestricted.logl aic bic ntotal 
  # bic2 rmsea rmsea.ci.lower rmsea.ci.upper rmsea.ci.level rmsea.pvalue 
  # rmsea.close.h0 rmsea.notclose.pvalue rmsea.notclose.h0 rmr rmr_nomean
  # srmr srmr_bentler srmr_bentler_nomean crmr crmr_nomean 
  # srmr_mplus srmr_mplus_nomean cn_05 cn_01 gfi agfi pgfi mfi ecvi
  
  # get fit coefs for each group separately
  group_values <- unique(data[,group])
  fitcoefs_by_group <- c()
  for (lupe in 1:length(group_values)) {
    
    CFA.all <- cfa(model=model, 
                   data = subset(data, eval(parse(text=group)) == group_values[lupe]),
                   estimator = estimator)
    
    fitcoefs_by_group <- rbind(fitcoefs_by_group, fitMeasures(CFA.all)[fit_coef_prefs])
  }
  rownames(fitcoefs_by_group) <- group_values
  
  
  CFA.Configural <- cfa(model=model, data=data, group=group, estimator = estimator)
  
  CFA.Metric <- cfa(model=model, data=data, group=group,
                    group.equal = c('loadings'), estimator = estimator)
  
  CFA.Scalar <- cfa(model=model, data=data, group=group,
                    group.equal = c('loadings','intercepts'), estimator = estimator)
  
  CFA.Strict <- cfa(model=model, data=data, group=group,
                    group.equal = c('loadings','intercepts', 'residuals'),
                    estimator = estimator)

    
  # there is/was a problem running summary on an semTools::compareFit object, as described
  
  # https://groups.google.com/g/lavaan/c/RqKf4_wQ1e4
  # 
  # https://github.com/simsem/semTools/issues/116
  # 
  # https://stackoverflow.com/questions/63065696/error-in-getmethodsummary-signature-fitdiff/63600180#63600180
  
  # it works ok outside of a function, but not within my function
  # the problem is with the object type, S4 vs S3
  
  # this code extracts the semTools::compareFit object into a useable list
  # it should appear before the semTools::compareFit command
  # NOTE: it does not work when just running the commands by themselves, outside of 
  #       package building
  summary.FitDiff <- function(object){
    output <- list(fit = object@fit, fit.diff = object@fit.diff, 
                   chisq_diffs = object@nested)
    return(invisible(output))
  }
  
  CompareAll <- semTools::compareFit(CFA.Configural,  # semTools::
                           CFA.Metric,
                           CFA.Scalar,
                           CFA.Strict)
  
  # # to prevent display of output (re: from summary(CompareAll ...)
  ## not needed when using the above summary.FitDiff function
  # quiet_sum <- function(x) {sink(tempfile()); on.exit(sink()); invisible(summary(x))}
  # 
  # mod_comps <- quiet_sum(summary(CompareAll))

  mod_comps <- summary(CompareAll)
  
  # inv_model_fits <- mod_comps@fit
  inv_model_fits <- mod_comps$fit
  inv_model_fits <- inv_model_fits[,fit_coef_prefs]
  
  # inv_model_fit_diffs <- mod_comps@fit.diff
  inv_model_fit_diffs <- mod_comps$fit.diff
  inv_model_fit_diffs <- inv_model_fit_diffs[,fit_coef_prefs] 
  
  # chisq_diffs <- mod_comps@nested
  chisq_diffs <- mod_comps$chisq_diffs
  chisq_diffs <- chisq_diffs[,c('Df','AIC','BIC','Chisq','Chisq diff','RMSEA',
                                'Df diff','Pr(>Chisq)')]
  
  if (verbose) {
    
    cat('\n\nFactorial_Invariance\n')
    
    cat('\nThe model:\n')
    cat(model, '\n')
    
    cat('\n\nThe levels of "group" and their Ns:')
    print(table(data[,group]))
    
    cat('\n\nFit coefficients for each group separately:\n')
    print(round(fitcoefs_by_group, 3), print.gap=4)
    
    cat('\n\nInvariance Model Fit Coefficients:\n')
    print(round(inv_model_fits, 3), print.gap=4)
    
    cat('\n\nDifferences in Invariance Model Fit Coefficients:\n')
    print(round(inv_model_fit_diffs, 3), print.gap=4)
    
    cat('\n\nChi-Squared Difference Tests:\n')
    print(round(chisq_diffs, 3), print.gap=4)
  }
  
  output <- list(fitcoefs_by_group=fitcoefs_by_group, 
                 model_Configural = parameterEstimates(CFA.Configural),
                 model_Metric     = parameterEstimates(CFA.Metric),
                 model_Scalar     = parameterEstimates(CFA.Scalar),
                 model_Strict     = parameterEstimates(CFA.Strict),
                 inv_model_fits=inv_model_fits, 
                 inv_model_fit_diffs=inv_model_fit_diffs, 
                 chisq_diffs=chisq_diffs)
  
  return(invisible(output))  
}

