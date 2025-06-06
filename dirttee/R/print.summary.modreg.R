


print.summary.modreg <-
  function(x, ...){
    cat("\nFormula:\n", x$formula, "\n")
    cat("\nParametric Coefficients:\n")
    if(is.null(x$SE)){
      print(x$coefficients)
    } else{
      tt <- cbind(x$coefficients, x$SE)
      colnames(tt) <- c("Estimate", "Std. Error")
      print(tt)
    }
    
    
    if(!is.numeric(x$bw_method)){
      cat("\nThe algorithm for bandwidth estimation was:", x$bw_method)
      cat('\nEstimated bandwidth:                            ',x$bw)
    } else{
      cat("\nThe bandwidth was fixed at:", x$bw)
    }
    
    if(x$bw_method == "Plugin"){
      cat("\n\nThe Plugin algorithm had", x$iterations_plugin, "iterations (maximum is 10 per default).")
    }
    

    
    nloptr_codes <- c(
    "The optimization of hyperparameter was sucessful",
    "Optimization of hyperparameter finished, because the terminal value was reached",
    "Optimization of hyperparameter finished, because the tolerance for the evaluation function was reached",
    "Optimization of hyperparameter finished, because the tolerance for the objective value was reached",
    "Optimization of hyperparameter finished, because the maximum number of iterations was reached",
    "Optimization of hyperparameter finished, because the maximal time was reached"
    )
    if(is.numeric(x$hp_status)){
      if(x$hp_status < 0){
        cat("\n\nOptimization of hyperparameter failed. Fur further information check hp_opt.")
      }else{
        cat("\n\n",nloptr_codes[x$hp_status]," after ",x$hp_iterations," iterations.", sep = "")
      }
    }
   
    if(x$converged){
      cat("\n\nThe final weighted least squares algorithm converged after", x$iterations, "iterations.\n")
    }else{
      cat("\n\nThe final weighted least squares algorithm did not converge within", x$iterations, "iterations.\n")
    }
    
    # cat('\n\nResiduals:\n')
    # qresi <- t(data.frame(quantile(residuals(x$reg))))
    # colnames(qresi) <- c('Min','1Q','Median','3Q','Max')
    # rownames(qresi) <- ""
    # print(qresi)
    
    cat('\nAIC:                         ',x$aic)
    cat('\nEffective degrees of freedom:',x$edf)
    cat('\nPseudo log-Likelihood:       ',x$pseudologlik)
  }
