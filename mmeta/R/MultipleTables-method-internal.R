

MultipleTables._setControlList <- function(multiple_tables_object, control_user){
  
  if(multiple_tables_object$measure == 'RD'){
    lower_default <- -1
  } else lower_default <- 0
  
  control_default <- list(
    n_samples = 5000,
    mcmc_initial = c(0.5, 0.5),
    upper_bound = 100,
    lower_bound = lower_default,
    num_grids = 2048,
    optim_method = "L-BFGS-B",
    maxit = 1000,
    initial_values = NULL
  )
  
  control_output <- control_default
  
  if(is.null(control_user) | length(control_user) == 0)
    return(control_output)
  ## replace default by user-specificied value
  names_users_input <- names(control_user)
  num_names <- length(names_users_input)
  for(i in 1:num_names){
    name_option <- names_users_input[i]
    if(name_option %in% names(control_default)){
      control_output[[name_option]] <-  control_user[[name_option]] 
    }
  }
  return(control_output)
}



MultipleTables._initialVal <- function(multiple_tables_object, control, verbose){
  
  if(!is.null(control$initial_values)){
    init_val <- control$initial_values
    if(length(init_val) < 5){
      stop('length of initial values should be 5.')
    }
    checkModelParameters(a1 = init_val[1], b1 = init_val[2],
                         a2 = init_val[3], b2 = init_val[4],
                         rho = init_val[5],
                         model = multiple_tables_object$model,
                         additional_infor  = 'For initial values:')
    
  } else{
    ## use beta binomial model
    init_val_log <- init_val_log <- rep(0, NUM_PARAMETERS_SARMANOV)
    mydat <- multiple_tables_object$data 
    ## p1
    BBfit1 <- aod::betabin(cbind(y, n-y)~1, ~1, 
                           data=data.frame(y=mydat$y1,n= mydat$n1))
    a_ini1 <- expit(as.numeric(BBfit1@param[1]))*(1/as.numeric(BBfit1@param[2])-1)
    b_ini1 <- (1/as.numeric(BBfit1@param[2])-1)*(1-expit(as.numeric(BBfit1@param[1])))
    ## p2
    BBfit2 <- aod::betabin(cbind(y, n-y)~1, ~1, data=data.frame(y=mydat$y2,n=mydat$n2))
    a_ini2 <- expit(as.numeric(BBfit2@param[1]))*(1/as.numeric(BBfit2@param[2])-1)
    b_ini2 <- (1/as.numeric(BBfit2@param[2])-1)*(1-expit(as.numeric(BBfit2@param[1])))
    init_val <- c(a_ini1, b_ini1, a_ini2, b_ini2, 0)
  }
  init_val_log <- parmsToTransformed(init_val)
  names(init_val_log) <- c('loga1','logb1','loga2','logb2','eta')
  return(init_val_log)
}




MultipleTables._priorParemetersEstimate <- function(multiple_tables_object, control, verbose){
  
  
  
  ## initial values
  init_val_log <- MultipleTables._initialVal(multiple_tables_object, control, verbose)
  
  

  
  
  
  ## maximize likelihood
  optim_obj_inde <- optim(init_val_log[1:NUM_PARAMETERS_INDEPENDENT], 
                          logLikIndep, 
                          method = control$optim_method,
                          lower=rep(-20,NUM_PARAMETERS_INDEPENDENT), 
                          upper=rep(20,NUM_PARAMETERS_INDEPENDENT),
                          control = list(fnscale=-1,maxit=control$maxit),
                          hessian = TRUE, mydat = multiple_tables_object$data )  
  
  
  loglik_value_inde <- optim_obj_inde$value
  
  ### For independent model, not need to fit Sar model and return directly
  if(multiple_tables_object$model == "Independent") {
    
    ### fetch the MLE for prior distribution
    prior_mle <- parmsToOriginal(optim_obj_inde$par); ##tranfer back to original scale
    ## check whether there is any problem
    checkModelParameters(a1 = prior_mle['a1'], b1= prior_mle['b1'], 
                         a2 = prior_mle['a2'], b2 = prior_mle['b2'] , 
                         rho = prior_mle['rho'], 
                         model = multiple_tables_object$model, 
                         additional_infor = 'MLE for independent model: \n')
    
    ### correlation test
    correlation_test <- liklihoodRatioTest(loglik_value_inde, loglik_value_inde)
    
    ### hessian matrix 
    hessian_log <- optim_obj_inde$hessian
    colnames(hessian_log) <- c("loga1","logb1","loga2","logb2")
    rownames(hessian_log) <- c("loga1","logb1","loga2","logb2")
    
    ## construct return list
    prior_estimate_and_test <- list(
      prior_mle = prior_mle,
      chi2_value = correlation_test$chi2_value , 
      p_value  = correlation_test$p_value,
      hessian_log=hessian_log,
      cov_matrix_log = inverseMatrixFunc(-hessian_log)
    )
    
    return(prior_estimate_and_test) 
    
  } else{
    ### For Sarmanov model,
    optim_obj_sar <- optim(init_val_log, 
                         logLikSar, 
                         method = control$optim_method,
                         lower=rep(-20,NUM_PARAMETERS_SARMANOV), 
                         upper=rep(20,NUM_PARAMETERS_SARMANOV),
                         control = list(fnscale=-1,maxit=control$maxit),
                         hessian=TRUE, mydat = multiple_tables_object$data )
    
    loglik_value_sar <- optim_obj_sar$value
    
    ### fetch the MLE for prior distribution
    prior_mle <- parmsToOriginal(optim_obj_sar$par); ##tranfer back to original scale
    ## check there is any problem
    checkModelParameters(a1 = prior_mle['a1'], b1= prior_mle['b1'], 
                         a2 = prior_mle['a2'], b2 = prior_mle['b2'] , 
                         rho = prior_mle['rho'], 
                         model = multiple_tables_object$model, 
                         additional_infor = 'MLE for Sarmanov model: \n')
    
    
    ### correlation test
    correlation_test <- liklihoodRatioTest(loglik_value_inde, loglik_value_sar)
    
    hessian_log <- optim_obj_sar$hessian
    colnames(hessian_log) <- c("loga1","logb1","loga2","logb2","eta")
    rownames(hessian_log) <- c("loga1","logb1","loga2","logb2","eta")
    

    
    ## output
    prior_estimate_and_test <- list(
      prior_mle = prior_mle,
      chi2_value = correlation_test$chi2_value , 
      p_value  = correlation_test$p_value,
      hessian_log = hessian_log,
      cov_matrix_log = inverseMatrixFunc(-hessian_log)
    )
    return(prior_estimate_and_test)
  }
}



MultipleTables._isObjectFitted <- function(multiple_tables_object){
  ## check input
  if (!inherits(multiple_tables_object, "MultipleTables"))
    stop("Use only with 'MultipleTables' objects.\n")
  
  if (is.null(multiple_tables_object$prior_mle))
    stop("Use modelFit method before summary \n")
}


MultipleTables._overallMeasureEstimate <- function(multiple_tables_object){
  
  overall_measure_estimate <- overallMeasureCal(multiple_tables_object$prior_mle, 
                                                multiple_tables_object$hessian_log , 
                                                measure = multiple_tables_object$measure, 
                                                alpha = multiple_tables_object$alpha)
  names(overall_measure_estimate$overall) <- NULL
  names(overall_measure_estimate$CI) <- c('lower','upper')
  return(overall_measure_estimate)

  
  
}



MultipleTables._studySpecificPosteriorSamples <- function(multiple_tables_object, control){
  
  
  ## reformat parms and data
  parms_prior <- vectorToList(multiple_tables_object$prior_mle)
  mydata <- multiple_tables_object$data[c('y1','n1','y2','n2','study_name')]
  num_studies <- nrow(mydata)
  output_samples <- list()
  
  ### draw posterior samples for each study
  for (row in 1:num_studies) {
    study_name = mydata[row, "study_name"] 
    samples_study_i <- samplePosterior(parms_prior, 
                    list(y1 = mydata[row, "y1"], 
                         n1 = mydata[row, "n1"], 
                         y2 = mydata[row, "y2"], 
                         n2 = mydata[row, "n2"]), 
                    multiple_tables_object$model, 
                    multiple_tables_object$measure, 
                    control$n_samples * 1.1,
                    control$mcmc_initial)
    output_samples[[study_name]] <- samplesTrucation(samples_study_i, 
                                                     lower = control$lower_bound,
                                                     upper = control$upper_bound,
                                                     n_samples = control$n_samples)
  }
  
  
  return(output_samples)
} 


MultipleTables._studySpecificPosteriorDensity <- function(multiple_tables_object,control){
  
  mydata <- multiple_tables_object$data[c('y1','n1','y2','n2','study_name')]
  num_studies <- nrow(mydata)
  study_density <- list()
  
  for(study_id in 1:num_studies){
    study_name <- mydata[study_id, "study_name"]
    
    if(multiple_tables_object$method == 'sampling'){
      density_estimate <- density(multiple_tables_object$samples[[study_name]], 
                                             from = control$lower_bound, 
                                             to = control$upper_bound, 
                                             n = control$num_grids)
      study_density[[study_name]] <-  list( x = density_estimate$x, y = density_estimate$y)
    } else{
        study_density[[study_name]]<- densityMeasureGrids(parms_prior = vectorToList(multiple_tables_object$prior_mle), 
                          data = list(y1 = mydata[study_id, "y1"], 
                                      n1 = mydata[study_id, "n1"], 
                                      y2 = mydata[study_id, "y2"], 
                                      n2 = mydata[study_id, "n2"]), 
                          model = multiple_tables_object$model, 
                          measure = multiple_tables_object$measure,
                          from = control$lower_bound, 
                          to = control$upper_bound, 
                          num_grids = control$num_grids)
       
    }
  }
  

  return(study_density)
}




MultipleTables._StudySpecificMeasureEstimate <- function(multiple_tables_object, control){
  
  mydata <- multiple_tables_object$data[c('y1','n1','y2','n2','study_name')]
  num_studies <- nrow(mydata)
  study_specific_summary_df <- NULL
  digit <- multiple_tables_object$digit
  for(study_id in 1:num_studies){
    study_name <- mydata[study_id, "study_name"]
    
    if(multiple_tables_object$method == "sampling"){
      study_i_summary <- summaryStatisticsSampling(
                          multiple_tables_object$samples[[study_name]], 
                          multiple_tables_object$alpha, 
                          lower = control$lower_bound, 
                          upper = control$upper_bound)  

    } else{
      
        study_i_summary <- summaryStatisticsExact (vectorToList(multiple_tables_object$prior_mle), 
                                                   list(y1 = mydata[study_id, "y1"], 
                                                               n1 = mydata[study_id, "n1"], 
                                                               y2 = mydata[study_id, "y2"], 
                                                               n2 = mydata[study_id, "n2"]),
                                                    multiple_tables_object$samples[[study_name]], 
                                                    multiple_tables_object$measure, 
                                                    multiple_tables_object$model,
                                                    multiple_tables_object$alpha, 
                                                    is_prior = FALSE, 
                                                    lower = control$lower_bound, 
                                                    upper = control$upper)
    }
    
    ## output
    new_row = data.frame(
      study_name = study_name,
      mean = round(study_i_summary$mean, digit),
      median = round(study_i_summary$median, digit),
      ET_lower = round(study_i_summary$ET_CI[1],digit),
      ET_upper = round(study_i_summary$ET_CI[2],digit),
      HRD_lower = round(study_i_summary$HDR_CI[1],digit),
      HRD_upper = round(study_i_summary$HDR_CI[2], digit)
    )
    
    study_specific_summary_df <- rbind(study_specific_summary_df, new_row)
    

  }
  rownames(study_specific_summary_df)<- NULL
  
  return(study_specific_summary_df)
}

MultipleTables._summaryPrint <- function(multiple_tables_object){
  
  measurenames <- c('Odds Ratio', 'Risk Ratio', 'Risk Difference')
  names(measurenames) <- c('OR', 'RR', 'RD')
  
  siglevel <- paste(as.character((1-multiple_tables_object$alpha)*100),"%",sep="")
  digit <- multiple_tables_object$digit
  model <- multiple_tables_object$model
  
  if (model=="Sarmanov") 
    cat("Model: Sarmanov Beta-Binomial Model",fill=TRUE)
  if (model=="Independent") 
    cat("Model: Independent Beta-Binomial Model",fill=TRUE)
  
  ### Overall 
  cat("THe overall ",measurenames[multiple_tables_object$measure],fill=TRUE)
  overall_measure <- multiple_tables_object$overall_measure_estimation
  CI_string <- CItoString(overall_measure$confindent_interval,digit)
  cat("Estimate:", round(overall_measure$point,digit),fill=TRUE)
  cat(siglevel," confident interval: ", CI_string ,fill=TRUE)
  cat("",fill=TRUE)
  
  
  
  cat("Maximum likelihood estimates of hyperparameters:",fill=TRUE)
  if (model=="Sarmanov"|model=="Independent") {
    chi2_value <- multiple_tables_object$chi2_value
    p_value <- multiple_tables_object$p_value
    a1 <- multiple_tables_object$prior_mle[1]; 
    b1 <- multiple_tables_object$prior_mle[2]
    a2 <- multiple_tables_object$prior_mle[3]; 
    b2 <- multiple_tables_object$prior_mle[4]
    rho <- multiple_tables_object$prior_mle[5]
    
    cat("a1 =",round(a1,digit),", ","b1 =",round(b1,digit),", ","a2 =",round(a2,digit),
        ", ","b2 =",round(b2,digit),", ","rho =",round(rho,digit),sep="",fill=TRUE)
    cat("Likelihood ratio test for within-group correlation (H0: rho=0):",sep="",fill=TRUE)
    cat("chi2: ",round(chi2_value,digit),"; ","p-value: ",round(p_value, digit),sep="",fill=TRUE)
    
  }
  
  
  cat("",fill=TRUE)
  cat("Study-Specifc",measurenames[multiple_tables_object$measure],":",sep="",fill=TRUE)
  printout <- data.frame(Mean = multiple_tables_object$specific_summary$mean,
                         Lower = multiple_tables_object$specific_summary$ET_lower,
                         Upper = multiple_tables_object$specific_summary$ET_upper)
  rownames(printout) <- multiple_tables_object$specific_summary$study_name
  print(printout)
  cat("\n")  
}



MultipleTables._plotDensityOverlay <- function(multiple_tables_object,
                                   selected_study_names, 
                                   xlim = xlim,
                                   add_vertical = add_vertical,
                                   by = by){
  group_names <- selected_study_names
  density_df <- densityListToDf(multiple_tables_object$density, 
                                group_names)
  
  ggplot2_obj <- densityOverlay(density_df, multiple_tables_object$measure, 
                                group_name = 'study_name',
                                xlim = xlim, 
                                add_vertical = add_vertical,
                                by = by)

  return(ggplot2_obj)

  
  
}



MultipleTables._plotDensitySideBySide <- function(multiple_tables_object,
                                        selected_study_names, 
                                      xlim = xlim,
                                      add_vertical){
  
  
  group_names <- selected_study_names
  density_df <- densityListToDf(multiple_tables_object$density, 
                                group_names)
  ggplot2_obj <- densitySideBySide(density_df, multiple_tables_object$measure, 
                                group_name = 'study_name',
                                xlim = xlim, 
                                add_vertical = add_vertical)
  
  return(ggplot2_obj)
  
  
}

MultipleTables._plotForest <-function(multiple_tables_object,
                                      selected_study_names, 
                                      xlim = NULL,
                                     add_vertical = NULL,
                                      show_CI = show_CI
                                      ){
  
  digit <- multiple_tables_object$digit 
  ## add overall measure estimation to the data
  specific_summary <- (multiple_tables_object$specific_summary)[c('study_name','mean','ET_lower','ET_upper')]
  num_studies <- nrow(specific_summary)
  specific_summary$study_id <- 1:num_studies
  row_overall <- data.frame(study_id = num_studies + 1,
                            study_name = 'Overall',
                            mean = multiple_tables_object$overall_measure_estimation$point,
                            ET_lower = multiple_tables_object$overall_measure_estimation$confindent_interval[1],
                            ET_upper = multiple_tables_object$overall_measure_estimation$confindent_interval[2])
  specific_summary <- rbind(specific_summary, row_overall)
  specific_summary$cishow <- forstCIString(specific_summary, digit)
  
  ggplot2_obj <- forestPlot(specific_summary, 
                            multiple_tables_object$measure, 
                            group_name = 'study_name',
                            xlim = xlim, 
                            add_vertical = add_vertical,
                            show_CI = show_CI)
  
  return(ggplot2_obj)
  
}






