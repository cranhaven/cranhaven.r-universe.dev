


SingleTable._setControlList <- function(single_table_Obj, control_user){
  if(single_table_Obj$measure == 'RD'){
    lower_default <- -1
  } else lower_default <- 0
  
  control_default <- list(
    n_samples = 5000,
    mcmc_initial = c(0.5, 0.5),
    upper_bound = 100,
    lower_bound = lower_default,
    num_grids = 2048
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


SingleTable._sampleGen <- function(single_table_Obj, control){
  
  
  
  ### draw samples
  samples_posterior <- samplePosterior(single_table_Obj$parms_prior, 
                    single_table_Obj$data, 
                    single_table_Obj$model, 
                    single_table_Obj$measure, 
                    control$n_samples * 1.,
                    control$mcmc_initial)
  
  samples_prior <- samplePosterior(single_table_Obj$parms_prior, 
                                       list(y1=0, n1 = 0, y2 = 0, n2 = 0), 
                                       single_table_Obj$model, 
                                       single_table_Obj$measure, 
                                       control$n_samples * 1.1,
                                       control$mcmc_initial)
  
  ## output
  return(list(
              posterior = samplesTrucation(samples_posterior,lower = control$lower_bound, 
                                           upper = control$upper_bound,
                                           n_samples = control$n_samples), 
              prior = samplesTrucation(samples_prior, lower = control$lower_bound, 
                                       upper = control$upper_bound,
                                       n_samples = control$n_samples))
        )
}


SingleTable._densityCal <- function(single_table_Obj, control){
  
    
  method <- single_table_Obj$method
  
  ## estimate  density: exact or sample based
  if(method == 'sampling'){
    density_posterior_estimate <- density(single_table_Obj$samples$posterior, 
                                          from = control$lower_bound, 
                                          to = control$upper_bound, 
                                          n = control$num_grids)
    density_prior_estimate <- density(single_table_Obj$samples$prior, 
                                      from = control$lower_bound, 
                                      to = control$upper_bound, 
                                      n=control$num_grids)
    
    density_posterior <- list( x = density_posterior_estimate$x, y = density_posterior_estimate$y)
    density_prior <- list( x = density_prior_estimate$x, y = density_prior_estimate$y)
    
  }else {
    density_posterior <- densityMeasureGrids(parms_prior = single_table_Obj$parms_prior, 
                                                      data = single_table_Obj$data, 
                                                      model = single_table_Obj$model, 
                                                      measure = single_table_Obj$measure,
                                                      from = control$lower_bound, 
                                                      to = control$upper_bound, 
                                                      num_grids = control$num_grids)
    density_prior <- densityMeasureGrids(parms_prior = single_table_Obj$parms_prior, 
                                                      data = list(y1 = 0, n1 = 0, y2 = 0, n2= 0), 
                                                      model = single_table_Obj$model, 
                                                      measure = single_table_Obj$measure,
                                                      from = control$lower_bound, 
                                                      to = control$upper_bound, 
                                                      num_grids = control$num_grids)
    
  }
  return(
    list(posterior = density_posterior,
         prior = density_prior)
  )
  
  
 
  
}


SingleTable._summaryCalSampling <- function(single_table_Obj, control){
  
  
  summary_list = list(
    posterior = summaryStatisticsSampling(single_table_Obj$samples$posterior, 
                                          single_table_Obj$alpha,
                                          lower = control$lower_bound,
                                          upper = control$upper_bound),  
    prior = summaryStatisticsSampling(single_table_Obj$samples$prior, 
                                           single_table_Obj$alpha,
                                           lower = control$lower_bound,
                                           upper = control$upper_bound)
  )
  return(summary_list)
}


SingleTable._summaryCalExact <- function(single_table_Obj, control){
  
  posterior_summary <- summaryStatisticsExact( single_table_Obj$parms_prior,
                                      single_table_Obj$data,
                                      single_table_Obj$samples$posterior,
                                      single_table_Obj$measure,
                                      single_table_Obj$model,
                                      single_table_Obj$alpha, 
                                      is_prior = FALSE, 
                                      lower = control$lower_bound, 
                                      upper = control$upper_bound)
  
  prior_summary <- summaryStatisticsExact(single_table_Obj$parms_prior,
                            single_table_Obj$data,
                            single_table_Obj$samples$prior,
                            single_table_Obj$measure,
                            single_table_Obj$model,
                            single_table_Obj$alpha,
                            is_prior =TRUE,
                            lower = control$lower_bound,
                            upper = control$upper_bound)
  summary_list = list(
    posterior = posterior_summary, 
    prior = prior_summary
  )
  return(summary_list)
}




SingleTable._isObjectFitted <- function(single_table_Obj){
  ## check input
  if (!inherits(single_table_Obj, "SingleTable"))
    stop("Use only with 'SingleTable' objects.\n")
  
  if (is.null(single_table_Obj$density) | is.null(single_table_Obj$samples))
    stop("Use modelFit method before summary \n")
}

SingleTable._summaryPrint <- function(single_table_Obj){
  siglevel <- paste(as.character((1-single_table_Obj$alpha)*100),"%",sep="")
  digit <- single_table_Obj$digit
  
  if (single_table_Obj$measure == "OR") measure_full_name<-"Odds ratio"
  if (single_table_Obj$measure == "RR") measure_full_name<-"Relative risk"
  if (single_table_Obj$measure == "RD") measure_full_name<-"Risk difference"
  
  cat("Measure:",measure_full_name,'\n')
  cat("Model: ", single_table_Obj$model ," Beta-Binomial Model \n")
  cat('Posterior distribution: \n')
  cat("Mean: ",round(single_table_Obj$summary$posterior$mean,digit),'\n')
  cat("Median: ",round(single_table_Obj$summary$posterior$median,digit), '\n')
  cat(siglevel," ET CI: ", CItoString(single_table_Obj$summary$posterior$ET_CI, digit),'\n')
  cat(siglevel," HDR CI: ", CItoString(single_table_Obj$summary$posterior$HDR_CI,digit),'\n')
  
  
  cat("\n")
}



SingleTable._plotSideBySide <- function(single_table_Obj, 
                                        xlim = xlim, 
                                        add_vertical = add_vertical){
  group_names <- c('posterior', 'prior')
  density_df <- densityListToDf(single_table_Obj$density, group_names)
  ggplot2_obj <- densitySideBySide(density_df, single_table_Obj$measure, 
                                   group_name = 'Type',
                                   xlim = xlim, 
                          add_vertical = add_vertical)
  return(ggplot2_obj)
  
}



SingleTable._plotOverlay <- function(single_table_Obj, 
                                     xlim = xlim, 
                                     add_vertical = add_vertical,
                                     by = by){
  group_names <- c('posterior', 'prior')
  density_df <- densityListToDf(single_table_Obj$density, group_names)
  ggplot2_obj <- densityOverlay(density_df, single_table_Obj$measure, xlim = xlim, 
                                group_name = 'Type',
                                add_vertical = add_vertical,
                                by = by)
  return(ggplot2_obj)
    
  
  
  
}