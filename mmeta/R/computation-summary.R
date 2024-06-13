

summaryStatisticsSampling <- function(samples, alpha, lower = lower, upper = upper){
  return(list(mean = mean(samples,na.rm = TRUE), 
              median = median(samples, na.rm = TRUE), 
              ET_CI = quantile(samples,probs = c(alpha/2, 1- alpha/2),na.rm=TRUE), 
              HDR_CI = minlength_CI(samples,alpha, left = lower, right = upper) ))
}




summaryStatisticsExact <- function(parms_prior, 
                                   data,
                                   samples, 
                                   measure, 
                                   model,
                                   alpha, 
                                   is_prior = FALSE, 
                                   lower = lower, 
                                   upper = upper){
  
  summary_statistics <- summaryStatisticsSampling(samples, alpha, 
                                                 lower = lower, upper = upper)
  if(is_prior == TRUE){
    data  <- list(y1 = 0, n1 = 0, y2 = 0, n2 = 0)
  } 
  
  mean_exact <- kthMomentExact(parms_prior = parms_prior, 
                               data = data ,
                               measure = measure, 
                               model= model, 
                               kth=1)
  
  summary_statistics$mean <- mean_exact
  return(summary_statistics)
  
  
}
