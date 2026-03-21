#' @importFrom dplyr "%>%"
select_best_mats <- function(output, initial_matrix, future_intx, current.period){
  x <- lapply(output, identify_inconsistencies)
  icounts <-  lapply(x, "[[", "i_count")
  
  #Limit MIOs to only those with minimal inconsistencies
  output_minimal_i <- output[which(icounts == icounts[[which.min(icounts)]])]
  
  #Calculate proportion of dyadic relationships that are identical between each
  #MIO and the previous order (with new individuals added)
  dists <- lapply(output_minimal_i, dyadic_similarity, order2 = initial_matrix)
  
  #Limit MIOs to only those that are maximally similar to previous order
  output_best <- output_minimal_i[dists == dists[[which.max(dists)]]]
  output_best <- unique(output_best)
  ##If more than one MIO remains, compare using spearman's correlation
  if(length(output_best) > 1){
    corrs <- lapply(output_best, order_corr, original = initial_matrix)
    output_maximum_corr <- output_best[corrs == corrs[[which.max(corrs)]]]
    #If more than one MIO remains, select MIOs that are most consistent with 
    #interactions from current study period
    if(length(output_maximum_corr) > 1){
      incons <- lapply(X = output_maximum_corr,
                       FUN = function(x)(initial_matrix[dimnames(x)[[1]], dimnames(x)[[1]]][lower.tri(initial_matrix[dimnames(x)[[1]], dimnames(x)[[1]]])] %>%
                                           sum()))
      
      output_most_consistent <- output_maximum_corr[incons == incons[[which.min(incons)]]]
      #If more than one MIO remains, select MIOs that are most consistent with 
      #interactions from subsequent study period
      if(length(output_most_consistent) > 1){
        future_incons <- lapply(X = output_most_consistent,
                                FUN = function(x)(future_intx[dimnames(x)[[1]], dimnames(x)[[1]]][lower.tri(future_intx[dimnames(x)[[1]], dimnames(x)[[1]]])] %>%
                                                    sum()))
        consistent_with_future <- output_most_consistent[future_incons == future_incons[[which.min(future_incons)]]]
        #If more than one MIO remains, randomly select one of the remaining MIOs
        #and print warning
        if(length(consistent_with_future) == 1){
          return(consistent_with_future)
        }else{
          cat(gsub('q', current.period, '\n     CAUTION: q failed to converge! '))
          return(consistent_with_future)
        }
      }else{return(output_most_consistent)}
    }else{return(output_maximum_corr)}
  }else{return(output_best)}
}