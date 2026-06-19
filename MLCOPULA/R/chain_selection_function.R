#########################################
# CHAIN SELECTION FUNCTION
# Runs both chain estimation algorithms
# Selects the best chain based on the sum of mutual information

selection <- function(mi_matrix){
  result_1 <- chain_graphical_model(mi_matrix)
  result_2 <- chain_tst(mi_matrix)
  
  if (result_1$total_MI > result_2$total_MI){
    return(result_1)
  }else{
    return(result_2)
  }
}