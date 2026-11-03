#' Function to create the matrix of the support points for the common regressors Xc
#'
#' @param dimX the dimension of the common regressors Xc.
#' @param c_var the label of these regressors.
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors.
#'
#' @return
#' a matrix of the values of the support points for the common regressors Xc
#'
create_values <- function(dimX,c_var,Rdata){
  ### to do, modify to include the possibility of HD factor variables.
  res = vector("list")
  for(i in 1:length(c_var)){
    res[[i]] <- sort(unique(Rdata[,c_var[i]]))
  }

  if(dimX ==1){
    res =res[[1]]
  }else if(dimX ==2){
    res = expand.grid(res[[1]],res[[2]])
  }else if(dimX ==3){
    res = expand.grid(res[[1]],res[[2]],res[[3]])
  }else if(dimX ==4){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]])
  }else if(dimX ==5){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]])
  }else if(dimX ==6){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]])
  }else if(dimX ==7){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]],res[[7]])
  }else if(dimX ==8){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]],res[[7]],res[[8]])
  }else if(dimX ==9){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]],res[[7]],res[[8]],res[[9]])
  }else if(dimX ==10){
    res = expand.grid(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]],res[[7]],res[[8]],res[[9]],res[[10]])
  }

  return(res)
}
