#' Function to tabulate the values the common regressors Xc whatever the dimension.
#'
#' @param k the considered value in "values"
#' @param values  the different unique points of support of the common regressor Xc.
#' @param Xc0 dataset containing Xc, the common regressors.
#' @param dimXc the dimension of Xc
#'
#' @return
#' a matrix of the number of times the kth value in the vector values appears.
tabulate_values <- function(k,values,Xc0,dimXc){

  if(dimXc==1){
    val = values[k,]
    sel_x = (Xc0==val)
  }else{
    val = t(as.matrix(values[k,]))
    sel_x = matrix(1,dim(Xc0)[1],1)
    for(ddd in 1:dimXc){
      sel_x =  sel_x & (Xc0[,ddd]==val[ddd])
    }
    sel_x = matrix( sel_x,dim(Xc0)[1],1)
  }

  return(sum(sel_x))
}
