#' Estimate h-step ahead forecasts based on the recovered transition matrix.
#'
#' @param yf A d x T data matrix where d is the number of observed variables and T is the number of timepoints.
#' @param h An integer indicating the forecast horizon.
#' @param A A d x d transition matrix.
#' @keywords var forecast internal
#' @noRd
var_forecast = function(yf, h, A){

  T1 = dim(yf)[2];
  k = dim(A)[1];
  p = dim(A)[2]/k;

  id = seq(from= T1, to = T1-p+1, by=-1);

  Y1 = Y = matrix(0, k, h);
  for(r in 1:h){
  	Y[,r] = A%*%as.vector(yf[,id]);
        yf = cbind(yf[,-1], Y[,r]);
  	Y1[,r] = Y[,r];
  }

  return(Y1) # Final data is k*T matrix
}
