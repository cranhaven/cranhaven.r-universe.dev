#' This function computes the AS test using DGM implementation in the package RationalExp
#'
#' @param lamb the point under the form lambda q to be tested.
#' @param YY the observations of the outcome variable.
#' @param XX the observations of the regressor X'q variable.
#' @param tuningParam the list of tuning parameters. For the details see the function "test" in the package RationalExp.
#'
#' @return
#' the result of the test at level 5%
#'

AStest<- function(lamb,YY,XX,tuningParam=NULL){
  y_tilde <- rbind(matrix(YY,length(YY),1),matrix(lamb*(XX),length(XX),1)) ## concatenation of y then psi
  D <- rbind(matrix(1,length(YY),1),matrix(0,length(XX),1)) ## vector of D's
  res <-  test(y_tilde ,D,NULL,NULL,NULL,1,tuningParam)
  test0 = (res[[7]]<0.05)
  return(test0)
}
