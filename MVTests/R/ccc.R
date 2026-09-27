#' @title  Concordance Correlation Coefficient
#'
#' @description
#' Classical Concordance Correlation Coefficient
#' @details
#' \code{ccc} function calculates directly classical concordance correlation coefficient.
#' 
#' 
#' @param x the vector which contains the first variable values
#' @param y the vector which contains the second variable values
#' @export
#'
#' @return a list with 1 elements:
#' \item{coef}{The value of concordance correlation coeffient}
#' @references Bulut, H (2025). A Robust Concordance Correlation Coefficient. (Unpublished)
#' @references Lin, L. I. "A Concordance Correlation-Coefficient to Evaluate Reproducibility." Biometrics 45, no. 1 (1989): 255-68. 
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' x<-rnorm(50)
#' y<-2+3*x+rnorm(50,mean = 3)
#' ccc(x,y)

ccc<-function(x,y){
  mu_x<-mean(x)
  mu_y<-mean(y)
  
  sxx<-var(x)
  syy<-var(y)
  sxy<-cov(x,y)
  coef<-(2*sxy)/(sxx+syy+(mu_x-mu_y)^2)
  return(list(coef=coef))
}

