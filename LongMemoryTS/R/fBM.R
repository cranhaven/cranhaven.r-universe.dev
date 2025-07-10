##########################################################################################
####            Fractional Brownian Motion / Bridge of Type I                      #######
##########################################################################################

#---------------------------       Fractional Brownian Motion of Type I       ------------------------------------#

#' @title Fractional Brownian Motion / Bridge of Type I or II.
#' @description \code{fBM} simulates a fractional Brownian motion / bridge of type I or II.
#' @param n number of increments in the fractional Brownian motion.
#' @param d memory parameter -0.5<d<0.5. Note that \code{d=H-1/2}.
#' @param type either \code{"I"} or \code{"II"}, to define the type of motion.
#' @param bridge either \code{TRUE} of \code{FALSE}, to specify whether ar fractional
#' Brownian motion or bridge should be returned. Default is \code{FALSE} so that the function
#' returns a fractional Brownian motion.
#' @references Marinucci, D., Robinson, P. M. (1999). Alternative forms of fractional Brownian motion. 
#' Journal of statistical planning and inference, 80(1-2), 111 - 122.
#' 
#' Davidson, J., Hashimzade, N. (2009). Type I and type II fractional Brownian motions: A reconsideration. 
#' Computational statistics & data analysis, 53(6), 2089-2106. 
#' 
#' Bardet, J.-M. et al. (2003): Generators of long-range dependent processes: a survey. 
#' Theory and applications of long-range dependence, pp. 579 - 623, Birkhauser Boston.
#' 
#' @author Kai Wenger
#' @examples
#' n<-1000
#' d<-0.4
#' set.seed(1234)
#' motionI<-fBM(n,d, type="I")
#' set.seed(1234)
#' motionII<-fBM(n,d, type="II")
#' ts.plot(motionI, ylim=c(min(c(motionI,motionII)), max(motionI,motionII)))
#' lines(motionII, col=2)
#' @export

fBM <- function(n,d, type=c("I","II"), bridge=FALSE){
  type<-type[1]
  if((type%in%c("I","II"))==FALSE)stop('type must be either "I" or "II"')
  if(bridge%in%c(TRUE,FALSE)==FALSE)stop('bridge must be either TRUE or FALSE')
  n          <- n+1
  
  ## simulate second term which is identical for type I and type II processes
  tt         <- (1:(n-1))
  s          <- (0:(n-1))
  TT         <- matrix(tt,n-1,n)
  S          <- matrix(s,n-1,n,byrow=TRUE)
  trend      <- TT-S
  
  randomvar<- matrix(rnorm(n),n-1,n,byrow=TRUE)
  integral <- trend^(d)*randomvar
  integral[is.infinite(integral)] <- 0
  
  fbmI_part <- randomvar[1,n]-randomvar[1,]
  fbmI_part <- fbmI_part[-n]
  
  integral[upper.tri(integral)] <- 0
  if(type=="II"){integral<-sqrt(2*d+1)*integral}
  
  second_term    <- rowSums(integral)
  
  first_term<-0
  C_square<-1
  
  if(type=="I"){
    n2         <- n
    tt  <- t2  <- (1:(n-1))
    s2         <- ((-1.5*n2):0)
    T2         <- matrix(t2,(n-1),(1.5*n2)+1)
    S2         <- matrix(s2,n-1,(1.5*n2)+1,byrow=TRUE)
    first_term <- ((T2-S2)^(d)-(-S2)^(d))*matrix(rnorm(1.5*n2+1),n-1,(1.5*n2)+1,byrow=TRUE)
    first_term[,1.5*n+1] <- rep(0,(n-1))
    first_term <- rowSums(first_term)
    C_square       <- (gamma(d+1)^(2))/(gamma(2*d+2)*sin((d+0.5)*pi))
    
  }

  B_d            <- (1/sqrt(C_square))*(first_term+second_term)/(n^(1/2+d))
  
  if(bridge==TRUE){B_d<- B_d-(1:(n-1))/(n-1)*B_d[n-1]}
  return(B_d)
}
