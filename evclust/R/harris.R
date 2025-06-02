#' Harris gradient-based optimization algorithm
#'
#' The optimization algorithm implemented in \code{harris} is described on Silva & Almeida (1990) and
#' summarized in Denoeux & Masson (2004). The four parameters are:
#' \describe{
#'   \item{options[1]}{Display parameter : 1 (default) displays some results.}
#'   \item{options[2]}{Maximum number of iterations (default: 100).}
#'   \item{options[3]}{Relative error for stopping criterion (default: 1e-4).}
#'   \item{options[4]}{Number of iterations between two displays.}
#'  }
#'
#' @param fun Function to be optimized. The function 'fun' should return a scalar function value 'fun' 
#' and a vector 'grad' containing the partial derivatives of fun at x.
#' @param x Initial value (a vector).
#' @param options Vector of parameters (see details).
#' @param tr If TRUE, returns a trace of objective function vs CPU time
#' @param ... Additional parameters passed to fun
#'
#' @return A list with three attributes:
#'  \describe{
#'   \item{par}{The minimizer of fun found.}
#'   \item{value}{The value of fun at par.}
#'   \item{trace}{The trace, a list with two attributes: 'time' and 'fct' (if tr==TRUE).}
#'  }
#'
#'@references F. M. Silva and L. B. Almeida. Speeding up backpropagation. In Advanced Neural 
#'Computers, R. Eckmiller, ed., Elsevier-North-Holland, New-York, 151-158, 1990.
#'
#'T. Denoeux and M.-H. Masson. EVCLUS: Evidential Clustering of Proximity Data.
#'IEEE Transactions on Systems, Man and Cybernetics B, Vol. 34, Issue 1, 95--109, 2004.
#'
#' @author Thierry Denoeux.
#'
#' @export
#'
#' @seealso \code{\link{pcca}}
#' 
#' @examples 
#' opt<-harris(function(x) return(list(fun=sum(x^2),grad=2*x)),rnorm(2),tr=TRUE)
#' print(c(opt$par,opt$value))
#' plot(opt$trace$fct,type="l")
#'

harris<- function(fun,x,options=c(1,100,1e-4,10),tr=FALSE,...){

pas <- rep(0.1,length(x))
a <- 1.2
b <- 0.8
c <- 0.5
ovf <- 1e4 
unf <- 1e-6
alpha<-0.9

it <- 0
gain<-1

fungrad<-fun(x,...) 
yp<-fungrad$fun
gp<-fungrad$grad
xp<-x
x <- xp - pas * gp

# new part on trace
if(tr){
  Trace<-list(time=matrix(0,options[2]+1,3),fct=rep(0,options[2]+1))
  Trace$time[1,]<-c(0,0,0)
  Trace$fct[1]<-yp
  ptm<-proc.time()[1:3]
} else Trace<-NULL

while((gain/abs(yp) >= options[3]) & (it < options[2])){
  it<-it+1
  fungrad<-fun(x,...) 
  y<-fungrad$fun
  g<-fungrad$grad
  
#---
  if(tr){
    Trace$time[it+1,]<-proc.time()[1:3]-ptm
    Trace$fct[it+1]<-y
  } 
#---
  if(options[1] >0){
    if(it %% options[4]==1) print(c(it,y,gain/abs(yp))) 
  }
  if(y > yp){
    x <- xp
    g <- gp 
    pas <- pas * c
    x <- x - pas * g
  }    
  else{
    gain<- alpha*gain+(1-alpha)*abs(yp-y)
    xp <- x
    test <- as.integer((g * gp) >= 0)
    pas <- ((test * a) + ((1-test) * b)) * pas
    pas <- as.integer(pas<=ovf) * pas + as.integer(pas>ovf) * ovf
    pas <- as.integer(pas>=unf) * pas + as.integer(pas<unf) * unf
    gp <- g
    x <- x - pas * g
    yp <- y
  }
}
return(list(par=x,value=y,trace=Trace))
}
