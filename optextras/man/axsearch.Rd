\name{axsearch}
\alias{axsearch}
\encoding{UTF-8}
\title{Perform axial search around a supposed minimum and provide diagnostics}
\description{Nonlinear optimization problems often terminate at points in the 
parameter space that are not satisfactory optima. This routine conducts an axial
search, stepping forward and backward along each parameter and computing the objective
function. This allows us to compute the \code{tilt} and \code{radius of curvature} or
\code{roc} along that parameter axis. 

\code{axsearch} assumes that one is MINIMIZING the function \code{fn}. While we believe
that it will work using the wrapper \code{ufn} from this package with the 'maximize=TRUE'
setting, we believe it is much safer to write your own function that is to be minimized.
That is minimize  (-1)*(function to be maximized). All discussion here is in
terms of minimization.

Axial search may find parameters with a function value lower than that at the 
supposed minimum, i.e., lower than \code{fmin}. 

In this case \code{axsearch} exits immediately with the new function value and
parameters. This can be used to restart an optimizer, as in the optimx wrapper.

}
\usage{
       axsearch(par, fn=NULL, fmin=NULL, lower=NULL, upper=NULL, bdmsk=NULL, trace=0, ...)
}
\arguments{
 \item{par}{A numeric vector of values of the optimization function parameters that are
     at a supposed minimum.}
 \item{fn}{The user objective function}
 \item{fmin}{The value of the objective function at the parameters \code{par}. ?? what if fmin==NULL?}
 \item{lower}{A vector of lower bounds on the parameters.}
 \item{upper}{A vector of upper bounds on the parameters.}
 \item{bdmsk}{An indicator vector, having 1 for each parameter that is "free" or
     unconstrained, and 0 for any parameter that is fixed or MASKED for the
     duration of the optimization. Partly for historical reasons, we use the 
     same array during the progress of optimization as an indicator that a 
     parameter is at a lower bound (bdmsk element set to -3) or upper bound (-1).}
 \item{trace}{If trace>0, then local output is enabled.}
 \item{...}{Extra arguments for the user function.}
}

\details{
  The axial search MAY give a lower function value, in which case, one can restart.
  Its primary use is in presenting some features of the function surface in the
  tilt and radius of curvature measures returned. However, better measures should
  be possible, and this function should be regarded as largely experimental.

}

\value{
  A list with components:
  \item{bestfn}{The lowest (best) function value found (??maximize??) during the axial search, 
        else the original fmin value. (This is actively set in that case.)}
  \item{par}{The vector of parameters at the best function value. }
  \item{details}{A data frame reporting the original parameters, the forward step and backward
       step function values, the size of the step taken for a particular parameter, the tilt and
       the roc (radius of curvature). Some elements will be NA if we find a lower function
       value during the axial search.}
}
\examples{
#####################
# require(optimx)
require(optextras)
# Simple bounds test for n=4
bt.f<-function(x){
  sum(x*x)
}

bt.g<-function(x){
  gg<-2.0*x
}

n<-4
lower<-rep(0,n)
upper<-lower # to get arrays set
bdmsk<-rep(1,n)
# bdmsk[(trunc(n/2)+1)]<-0
for (i in 1:n) { 
  lower[i]<-1.0*(i-1)*(n-1)/n
  upper[i]<-1.0*i*(n+1)/n
}
xx<-0.5*(lower+upper)

cat("lower bounds:")
print(lower)
cat("start:       ")
print(xx)
cat("upper bounds:")
print(upper)

abtrvm <- list() # ensure we have the structure

cat("Rvmmin \n\n")
# Note: trace set to 0 below. Change as needed to view progress. 

# Following can be executed if package optimx available
# abtrvm <- optimr(xx, bt.f, bt.g, lower=lower, upper=upper, method="Rvmmin", 
#                 control=list(trace=0))
# Note: use lower=lower etc. because there is a missing hess= argument
# print(abtrvm)

abtrvm$par <- c(0.00, 0.75, 1.50, 2.25)
abtrvm$value <- 7.875
cat("Axial search")
axabtrvm <- axsearch(abtrvm$par, fn=bt.f, fmin=abtrvm$value, lower, upper, bdmsk=NULL, 
                     trace=0)
print(axabtrvm)

abtrvm1 <- list() # set up structure
# Following can be executed if package optimx available
# cat("Now force an early stop\n")
# abtrvm1 <- optimr(xx, bt.f, bt.g, lower=lower, upper=upper, method="Rvmmin", 
#                   control=list(maxit=1, trace=0))
# print(abtrvm1)

abtrvm1$value <- 8.884958
abtrvm1$par <- c(0.625, 1.625, 2.625, 3.625)

cat("Axial search")
axabtrvm1 <- axsearch(abtrvm1$par, fn=bt.f, fmin=abtrvm1$value, lower, upper, bdmsk=NULL, 
                      trace=0)
print(axabtrvm1)

cat("Do NOT try axsearch() with maximize\n")

}
\keyword{nonlinear}
\keyword{optimize}
\keyword{axial}
\keyword{search}
