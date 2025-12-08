\name{histo}
\alias{histo}
\title{ plot the histogram of the variable T }
\description{
   plot of the random variable T and the equivalent function (\eqn{t->CR*exp(-R*t)}).
   in some cases, there are also plotted the theoretical and the De Vielder functions
}
\usage{
histo(X, disXi = NULL, disP = NULL, plotDV = FALSE)
}
\arguments{
  \item{X}{ a list with the following components : T the observation of the random variable ; R the risk constant (estimated or theoretical calculated)
  ; CR the risk constand deduced from R ; lambdaEmp the estimation of the lambda of the De Vielder method (only available if \code{DV=TRUE} and if there is no problem with De Vielder method)
  ; muEmp the estimation of the mu of the De Vielder method (only available if \code{DV=TRUE}); lambdaHat the best estimation of lambda : the ESBVM of lambda
  ; muHat the best estimation of mu : the value of mu which maximize the loglikelihood (only available if the parametric estimation has been done)}
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }

  \item{disP}{ he side effect probability (success  probability of Zi) p :
disP is a 3 elements list : disfun stands for a distribution function
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{plotDV}{ a logical for : do you want to plot the De Vielder "function" }
}
\value{
a NULL object.
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
#use mainSimul to make the first argument
arg1Exp <- list(rangen=rexp,nbparam=1,param=list(1/3));
arg1Bin <- list(rangen=rbinom,nbparam=2,param=list(1,1/20));
arg1Unif <- list(rangen=runif,nbparam=2,param=list(0,20));
arg1Lnorm <- list(rangen=rlnorm,nbparam=2,param=list(1/4,1));

arg2Exp <- list(disfun=pexp,nbparam=1,param=list(1/5));
arg2Cst <- list(disfun=pcst <- function(x,p) p ,nbparam=1,param=list(1/3));
arg2Comp <- list(disfun=pcomp <- function(x,mu1,mu2,mu3)
{1-1/3*exp(-mu1* x)-1/2*exp(-mu2 *x)-1/6*exp(-mu3 *x)}
,nbparam=3,param=list(1/3,1/5,1/10));
arg2Gamma <- list(disfun=pgamma,nbparam=2,param=list(3,1/3));
arg2Lnorm <- list(disfun=plnorm,nbparam=2,param=list(1/20,2));

T <- mainSimul(100,100,arg1Exp,arg2Exp)

histo(T,arg1Exp,arg2Exp)
}
\keyword{ print }
