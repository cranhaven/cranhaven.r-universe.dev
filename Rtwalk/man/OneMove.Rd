
\name{OneMove}

\alias{OneMove}

\title{One move of the t-walk}

\description{
Evaluates the t-walk kernel once and returns the proposed jumping points and the acceptance propability.
}

\usage{
OneMove(dim, Obj, Supp, x, U, xp, Up,
  at=6, aw=1.5, pphi=min( dim, 4)/dim, F1=0.4918, F2=0.9836, F3=0.9918
  , ...) 
}


\arguments{

    \item{dim}{
        dimension of the objective function.
        } 
    \item{Obj}{
        a function that takes a vector of length=\code{dim} and returns -log of the objective function, besides an adding constant.
        }
    \item{Supp}{
        a function that takes a vector of length=\code{dim} and returns TRUE if the vector is within the support of the objective
        and FALSE otherwise.  Supp is *always* called right before Obj.
        }
    \item{x}{First of a pair of initial points, within the support of the objective function. \code{x0} and \code{xp} must be different.}
    \item{U}{Current value of \code{Obj} at x.}
    \item{xp}{Second of a pair of initial points, within the support of the objective function. \code{x0} and \code{xp} must be different.}
    \item{Up}{Current value of \code{Obj} at xp.}
    \item{at}{The remaining parameters are the traverse and walk kernel parameters, the parameter choosing probability and the cumulative probabilities
        of choosing each kernel.  These are not intended to be modified in standard calculations.}
    \item{aw}{See description for \code{at}.}
    \item{pphi}{See description for \code{at}.}
    \item{F1}{See description for \code{at}.}
    \item{F2}{See description for \code{at}.}
    \item{F3}{See description for \code{at}.}
    \item{...}{Other parameters passed to \code{Obj}.}
}

\value{

	A list with the following items:
	
%	\tabular{ll}{
	
	 \code{y, yp}   propolsals. \cr
	\code{propU, propUp}   value of the objective at y and yp. \cr
	\code{A} Metropilis-Hastings ratio, acceptance probability = min(1,move$A). \cr
	\code{funh}   Kernel used: 1=traverse, 2=walk, 3=hop, 4=blow. \cr 
	
%	}  
}


\references{
Christen JA and Fox C (2010). A general purpose sampling algorithm for continuous distributions (the t-walk)., Bayesian Analysis, 5 (2), 263-282. URL: http://ba.stat.cmu.edu/journal/2010/vol05/issue02/christen.pdf
}


\author{
  J Andres Christen (CIMAT, Guanajuato, MEXICO).
}

\seealso{\code{\link{Runtwalk}}}

\examples{  

#### We first load the twalk package:
library(Rtwalk)



#### A ver simple example, 4 independent normals N(0,1):
x <- runif( 4, min=20, max=21)
xp <-  runif( 4, min=20, max=21)
U <- sum(x^2)/2
Up <- sum(x^2)/2

move  <- OneMove( dim=4, Obj=function(x) { sum(x^2)/2 }
  , Supp=function(x) { TRUE }, x=x, U=U, xp=xp, Up=Up)

if (runif(1) < move$A) ### the actual acceptance probability is min(1,A)
{ ## accepted
      x <- move$y
      U <- move$propU

      xp <- move$yp
      Up <- move$propUp

}
##else Not accepted


### etc.
}

\keyword{Automatic}
\keyword{self adjusting}
\keyword{MCMC}
\keyword{stand alone kernel}
