\name{mra.control}
\alias{mra.control}

\title{ mra.control - Control over MRA fitting process}
\description{
Auxiliary function providing a user interface for 
\code{mra} fitting. Typically only used when calling one of the 
\code{*.estim} routines. 
}

\usage{
mra.control(algorithm=1, maxfn=1000, cov.meth=1, trace=0, tol=1e-07 )
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{algorithm}{ Integer specifying the maximization algorithm to use.  If \code{algorithm} = 1, 
    the VA09AD algorithm from the HSL library is used. The VA09AD algorithm is very reliable, 
    has been tested extensively (same algorithm as Program MARK), and will almost always find the 
    maximum likelihood estimates. This parameter was added to allow easy addition of other 
    algorithms in the future, but no other options are currently implemented because VA09AD 
    works so well. Anything other than 1 throws a warning and resets the value 
    to 1.  }
  \item{maxfn}{Maximum number of likelihood evaluations allowed during the fitting process.
    This determines when the minimization routine stops and concludes that the problem 
    will not converge.  The routine stops after the likelihood has been evaluated 
    \code{maxfn} times within the minimization routine (but not the hessian or gradient routine).    
    Decreasing this value will cause the fitting routine to give up on badly-behaved 
    models quicker and return faster.  In some simulation where ill-conditioned data 
    can be generated, decreasing this parameter can speed up the 
    simulation tremendously. 
    Increasing this value may allow marginally-behaved (slow to converge) models to converge. 
    }
  \item{cov.meth}{ Integer specifying the covariance estimation method. \code{cov.meth} = 1 
    takes numerical 2nd partial derivatives.  \code{cov.meth} = 2 inverts the Hessian 
    of the maximization.  Method 1 (numeric 2nd derivatives) is the preferred method, but
        is computationally expensive and therefore slower than method 2.  The difference in 
        speed is minimal when number of parameters < 15. Method 2 variances are 
        generally very close to method 1 variances, and could be used during initial model 
        fitting stages when exact estimation of variance may not be necessary.    }
  \item{trace}{Integer controlling output of intermediate results. If \code{trace} != 0, 
    a few lines will be written to the R console and a log file (named \code{mra.log}) will be 
    opened in the current directory and details of the fitting process will 
    be written there.  The level of detail written to the log depends on the value of trace. 
    If \code{trace} == 1, details written to the log include TEST2 and TEST3 results used to determine 
    c-hat, details from the fitting algorithm, the variance covariance matrix, 
    singular values of the VC matrix used to determine \code{df}, and other 
    statistics. If \code{trace} == 2, all trace == 1 details plus the likelihood output is written 
    to the log.  If \code{trace} == 3, all trace == 2 details plus the gradient is written 
    to the log.  
    
    When using \code{algorithm} = 1 (VA09AD), \code{trace} has additional meaning. If \code{trace} > 3, 
    the coefficient and gradient vectors are written every |trace| iterations and also on exit.
    Output is of the form: Function value, beta(1), beta(2), ..., beta(n), G(1), ...G(n). 
    Coefficient and gradient values are suppressed if \code{trace} < 0 (only final results printed).  
    Intermediate values from within VA09AD are suppressed if \code{trace} > \code{maxit} + 1, but 
    other intermediate values are output. 
    
    \code{trace} > 1 probably produces more output than the casual (non-geek statistician) wants 
    to look at. But, geek statisticians may find \code{trace} > 3 useful for assessing stability and 
    determining when the routine gets stuck in a local minima. 

    }
  \item{tol}{Vector or scalar of tolerance(s) for coefficients in the model.  Minimization stops and 
    concludes it has converged when |delta.b(i)| < tol(i) for all i, where delta.b(i) is
    change in parameter i on an iteration.}
}

\value{
A list with the input arguments as components.
}

\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{\code{\link{F.cjs.estim}}, \code{\link{F.huggins.estim}} }

\examples{

data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipper.cjs <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), 
  dipper.histories, control=mra.control(trace=1, maxfn=200) )

}


\keyword{ models }% __ONLY ONE__ keyword per line

