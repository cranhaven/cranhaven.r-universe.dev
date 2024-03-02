\name{estMaxLik}
\alias{estMaxLik}
\alias{estMaxLik.TSdata}
\alias{estMaxLik.TSmodel}
\alias{estMaxLik.TSestModel}
\title{Maximum Likelihood Estimation}
\description{Maximum likelihood estimation.}
\usage{
    estMaxLik(obj1, obj2=NULL, ...) 
    \method{estMaxLik}{TSmodel}(obj1, obj2, algorithm="optim",
	algorithm.args=list(method="BFGS", upper=Inf, lower=-Inf, hessian=TRUE),
	...)
    \method{estMaxLik}{TSestModel}(obj1, obj2=TSdata(obj1), ...)
    \method{estMaxLik}{TSdata}(obj1, obj2, ...) 
}
\arguments{
    \item{obj1}{an object of class TSmodel, TSdata or TSestModel}
    \item{obj2}{TSdata or a TSmodel to be fitted with obj1.}
    \item{algorithm}{the algorithm ('optim', or 'nlm' ) to use for maximization.}
    \item{algorithm.args}{arguments for the optimization algorithm.}
    \item{...}{arguments passed on to other methods.}
}
\value{
The value returned is an object of class TSestModel with  additional
elements \code{est$converged}, which is TRUE or FALSE indicating convergence, 
\code{est$converceCode}, which is the code returned by the estimation algorithm, 
and \code{est$results}, which are detailed results returned by the estimation 
algorithm. The hessian and gradient in results could potentially 
be used for restarting in the case of non-convergence, but that has not 
yet been implemented.
}
\details{
  One of \code{obj1} or \code{obj2} should specify a \code{TSmodel} and 
  the other \code{TSdata}. If \code{obj1} is a \code{TSestModel} and 
  \code{obj2} is \code{NULL}, then the data is extracted from \code{obj1}. 
  The \code{TSmodel} object is used to specify both the initial parameter 
  values and the model structure (the placement of the parameters
  in the various arrays of the TSmodel). Estimation attempts to minimimize the
  negative log likelihood (as returned by \code{l} ) of the given model 
  structure by adjusting the
  parameter values. A \code{TSmodel} can also have constant values in 
  some array elements, and these are not changed. (See \code{SS},
  \code{ARMA} and  \code{fixConstants} regarding setting of constants.)
  
  With the number of parameter typically used in multivariate time series 
  models, the default maximum number of iterations may not be enough. Be sure to
  check for convergence (a warning is printed at the end, or use \code{summary}
  on the result). The maximum iterations is passed to the estimation algorithm 
  with \code{algorithm.args}, but the elements of that list will depend on the 
  specified optimization \code{algorithm} (so see the help for the alogrithm). 
  The example below is for the default \code{optim} algorithm.
}
\section{Warning}{
  Maximum likelihood estimation of multivariate time series models tends to be
  problematic, even when a good structure and good starting parameter values
  are known. This is especially true for state space models. Also, it seems that
  in-sample fit is often obtained at the expense of out-of-sample forecasting
  ability. If a prior model structure is not important then the \code{bft}
  estimation method may be preferable.
}
\seealso{
\code{\link{optim}},
\code{\link{nlm}},
\code{\link{estVARXls}},
\code{\link{bft}},
\code{\link{TSmodel}},
\code{\link{l}},
\code{\link{SS}},
\code{\link{ARMA}},
\code{\link{fixConstants}}
}
\examples{
  true.model <- ARMA(A=c(1, 0.5), B=1)
  est.model <-  estMaxLik(true.model,  simulate(true.model))
  summary(est.model)
  est.model
  tfplot(est.model)
  est.model <-  estMaxLik(true.model,  simulate(true.model),
    algorithm.args=list(method="BFGS", upper=Inf, lower=-Inf, hessian=TRUE,
                        control=list(maxit=10000)))
}
\concept{DSE}
\keyword{ts}

