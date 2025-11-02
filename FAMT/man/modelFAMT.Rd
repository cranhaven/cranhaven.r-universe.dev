\name{modelFAMT}
\alias{modelFAMT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The FAMT complete multiple testing procedure
}
\description{This function implements the whole FAMT procedure (including \code{\link{nbfactors}} and \code{\link{emfa}}). The number of factors considered in the model is chosen to reduce the variance of the number of the false discoveries. The model parameters are estimated using an EM algorithm. Factor-adjusted tests statistics are derived, as well as the corresponding p-values.
}
\usage{
modelFAMT(data, x = 1, test = x[1], nbf = NULL, maxnbfactors = 8, 
min.err = 0.001)
}
\arguments{
  \item{data}{'FAMTdata' object, see \code{\link{as.FAMTdata}}}
  \item{x}{Column number(s) corresponding to the experimental condition and the optional covariates (1 by default) in the covariates data frame.}
  \item{test}{Column number corresponding to the experimental condition (x[1] by default) one which the test is performed.}
  \item{nbf}{The number of factors of the FA model (NULL by default). If NULL, the function estimates the optimal nbf (see \code{\link{nbfactors}})}
  \item{maxnbfactors}{The maximum number of factors (8 by default)}
  \item{min.err}{Stopping criterion value for iterations (default value:0.001)}
}
%\details{

\value{
  \item{adjpval}{Vector of FAMT factor-adjusted p-values}
  \item{adjtest}{Vector of FAMT factor-adjusted F statistics}
  \item{adjdata}{Factor-adjusted FAMT data}
  \item{FA}{Estimation of the FA model parameters}
  \item{pval}{Vector of classical p-values}
  \item{x}{Column number(s) corresponding to the experimental condition and the optional covariates in the covariates data frame}
  \item{test}{Column number corresponding to the experimental condition on which the test is performed}
  \item{nbf}{The number of factors used to fit the FA model}
  \item{idcovar}{The column number used for the array identification in the 'covariates' data frame}
}

\references{Friguet C., Kloareg M. and Causeur D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104:488, p.1406-1415 }

\author{David Causeur}

\note{The user can perform individual test statistics putting the number of factors (\code{nbf}) equal to zero.  
The result of this function is a 'FAMTmodel'. It is used as argument in other functions of the package : \code{\link{summaryFAMT}}, \code{\link{pi0FAMT}} or \code{\link{defacto}}. 
We advise to carry out a summary of FAMT model with the function \code{\link{summaryFAMT}}.
}

\seealso{\code{\link{as.FAMTdata}}, \code{\link{raw.pvalues}}, \code{\link{nbfactors}}, \code{\link{emfa}}, \code{\link{summaryFAMT}}
}

\examples{
## Reading 'FAMTdata'
data(expression)
data(covariates)
data(annotations)

chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)

# Classical method with modelFAMT 
\dontrun{modelpval=modelFAMT(chicken,x=c(3,6),test=6,nbf=0)}
\dontrun{summaryFAMT(modelpval)}

# FAMT complete multiple testing procedure
# when the optimal number of factors is unknown
\dontrun{model = modelFAMT(chicken,x=c(3,6),test=6)

# when the optimal number of factors has already been estimated 
 model = modelFAMT(chicken,x=c(3,6),test=6,nbf=3)

summaryFAMT(model)
hist(model$adjpval)}
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
