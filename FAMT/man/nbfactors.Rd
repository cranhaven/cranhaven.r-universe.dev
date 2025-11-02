\name{nbfactors}
\alias{nbfactors}

\title{Estimation of the optimal number of factors of the FA model}
\description{The optimal number of factors of the FA model is estimated to minimize the variance of the number of false positives (see Friguet et al., 2009).}
\usage{
nbfactors(data, x = 1, test = x[1], pvalues = NULL, maxnbfactors = 8, 
diagnostic.plot = FALSE, min.err = 0.001)
}
\arguments{
  \item{data}{'FAMTdata' object, see \code{\link{as.FAMTdata}}}
  \item{x}{Column number(s) corresponding to the experimental condition and the optional covariates (1 by default) in the covariates data frame}
  \item{test}{Column number corresponding to the experimental condition (x[1] by default) on which the test is performed}
  \item{pvalues}{Vector of p-values for the individual tests. If NULL, the classical procedure is applied (see \code{\link{raw.pvalues}})}
  \item{maxnbfactors}{The maximum number of factors for the FA model (8 by default)}
  \item{diagnostic.plot}{boolean (FALSE by default). If TRUE, the values of the variance inflation criteria for each number of factors are plotted}
  \item{min.err}{Stopping criterion value for iterations (default value : 0.001)}
}
%\details{

\value{
  \item{optimalnbfactors}{Optimal number of factors of the FA model (an elbow criterion is used)}
  \item{criterion }{Variance criterion for each number of factors}
}
\references{ Friguet C., Kloareg M. and Causeur D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104:488, p.1406-1415 }

\author{ David Causeur }
%\note{
\seealso{\code{\link{as.FAMTdata}}, \code{\link{emfa}}
}
\examples{ 
## Reading 'FAMTdata'
data(expression)
data(covariates)
data(annotations)
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)

# Estimation of the number of factors 
\dontrun{nbfactors(chicken,x=c(3,6),test=6)}

# Estimation of the number of factors with a graph of variance inflation 
# criterion
\dontrun{nbfactors(chicken,x=c(3,6),test=6, diagnostic.plot=TRUE)}

}
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
