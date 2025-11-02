\name{emfa}
\alias{emfa}
\title{Factor Analysis model adjustment with the EM algorithm}

\description{A function to fit a Factor Analysis model with the EM algorithm.}
\usage{
emfa(data, nbf, x = 1, test = x[1], pvalues = NULL, min.err = 0.001)
}
\arguments{
  \item{data}{'FAMTdata' object, see \code{\link{as.FAMTdata}}}
  \item{nbf}{Number of factors of the FA model, see \code{\link{nbfactors}}}
  \item{x}{Column number(s) corresponding to the experimental condition and the optional covariates (1 by default) in the covariates data frame.}
  \item{test}{Column number corresponding to the experimental condition (x[1] by default) on which the test is performed.}
  \item{pvalues}{p-values of the individual tests. If NULL, the classical procedure is applied (see \code{\link{raw.pvalues}})}
  \item{min.err}{Stopping criterion value for iterations in EM algorithm (default value: 0.001)}
}
\details{In order to use this function, the number of factors is needed (otherwise, use \code{\link{nbfactors}}). 
}
\value{
  \item{B}{Estimation of the loadings}
  \item{Psi}{Estimation of Psi}
  \item{Factors}{Scores of the individuals on the factors}
  \item{commonvar}{Proportion of genes common variance (modeled on the factors)}
  \item{SelectHo}{Vector of row numbers corresponding to the non-significant genes}
}

\references{Friguet C., Kloareg M. and Causeur D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104:488, p.1406-1415 }

\author{David Causeur}

%\note{
\seealso{\code{\link{as.FAMTdata}}, \code{\link{nbfactors}}
}
\examples{
## Reading 'FAMTdata'
data(expression)
data(covariates)
data(annotations)
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)

# EM fitting of the Factor Analysis model
chicken.emfa = emfa(chicken,nbf=3,x=c(3,6),test=6)
chicken.emfa$commonvar
}
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
