\name{raw.pvalues}
\alias{raw.pvalues}
\title{Calculation of classical multiple testing statistics and p-values}
\description{Calculates for each gene expression, the Fisher test statistics and the corresponding p-value for H0: the gene expression does not depend on the experimental condition in a model with possible covariates.}
\usage{
raw.pvalues(data, x = 1, test = x[1])
}
\arguments{
  \item{data}{'FAMTdata' object, see \code{\link{as.FAMTdata}}}
  \item{x}{Column number(s) corresponding to the experimental condition and the optional covariates (1 by default) in the 'covariates' data frame.}
  \item{test}{Column number corresponding to the experimental condition (x[1] by default) of interest in the multiple testing procedure.}
}
%\details{
\value{
  \item{pval}{Vector containing the p-values}
  \item{test}{Vector containing the F statistics}
  \item{resdf}{Residual degrees of freedom}
}
%\references{}
\author{David Causeur}
%\note{
\seealso{ \code{\link{as.FAMTdata}}
}
\examples{
data(expression)
data(covariates)
data(annotations)

# Create the 'FAMTdata'
############################################
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)
# 'FAMTdata' summary
summaryFAMT(chicken)

# Calculation of classical p-values
############################################
# test on the 6th covariate: 
rawpval = raw.pvalues(chicken,x=6)
hist(rawpval$pval)

# with a supplementary covariate (third column of the covariates data frame)
\dontrun{rawpval = raw.pvalues(chicken,x=c(3,6),test=6)}
\dontrun{hist(rawpval$pval)}
}
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
