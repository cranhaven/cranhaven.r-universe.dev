\name{summaryFAMT}
\alias{summaryFAMT}
\title{Summary of a FAMTdata or a FAMTmodel}

\description{The function produces summaries of 'FAMTdata' or 'FAMTmodel'. The function involves a specific method depending on the class of the main argument.

If the main argument is a 'FAMTdata' object, the function provides, for the 'expression file', the number of tests (which corresponds to the number of genes or rows), the sample size (which is the number of arrays or columns). The function provides classical summaries for 'covariates' and 'annotations' data (see summary in \code{\link{FAMT-package}}). 

If the argument is a 'FAMTmodel', the function provides the numbers of rejected genes using classical and FAMT analyses, the annotation characteristics of significant genes, and the estimated proportion of true null hypotheses.
}

\usage{
summaryFAMT(obj, pi0 = NULL, alpha = 0.15, info = c("ID", "Name"))
}
\arguments{
  \item{obj}{'FAMTdata' or 'FAMTmodel', see also \code{\link{as.FAMTdata}}, \code{\link{modelFAMT}}}
  \item{pi0}{Proportion of tests under H0. NULL, by default, it is estimated.}
  \item{alpha}{Type I levels for the control of the false discovery rate (0.15 by default) if the first argument is 'FAMTmodel' (it can be a single value or a vector).}
  \item{info}{Names of the columns containing the genes identification and array names in the original data frames, necessary if the first argument is 'FAMTmodel'}
}
%\details{}
\value{
If the argument is a 'FAMTdata': 
  a list with components expression:
  \item{expression$'Number of tests'}{Number of genes}
  \item{expression$'Sample size'}{Number of arrays}
  \item{covariates}{Classical summary of covariates}
  \item{annotations}{Classical summary of annotations}
If the argument is a 'FAMTmodel':
  \item{nbreject}{Matrix giving the numbers of rejected genes with the classical analysis and with the FAMT analysis for the given Type I levels alpha.}
  \item{DE}{Identification of the significant genes by their annotations.}
  \item{pi0}{Estimation of the proportion of true null hypotheses, estimated with the "smoother" method, see \code{\link{pi0FAMT}}.}
}
%\references{
%}
\author{David Causeur
}
%\note{
%}

\seealso{\code{\link{as.FAMTdata}}, \code{\link{modelFAMT}}
}
\examples{
## Reading 'FAMTdata'
data(expression)
data(covariates)
data(annotations)
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)

## Summary of a 'FAMTdata'
#############################################
summaryFAMT(chicken)

## Summary of a 'FAMTmodel'
#############################################
# FAMT complete multiple testing procedure 
model = modelFAMT(chicken,x=c(3,6),test=6,nbf=3)
summaryFAMT(model)
}

% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
