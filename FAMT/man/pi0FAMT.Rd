\name{pi0FAMT}
\alias{pi0FAMT}
\title{Estimation of the Proportion of True Null Hypotheses
}
\description{A function to estimate the proportion \code{pi0} of true null hypotheses from a 'FAMTmodel' (see also function "pval.estimate.eta0" in package "fdrtool").}
\usage{pi0FAMT(model, method = c("smoother", "density"), 
diagnostic.plot = FALSE)}
\arguments{
  \item{model}{'FAMTmodel' object (see \code{\link{modelFAMT}})}
  \item{method}{algorithm used to estimate the proportion of null p-values. Available options are "density" and "smoother" (as described in Friguet and Causeur, 2010)}
  \item{diagnostic.plot}{if TRUE the histogram of the p-values with the estimate of \code{pi0} horizontal line is plotted. With the "smoother" method, an additional graph is displayed showing the spline curve used to estimate \code{pi0}. With the "density" method, the estimated convex density of the p-values is plotted onto the histogram}
  }

\details{The quantity \code{pi0}, i.e. the proportion of null hypotheses, is an important parameter when controlling the false discovery rate (FDR). A conservative choice is \code{pi0} = 1 but a choice closer to the true value will increase efficiency and power - see Benjamini and Hochberg (1995, 2000), Black(2004) and Storey (2002) for details.
The function \code{pi0FAMT} provides 2 algorithms to estimate this proportion. The "density" method is based on Langaas et al. (2005)'s approach where the density of p-values f(p) is first estimated considering f as a convex function, and the estimation of \code{pi0} is got for p=1. The "smoother" method uses the smoothing spline approach proposed by Storey and Tibshirani(2003).
}
\value{
\code{pi0}{The estimated proportion \code{pi0} of null hypotheses.}}

\references{
Friguet C. and Causeur D. (2010) Estimation of the proportion of true null hypohteses in high-dimensional data under dependence. Submitted.

"density" procedure: Langaas et al (2005) Estimating the proportion of true null hypotheses, with application to DNA microarray data. JRSS. B, 67, 555-572.

"smoother" procedure: Storey, J. D., and R. Tibshirani (2003) Statistical significance for genome-wide experiments. Proc. Nat. Acad. Sci. USA, 100, 9440-9445. }
\author{ Chloe Friguet & David Causeur }

\seealso{\code{\link{modelFAMT}}
}
\examples{
# Reading 'FAMTdata'
data(expression)
data(covariates)
data(annotations)
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)

# FAMT complete multiple testing procedure
model = modelFAMT(chicken,x=c(3,6),test=6,nbf=3)

# Estimation of the Proportion of True Null Hypotheses
# "density" method 
\dontrun{ pi0FAMT(model,method="density",diagnostic.plot=TRUE)}

# "smoother" method
pi0FAMT(model,method="smoother",diagnostic.plot=TRUE)

}
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
