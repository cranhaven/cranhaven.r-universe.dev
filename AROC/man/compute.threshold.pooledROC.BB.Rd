\name{compute.threshold.pooledROC.BB}
\alias{compute.threshold.pooledROC.BB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooled ROC-based threshold values.
}
\description{
Estimates pooled ROC-based threshold values using the Bayesian bootstrap estimator proposed by Gu et al. (2008).
}
\usage{
compute.threshold.pooledROC.BB(object, FPF = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{AROC} as produced by \code{\link{pooledROC.BB}}.}
  \item{FPF}{Numeric vector with the FPF at which to calculate the pooled ROC-based threshold values. Atomic values are also valid.}
}
\value{As a result, the function provides a list with the following components:
\item{thresholds}{A matrix with the posterior mean and posterior 2.5\% and 97.5\% quantiles of the pooled ROC-based threshold values. The matrix has as many rows as different FPFs.}
\item{FPF}{the supplied FPF argument}
\item{TPF}{TPFs corresponding to the estimated threshold. In addition to the posterior mean, the 95\% pointwise credible band is also returned.}
}
\references{
Gu, J., Ghosal, S., and Roy, A. (2008). Bayesian bootstrap estimation of ROC curve. Statistics in Medicine, \bold{27}, 5407--5420.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pooledROC.BB}}
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\dontshow{
m0_BB <- pooledROC.BB(y0 = newpsa$l_marker1[newpsa$status == 0],
y1 = newpsa$l_marker1[newpsa$status == 1], B = 50)

### Threshold values for a fixed FPF
th_m0_BB <- compute.threshold.pooledROC.BB(m0_BB, FPF = 0.1)

th_m0_BB$threshold
}
\donttest{
m0_BB <- pooledROC.BB(y0 = newpsa$l_marker1[newpsa$status == 0], 
            y1 = newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 5000)

### Threshold values for a fixed FPF
th_m0_BB <- compute.threshold.pooledROC.BB(m0_BB, FPF = 0.1)

th_m0_BB$threshold
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
