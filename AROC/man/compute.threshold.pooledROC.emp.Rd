\name{compute.threshold.pooledROC.emp}
\alias{compute.threshold.pooledROC.emp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooled ROC-based threshold values.
}
\description{
Estimates pooled ROC-based threshold values using the empirical estimator proposed by Hsieh and Turnbull (1996).
}
\usage{
compute.threshold.pooledROC.emp(object, FPF = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{AROC} as produced by \code{\link{pooledROC.emp}}.}
  \item{FPF}{Numeric vector with the FPF at which to calculate the pooled ROC-based threshold values. Atomic values are also valid.}
}
\value{As a result, the function provides a list with the following components:
\item{thresholds}{A vector with the estimated pooled ROC-based threshold values, one for each specified FPF.}
\item{FPF}{the supplied FPF argument}
\item{TPF}{TPFs corresponding to the estimated threshold.}
}
\references{
Hsieh, F., and Turnbull, B.W. (1996). Nonparametric and semiparametric estimation of the receiver operating characteristic curve, The Annals of Statistics, \bold{24}, 25--40.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pooledROC.emp}}
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\dontshow{
m0_emp <- pooledROC.emp(y0 = newpsa$l_marker1[newpsa$status == 0],
y1 = newpsa$l_marker1[newpsa$status == 1], B = 50)

### Threshold values for a fixed FPF
th_m0_emp <- compute.threshold.pooledROC.emp(m0_emp, FPF = 0.1)

th_m0_emp$threshold
}
\donttest{
m0_emp <- pooledROC.emp(y0 = newpsa$l_marker1[newpsa$status == 0], 
            y1 = newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 500)

### Threshold values for a fixed FPF
th_m0_emp <- compute.threshold.pooledROC.emp(m0_emp, FPF = 0.1)

th_m0_emp$threshold
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
