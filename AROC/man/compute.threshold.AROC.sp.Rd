\name{compute.threshold.AROC.sp}
\alias{compute.threshold.AROC.sp}
\title{
AROC-based threshold values.
}
\description{
Estimates AROC-based threshold values using the semiparametric approach proposed by Janes and Pepe (2009).
}
\usage{
compute.threshold.AROC.sp(object, newdata, FPF = 0.5)
}
\arguments{
  \item{object}{An object of class \code{AROC} as produced by \code{\link{AROC.sp}}.}
  \item{newdata}{Data frame with the covariate values at which threshold values are required.}
  \item{FPF}{Numeric vector with the FPF at which to calculate the AROC-based threshold values. Atomic values are also valid.}
}
\details{
Estimation of the covariate-adjusted ROC curve (AROC) using the semiparametric approach proposed by Janes and Pepe (2009) involves the estimation of the conditional distribution function for the diagnostic test outcome in the healthy population

\deqn{F_{\bar{D}}(y|\mathbf{X}_{\bar{D}}) = Pr\{Y_{\bar{D}} \leq y | \mathbf{X}_{\bar{D}}\}.}

This function makes use of this estimate in order to calculate AROC-based threshold values. In particular, for a covariate value \eqn{\mathbf{x}} and a FPF = t, the AROC-based threshold value is calculated as follows

\deqn{\hat{c}_{\mathbf{x}} = \hat{F}^{-1}_{\bar{D}}(1-t|\mathbf{X}_{\bar{D}} = \mathbf{x}).}  

}
\value{A matrix with the computed AROC-based threshold values. The matrix has as many columns as different covariate vector values, and as many rows as different FPFs.}
\references{
Janes, H., and Pepe, M.S. (2009). Adjusting for covariate effects on classification accuracy using the covariate-adjusted receiver operating characteristic curve. Biometrika, 96(2), 371 - 382.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.sp}}
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m3 <- AROC.sp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa,
p = seq(0,1,l=101), B = 500)

FPF = c(0.1, 0.3)
newdata <- data.frame(age = seq(52, 80, l = 50))

th_sp <- compute.threshold.AROC.sp(m3, newdata, FPF)
names(th_sp)
}
\dontshow{
m3 <- AROC.sp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa,
p = seq(0,1,l=101), B = 0)

FPF = c(0.1, 0.3)
newdata <- data.frame(age = 52)

th_sp <- compute.threshold.AROC.sp(m3, newdata, FPF)
names(th_sp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
