\name{pooledROC.emp}
\alias{pooledROC.emp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Empirical estimation of the pooled ROC curve.
}
\description{
Estimates the pooled ROC curve using the empirical estimator proposed by Hsieh and Turnbull (1996).
}
\usage{
pooledROC.emp(y0, y1, p = seq(0, 1, l = 101), B = 500, 
	method = c("ncoutcome", "coutcome"))
}
\arguments{
  \item{y0}{Diagnostic test outcomes in the healthy group.}
  \item{y1}{Diagnostic test outcomes in the diseased group.}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{B}{An integer value specifying the number of bootstrap resamples for the construction of the confidence intervals. By default 500.}
  \item{method}{A character string specifying if bootstrap resampling (for the confidence intervals) should be done with or without regard to the disease status (``coutcome'' or ``noutcome''). In both cases, a naive bootstrap is used. By default, the resampling is done conditionally on the disease status.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
As a result, the function provides a list with the following components:
\item{call}{the matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated}
\item{ROC}{Estimated pooled ROC curve, and corresponding 95\% confidence intervals (if required)}
\item{AUC}{Estimated pooled AUC, and corresponding 95\% confidence intervals (if required).}
}
\references{
Hsieh, F., and Turnbull, B.W. (1996). Nonparametric and semiparametric estimation of the receiver operating characteristic curve, The Annals of Statistics, 24, 25-40.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.bnp}}, \code{\link{AROC.bsp}}, \code{\link{AROC.sp}}, \code{\link{AROC.kernel}}, \code{\link{pooledROC.BB}} or \code{\link{pooledROC.emp}}.
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m0_emp <- pooledROC.emp(newpsa$l_marker1[newpsa$status == 0],
newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 500)

summary(m0_emp)

plot(m0_emp)
}
\dontshow{
m0_emp <- pooledROC.emp(newpsa$l_marker1[newpsa$status == 0],
newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 0)

summary(m0_emp)

plot(m0_emp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
