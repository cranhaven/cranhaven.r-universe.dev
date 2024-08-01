\name{pooledROC.BB}
\alias{pooledROC.BB}
\title{
Bayesian bootstrap estimation of the pooled ROC curve.
}
\description{
Estimates the pooled ROC curve using the Bayesian bootstrap estimator proposed by Gu et al. (2008).
}
\usage{
pooledROC.BB(y0, y1, p = seq(0, 1, l = 101), B = 5000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y0}{Diagnostic test outcomes in the healthy group.}
  \item{y1}{Diagnostic test outcomes in the diseased group.}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{B}{An integer value specifying the number of Bayesian bootstrap resamples. By default 5000.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
As a result, the function provides a list with the following components:
\item{call}{the matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated}
\item{ROC}{Estimated pooled ROC curve, and corresponding 95\% credible intervals}
\item{AUC}{Estimated pooled AUC, and corresponding 95\% credible intervals.}
}
\references{
Gu, J., Ghosal, S., and Roy, A. (2008). Bayesian bootstrap estimation of ROC curve. Statistics in Medicine, 27(26), 5407 - 5420.
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
m0_BB <- pooledROC.BB(newpsa$l_marker1[newpsa$status == 0],
newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 5000)

summary(m0_BB)

plot(m0_BB)
}
\dontshow{
m0_BB <- pooledROC.BB(newpsa$l_marker1[newpsa$status == 0],
newpsa$l_marker1[newpsa$status == 1], p = seq(0,1,l=101), B = 500)

summary(m0_BB)

plot(m0_BB)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
