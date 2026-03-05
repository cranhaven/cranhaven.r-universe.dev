\name{coxre}
\title{Cox Proportional Hazards Model with Random Effect}
\alias{coxre}
\alias{print.llrf}
\usage{
coxre(response, censor, nest=NULL, cov=NULL, stratified=FALSE,
	cumul=FALSE,estimate=1, iter=10, print.level=0, ndigit=10,
	gradtol=0.00001, steptol=0.00001, iterlim=100, fscale=1,
	typsize=abs(estimate), stepmax=estimate)
}
\description{
\code{coxre} fits a Cox proportional hazards model to event history
data using a gamma distribution random effect. The parameter, gamma,
is the variance of this mixing distribution.

If a matrix of response times is supplied, the model can be stratified
by columns, i.e. a different intensity function is fitted for each
column. To fit identical intensity functions to all response types,
give the times as a vector.
}
\arguments{
\item{response}{Vector or matrix of times to events, with one column
per type of response (or subunit).}
\item{censor}{Corresponding vector or matrix of censoring indicators.
If NULL all values are set to one.}
\item{nest}{Vector indicating to which unit each observation belongs.}
\item{cov}{One covariate}
\item{stratified}{If TRUE, a model stratified on type of response
(the columns of response) is fitted instead of proportional intensities.}
\item{cumul}{Set to TRUE if response times are from a common origin
instead of times to (or between) events.}
\item{estimate}{Initial estimate of the frailty parameter.}
\item{iter}{Maximum number of iterations allowed for the inner EM loop.}
\item{print.level}{\code{nlm} control options.}
\item{ndigit}{\code{nlm} control options.}
\item{gradtol}{\code{nlm} control options.}
\item{steptol}{\code{nlm} control options.}
\item{iterlim}{\code{nlm} control options.}
\item{fscale}{\code{nlm} control options.}
\item{typsize}{\code{nlm} control options.}
\item{stepmax}{\code{nlm} control options.}
}
\author{D.G. Clayton and J.K. Lindsey}
\references{
Clayton, D. (1987) The analysis of event history data: a review of
progress and outstanding problems.  Statistics in Medicine 7: 819-841
}
\seealso{
\code{\link[event]{kalsurv}}.
}
\examples{
# 11 individuals, each with 5 responses
y <- matrix(c(51,36,50,35,42,
	27,20,26,17,27,
	37,22,41,37,30,
	42,36,32,34,27,
	27,18,33,14,29,
	43,32,43,35,40,
	41,22,36,25,38,
	38,21,31,20,16,
	36,23,27,25,28,
	26,31,31,32,36,
	29,20,25,26,25),ncol=5,byrow=TRUE)
# Different intensity functions
coxre(response=y, censor=matrix(rep(1,55),ncol=5), nest=1:11,
	est=0.7, stratified=TRUE)
# Proportional intensity functions for the five responses
coxre(response=y, censor=matrix(rep(1,55),ncol=5), nest=1:11,
	est=0.7, stratified=FALSE)
# Identical intensity functions
coxre(response=as.vector(t(y)), censor=rep(1,55),
	nest=rep(1:11,rep(5,11)), est=0.7)
}
\keyword{models}
