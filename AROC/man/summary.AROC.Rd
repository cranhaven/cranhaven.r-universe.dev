\name{summary.AROC}
\alias{summary.AROC}
\title{
Summary method for \code{AROC} objects
}
\description{
Default summary method for objects fitted with \code{AROC.bnp()}, \code{AROC.bsp()}, \code{AROC.sp()}, \code{AROC.kernel()}, \code{pooledROC.BB()} or \code{pooledROC.emp()} functions.
}
\usage{
\method{summary}{AROC}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{AROC} as produced by \code{AROC.bnp()}, \code{AROC.bsp()}, \code{AROC.sp()}, \code{AROC.kernel()}, \code{pooledROC.BB()} or \code{pooledROC.emp()}}
  \item{\dots}{Further arguments passed to or from other methods. Not yet implemented.}
}
\details{
A short summary is printed including the area under the covariate-adjusted ROC curve (AAUC) or the area under the pooled ROC curve (AUC), and if required, the partial area under the covariate-adjusted ROC curve (pAAUC). For the Bayesian methods, and if required, the function also provides the log pseudo marginal likelihood (LPML) and/or widely applicable information criterion (WAIC).
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
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
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 5000, nburn = 1000)

summary(m0)
}
\dontshow{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 500, nburn = 100)

summary(m0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
