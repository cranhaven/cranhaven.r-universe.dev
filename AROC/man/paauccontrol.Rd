\name{paauccontrol}
\alias{paauccontrol}
\title{
Partial area under the covariate-adjusted ROC curve (pAAUC).
}
\description{
Used to set various parameters controlling the estimation of the partial area under the covariate-adjusted ROC curve (pAAUC).
}
\usage{
paauccontrol(compute = FALSE, value = 1)
}
\arguments{
  \item{compute}{Logical value. If TRUE the partial area under the covariate-adjusted ROC curve (pAAUC) is estimated.}
  \item{value}{Numeric value. Pre-specified maximum false positive fraction (FPF) at which to calculate the pAAUC.}
}
\details{The value returned by this function is used as a control argument of the \code{\link{AROC.bnp}} and \code{\link{AROC.bsp}} functions.
}
\value{a list with components for each of the possible arguments.}
\references{
Inacio de Carvalho, V., and Rodriguez-Alvarez, M. X. (2018). Bayesian nonparametric inference for the covariate-adjusted ROC curve. Technical report.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.bnp}} and \code{\link{AROC.bsp}}
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
p = seq(0,1,l=101), paauc = list(compute = TRUE, value = 0.3),
compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 5000, nburn = 1000)

summary(m0)
}
\dontshow{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), paauc = list(compute = TRUE, value = 0.3),
compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 500, nburn = 100)

summary(m0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
