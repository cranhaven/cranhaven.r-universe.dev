\name{cdf.qp}
\alias{cdf.qp}
\alias{cdf.bundle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of the conditional CDF based on expectile curves
}
\description{
Estimating the CDF of the response for a given value of covariate. Additionally quantiles are computed
from the distribution function which allows for the calculation of regression quantiles.
}
\usage{
cdf.qp(expectreg, x = NA, qout = NA, extrap = FALSE, e0 = NA, eR = NA,
       lambda = 0, var.dat = NA)

cdf.bundle(bundle, qout = NA, extrap = FALSE, quietly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expectreg, bundle}{
An object of class expectreg or subclass bundle respectively. The number of expectiles should be high enough to ensure accurate estimation. 
One approach would be to take as many expectiles as data points. Also make sure that extreme expectiles are incuded, e.g. expectiles corresponding to very small and large asymmetrie values.
}
  \item{x}{
The covariate value where the CDF is estimated. By default the first covariate value.
}
  \item{qout}{
Vector of quantiles that will be computed from the CDF.
}
  \item{extrap}{
If TRUE, extreme quantiles will be extrapolated linearly, otherwise the maximum of the CDF is used.
}
  \item{e0}{
Scalar number which offers the possibility to specify an artificial minimal expectile (for example the minimum of the data) used for the calculation. By default e0 = e1 + (e1 - e2) where e1 is the actual minimal expectile and e2 the second smallest expectile.
}
  \item{eR}{
Scalar number which offers the possibility to specify an artificial maximal expectile (for example the maximum of the data) used for the calculation. By default eR = eR-1 + (eR-1 - eR-2) where eR-1 is the actual maximal expectile and eR-2 the second largest expectile.
}
  \item{lambda}{
Positive Scalar. Penalty parameter steering the smoothness of the fitted CDF. By default equal to 0 which means no penalization.
}
  \item{var.dat}{
Positive Scalar. If a penalization is applied (i.e. \code{lambda} unequal to 0), this argument can be used to let the penalty depend on the variance of the expectiles (which is the default).
}
  \item{quietly}{
If programm should run quietly.
}
}
\details{
Expectile curves can describe very well the spread and location of a scatterplot. With
a set of curves they give good impression about the nature of the data. This information
can be used to estimate the conditional density from the expectile curves.
The results of the bundle model are especially suited in this case
as only one density will be estimated which can then be modulated to
over the independent variable x. The 
density estimation can be formulated as penalized least squares problem that results in a smooth non-negative
density.
The theoretical values of a quantile regression at this covariate value
are also returned for adjustable probabilities \code{qout}.
}
\value{
A list consisting of
  \item{x}{ vector of expectiles where the CDF is computed. }
  \item{cdf }{ vector of values of the CDF at the expectiles \code{x}. }
  \item{quantiles }{ vector of quantile values estimated from the CDF. }
  \item{qout }{ vector of probabilities for the calculated quantiles. }
%% ... return(list( x = e, cdf = F, quantiles = quant, qout = qout))
}
\references{
Schnabel SK and Eilers PHC (2010)
\emph{ A location scale model for non-crossing expectile curves }
(working paper)

Schulze Waltrup L, Sobotka F, Kneib T and Kauermann G (2014)
\emph{ Expectile and Quantile Regression - David and Goliath? }
Statistical Modelling.
}
\author{
Goeran Kauermann, Linda Schulze Waltrup \cr
Ludwig Maximilian University Munich \cr
\url{https://www.lmu.de} \cr

Fabian Sobotka \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr

Sabine Schnabel \cr
Wageningen University and Research Centre \cr
\url{https://www.wur.nl} \cr

Paul Eilers \cr
Erasmus Medical Center Rotterdam \cr
\url{https://www.erasmusmc.nl} \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expectreg.ls}}, \code{\link{expectreg.qp}}
}
\examples{
d = expectreg.ls(dist ~ rb(speed),data=cars,smooth="f",lambda=5,estimate="restricted",
                 expectiles=c(0.0001,0.001,seq(0.01,0.99,0.01),0.999,0.9999))
e = cdf.qp(d,15,extrap=TRUE)
e 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ nonparametric }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ nonlinear }
