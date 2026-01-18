\name{gboot}
\alias{gboot}
\title{
 Generalized bootstrapping
}
\description{
 Generalized bootstrapping
}
\usage{
  gboot(x,gldobj,statistic,...)
}
\arguments{
  \item{x}{A random sample.}
  \item{gldobj}{Either an object fitting a GLD or EGLD to data 'x'.}
  \item{statistic}{User defined function to resample from 'x'.  
   'fun' could be parametric or non-parametric. }
  \item{...}{Controls}
}
\references{
Wang, B., Mishra, S.N., Mulekar, M., Mishra, N.S., Huang, K., (2010). Generalized Bootstrap Confidence Intervals for High Quantiles, In: \emph{Karian ZA, Dudewicz, EJ eds. The Handbook on Fitting Statistical Distributions with R}. CRC Press. 2010: 877-913.

Wang, B., Mishra, S.N., Mulekar, M., Mishra, N.S., Huang, K., (2010). Comparison of bootstrap and generalized bootstrap methods for estimating high quantiles, \emph{Journal of Statistical Planning and Inferences}, \bold{140}. 2926-2935. DOI: 10.1016/j.jspi.2010.03.016.

Karian, Z.A.,
  Dudewicz, E.J., McDonald, P., 1996. The Extended Generalized Lambda
  Distribution System for Fitting Distributions to Data:
  history,completion of theory, tables, applications, the ``final
  word'' on moment fits, \emph{Comm. in Statist.- Simul. \& Comput.}
  \bold{25}(3), 611-642.

Karian, Z.A., Dudewicz,
  E.J., 2000. \emph{Fitting Statistical Distributions: The Generalized
  Lambda Distribution and Generalized Bootstrap Methods}, Chapman and
  Hall/CRC.

Dudewicz, E.J., 1992. The
  Generalized Bootstrap, Bootstrapping and Related Techniques, In:
  K.H., G. Rothe, W. Sendler, eds., V. 376 of \emph{Lecture Notes in
    Economics and Mathematical Systems}, Springer-Verlag, Berlin,
  31-37.

}
\examples{

data(ofc)
X = ofc$x0
Ta = function(x) mean(x<31)
gld0 = fit.gld(X)
(out = gboot(X,gld0,statistic=Ta,R=100))
gld1 = fit.egld(X)
(out = gboot(X,gld1,statistic=Ta,R=100))

}
\keyword{density}
\keyword{models}
