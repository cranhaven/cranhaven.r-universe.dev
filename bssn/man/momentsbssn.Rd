\name{momentsbssn}
\alias{momentsbssn}
\alias{meanbssn}
\alias{varbssn}
\alias{skewbssn}
\alias{kurtbssn}


\title{Moments for the Birnbaum-Saunders model based on Skew-Normal distribution
}
\description{Mean, variance, skewness and kurtosis for the Birnbaum-Saunders model based on Skew-Normal distribution defined in Filidor et. al (2011).
}
\usage{
meanbssn(alpha=0.5,beta=1,lambda=1.5)
varbssn(alpha=0.5,beta=1,lambda=1.5)
skewbssn(alpha=0.5,beta=1,lambda=1.5)
kurtbssn(alpha=0.5,beta=1,lambda=1.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{alpha}{
shape parameter \eqn{\alpha}.
}
  \item{beta}{
scale parameter \eqn{\beta}.
}
  \item{lambda}{
skewness parameter \eqn{\lambda}.
}
}
%\details{

%}
\value{
\code{meanbssn} gives the mean, \code{varbssn} gives the variance, \code{skewbssn} gives the skewness, \code{kurtbssn} gives the kurtosis.
}

\references{
Vilca, Filidor; Santana, L. R.; Leiva, Victor; Balakrishnan, N. (2011). Estimation of extreme percentiles in Birnbaum Saunders distributions. Computational Statistics & Data Analysis (Print), 55, 1665-1678.

Santana, Lucia; Vilca, Filidor; Leiva, Victor (2011). Influence analysis in skew-Birnbaum Saunders regression models and applications. Journal of Applied Statistics, 38, 1633-1649.
}

\author{Rocio Maehara \email{rmaeharaa@gmail.com} and Luis Benites \email{lbenitesanchez@gmail.com}
}

\seealso{
\code{\link{bssn}}, \code{\link{EMbssn}}, \code{\link{momentsbssn}}, \code{\link{ozone}}, \code{\link{reliabilitybssn}}
}

\examples{
## Let's compute some moments for a Birnbaum-Saunders model based on Skew normal Distribution.
# The well known mean, variance, skewness and kurtosis
meanbssn(alpha=0.5,beta=1,lambda=1.5)
varbssn(alpha=0.5,beta=1,lambda=1.5)
skewbssn(alpha=0.5,beta=1,lambda=1.5)
kurtbssn(alpha=0.5,beta=1,lambda=1.5)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Birnbaum-Saunders Skew-Normal}
\keyword{Moments}

