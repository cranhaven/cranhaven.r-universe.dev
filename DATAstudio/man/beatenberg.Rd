\encoding{UTF-8}
\name{beatenberg}
\alias{beatenberg}
\title{Beatenberg Forest Temperature Data (In Unit Fréchet Scale)}
\description{
  Preprocessed pairs of temperatures in unit Fréchet scale from
  Beatenberg forest, registered under forest cover and in the open
  field. 
}
\usage{beatenberg}
\format{
  The \code{beatenberg} data frame has 2839 rows and 2 columns:
  \code{x} (forest cover) and \code{y} (open field).
}
\details{
  Preprocessing is conducted as described in Ferrez et al (2011),
  and for applications of this dataset within the context of extreme
  value theory see de Carvalho \emph{et al.} (2013), de Carvalho and
  Davison (2014) as well as Castro and de Carvalho (2017).
}
\references{
  Castro, D. & de Carvalho, M. (2017) Spectral density regression for
  bivariate extremes. \emph{Stochastic Environmental Research and
    Risk Assessment}, \bold{31}, 1603-1613.
  
  de Carvalho, M., Oumow, B., Segers, J. and Warchol, M. (2013)
  A Euclidean likelihood estimator for bivariate tail dependence.
  \emph{Communications in Statistics—Theory and Methods}, \bold{42},
  1176-1192.
  
  de Carvalho, M. & Davison, A. C. (2014)
  Spectral density ratio models for multivariate extremes.
  \emph{Journal of the American Statistical Association}, \bold{109},
  764-776.
  
  Ferrez, J., A. C. Davison, and Rebetez., M. (2011) Extreme
  temperature analysis under forest cover compared to an open field.
  \emph{Agricultural and Forest Meteorology}, \bold{151}, 992-1001.
}
\examples{
## de Carvalho et al (2013, Fig. 5)
data(beatenberg)
attach(beatenberg)
plot(x, y, log = "xy", pch = 20, xlab = "Forest Cover", ylab = "Open Field")

\dontrun{
## install package extremis if not installed
if (!require("extremis")) install.packages("extremis")

## de Carvalho et al (2013, Fig. 7)
data(beatenberg)
fit <- bev.kernel(beatenberg, tau = 0.98, nu = 163, raw = FALSE)
plot(fit)
rug(fit$w)
}
}
\keyword{Climatological}
