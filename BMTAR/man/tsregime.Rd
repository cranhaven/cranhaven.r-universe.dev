\name{tsregime}
\alias{tsregime}

\title{Creation of class \dQuote{\code{tsregime}} for some data}
\description{
The function tsregime is used to create time-series-regime objects.
}
\usage{
tsregime(Yt, Zt = NULL, Xt = NULL, r = NULL)
}

\arguments{
  \item{Yt}{matrix \eqn{(Nxk)} type object, observed process (admit NA values). Not NULL}
  \item{Zt}{matrix \eqn{(Nx1)} type object, threshold process (admit NA values). Default NULL}
  \item{Xt}{matrix \eqn{(Nx\nu)} type object, covariate process (admit NA values). Default NULL}
  \item{r}{numeric type, threshold value (within the range of \eqn{Z_t}) if known. Default NULL}
}

\details{
 Create a class \dQuote{\code{tsregime}} object composed of: \eqn{Y_t} and \eqn{X_t} stochastics processes such that \eqn{Y_t=[Y_{1t},...,Y_{kt}]}', \eqn{X_t=[X_{1t},...,X_{\nu t}]'} and \eqn{Z_t} is a univariate process. Where \eqn{Y_t} follows a MTAR model with threshold variable \eqn{Z_t}
  \deqn{
Y_t= \Phi_{0}^(j)+\sum_{i=1}^{p_j}\Phi_{i}^{(j)} Y_{t-i}+\sum_{i=1}^{q_j} \beta_{i}^{(j)} X_{t-i} + \sum_{i=1}^{d_j} \delta_{i}^{(j)} Z_{t-i}+ \Sigma_{(j)}^{1/2} \epsilon_{t}
}
\deqn{if r_{j-1}< Z_t \leq r_{j}}
Missing data is allowed for processes \eqn{Y_t}, \eqn{X_t} and \eqn{Z_t} (can then be estimated with \dQuote{\code{mtarmissing}} function). In the case of known r, the output returns the percentages of observations found in each regimen.
}
\value{
  Return a list type object of class \dQuote{\code{tsregime}}:
  \item{Yt}{stochastic output process}
  \item{Xt}{stochastic covariate process (if enter)}
  \item{Zt}{stochastic threshold process (if enter)}
  \item{N}{number of observations}
  \item{k}{number of variables}
  If r known:
  \item{r}{threshold value}
  \item{Ind}{numeric type, number of the regime each observation belong}
  \item{Summary_r}{data.frame type, number and proportion of observations in each regime}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\seealso{
\code{\link[BMTAR]{mtaregime}},
\code{\link[BMTAR]{mtarinipars}},
\code{\link[BMTAR]{mtarsim}}
}
\examples{
data("datasim")
yt = datasim$Sim
Yt = yt$Yt
Zt = yt$Zt
(datos = tsregime(Yt,Zt))
autoplot.tsregime(datos,1)
autoplot.tsregime(datos,2)
}

\keyword{Observed process}

