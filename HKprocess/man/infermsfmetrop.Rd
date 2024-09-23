\name{infermsfmetrop}

\alias{infermsfmetrop}

\title{
Bayesian inference for \ifelse{html}{\out{&mu;}}{\eqn{\mu}{mu}} and
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}}^2 for the AR(1) process.
}

\description{
The function infermsmetrop is used to create a sample from the posterior
distribution of \ifelse{html}{\out{&mu;}}{\eqn{\mu}{mu}} and
\ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}}^2, using eqs. 8,9 in Tyralis
and Koutsoyiannis (2014) for the AR(1) process.
}

\usage{
infermsfmetrop(fbayes,data)
}

\arguments{
  \item{data}{time series data }
  \item{fbayes}{phi parameter simulation sample, should be a vector}
}

\value{
A matrix with two columns, the first corresponding to the sample of
\ifelse{html}{\out{&mu;}}{\eqn{\mu}{mu}} and the second corresponding to the
sample of \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}{sigma}}^2.
}

\author{Hristos Tyralis}

\references{
Tyralis H, Koutsoyiannis D (2014) A Bayesian statistical model for deriving
the predictive distribution of hydroclimatic variables.
\emph{Climate Dynamics} \bold{42(11-12)}:2867--2883.
\doi{10.1007/s00382-013-1804-y}.
}

\examples{
# Posterior distribution of the mu and sigma parameters of the AR(1) process for
# the Nile time series.

set.seed(12345)

samp.sim1 <- inferf(Nile, 500)

samp.sim2 <- infermsfmetrop(samp.sim1, Nile)

hist(samp.sim2[,1], breaks = 20, main = expression(paste("Histogram of ",mu)),
    xlab = expression(paste(mu)))

hist(sqrt(samp.sim2[,2]), breaks = 20,
    main = expression(paste("Histogram of ",sigma)),
    xlab = expression(paste(sigma)))
}

\keyword{models}