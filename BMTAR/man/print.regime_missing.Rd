\name{print.regime_missing}
\alias{print.regime_missing}

\title{Print estimates of a regime_missing object of the function output mtarmissing}

\description{
Print estimates output of mtarmissing function.
}
\usage{
\method{print}{regime_missing}(object, ...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{regime_model}}. Not NULL}
\item{...}{Other plotting parameters to affect the plot.}
}

\details{
Print the estimates for the outputs corresponding to the function \dQuote{\code{mtarmissing}} which return an object of class \dQuote{\code{regime_missing}}.
}
\value{
  Return to console.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{mtarmissing}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data('missingest')
print.regime_missing(missingest)
}
