\name{print.regime_number}
\alias{print.regime_number}

\title{print regime_number object for the function outputs mtarnumreg}

\description{
Print estimates for the results of the mtarnumreg function.
}
\usage{
\method{print}{regime_number}(object, ...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{regime_number}}. Not NULL}
\item{...}{Other print parameters that affect.}
}

\details{
Print estimates outputs corresponding to function \dQuote{\code{mtarnumreg}} which return an object of class \dQuote{\code{regime_number}}.
}
\value{
  Return to console.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{mtarnumreg}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim_numreg")
print.regime_number(datasim_numreg)
}
