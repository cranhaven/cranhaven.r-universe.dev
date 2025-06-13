\name{print.tsregime}
\alias{print.tsregime}

\title{Print tsregime object}

\description{
Print the structure of a object class \dQuote{\code{tsregime}}.
}
\usage{
\method{print}{tsregime}(object, ...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{tsregime}}. Not NULL}
\item{...}{Other parameters that affect print.}
}

\details{
Print the structure of a object class \dQuote{\code{tsregime}}.
}
\value{
  Return to console.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{tsregime}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
data = datasim
print.tsregime(data$Sim)
}
