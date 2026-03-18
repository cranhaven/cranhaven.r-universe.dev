\name{variance_drv}
\encoding{WINDOWS-1252}
\alias{variance_drv}
\title{Compute the variance of the present value
of an annuity using "discrete random variable"
approach.
}
\usage{
variance_drv(data,years)
}
\arguments{\item{data}{ A vector X of interest rates. }
\item{years}{ The number of years of the income. Default is 10 years.}

}

\description{
Compute the variance of the present value
of an annuity using "discrete random variable"
approach.
}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}



\examples{

# example 1
data = c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
data=data/100
variance_drv(data)

}

