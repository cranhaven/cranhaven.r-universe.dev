\name{PV_pre_exact}
\encoding{WINDOWS-1252}
\alias{PV_pre_exact}
\title{Compute the present value of an
annuity-due considering only non-central
moments of negative orders.
}
\usage{
PV_pre_exact(data,years)
}
\arguments{\item{data}{ A vector of interest rates. }
\item{years}{ The number of years of the income. Default is 10 years. }

}

\description{Compute the present value of an
annuity-due considering only non-central
moments of negative orders.

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}


\examples{

# example 1
data=c(0.0177, 0.0185, 0.0185, 0.0184, 0.0184, 0.0183, 0.0185, 0.0185, 0.0188, 
0.0185, 0.0180, 0.0184, 0.0191, 0.0185, 0.0184, 0.0185, 0.0186, 0.0185, 0.0188, 0.0186)
PV_pre_exact(data,10)



}

