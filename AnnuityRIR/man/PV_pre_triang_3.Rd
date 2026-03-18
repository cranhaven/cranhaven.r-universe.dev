\name{PV_pre_triang_3}
\encoding{WINDOWS-1252}
\alias{PV_pre_triang_3}
\title{Compute the present value of an
annuity-due considering only non-central moments of negative
orders. The calculation is performed by using the function
$triangular\_moments\_3$ for
the moments greater than \eqn{-2} (in absolute value).
}
\usage{
PV_pre_triang_3(data,years)
}
\arguments{\item{data}{ A vector of interest rates expressed as percentages. }
\item{years}{ The number of years of the income. Default is 10 years. }


}

\description{Compute the present value of an
annuity-due considering only non-central moments of negative
orders. The calculation is performed by using the function
$triangular\_moments\_3$ for
the moments greater than \eqn{-2} (in absolute value).

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}


\examples{

data=c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
PV_pre_triang_3(data,10)

}

