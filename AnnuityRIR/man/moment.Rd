\name{moment}
\encoding{WINDOWS-1252}
\alias{moment}
\title{Compute the exact moments of a distribution.
}
\usage{
moment(x,order,central, absolute, na.rm)
}

\arguments{\item{x}{ A vector X of interest rates. }
\item{order}{ The order of moment that should be computed. Default is 1. }
\item{central}{If central moments are to be computed. Default is "FALSE".}
\item{absolute}{If absolute moments are to be computed. Default is "FALSE".}
\item{na.rm}{If missing values should be removed. Default is "FALSE".}


}

\description{
Compute the exact moments of a distribution.

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}

\source{
Cruz Rambaud, S.; Maturo, F. and Sánchez Pérez A. M.  (2015):
“Approach of the value of an annuity when non-central moments of the capitalization factor are known:
an R application with interest rates following normal and beta distributions”. \emph{Ratio Mathematica}, 28(1),
pp. 15-30. doi: 10.23755/rm.v28i1.25.
}

\examples{

#example 1
data=c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
moment(data,3)





}

