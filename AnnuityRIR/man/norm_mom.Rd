\name{norm_mom}
\encoding{WINDOWS-1252}
\alias{norm_mom}
\title{Fit the data to a normal curve and compute the
moments of the normal distribution
according to the definition (as integral).
}
\usage{
norm_mom(data,order)
}

\arguments{\item{data}{ A vector X of interest rates. }
\item{order}{ The order of moment that should be computed. }


}

\description{
Fit the data to a normal curve and compute the
moments of the normal distribution
according to the definition (as integral).

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
data=data/100
norm_mom(data,5)





}

