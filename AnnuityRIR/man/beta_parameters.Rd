\name{beta_parameters}
\encoding{WINDOWS-1252}
\alias{beta_parameters}
\title{Compute the parameters of the beta distribution
and plot normalized data. 
}
\usage{
beta_parameters(data)
}
\arguments{\item{data}{ A vector of interest rates. }


}

\description{
Compute the parameters of
the beta distribution
and plot normalized data. 

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}

\source{
Cruz Rambaud, S.; Maturo, F. and Sánchez Pérez A. M.  (2015):
"Approach of the value of an annuity when non-central moments of the capitalization factor are known:
an R application with interest rates following normal and beta distributions". \emph{Ratio Mathematica}, 28(1),
pp. 15-30. doi: 10.23755/rm.v28i1.25. 
}

\examples{

# example 1
data=c(0.00,-0.05,-0.05,-0.06,-0.06,0.02,-0.06,-0.05,-0.04,-0.05,
-0.03,-0.06,0.04,-0.05,-0.08,-0.05,-0.12,-0.03,-0.05,-0.04,-0.06)
beta_parameters(data)

# example 2
data<-rnorm(n=200,m=0.075,sd=0.2)
beta_parameters(data)


}

