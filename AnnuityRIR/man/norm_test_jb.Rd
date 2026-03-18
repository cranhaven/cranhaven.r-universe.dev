\name{norm_test_jb}
\encoding{WINDOWS-1252}
\alias{norm_test_jb}
\title{Compute the Jarque-Bera test for
checking the assumption of normality
of the interest rates distribution and returns the parameters of the fitted normal distribution.
}
\usage{
norm_test_jb(data)
}
\arguments{\item{data}{ A vector of interest rates. }

}

\description{
Compute the Jarque-Bera test for
checking the assumption of normality
of the interest rates distribution and returns the parameters of the fitted normal distribution.
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
data=c(0.298,0.255,0.212,0.180,0.165,0.163,0.167,0.161,
0.154,0.128,0.079,0.059,0.042,-0.008,-0.012,-0.002)
norm_test_jb(data)

# example 2
data<-rnorm(n=30,m=0.03,sd=0.01)
norm_test_jb(data)

# example 3
data=runif(999, min = 0, max = 1)
norm_test_jb(data)

# example 4
data=c(0.00,-0.05,-0.05,-0.06,-0.06,0.02,-0.06,-0.05,-0.04,-0.05,
-0.03,-0.06,0.04,-0.05,-0.08,-0.05,-0.12,-0.03,-0.05,-0.04,-0.06)
norm_test_jb(data)
}

