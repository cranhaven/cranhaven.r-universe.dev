\name{PV_post_mood_nm}
\encoding{WINDOWS-1252}
\alias{PV_post_mood_nm}
\title{Compute the present expected value
of an \eqn{n}-payment annuity, with payments of 1 unit each made at
the end of every year (annuity-immediate), valued at the rate \eqn{X},
with the method of Mood \emph{et al.} using some negative moments of the distribution.}


\usage{
PV_post_mood_nm(data,years)
}
\arguments{\item{data}{A vector of interest rates.}
\item{years}{The number of years of the income. Default is 10 years.}

}

\description{
Compute the present expected value
of an \eqn{n}-payment annuity, with payments of 1 unit each made at
the end of every year (annuity-immediate), valued at the rate \eqn{X},
with the method of Mood \emph{et al.} using some negative moments of the distribution.}


\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}

\source{
Mood, A. M.; Graybill, F. A. and Boes, D. C. (1974). \emph{Introduction to the Theory of Statistics}
(3rd Ed.). New York: McGraw Hill.

Rice, J. A. (1995). \emph{Mathematical Statistics and Data Analysis} (2nd Ed.). California: Ed. Duxbury Press.

}

\examples{
#example 1
data=c(0.298,0.255,0.212,0.180,0.165,0.163,0.167,0.161,0.154,
0.128,0.079,0.059,0.042,-0.008,-0.012,-0.002)
PV_post_mood_nm(data)

# example 2
data<-rnorm(n=30,m=0.03,sd=0.01)
PV_post_mood_nm(data)

# example 3
data = c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
data=data/100
PV_post_mood_nm(data)
}

