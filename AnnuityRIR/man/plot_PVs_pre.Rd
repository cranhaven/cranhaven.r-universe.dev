\name{plot_PVs_pre}
\encoding{WINDOWS-1252}
\alias{plot_PVs_pre}
\title{Plot the present expected values of an
\eqn{n}-payment annuity, with payments of 1 unit each made at the
beginning of every year (annuity-due), valued at the rate \eqn{X},
using different approaches.}


\usage{
plot_PVs_pre(data,years,lwd,lty1,lty2,lty3,lty4,lty5,lty6)
}
\arguments{\item{data}{A vector of interest rates.}
\item{years}{The number of years of the income. Default is 10 years.}
\item{lwd}{The width of the curve. Default is 1.5. }
\item{lty1}{The style of the curve for the "arctan" approximation. Default is 1. }
\item{lty2}{The style of the curve for the "cubic" approximation. Default is 2. }
\item{lty3}{The style of the curve for the "mood with positive moments" approximation. Default is 3. }
\item{lty4}{The style of the curve for the "mood with negative moments" approximation. Default is 4. }
\item{lty5}{The style of the curve for the exact value. Default is 5. }
\item{lty6}{The style of the curve for "triangular distribution" approximation. Default is 6. }
}

\description{Plot the present expected values of an
\eqn{n}-payment annuity, with payments of 1 unit each made at the
beginning of every year (annuity-due), valued at the rate \eqn{X},
using different approaches.}


\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}



\examples{

# example 1
data = c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
data=data/100
plot_PVs_pre(data)

# example 2
data<-rnorm(n=30,m=0.03,sd=0.003)
plot_PVs_pre(data)

}

