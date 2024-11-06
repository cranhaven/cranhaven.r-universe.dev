\name{var2stocks}
\alias{var2stocks}
\title{
Calculate the value at risk of two stocks.
}
\description{
Function to calculate the value at risk of two stocks.
}
\usage{
var2stocks()
}
\details{
The user inputs are as follows:\cr
Value of the first stock: to be entered in numbers for e.g. 110.50\cr
Value of the second stock: to be entered in numbers for e.g. 170.50\cr
mu1: the expected return- to be entered in decimals. For e.g. 0.05 for 5 per cent \cr
mu2: the expected return- to be entered in decimals. For e.g. 0.06 for 6 per cent \cr
Sigma1 (or Volatility) per annum: to be entered in decimals. For e.g. 0.25 for 25 per cent \cr
Sigma2 (or Volatility) per annum: to be entered in decimals. For e.g. 0.3 for 30 per cent \cr
Confidence level: to be entered in decimals. For e.g. 0.95 for 95 per cent\cr
Correlation: a number between -1 and +1 to be entered in decimals. For e.g. 0.6\cr
Horizon (in months): For e.g. enter 12 for a year\cr
Distribution:  chosen between normal/lognormal\cr
}


\value{
The dollar value at risk of two stocks.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{var1stock}}
}

