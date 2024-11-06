\name{var1stock}
\alias{var1stock}
\title{
Calculate the value at risk of a single stock.
}
\description{
Function to calculate the value at risk of a single stock.
}
\usage{
var1stock()
}
\details{
The user inputs are as follows:\cr
Value of the stock: to be entered in numbers for e.g. 110.50\cr
mu: the expected return- to be entered in decimals. For e.g. 0.05 for 5 per cent \cr
Sigma (or Volatility) per annum: to be entered in decimals. For e.g. 0.25 for 25 per cent \cr
Confidence level: to be entered in decimals. For e.g. 0.95 for 95 per cent\cr
Horizon (in months): For e.g. enter 12 for a year\cr
Distribution:  chosen between normal/lognormal\cr
}


\value{
The dollar value at risk of a single stock.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{var2stocks}}
}

