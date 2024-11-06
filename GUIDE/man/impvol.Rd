\name{impvol}
\alias{impvol}
\title{
Calculate the Black scholes implied volatility of a European Call/Put.
}
\description{
Function to calculate the Black scholes implied volatility of a European Call/Put.
}
\usage{
impvol()
}
\details{
The user inputs are as follows:\cr
Exercise style:  chosen between European/American \cr
Spot: to be entered in numbers for e.g. 120.50\cr
Strike: to be entered in numbers for e.g. 110.50\cr
Risk free rate per annum: to be entered in decimals. For e.g. 0.05 for 5 per cent \cr
Maturity in number of years: to be entered in decimals. For e.g. 0.25 for a quarter year\cr
Dividend yield: to be entered in decimals. For e.g. 0.02 for 2 per cent \cr
Mkt price: to be entered in numbers for e.g. 12.50\cr
Type of Option:  chosen between Call/Put\cr
}


\value{
The Black scholes implied volatility of a European Call/Put.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{blackscholes}}
}

