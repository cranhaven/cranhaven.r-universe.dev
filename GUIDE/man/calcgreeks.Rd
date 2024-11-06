\name{calcgreeks}
\alias{calcgreeks}
\title{
Calculate the greeks for a European Call/Put.
}
\description{
Function to calculate the greeks for a European Call/Put.
}
\usage{
calcgreeks()
}
\details{
The user inputs are as follows:\cr
Spot: to be entered in numbers for e.g. 120.50\cr
Strike: to be entered in numbers for e.g. 110.50\cr
Maturity in number of years: to be entered in decimals. For e.g. 0.25 for a quarter year\cr
Dividend yield: to be entered in decimals. For e.g. 0.02 for 2 per cent \cr
Type of Option:  chosen between Call/Put \cr
Greek: chosen amongst Delta, Gamma, Vega, Theta, Rho\cr
Sigma (Volatility) per annum\cr
Risk free rate per annum: \cr
Clicking "+ / -" increases/decreases the value of the above two inputs.
}


\value{
The value of the chosen greek for a European Call/Put.
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

