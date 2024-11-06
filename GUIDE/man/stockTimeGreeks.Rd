\name{stockTimeGreeks}
\alias{stockTimeGreeks}
\title{
Plot of option greeks for a European Call/Put as a function of stock price and time.
}
\description{
Function to plot of option greeks for a European Call/Put as a function of stock price and time.
}
\usage{
stockTimeGreeks()
}
\details{
The user inputs are as follows:\cr
Type of Option:  chosen between Call/Put\cr
Greek: chosen amongst Delta, Gamma, Vega, Theta, Rho\cr
Sigma (Volatility) per annum\cr
Risk free rate per annum: \cr
Clicking "+ / -" increases/decreases the value of the above two inputs.
}


\value{
Plot of option greeks for a European Call/Put as a function of stock price and time.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{calcgreeks}}
}

