\name{irswapvalue}
\alias{irswapvalue}
\title{
Calculate the value of an interest rate swap.
}
\description{
Function to calculate the value of an interest rate swap.
}
\usage{
irswapvalue()
}
\details{
The user inputs are as follows:\cr
Notional: to be entered in decimals for e.g. 1000000\cr
Fixed rate: entered in decimals for e.g. 0.05 for 5 per cent\cr
Last spot rate: entered in decimals for e.g. 0.05 for 5 per cent\cr
Months for first payment: enter 3 for 3 months\cr
Spot rates: enter with comma separation. e.g. 0.054, 0.056, 0.058\cr
Frequency of spot rates: chosen amongst continuous/quarterly/semi-annual/annual\cr
Settlement frequency:  chosen amongst quarterly/semi-annual/annual\cr
}


\value{
The Value of an interest rate swap.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{curswapvalue},\link[GUIDE]{cdswap}}
}

