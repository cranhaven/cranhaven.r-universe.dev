\name{cdswap}
\alias{cdswap}
\title{
Calculate the spread in a credit default swap.
}
\description{
Function to calculate the spread in a credit default swap.
}
\usage{
cdswap()
}
\details{
The user inputs are as follows:\cr
Notional: to be entered in numbers for e.g. 1000000\cr
Risk free rate: entered in decimals for e.g. 0.05 for 5 per cent\cr
Maturity in yrs: entered for e.g. 5 for 5 years\cr
Probability of Default: entered in decimals for e.g. 0.02 for 2 per cent\cr
Default assumption: chosen amongst End of Q1/End of half year/End of Q3/End of Year\cr
recovery rate: Clicking on "+/-" incrases/decreases the recovery rate.\cr
}


\value{
The spread in a credit default swap.
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

