\name{curswapvalue}
\alias{curswapvalue}
\title{
Calculate the value of a fixed-fixed currency swap.
}
\description{
Function to calculate the value of a fixed-fixed currency swap.
}
\usage{
curswapvalue()
}
\details{
The user inputs are as follows:\cr
Notional(Home): to be entered in  numbers for e.g. 1000000\cr
Payment rate (Home): entered in decimalsfor e.g. 0.05 for 5 per cent\cr
Interest rate (Home): entered in decimalsfor e.g. 0.05 for 5 per cent\cr
Notional(Foreign): to be entered in numbers for e.g. 1200000\cr
Payment rate (Foreign): entered in decimals for e.g. 0.05 for 5 per cent\cr
Interest rate (Foreign): entered in decimals for e.g. 0.05 for 5 per cent\cr
Months for first payment: enter 3 for 3 months\cr
Spot exchange rate: units of home currency per unit of foreign currency. e.g. 1.5 dollars per pound is entered as 1.5\cr
Frequency of spot rates: chosen amongst continuous/quarterly/semi-annual/annual\cr
Number of periods: corresponds to settlement frequency. for e.g. if settlement frequency is chosen as semi-annual, a value of 3 (Number of periods) means three semi-annums.
Settlement frequency:  chosen amongst quarterly/semi-annual/annual\cr
}


\value{
The Value of a fixed-fixed currency swap.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{irswapvalue},\link[GUIDE]{cdswap}}
}

