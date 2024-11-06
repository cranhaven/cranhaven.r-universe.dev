\name{bonddur}
\alias{bonddur}
\title{
Calculate the duration of a bond.
}
\description{
Function to calculate the duration of a bond.
}
\usage{
bonddur()
}
\details{
The user inputs are as follows:\cr
Face Value: to be entered in numbers for e.g. 1200.50\cr
Coupon rate: percent per annum\cr
Discount rate: percent per annum\cr
Maturity: number of years\cr
Note: Clicking on the '+' and '-' respectively increases and decreases the value.\cr
Coupon Payments:  chosen amongst Quarterly/Semi-annual/Annual\cr
Frequency of rates:  chosen amongst continuous/same as coupon/annual\cr
Duration formula:  chosen between Macaulay and Modified\cr
}


\value{
Duration of a bond.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{bondchange},\link[GUIDE]{bondprice}}
}
