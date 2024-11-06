\name{fravalue}
\alias{fravalue}
\title{
Calculate the value of a forward rate agreement.
}
\description{
Function to Calculate value of a forward rate agreement.
}
\usage{
fravalue()
}
\details{
The user inputs are as follows:\cr
Notional: enter notional value of FRA. e.g. 1 mn is entered as 1000000\cr
Fixed Rate:  enter in decimals. For e.g. 5% is entered as 0.05\cr
Fwd Rate:  enter in decimals. For e.g. 8% is entered as 0.08\cr
Months1:  number of months for the loan period to begin- enter 3 for 3 months\cr
Months2:  number of months for the loan period to end- enter 6 for 6 months\cr
}


\value{
The value of a forward rate agreement.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{fra}}
}
