\name{fra}
\alias{fra}
\title{
Calculate the forward rate.
}
\description{
Function to Calculate the forward rate.
}
\usage{
fra()
}
\details{
The user inputs are as follows:\cr
Months1:  number of months for the loan period to begin- enter 3 for 3 months\cr
Rate1:  enter in decimals. For e.g. 5% is entered as 0.05\cr
Months2:  number of months for the loan period to end- enter 6 for 6 months\cr
Rate2:  enter in decimals. For e.g. 8% is entered as 0.08\cr
Frequency of spot rates: chosen between Continuous and Loan period\cr
}


\value{
The forward rate of interest.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{forwardcurrency},\link[GUIDE]{forwardcommodity}}
}
