\name{futurescurrency}
\alias{futurescurrency}
\title{
Calculate the value of a currency  futures.
}
\description{
Function to calculate the value of a currency  futures.
}
\usage{
futurescurrency()
}
\details{
The user inputs are as follows:\cr
Spot:  entered in decimals. For e.g. 105.50\cr
Risk free Rate:  entered in decimals. For e.g. 5 per cent is entered as 0.05\cr
Maturity:  entered in number of years. For e.g. half year is 0.5\cr
Foreign Interest(s): entered with comma separation if it is in cash and occuring at multiple times e.g. 2.50, 3.0. If it is in yield terms, it is entered in decimals. e.g. 0.02 for 2 per cent\cr
Interest time(s):entered with comma separation e.g. 0.25,0.50 for 3 months and 6 months\cr
Type of Income:  chosen between yield/cash\cr
}


\value{
The value of a currency  futures.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{futuresstock},\link[GUIDE]{futurescommodity}}
}
