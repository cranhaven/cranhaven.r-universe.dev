\name{bondchange}
\alias{bondchange}
\title{
Calculate the change in the price of a bond for change in yield based on the duration or duration and convexity approximtion.

}
\description{
Function to calculate change in the price of a bond for change in yield based on the duration or duration and convexity approximation.
}
\usage{
bondchange()
}
\details{
The user inputs are as follows:\cr
Face Value: to be entered in numbers for e.g. 1200.50\cr
Modified Duration: percent per annum\cr
Convexity: percent per annum\cr
Change in yield (in basis points): clicking on "+/-" increases/decreases the yield.\cr
Formula/Approximation:  chosen between Duration/Duration and Convexity\cr
}


\value{
The change in the price of a bond for change in yield based on the duration or duration and convexity approximtion.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{bondchange},\link[GUIDE]{bonddur}}
}

