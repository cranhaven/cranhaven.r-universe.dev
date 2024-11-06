\name{swaptreegui}
\alias{swaptreegui}
\title{
Plot a swap  Tree
}
\description{
Function to plot a Swap  Tree
}
\usage{
swaptreegui()
}
\details{
The user inputs are as follows:\cr
Face Value: to be entered in numbers for e.g. 120.50\cr
Rate per annum: to be entered in percent. For e.g. enter 5.0 for 5 percent \cr
u: up move factor- to be entered in decimals. For e.g. 1.25 \cr
d: down move factor- to be entered in decimals. For e.g. 0.80\cr
q: probability of up move- to be entered in decimals. For e.g. 0.60\cr
Fixed Rate: to be entered in percent. For e.g. 4.5 for 4.5 percent \cr
Swap Maturity: Clicking on the '+' and '-' respectively increases and decreases the value.\cr
Plot type: chosen between Swap  tree / Rate tree
}


\value{
A plot of Swap  Tree with user specified parameters.
}

\references{
David G. Luenberger, "Investment Science", 2/E, Oxford University Press, 2013. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{ratetreegui},\link[GUIDE]{swaptiontreegui}}
}

