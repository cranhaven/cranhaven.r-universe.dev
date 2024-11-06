\name{floortreegui}
\alias{floortreegui}
\title{
Plot a Floor  Tree
}
\description{
Function to plot a Floor  Tree
}
\usage{
floortreegui()
}
\details{
The user inputs are as follows:\cr
Face Value: to be entered in numbers for e.g. 120.50\cr
Strike: to be entered in numbers for e.g. 110.50\cr
Rate per annum: to be entered in percent. For e.g. enter 5.0 for 5 percent \cr
u: up move factor- to be entered in decimals. For e.g. 1.25 \cr
d: down move factor- to be entered in decimals. For e.g. 0.80\cr
q: probability of up move- to be entered in decimals. For e.g. 0.60\cr
Coupon: to be entered in percent. For e.g. enter 5.0 for 5 percent \cr
Floor Maturity: Clicking on the '+' and '-' respectively increases and decreases the value.\cr
Plot type: chosen between Floor  tree / Rate tree
}


\value{
A plot of Floor  Tree with user specified parameters.
}

\references{
David G. Luenberger, "Investment Science", 2/E, Oxford University Press, 2013. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{ratetreegui},\link[GUIDE]{captreegui}}
}

