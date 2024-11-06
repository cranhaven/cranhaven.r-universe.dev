\name{varbehavior}
\alias{varbehavior}
\title{
Plot the behavior of value at risk as a function of its determinants.
}
\description{
Function to plot the behavior of value at risk as a function of its determinants.
}
\usage{
varbehavior()
}
\details{
The user inputs are as follows:\cr
weight1: The weight of stock 1\cr
mu1: the expected return \cr
Sigma1 (or Volatility) per annum: \cr
mu2: the expected return\cr
Sigma2 (or Volatility) per annum: \cr
Clicking on "+/-" increases/decreases the values of each of the above parameters.
}


\value{
A graph of the behavior of value at risk as a function of its determinants.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{var1stock},\link[GUIDE]{var2stocks}}
}

