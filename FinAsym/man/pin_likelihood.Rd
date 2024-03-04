\name{pin_likelihood}
\alias{pin_likelihood}
\title{
Log-likelihood function for the estimation of the structural model of Easley and O'Hara (1987).
}
\description{
This function computes the value of the log-likelihood function (negative of) for the model of strategic trading with asymmetric information proposed by Easley and O'Hara (1987). This is an application of the sequential trade model of Kyle (1985).
}
\usage{
pin_likelihood(params, n_trades)
}
\arguments{
  \item{params}{
Parameters of the structural model in the following order: epsilon, mu, alpha, delta.
}
  \item{n_trades}{
A data matrix with 3 rows and a number of columns equal to the trading days. The first row includes the number of implicit trades without identified sign, the so-called 'no trades'. Buy-initiated trades are stored along the second row. Sell-initiated trades are indexed by the third row. 
}
}
\details{
The application of the model of Easley and O'Hara is based on the assumption that an equal share of uninformed traders buys or sells the asset, i.e. lambda=1/2. I also assume that the trader cannot make a distinction between uninformed counterparts who are willing to trade and those who are not, i.e. epsilon_s=epsilon_b=1/2.   
}
\value{
\item{likel_final }{Value of the likelihood function.}
}
\references{
D. Easley and M. O'Hara. Price, Trade Size, and Information in Securities Markets. Journal of Financial Economics, 19(1), 69-90, 1987.

L. R. Glosten and L. E. Harris. Estimating the Components of the Bid/Ask Spread. Journal of Financial Economics, 21(1), 123-142, 1988.

A. S. Kyle. Continuous Auctions and Insider Trading. Econometrica, 53(6):1315-1335, 1985.
}
\author{
Paolo Zagaglia <paolo.zagaglia@gmail.com>
}



\examples{
#Sample data
mydata<-matrix(c(11, 17, 14, 12, 11, 14, 
17, 14, 16, 8, 9, 13, 13, 13, 15, 7, 11, 
13, 14, 19, 14, 7, 11, 6, 12, 6, 17, 10, 
5, 7), nrow=3, byrow=TRUE)

#Make a starting guess at the solution
par0 <- c(0.5, 0.5, 0.5, 0.5)  

param_optim <- optim(par0, pin_likelihood, 
gr=NULL, mydata)
}

\keyword{ pin_likelihood }

