\name{jVaR}
\alias{jVaR}
\title{
Value at Risk Function
}
\description{
Compute VaR by many methods.

See the report: Value at Risk.<researchgate.net>
}
\usage{
jVaR(type, Return, Alpha, N_th_day)
}

\arguments{
  \item{type}{
  Computing method. 
  
	. 'non_adjust_hist': Historical method without any adjustment.
	
	. 'grch11_hist': Historical method with adjustment by Garch(1,1) method.
	
	. 'ewhv_hist': Exponential Weighted method.
	
	. 'ewma_hist': Historical method with adjustment by EWMA method.
	
	. 'kernel_hist': Estimating density function using kernel fitting method.
	
	. 'grch11_kernel_hist': Kernel fitting method apply on return adjusted by Garch(1,1).
	
	. 'ewma_kernel_hist': Kernel fitting method apply on return adjusted by EWMA.
	
	. 'garch11': Garch(1,1) method.
	
	. 'normal': Normal return method.
	
	. 'mle_normal': Normal return method (Estimating by maximum likelihood method).
	
	. 'monte_carlo': Simulation method.   
}

  \item{Return}{
  
A return series that computed from price series.
}

  \item{Alpha}{
  
Given probability of the event that loss exceeds VaR.
}

  \item{N_th_day}{
  
Time point of VaR computing (...,-1,0,1,...)

   
	   . -1 : previous day
	   
       .  0 : present
	   
       .  1 : next day
   	   
}
}

\value{
Value at Risk at the time point.
}
\references{
Value at Risk.(reserchgate.net)
}
\author{
Hung Vu
}
\note{
viet-hung.vu@jvn.edu.vn
}


\seealso{
https://www.researchgate.net/profile/Vu_Hung4
}
\examples{
y <- c(11, 12, 10, 13, 12, 14, 13, 15, 13, 14, 12)
s <- jReturn(y)
alpha <- 0.2
h <- 0
v <- jVaR('non_adjust_hist',s,alpha,h)
}
\keyword{ jVaRLim}
\keyword{ jMCPriLim }
\keyword{ jMCPri }
\keyword{ jTestVaR }