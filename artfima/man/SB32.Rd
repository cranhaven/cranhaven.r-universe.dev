\name{SB32}
\alias{SB32}
\docType{data}
\title{
Turbulent flow data from Station SB32
}
\description{
Turbulent flow water time series, Lake Huron, during 2009-2010.
Sampled every second.
}
\usage{data("SB32")}
\format{
  The format is:
 num [1:5374] -2.2 1.4 -0.6 -0.4 -1.5 -2.6 -0.9 0.5 -0.9 1.5 ...
}
\details{
See paper by Meerschaert, Sabzikar, Phanikumar and Zeleke (2014).
}
\references{
M.M. Meerschaert, Farzad Sabzikar, M.S. Phanikumar, and A. Zeleke, 
Tempered fractional time series model for turbulence in geophysical flows, 
Journal of Statistical Mechanics: Theory and Experiment, 
Vol. 2014 p. P09023 (13 pp.) doi:10.1088/1742-5468/2014/09/P09023.
}

\examples{
data(SB32)
str(SB32)

#Figure from our paper
\dontrun{
ans0 <- artfima(SB32, fixd=5/6)
ans1 <- artfima(SB32, arimaOrder=c(1,0,2)) #best
p <- ans1$arimaOrder[1]
q <- ans1$arimaOrder[3]
sigmaSq1 <- ans1$sigmaSq
sigmaSq0 <- ans0$sigmaSq
w <- SB32
n <- length(w)
Ip <- Periodogram(w)
fr <- (1/n)*(1:length(Ip))
plot(log(fr), log(Ip), xlab="log frequency", ylab="log power", 
     type="p", col=rgb(0,0,1,0.4), pch=16)
y <- sigmaSq1*artfimaSDF(n=length(SB32), obj=ans1, plot="none")
lines(log(fr), log(y), type="l", lwd=2.5, col="red")
y0 <- sigmaSq0*artfimaSDF(n=length(SB32), obj=ans0, plot="none")
lines(log(fr), log(y0), type="l", lwd=3.5, col="green", lty=2)
TFD_label <- expression(paste("TFD, ", delta == 5/6, ", ", 
                              hat(lambda) == 0.045))
legend(x=-8, y=-5, xjust=0, yjust=0, legend=c("ARTFIMA(1,0,2)", TFD_label), 
       lty=c(1,2), lwd=c(2.5,3.5), col=c("red", "green"), bty="n")
}

}
\keyword{datasets}
