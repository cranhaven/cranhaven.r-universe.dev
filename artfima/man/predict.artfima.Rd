\name{predict.artfima}
\alias{predict.artfima}
\title{
Predict method for artfima
}
\description{
The optimal minimum mean square error forecast and its standard deviation for
lags 1, 2, ..., n.ahead is computed at forecast origin starting at the end
of the observed series used in fitting. The exact algorithm discussed in
McLeod, Yu and Krougly is used.
}
\usage{\method{predict}{artfima}(object, n.ahead=10, ...)}

\arguments{
  \item{object}{ object of class "artfima" }
  \item{n.ahead}{ number of steps ahead to forecast }
  \item{...}{ optional arguments }
}

\value{a list with two components
\item{Forecasts}{Description of 'comp1'}
\item{SDForecasts }{Description of 'comp2'}
}

\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{\code{\link{predict.Arima}}}

\references{
McLeod, A.I., Yu, Hao and Krougly, Z. (2007).  
Algorithms for Linear Time Series Analysis: With R Package.  
Journal of Statistical Software  23/5 1-26.
}  

\examples{
ans <- artfima(seriesa, likAlg="Whittle")
predict(ans)
#compare forecasts from ARTFIMA etc.
  \dontrun{
ML <- 10
ans <- artfima(seriesa)
Ftfd <- predict(ans, n.ahead=10)$Forecasts 
ans <- artfima(seriesa, glp="ARIMA", arimaOrder=c(1,0,1))
Farma11 <- predict(ans, n.ahead=10)$Forecasts 
ans <- artfima(seriesa, glp="ARFIMA")
Ffd <- predict(ans, n.ahead=10)$Forecasts
#arima(0,1,1)
ans <- arima(seriesa, order=c(0,1,1))
fEWMA <- predict(ans, n.ahead=10)$pred
yobs<-seriesa[188:197]
xobs<-188:197
y <- matrix(c(yobs,Ffd,Ftfd,Farma11,fEWMA), ncol=5)
colnames(y)<-c("obs", "FD", "TFD", "ARMA11","FEWMA")
x <- 197+1:ML
x <- matrix(c(xobs, rep(x, 4)), ncol=5)
plot(x, y, type="n", col=c("black", "red", "blue", "magenta"),
     xlab="t", ylab=expression(z[t]))
x <- 197+1:ML
points(xobs, yobs, type="o", col="black")
points(x, Ffd, type="o", col="red")
points(x, Ftfd, type="o", col="blue")
points(x, Farma11, type="o", col="brown")
points(x, fEWMA, type="o", col="magenta")
legend(200, 18.1, legend=c("observed", "EWMA", "FD", "TFD", "ARMA"),
       col=c("black", "magenta", "red", "blue", "brown"),
       lty=c(rep(1,5)))
  }
}

\keyword{ts}


