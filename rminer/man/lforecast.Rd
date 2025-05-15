\name{lforecast}
\alias{lforecast}
\title{
Compute long term forecasts.
}
\description{
Performs multi-step forecasts by iteratively using 1-ahead predictions as inputs
}
\usage{
lforecast(M, data, start, horizon)
}
\arguments{
  \item{M}{fitted model, the object returned by \code{\link{fit}}.}
  \item{data}{training data, typically built using \code{\link{CasesSeries}}.}
  \item{start}{starting period (when out-of-samples start).}
  \item{horizon}{number of multi-step predictions.}
}
\details{
Check the reference for details.
}
\value{
Returns a numeric vector with the multi-step predictions.
}
\references{
\itemize{
\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr

\item To check for more details:\cr
P. Cortez.\cr
Sensitivity Analysis for Time Lag Selection to Forecast Seasonal Time Series using Neural Networks and Support Vector Machines.\cr
In Proceedings of the IEEE International Joint Conference on Neural Networks (IJCNN 2010), pp. 3694-3701, Barcelona, Spain, July, 2010. 
IEEE Computer Society, ISBN: 978-1-4244-6917-8 (DVD edition).\cr
\doi{10.1109/IJCNN.2010.5596890}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\seealso{
\code{\link{fit}}, \code{\link{CasesSeries}}, \code{\link{predict.fit}}, \code{\link{mgraph}}.
}
\examples{
ts=c(1,4,7,2,5,8,3,6,9,4,7,10,5,8,11,6,9)
d=CasesSeries(ts,c(1,2,3))
M=fit(y~.,d[1:7,],model="mlpe",search=2)
P1=predict(M,d[8:14,]) # single-step predictions
P2=lforecast(M,d,8,7) # multi-step predictions, horizon=7
print(mmetric(d$y[8:14],P1,"MAE"))
print(mmetric(d$y[8:14],P2,"MAE"))
L=vector("list",2); pred=vector("list",1);test=vector("list",1)
pred[[1]]=P1; test[[1]]=d$y[8:14]; L[[1]]=list(pred=pred,test=test,runs=1)
pred[[1]]=P2; test[[1]]=d$y[8:14]; L[[2]]=list(pred=pred,test=test,runs=1)
mgraph(L,graph="REG",Grid=10,leg=c("y","P1","P2"),col=c("black","cyan","blue"))
mgraph(L,graph="RSC",Grid=10,leg=c("P1","P2"),col=c("cyan","blue"))
}
\keyword{ regression }
\keyword{ neural }
\keyword{ nonlinear }
\keyword{ ts }
