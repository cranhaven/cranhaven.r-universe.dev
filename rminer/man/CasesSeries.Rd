\name{CasesSeries}
\alias{CasesSeries}
\title{
Create a training set (data.frame) from a time series using a sliding window.
}
\description{
Create a training set (data.frame) from a time series using a sliding window.
}
\usage{
CasesSeries(t, W, start = 1, end = length(t))
}
\arguments{
  \item{t}{a time series (numeric vector).}
  \item{W}{a sliding window (with time lags, numeric vector).}
  \item{start}{starting period.}
  \item{end}{ending period.}
}
\details{
Check reference for details.
}
\value{
Returns a data.frame, where \code{y} is the output target and the inputs are the time lags.
}
\references{
\itemize{
\item To check for more details:\cr
P. Cortez.\cr
Sensitivity Analysis for Time Lag Selection to Forecast Seasonal Time Series using Neural Networks and Support Vector Machines.\cr
In Proceedings of the IEEE International Joint Conference on Neural Networks (IJCNN 2010), pp. 3694-3701, Barcelona, Spain, July, 2010. 
IEEE Computer Society, ISBN: 978-1-4244-6917-8 (DVD edition).\cr
\doi{10.1109/IJCNN.2010.5596890}\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\seealso{
\code{\link{fit}}, \code{\link{lforecast}}, \code{\link{predict.fit}}.
}
\examples{
t=1:20
d=CasesSeries(1:10,c(1,3,4))
print(d)
d=CasesSeries(1:10,c(1,2,3))
print(d)
}
\keyword{ datasets }
\keyword{ ts }
