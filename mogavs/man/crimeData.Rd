\name{crimeData}
\alias{crimeData}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Crime Data Set with Imputed Values
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This is the communities and crime data set, but with missing values imputed with the \pkg{mclust} package.
}
\usage{data(crimeData)}

\seealso{
\code{\link{sampleData}}
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Redmond, M. (2009) Communities and Crime Data Set. \emph{UCI Machine Learning Repository}
}
\examples{
data(crimeData)
head(crimeData)
}
\keyword{datasets}
