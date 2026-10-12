\name{cancer}
\alias{cancer}
\docType{data}
\title{Primary tumor domain (cancer) data
%%   ~~ data name/kind ... ~~
}
\description{
Classification data set from the UCI Machine Learning Repository
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("cancer")}
\format{
  The format of the data is a list with components $obs and
 $lab.
 "cancer$obs" includes the observations that are stored as numerical values.
"cancer$lab" contains the labels of the data.
}
\details{
A simple classification data set containing 16 attributes with 339
observations. Since the true labels are known, this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Primary+Tumor}
}

\examples{
#data(cancer)
}
\keyword{datasets}
