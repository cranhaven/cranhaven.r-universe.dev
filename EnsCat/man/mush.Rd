\name{mush}
\alias{mush}
\docType{data}
\title{Mushroom data
%%   ~~ data name/kind ... ~~
}
\description{
Classification data set from the UCI Machine Learning Repository
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("mush")}
\format{
  The format of the data is a list with components $obs and
 $lab.
 "mush$obs" includes the observations that are stored as numerical values.
"mush$lab" contains the labels of the data.
}
\details{
A simple classification data set containing 22 attributes with 8124
observations, because the dataset is large, we only used the last 400 observations in our analysis. Since the true labels are known, this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Mushroom}
}

\examples{
data(mush)
}
\keyword{datasets}
