\name{lympho}
\alias{lympho}
\docType{data}
\title{Lymphography domian (lympho) data
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
 "lympho$obs" includes the observations that are stored as numerical values.
"lympho$lab" contains the labels of the data.
}
\details{
A simple classification data set containing 18 attributes with 148
observations. Since the true labels are known, this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Lymphography}
}

\examples{
data(lympho)
}
\keyword{datasets}
