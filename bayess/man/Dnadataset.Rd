\name{Dnadataset}
\alias{Dnadataset}
\docType{data}
\title{
DNA sequence of an HIV genome%%   ~~ data name/kind ... ~~
}
\description{
\code{Dnadataset} is a base sequence corresponding to a complete HIV
(which stands for Human Immunodeficiency Virus)
genome where A, C, G, and T have been recoded as 1,2,3,4.
It is modelled as a hidden Markov chain and is used in Chapter 7.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Dnadataset)}
\format{
  A data frame with 9718 rows and two columns, the first one corresponding
to the row number and the second one to the amino-acid value coded from 1 to 4.
}
\examples{
data(Dnadataset)
summary(Dnadataset)
}
\keyword{datasets}
\keyword{DNA}
