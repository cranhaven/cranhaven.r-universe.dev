\name{str_count}
\alias{str_count}
\title{
  Count words in a text
}
\description{
  Count words of characters in the string which satisfy a regular expression
}
\usage{
  str_count(x, pattern, sep)
}
\arguments{
  \item{x}{input string}
  \item{pattern}{regular expression}
  \item{sep}{a string used to separate columns, default is "".}
}
\value{
  \item{number}{return a number of words that satisfies a regular expression}
}
\examples{
  x = "file_1:b:$:carbonate:c:dimethylsulfide:coccoliths:co2:aragonite:calcite:"
  str_count(x,pattern=":co2:",sep="")
}
\keyword{count}
