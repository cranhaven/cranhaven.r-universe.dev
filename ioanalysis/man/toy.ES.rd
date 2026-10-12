\name{toy.ES}
\alias{toy.ES}
\docType{data}
\title{An example dataset of class \code{EasySelect}}

\description{An object of \code{EasySelect} class created from \code{\link{easy.select}}.}
\usage{data("toy.ES")}
\format{
  A character matrix with three columns and 5 rows with class \code{EasySelect}. The first row indicates which rows/columns of \code{toy.IO} are of interest. The second and third column are the regions and sectors that respectively match the the first column.
}

\examples{
data(toy.ES)
class(toy.ES)
}
\keyword{datasets}
