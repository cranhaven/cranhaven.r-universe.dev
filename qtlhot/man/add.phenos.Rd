\name{add.phenos}
\alias{add.phenos}
\title{
Add phenotypes to cross object.
}
\description{
Add phenotypes to cross object by checking index.
}
\usage{
add.phenos(cross, newdata = NULL, index = NULL)
}
\arguments{
  \item{cross}{object of class \code{cross}; see \code{\link[qtl]{read.cross}}}
  \item{newdata}{data frame with row names matching values of phenotype
    identified by \code{index} for object \code{cross}}
  \item{index}{character string name of phenotype in object
    \code{cross}; if \code{NULL}, then \code{newdata} must be of same
    size as \code{cross} with phenotypes in order}
}
\details{
The name \code{index} must be a phenotype in the \code{cross}
object. The row names of \code{newdata} are matched with values of \code{index}.
}
\value{
object of class \code{cross} with added phenotypes
}
\author{
Brian S. Yandell, \email{byandell@wisc.edu}
}
\seealso{\code{\link[qtl]{read.cross}}}
\examples{
\dontrun{
data(hyper)
x <- data.frame(x = rnorm(nind(hyper)))
hyperx <- add.phenos(hyper, x)
}
}
\keyword{utilities}
