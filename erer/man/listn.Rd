\name{listn}
\alias{listn}
\title{Generate a list object with names}
\description{Generate a list object with names.}
\usage{listn(...)}
\arguments{
  \item{... }{ individual objects to be included in a list.}
}
\details{This generates a list object by addressing the naming problem. For \code{list}, if no names are given, the list generated will have no names. In some situations, the number of individual objects is large and the names of these individual objects can be used as the names. This function addresses this need. If names are given, they will be used. If not, the names of individual objects will be used.}

\value{Return a list object with names.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\examples{

y1 <- 1:10
y2 <- c("a", "b")
listn(y1, y2)
listn(y1 = y1, y2)
listn(y1 = y1, y2.rev = y2, y2, 5:8, c("d", "f"))

identical(listn(y1, y2), listn(y1 = y1, y2))        # TRUE
identical(listn(y1, y2), list(y1 = y1, y2))        # FALSE
identical(listn(y1, y2), list(y1 = y1, y2=y2))     # TRUE
}

\keyword{manip}