\name{Alldups}
\alias{Alldups}
\title{Finds all duplicates}
\description{Finds duplicates from both ends, optionally returns indexes of duplicate groups}

\usage{Alldups(v, groups=FALSE)}

\arguments{
\item{v}{Vector, matrix or data frame}
\item{groups}{If TRUE, returns group indexes (non-duplicated are 0)}
}

\details{

This is extension of duplicated() which _does not_ skip the first
duplicate in each group. 'NA' consider for duplicates but do not count as
duplicate group.

If the first argument is a matrix or data frame and 'groups=TRUE',
Aldups() starts from converting them into character vector with
paste0(..., collapse="").

If 'groups=TRUE', Alldups() uses as.numeric(as.character(v)) twice to
index duplicated groups with natural numbers (and non-duplicated with 0).

}

\value{Logical vector of length equal to 'v', or numerical vector if 'groups=TRUE'}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{duplicated}}}

\examples{
aa <- c("one", "two", "", NA, "two", "three", "three", "three", NA, "", "four")
Alldups(aa)
data.frame(v=aa, dups=Alldups(aa), groups=Alldups(aa, groups=TRUE))

## clustering based on duplicates from rounding
(iris.dgr <- Alldups(round(iris[, 1:4]/10), groups=TRUE))
Misclass(iris.dgr, iris$Species, best=TRUE)
}

\keyword{manip}
