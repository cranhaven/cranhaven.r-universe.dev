\name{Ditto}
\alias{Ditto}
\title{Removes duplicated data values downstream}

\description{Replaces duplicated values with "ditto" string}

\usage{Ditto(x, ditto="")}

\arguments{
\item{x}{Vector, possibly with missing values}
\item{ditto}{String to replace with, typically empty string "" (default)}
}

\details{

If the first argument is not a character vector, Ditto() converts it to
the character.

}

\value{Vector with replaced values}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Fill}}}

\examples{
Ditto(c("a", "a", "", "b", "b"))
Ditto(c("a", "a", "", "b", NA, "b"))
Ditto(c("a", "a", "", "b", NA, "b"), ditto=NA)
Ditto(c("a", "a", "", "b", NA, "b"), ditto="--")
}

\keyword{manip}
