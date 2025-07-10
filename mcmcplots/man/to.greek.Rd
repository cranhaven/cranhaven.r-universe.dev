\name{.to.greek}
\alias{.to.greek}
\title{
Parse strings containing names of greek letters.
}
\description{
  Convert character vector with greek letters to an expression suitable for plotting greek symbols in titles and labels.
}
\usage{
.to.greek(instr)
}
\arguments{
  \item{instr}{
    a character vector
  }
}
\value{
  Parsed representation of the character vector.
}
\author{
  Ilya Goldin
}
\seealso{
  \code{\link[base]{parse}}
}
\examples{
\dontrun{.to.greek(c("alpha", "beta", "mybeta", "YourDelta"))}
}
\keyword{ utilities }
