\name{Ex.col}
\title{Examples of colors}
\alias{Ex.col}
\alias{Ex.cols}

\description{Examples of colors (current colors or all named colors)}

\usage{
Ex.col(all=FALSE)
Ex.cols(all=FALSE)
}

\arguments{
\item{all}{Show all named colors?}
}

\details{

If 'all=FALSE' (default), plots current colors along with their names and
numeric codes; "white" is added as the first color (with numeric code 0).
This plot does not usually look nice if the current palette contains more
than 40--45 colors.

If 'all=TRUE', plots all named colors plus (for completedness)
"transparent", which also can be used as color specification in R. Large
device is required to see all (almost 500) named colors.

For the palettes, run 'example(rainbow)' and other palette-related
commands.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{palette}}, \code{\link{rainbow}}, \code{\link{colors}}}

\examples{
Ex.cols()
Ex.cols(all=TRUE)
}

\keyword{hplot}
