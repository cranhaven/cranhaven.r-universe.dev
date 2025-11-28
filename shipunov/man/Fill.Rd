\name{Fill}
\alias{Fill}
\title{Fill data values downstream, like in spreadsheets}

\description{Replaces "ditto" values with preceding values}

\usage{Fill(x, ditto="")}

\arguments{
\item{x}{Vector, possibly with missing values}
\item{ditto}{What to fill, typically empty string "" (default) or NA}
}

\value{Vector with replaced values}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Ditto}}}

\examples{
aa <- c("a", "a", "", "b", "", "c", "d", "")
Fill(aa)
bb <- c("a", "a", NA, "b", NA, "c", "d", NA)
Fill(bb, ditto=NA)
dd <- c("", "a", "a", "", "", "b", NA, "", "c", "d", "")
Fill(dd)
}

\keyword{manip}
