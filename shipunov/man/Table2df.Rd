\name{Table2df}
\alias{Table2df}
\title{Convert table to data frame saving structure}

\description{Convert table to data frame saving structure}

\usage{Table2df(table)}

\arguments{
\item{table}{'table' object}
}

\details{
Convert contingency table into data frame and keep structure.
}

\value{Data frame}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Table2df(table(iris[, 5]))
}

\keyword{manip}
