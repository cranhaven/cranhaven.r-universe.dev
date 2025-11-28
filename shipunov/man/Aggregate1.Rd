\name{Aggregate1}
\alias{Aggregate1}
\title{Aggregates by one vector and uses it for row names}
\description{Aggregates by one vector and uses it for row names}

\usage{Aggregate1(df, by, ...)}

\arguments{
\item{df}{Data frame to aggregate}
\item{by}{Atomic object to use for aggregating}
\item{...}{Further arguments for 'aggregate()'}
}

\details{
'Aggregate1()' is an 'aggregate()' helper: aggregates only by one atomic variable and uses it for row names.
}

\value{Same as of 'aggregate()'}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{aggregate}}}

\examples{
trees3 <- sample(letters[1:3], nrow(trees), replace=TRUE)
Aggregate1(trees, trees3, median, na.rm=TRUE)
}

\keyword{manip}
