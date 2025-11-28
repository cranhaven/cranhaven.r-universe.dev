\name{Topm}
\alias{Topm}
\title{Stacks correlation matrix}
\description{Stacks (correlation) matrix and selects values which are above the ``level''}

\usage{Topm(X, level=0.45, values=0, corr=TRUE, square=TRUE)}

\arguments{
\item{X}{Data frame or matrix with values}
\item{level}{Threshold}
\item{values}{If > 0, ignores "level" and outputs until reaches number, if "all", outputs all values}
\item{corr}{If FALSE, does not show magnitude}
\item{square}{If FALSE, does not use lower triangle, some rows could be redundant}
}

\details{'Topm()' stacks (correlation) matrix and selects (and sorts) values which are above the ``level''.

Good for the analysis of correlation matrices.
}

\value{Data frame with correlation values}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Cor}}}

\examples{
Topm(cor(trees), corr=TRUE)
}

\keyword{correlation}
