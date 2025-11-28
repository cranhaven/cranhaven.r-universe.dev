\name{Linechart}
\alias{Linechart}
\title{Dotchart-like plot sfor every scaled variable grouped by factor}

\description{Dotchart-like plot for every scaled variable grouped by factor}

\usage{Linechart(vars, groups, xticks=TRUE, xmarks=TRUE, mad=FALSE, pch=19,
 se.lwd=1, se.col=1, ...)}

\arguments{
\item{vars}{Variables to draw (data frame)}
\item{groups}{Grouing factor}
\item{xticks}{Show xticks?}
\item{xmarks}{Show xmarks?}
\item{mad}{Show MAD instead of IQR?}
\item{pch}{Points type}
\item{se.lwd}{Lines width}
\item{se.col}{Lines color}
\item{...}{arguments to 'plot()'}
}

\details{

Linechart() is dotchart-based plot which shows medians and IQRs (or MADs)
for every scaled variable grouped by 'groups' factor.

Alternatives: trellis designs.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Boxplots}}}

\examples{

Trees <- trees
Trees[, 4] <- sample(letters[1:3], nrow(Trees), replace=TRUE)
Linechart(Trees[, 1:3], factor(Trees[, 4]))

Linechart(iris[, 1:4], iris[, 5])

}

\keyword{hplot}
