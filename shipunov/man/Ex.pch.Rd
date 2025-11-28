\name{Ex.pch}
\title{Point examples}

\alias{Ex.pch}
\alias{Ex.points}

\description{Point ('pch') examples}

\usage{
Ex.pch(extras=c("*", ".", "+", "a"), cex=2, col="black", bg="gray",
 coltext="black", cextext=1.2, main="")
Ex.points(extras=c("*", ".", "+", "a"), cex=2, col="black", bg="gray",
 coltext="black", cextext=1.2, main="")
}

\arguments{
\item{extras}{which extra symbols to show}
\item{cex}{point scale, default 2}
\item{col}{point color, default black}
\item{bg}{point background (for symbols with a 'bg'-colored interior), default gray}
\item{coltext}{text color, default black}
\item{cextext}{text scale, default 1.2}
\item{main}{plot title, no title by default}
}

\details{Point ('pch') examples, modified from 'example(points)'.}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{points}}}

\examples{
Ex.points()
}

\keyword{hplot}
