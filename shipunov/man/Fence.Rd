\name{Fence}
\alias{Fence}
\title{Colorize tips of 'hclust' plot}
\description{Uses segments() and Tcoords() to colorize 'hclust' plot}

\usage{
  Fence(hcl, fct, ex=0.05, lwd=2.5, horiz=FALSE, hang=0.1, ...)
}

\arguments{

  \item{hcl}{\code{hclust} object}

  \item{fct}{Variable to colorize labels, will be converted into factor}

  \item{ex}{The fraction of the plot height by which segments go up and
  down from the tips; by default, it is half of the 'hang'}

  \item{lwd}{Line width of segments}

  \item{horiz}{Plot on a horizontal tree?}

  \item{hang}{The fraction of the plot height by which labels should hang
  below the rest of the plot; by default, it is equal to the default
  'hang' from \code{hclust} which is 0.1}

  \item{...}{Further arguments to segments()}
}

%\details{}

\seealso{\code{\link{Tcoords}}, \code{\link{hclust}}}

\examples{

iris.h <- hclust(dist(iris[, -5]))
plot(iris.h, labels=FALSE)
Fence(iris.h, iris$Species)
legend("topright", legend=levels(iris$Species), col=1:3, lwd=2.5, bty="n")

}

\keyword{cluster}
