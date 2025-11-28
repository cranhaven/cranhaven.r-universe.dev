\name{SM.dist}
\alias{SM.dist}
\title{Simple Match distance}
\description{Calculates simple match distance}

\usage{SM.dist(data, zeroes=TRUE, cut=FALSE)}

\arguments{

\item{data}{Matrix (or data frame) with variables that should be used in
the computation of the distance between rows.}

\item{zeroes}{If FALSE (not default), zeroes will be ignored, so if data
is binary, result will be close to the asymmetric binary distance
('dist(..., method="binary")').}

\item{cut}{If TRUE (not default), attempt will be made to discretize all
numeric columns with number of breaks default to hist(); zeroes will be
saved.}

}

\details{

If argument is the data frame, SM.dist() internally converts it into the
matrix. If there are character values, they will be converted column-wise
to factors and then to integers.

SM.dist() ignores NAs when computing the distance values, and treates
zeroes the same way if 'zeroes=FALSE'.

}

\value{Distance object with distances among rows of 'data'}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{dist}}}

\examples{

(mm <- rbind(c(1, 0, 0), c(1, NA, 1), c(1, 1, 0)))
SM.dist(mm)
SM.dist(mm, zeroes=FALSE)
dist(mm, method="binary")

ii <- cluster::pam(SM.dist(sapply(iris[, -5], round)), k=3)
Misclass(ii$clustering, iris$Species, best=TRUE)

i2 <- cluster::pam(SM.dist(iris), k=3) # SM.dist() "consumes" all types of data
Misclass(i2$clustering, iris$Species, best=TRUE)

}

\keyword{multivariate}
