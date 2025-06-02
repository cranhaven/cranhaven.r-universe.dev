\name{create.U}
\alias{create.U}
\title{
Orthogonal Complement of the Column Space of a Matrix
}
\description{
Creates a full column rank matrix, \eqn{U}, with column space
equal to the orthogonal complement of the column space of \eqn{X}. That is,
\eqn{U} has column space equal to the null space of \eqn{X'}.
}
\usage{
create.U(X)
}
\arguments{
  \item{X}{A full column rank matrix.}
}
\value{
\code{create.U} returns a full column rank matrix \code{U}, with column
space equal to the orthogonal complement of the column space of \code{X}.
}
\author{
Joseph B. Lang
}
\seealso{
\code{\link{mph.fit}}
}
\examples{
X <- matrix(seq(1, 12), ncol = 2, byrow = TRUE)
create.U(X)
}
\keyword{math}% use one of  RShowDoc("KEYWORDS")

