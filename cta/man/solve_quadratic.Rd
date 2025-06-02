\name{solve_quadratic}
\alias{solve_quadratic}
\title{
Solve for Real Root(s) to the Quadratic Equation
}
\description{
Solves for real-valued roots to the quadratic equation \eqn{ax^{2} + bx + c = 0}.
}
\usage{
solve_quadratic(a, b, c)
}
\arguments{
  \item{a, b, c}{Coefficients in the quadratic equation
                 \eqn{ax^{2} + bx + c = 0}.}
}
\value{
\code{solve_quadratic} returns a list, which includes the following two objects:
\item{flag}{Indicates the number of distinct real roots to the quadratic equation. It can be one of \code{"infinite"}, \code{"none"}, \code{"one"},
or \code{"two"}.}
\item{x}{Real root(s) to the quadratic equation. If
         \code{flag = "infinite"}, we simply write \code{x = 0};
         If \code{flag = "none"}, we write \code{x = NA}.}
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{quadratic.fit}}
}
\examples{
solve_quadratic(1, 2, 1)
solve_quadratic(1, 2, 2)
solve_quadratic(0, 2, 1)
}
\keyword{math}% use one of  RShowDoc("KEYWORDS")
