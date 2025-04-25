\name{scalefun}
\alias{scalefun}
\title{Different forms of scaling}
\description{
  Function providing different forms of scaling in disciminant analysis
  - the resulting data matrix is mean-centered after the
  scaling. Modelled after functions in the st package.
}
\usage{
scalefun(sc.p = c("none", "log", "sqrt", "pareto", "auto"))
}
\arguments{
  \item{sc.p}{Type of scaling. A pass-through option, performing only
    mean-centering, is provided by argument "none".}
}
\value{A matrix. The function performs the required scaling, and
  mean-centers the result.}
\author{Ron Wehrens}
\examples{
X <- gen.data(5, nvar = 9, nsimul = 1)
FUN <- scalefun(sc.p = "pareto")
FUN(X$X[,,1])
}
\keyword{manip}
\keyword{multivariate}
