\name{xtest}
\alias{xtest}
\title{
    Test each pair relations
}
\description{
    We recommend four testings distribution to compare two samples:
    \enumerate{
              \item Kolmogorov Smirnov test
              \item Wilcoxon signed rank test 
              \item Student's t test
              \item Compare Groups of Growth Curves
            }
}
\usage{
  xtest(v1, v2)
}
\arguments{
  \item{v1}{a vector of the first entity}
  \item{v2}{a vector of the second entity} 
}
\details{
    The function \code{xtest} will combine the values in the first entity with the values in the second entity, each pair relations will be looking in documents. If this relationship exists, it will bring a value 1 otherwise 0  
}
\author{
Tien T. Phan
}
\seealso{
  \code{\link{ks.test}} Kolmogorov Smirnov test\cr
  \code{\link{wilcox.test}} Wilcoxon signed rank test\cr
  \code{\link{t.test}} Student's t test\cr
  \code{\link{compareGrowthCurves}} Compare Groups of Growth Curves\cr
}
\examples{
  #get all values of entity bioagressor
  b <- as.vector(xdata_value("b")[["value"]]) 
  xtest("colza",b)
}
\keyword{xtest}