\name{assessBsFun}
\alias{assessBsFun}
\title{Assess the performance of group-sparse loading estimate}
\description{
  Evaluate the model selection consistency rate (SCR), F-measure and the smallest canonical correlation and the larger values mean better peformance in model selection and parameter estimation.
}
\usage{
  assessBsFun(hB, B0)
}
\arguments{
  \item{hB}{a \code{p}-by-\code{q} matrix, the estimated loading matrix.}
  \item{B0}{a \code{p}-by-\code{q} matrix, the true loading matrix.}
}

\value{
  return a vecotor with three compoents whose names are scr,fmea, ccorB.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{ccorFun}}.
}
\examples{
  dat <- gendata_Fac(n = 300, p = 500)
  res <- gsspFactorm(dat$X)
  assessBsFun(res$sphB, dat$B0)
  n <- nrow(dat$X)
  res <- gsspFactorm(dat$X, lambda1=0.05*n^(1/4), lambda2=9*n^(1/4))
  assessBsFun(res$sphB, dat$B0)
}
