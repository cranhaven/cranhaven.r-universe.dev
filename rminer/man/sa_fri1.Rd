\name{sa_fri1}
\alias{sa_fri1}
\alias{sa_ssin}
\alias{sa_psin}
\alias{sa_int2}
\alias{sa_tree}
\alias{sa_ssin_2}
\alias{sa_ssin_n2p}
\alias{sa_int2_3c}
\alias{sa_int2_8p}
\docType{data}
\title{
Synthetic regression and classification datasets for measuring input importance of supervised learning models
}
\description{
5 Synthetic regression (sa_fri1, sa_ssin, sa_psin, sa_int2, sa_tree) and 4 classification (sa_ssin_2, sa_ssin_n2p, sa_int2_3c, sa_int2_8p) datasets for measuring input importance of supervised learning models
}
\usage{data(sa_fri1)}
\format{
  A data frame with 1000 observations on the following variables.
  \describe{
    \item{\code{x}n}{input (numeric or factor, depends on the dataset)}
    \item{\code{y}}{output target (numeric or factor, depends on the dataset)}
  }
}
\details{
Check reference or source for full details
}
\source{See references}
\references{
\itemize{
\item To cite the Importance function, sensitivity analysis methods or synthetic datasets, please use:\cr
P. Cortez and M.J. Embrechts.\cr
Using Sensitivity Analysis and Visualization Techniques to Open Black Box Data Mining Models.\cr
In Information Sciences, Elsevier, 225:1-17, March 2013.\cr
\doi{10.1016/j.ins.2012.10.039}\cr
}
}
\examples{
data(sa_ssin)
print(summary(sa_ssin))
\dontrun{plot(sa_ssin$x1,sa_ssin$y)}
}
\keyword{datasets}
