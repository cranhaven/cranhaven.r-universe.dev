\name{BCubed_metric}
\alias{BCubed_metric}
\title{
BCubed_metric
}
\description{
This is to evaluate the clustering method with Bcubed F sore.
}
\usage{
BCubed_metric(L, C, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{ real label of classes 
%%     ~~Describe \code{L} here~~
}
  \item{C}{ classification label of classes drawn by clustering method
  %%     ~~Describe \code{C} here~~
}
  \item{alpha}{ F metric parameter which used to average precision and recall
  %%     ~~Describe \code{alpha} 
}
}
\value{
The function returns Bcubed F score of the clustering method. The higher the value is, the better performance the clustering method can get.
}

\details{
The clustering evaluation method based on Bcubed F metric.
}

\references{
Reference coming soon!
}
\author{ Lin Zhang, PhD  <lin.zhang@cumt.edu.cn>
%%  ~~who you are~~
}

\examples{
L <- c(1,1,2,1,1,2,2)
C <- c(2,2,1,2,2,1,1)
alpha <- 0.5
Bcubed_score <- BCubed_metric(L, C, alpha)
Bcubed_score
}
\keyword{ statistical Inference }
