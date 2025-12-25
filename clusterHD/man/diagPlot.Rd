\name{diagPlot}
\alias{diagPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
diagnostic plots for HTK-Means Clustering
}
\description{
Make diagnostic plots for HTK-means clustering.
}
\usage{
diagPlot(HTKmeans.out, type = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HTKmeans.out}{ the output of a call to \code{\link{HTKmeans}.}
}
\item{type}{ if \code{type = 1}, plots the regularization path. If \code{type = 2}, plots the differences in WCSS and ARI against the number of active variables.}
}

\value{No return value, makes the plot directly.}

\details{
This visualization plots the regularization path or 
the differences in WCSS and ARI against the number of active variables.
}

\references{
Raymaekers, Jakob, and Ruben H. Zamar. "Regularized K-means through hard-thresholding." arXiv preprint arXiv:2010.00950 (2020).
}
\author{
J. Raymaekers and R.H. Zamar
}

\seealso{
\code{\link{HTKmeans}}
}
\examples{
X <- iris[, -5]
lambdas <- seq(0, 1, by = 0.01)
HTKmeans.out <- HTKmeans(X, 3, lambdas)
\donttest{
diagPlot(HTKmeans.out, 1)
diagPlot(HTKmeans.out, 2)
}
}
