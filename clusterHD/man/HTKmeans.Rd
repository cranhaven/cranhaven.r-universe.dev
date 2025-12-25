\name{HTKmeans}
\alias{HTKmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HTK-Means Clustering
}
\description{
Perform HTK-means clustering (Raymaekers and Zamar, 2022) on a data matrix.
}
\usage{
HTKmeans(X, k, lambdas = NULL,
         standardize = TRUE,
         iter.max = 100, nstart = 100,
         nlambdas = 50,
         lambda_max = 1,
         verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a matrix containing the data.
}
\item{k}{
the number of clusters.
}
\item{lambdas}{
a vector of values for the regularization parameter \code{lambda}.
Defaults to \code{NULL}, which generates a sequence of values automatically.
}
\item{standardize}{
logical flag for standardization to mean 0 and variance 1 of
the data in \code{X}. This is recommended, unless the variance
of the variables is known to quantify relevant information.
}
\item{iter.max}{
the maximum number of iterations allowed.
}
\item{nstart}{
number of starts used when k-means is applied to generate
the starting values for HTK-means. See below for more info.
}
\item{nlambdas}{
Number of lambda values to generate automatically.
}
\item{lambda_max}{
Maximum value for the regularization paramater \eqn{lambda}. If
\code{standardize = TRUE}, the default of 1 works well.
}
\item{verbose}{
Whether or not to print progress. Defaults to \code{FALSE}.
}
}
\details{
The algorithm starts by generating a number of sparse starting values. This is done using k-means on subsets of variables. See 
Raymaekers and Zamar (2022) for details. 
}
\value{
A list with components: \cr
  \itemize{
  \item{\code{HTKmeans.out} \cr
  A list with length equal to the number of lambda values supplied in \code{lambdas}.
Each element of this list is in turn a list containing
\item{centers }{A matrix of cluster centres.}
\item{cluster }{A vector of integers (from 1:\code{k}) indicating the cluster to which each point is allocated.}
\item{itnb }{The number of iterations executed until convergence}
\item{converged }{Whether the algorithm stopped by converging or through reaching the maximum number of itertions.}
  }
  \item{\code{inputargs} \cr
   the input arguments to the function.
  }
  }
}
\references{
Raymaekers, Jakob, and Ruben H. Zamar. "Regularized K-means through hard-thresholding." arXiv preprint arXiv:2010.00950 (2020).
}
\author{
J. Raymaekers and R.H. Zamar
}

\seealso{
\code{\link[stats]{kmeans}}
}
\examples{
X <- iris[, 1:4]
HTKmeans.out <- HTKmeans(X, k = 3, lambdas = 0.8)
HTKmeans.out[[1]]$centers
pairs(X, col = HTKmeans.out[[1]]$cluster)
}
