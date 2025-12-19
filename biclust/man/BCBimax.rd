\name{BCBimax}
\title{The Bimax Bicluster algorithm}
\alias{BCBimax}
\alias{Bimax}
\alias{BCBimax-class}
\alias{biclust,matrix,BCBimax-method}
\alias{BCrepBimax}
\alias{repBimax}
\alias{BCrepBimax-class}
\alias{biclust,matrix,BCrepBimax-method}



%- Also NEED an '\alias' for EACH other topic documented here.
\description{Performs Bimax Biclustering based on the framework by Prelic et. al.(2006). It searches for submatrices of ones in a logical matrix. Uses the original C code of the authors.}
\usage{
\S4method{biclust}{matrix,BCBimax}(x, method=BCBimax(), minr=2, minc=2, number=100)
\S4method{biclust}{matrix,BCrepBimax}(x, method=BCrepBimax(), minr=2, minc=2, number=100, maxc=12)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A logical matrix which represents the data.}
  \item{method}{Here BCBimax, to perform Bimax algorithm}
  \item{minr}{Minimum row size of resulting bicluster.}
  \item{minc}{Minimum column size of resulting bicluster.}
  \item{number}{Number of Bicluster to be found.}
  \item{maxc}{Maximum column size of resulting bicluster.}
}

\value{
  Returns an object of class \code{Biclust}.
}
\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\references{
Prelic, A.; Bleuler, S.; Zimmermann, P.; Wil, A.; Buhlmann, P.; Gruissem, W.; Hennig, L.; Thiele, L. & Zitzler, E. 
A Systematic Comparison and Evaluation of Biclustering Methods for Gene Expression Data Bioinformatics,
Oxford Univ Press, 2006, 22, 1122-1129
}



\seealso{ \code{\link{biclust}}, \code{\link{Biclust}}}
\examples{
%loma<-matrix(sample(c(0,1),1600,replace=TRUE),40,40)
 test <- matrix(rnorm(5000), 100, 50)
 test[11:20,11:20] <- rnorm(100, 3, 0.1)
 loma <- binarize(test,2)
 res <- biclust(x=loma, method=BCBimax(), minr=4, minc=4, number=10)
 res
}

\keyword{cluster}
\keyword{classif}
