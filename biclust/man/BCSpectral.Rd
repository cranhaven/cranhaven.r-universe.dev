\name{BCSpectral}
\alias{BCSpectral}
\alias{spectral}
\alias{spectral biclustering}
\alias{Kluger biclustering}
\alias{BCSpectral-class}
\alias{biclust,matrix,BCSpectral-method}


\title{The Spectral Bicluster algorithm}
\description{ Performs Spectral Biclustering as described in
  Kluger et al., 2003. Spectral biclustering supposes that normalized microarray
  data matrices have a checkerboard structure that can be discovered by the use
  of svd decomposition in eigenvectors, applied to genes (rows) and conditions
  (columns).}
\usage{
\S4method{biclust}{matrix,BCSpectral}(x, method=BCSpectral(), normalization="log", numberOfEigenvalues=6, 
minr=2, minc=2, withinVar=1, n_clusters = NULL, n_best = 3)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix where biclusters are to be found}
  \item{method}{Here BCSpectral, to perform Spectral algorithm}
  \item{normalization}{Normalization method to apply to mat. Three methods are
                     allowed as described by Kluger et al.:
                           "log" (Logarithmic normalization),
                           "irrc" (Independent Rescaling of Rows and Columns) and
                           "bistochastization". If "log" 
                                   normalization is used, be sure you can
                                   apply logarithm to elements in data matrix, if there are
                                   values under 1, it automatically will sum
                                   to each element in mat (1+abs(min(mat)))
                   Default is "log", as recommended by Kluger et al.}
  \item{numberOfEigenvalues}{the number of eigenValues considered to find
                   biclusters. Each row (gene) eigenVector will be combined
                   with all column (condition) eigenVectors for the first
                   numberOfEigenValues eigenvalues. Note that a high number could increase
                   dramatically time performance. Usually, only
                   the first eigenvectors are used.
                    With "irrc" and "bistochastization" methods, first
                    eigenvalue contains background (irrelevant) information, so
                    it is ignored.}
  \item{minr}{minimum number of rows that biclusters must have. The algorithm
               will not consider smaller biclusters.}
  \item{minc}{minimum number of columns that biclusters must have. The algorithm
               will not consider smaller biclusters.}
  \item{withinVar}{maximum within variation allowed. Since spectral biclustering
          outputs a checkerboard structure despite of relevance of individual cells, a
          filtering of only relevant cells is necessary by means of this within variation
          threshold.}
  \item{n_clusters}{vector with first element the number of row clusters and second element the number of column clusters. If \code{n_clusters = NULL}, the number of clusters will be estimated.}
  \item{n_best}{number of eigenvectors to which the data is projected for the final clustering step, recommended values are 2 or 3.}
}
%\details{}
\value{
   Returns an object of class \code{Biclust}.
}
\references{
    Kluger et al., "Spectral Biclustering of Microarray Data: Coclustering Genes and
  	Conditions", Genome Research, 2003, vol. 13, pages 703-716
  }

\author{

Sami Leon \email{Sami_Leon@URMC.Rochester.edu}

Rodrigo Santamaria \email{rodri@usal.es}
}

%\seealso{Maybe it's interesting to make public some of the submethods that
%  use spectral biclustering, as normalization methods or iterative k-means}
\examples{
# Random matrix with embedded bicluster  
test <- matrix(rnorm(5000),100,50)
test[11:20,11:20] <- rnorm(100,10,0.1)
image(test)

shuffled_test <- test[sample(nrow(test)), sample(ncol(test))]
image(shuffled_test)

# Without specifying the  number of row and column clusters
res1 <- spectral(shuffled_test,normalization="log", numberOfEigenvalues=6, 
                 minr=2, minc=2, withinVar=1, n_clusters = NULL, n_best = 3)
res1
image(shuffled_test[order(res1@info$row_labels), order(res1@info$column_labels)])


# Specifying the  number of row and column clusters
res2 <- spectral(shuffled_test,normalization="log", numberOfEigenvalues=6, 
                 minr=2, minc=2, withinVar=1, n_clusters = 2, n_best = 3)
res2
image(shuffled_test[order(res2@info$row_labels), order(res2@info$column_labels)])
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}
\keyword{multivariate}
