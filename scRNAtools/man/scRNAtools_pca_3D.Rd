\name{scRNAtools_pca_3D}
\alias{scRNAtools_pca_3D}
\title{3D PCA analysis}
\description{PCA analysis for scRNA sequencing data and present 3D figure.}
\usage{
scRNAtools_pca_3D(example1, types)
}
\arguments{
  \item{example1}{
scRNA sequencing data with header.
}
  \item{types}{
Cell types in the example data.
}
}
\author{
Qian Yang
}
\examples{
##3D PCA analysis
data(example1)
data(types)
scRNAtools_pca_3D(example1,types)##3D figure of PCA results.
}
