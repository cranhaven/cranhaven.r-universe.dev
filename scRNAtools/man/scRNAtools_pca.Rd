\name{scRNAtools_pca}
\alias{scRNAtools_pca}
\title{PCA analysis}
\description{PCA analysis for scRNA sequencing data}
\usage{
scRNAtools_pca(example1, types)
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
data(example1)
data(types)
pdf(file=file.path(tempdir(), "PCA_result-R.pdf"))##Save the figures of PCA results.
scRNAtools_pca(example1,types)
dev.off()
}
