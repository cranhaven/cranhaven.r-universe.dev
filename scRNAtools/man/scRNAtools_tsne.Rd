\name{scRNAtools_tsne}
\alias{scRNAtools_tsne}
\title{t-SNE analysis}
\description{t-SNE analysis for scRNA sequencing data}
\usage{
scRNAtools_tsne(exam)
}
\arguments{
  \item{exam}{
scRNA sequencing data with four genes. Users can reference the format and input their own data.
}
}
\references{
L.J.P. van der Maaten and G.E. Hinton. Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research 9(Nov):2579-2605, 2008.
}
\author{
Qian Yang
}
\examples{
data(exam)
scRNAtools_tsne(exam)
}
