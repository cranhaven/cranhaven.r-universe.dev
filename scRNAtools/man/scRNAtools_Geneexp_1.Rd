\name{scRNAtools_Geneexp_1}
\alias{scRNAtools_Geneexp_1}
\title{Present gene expression}
\description{
This function can present the expression of one gene in appointed cell type.
}
\usage{
scRNAtools_Geneexp_1(example, gene, types_all, n, col, pch, lwd)
}
\arguments{
  \item{example}{
scRNA sequencing data without header.
}
  \item{gene}{
One gene you are interested in.
}
  \item{types_all}{
Cell names of each type.
}
  \item{n}{
Number of cell names in scRNA sequening data.
}
  \item{col}{
The color of line in the figure.
}
  \item{pch}{
The shape of nodes in figure.
}
  \item{lwd}{
The width of lines in figure.
}
}
\author{
Qian Yang
}
\examples{
data(example)
data(types_all)
gene<-"CHD1";###Set the gene you are interested in.
n<-3;###Set the type of cells you are interested in.
col<-"red";###Set the color of line in the figure.
pch<-19;###Set the shape of nodes in figure.
lwd<-2;###Set the width of lines in figure.
scRNAtools_Geneexp_1(example,gene,types_all,n,col,pch,lwd)
}
