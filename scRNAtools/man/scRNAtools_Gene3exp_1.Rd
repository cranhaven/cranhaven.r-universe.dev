\name{scRNAtools_Gene3exp_1}
\alias{scRNAtools_Gene3exp_1}
\title{Present gene expression}
\description{This function can present the expression of two gene in appointed cell type.}
\usage{scRNAtools_Gene3exp_1(example,types_all,gene1,gene2,gene3,n,col_1,col_2,col_3,pch,lwd)}
\arguments{
  \item{example}{
scRNA sequencing data without header.
}
  \item{types_all}{
Cell names of each type.
}
  \item{gene1}{
Gene one you are interested in.
}
  \item{gene2}{
Gene two you are interested in.
}
  \item{gene3}{
Gene three you are interested in.
}
  \item{n}{
Number of cell names in scRNA sequening data.
}
  \item{col_1}{
The color of line of gene one in the figure.
}
  \item{col_2}{
The color of line of gene two in the figure.
}
  \item{col_3}{
The color of line of gene three in the figure.
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
gene1<-"CHD1"
gene2<-"CD82"
gene3<-"ASS1"
col_1="red"
col_2="blue"
col_3="green"
pch=19
lwd=2
n<-3
scRNAtools_Gene3exp_1(example,types_all,gene1,gene2,gene3,n,col_1,col_2,col_3,pch,lwd)
}
