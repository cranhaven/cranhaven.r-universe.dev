\name{scRNAtools_Geneexp}
\alias{scRNAtools_Geneexp}
\title{Expressed genes in scRNA sequencing data}
\description{Extracted the genes expressed in cells. Users can set the threshold value.}
\usage{
scRNAtools_Geneexp(example, types, num)
}
\arguments{
  \item{example}{
scRNA sequencing data without header.
}
  \item{types}{
Cell types in the example data.
}
  \item{num}{
Threshold value of expressed genes in appointed cell types. For example, we set 0.8 in example section.
}
}
\value{

\item{zset}{Gene expression data required the threshold value.}

}

\author{
Qian Yang
}
\examples{
data(example)
types<-"1"
num<-0.8
scRNAtools_Geneexp(example,types,num)
}
