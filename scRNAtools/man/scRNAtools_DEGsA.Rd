\name{scRNAtools_DEGsA}
\alias{scRNAtools_DEGsA}
\title{Identification of differentially expressed genes}
\description{Users can identify differentially expressed genes between two type of cells based on fold change value.}
\usage{
scRNAtools_DEGsA(example, types_all, type1, type2, num)
}
\arguments{
  \item{example}{
scRNA sequencing data with header.
}
  \item{types_all}{
Cell types in the example data.
}
  \item{type1}{
Cell type one.
}
  \item{type2}{
Cell type two.
}
  \item{num}{
Threshold value of expressed genes in appointed cell types. For example, we set 0.8 in example section.
}
}
\details{
The output data is the fold change value of differentially expressed genes.
}
\author{
Qian Yang
}
\examples{
data(example)
data(types)
type1<-"No malignant"
type2<-"Malignant"
num<-0.8;###type1 Vs type2
pdf(file=file.path(tempdir(), "DEGs.pdf"))
scRNAtools_DEGsA(example,types_all,type1,type2,num)
dev.off()
}
