\name{scRNAtools_inter_net}
\alias{scRNAtools_inter_net}
\title{Construction of interactive network in scRNA sequencing data}
\description{Construction of interactive network based on scRNa sequencing data.}
\usage{
scRNAtools_inter_net(corr_re, p, r, size, color)
}
\arguments{
  \item{corr_re}{
The results of correlation analysis, which including four columns, the first two columns are genes and the last two columns are correlation index and p-value,respectively.
}
  \item{p}{
The p-value of correlation index.
}
  \item{r}{
Correlation index
}
  \item{size}{
The size of nodes in the network.
}
  \item{color}{
The color of nodes in the network.
}
}
\author{
Qian Yang
}
\examples{
data(corr_re)
p<-0.05
r<-0.9
size<-5 #nodes size
color<-"#00B2EE" ##Color of nodes.
pdf(file=file.path(tempdir(), "interact_net.pdf"))
scRNAtools_inter_net(corr_re,p,r,size,color)
dev.off()
}
