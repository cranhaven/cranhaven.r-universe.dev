\name{scRNAtools-package}
\alias{scRNAtools-package}
\docType{package}
\title{Single Cell RNA Sequencing Data Analysis Tools}
\description{
We integrated the common analysis methods utilized in single cell RNA sequencing data, which included cluster method, PCA, the filter of differentially expressed genes, pathway enrichment analysis and correlated analysis methods.
}
\author{
Qian Yang
Maintainer: Qian Yang <bioqianyang@163.com>
}
\examples{
####Here list three main function, cluster, PCA and t-SNE####
####cluster####
data(example1);##Example data in this package.
k<-6;##set K based on your own requirement.
scRNAtools_cluster(example1,k)
####PCA####
data(example1)
data(types)
pdf(file=file.path(tempdir(), "PCA_result-R.pdf"))##Save the figures of PCA results.
scRNAtools_pca(example1,types)
dev.off()
####t-SNE#####
data(exam)
scRNAtools_tsne(exam)

####Gene expression###
data(example)
types<-"1"
num<-0.8
scRNAtools_Geneexp(example,types,num)
}
