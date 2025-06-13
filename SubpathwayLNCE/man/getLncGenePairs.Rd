\name{getLncGenePairs}
\alias{getLncGenePairs}
\title{Get the co-express lncRNA-mRNA interactions}
\description{
 Get the co-express lncRNA-mRNA interactions reached a significant positive threshold. These lncRNAs-mRNAs pairs are competitivly rugulated.
}
\usage{
getLncGenePairs(GeneExp,LncExp,a=0.025)
}
\arguments{
  \item{GeneExp}{A gene expression profile. The first row of gene expression profile is names of mRNA, and the gene expression profile and the lncRNA expression profile must be mached. }
  \item{LncExp}{A lncRNA expression profile. The first row of lncRNA expression profile is names of lncRNA, and the gene expression profile and the lncRNA expression profile must be mached.}
  \item{a}{A parameter,namely the significant positive threshold.}
}
\details{
Co-express coefficient for any pair of relations in the candidate LncRNA-mRNA network is calculated basing on matched LncRNA and mRNA expression profiles, those relations had reached a significant positive threshold were retained.
 }
\value{
A data frame. The data frame has two columns, the fist is lncRNA names, the other is mRNA names.
}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\examples{
\dontrun{


 GeneExp<-GetExampleData(exampleData="GeneExp")
 LncExp<-GetExampleData(exampleData="LncExp")
 LncGenePairs<-getLncGenePairs(GeneExp,LncExp,a=0.025)
 }
}
\keyword{ file }