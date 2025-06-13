\name{printGraphW}
\alias{printGraphW}
\title{Print the results of graph annotation and identification}
\description{
  Print the results of graph annotation and identification.
}
\usage{
     printGraphW(ann,detail=FALSE)
}
\arguments{
  \item{ann}{ A list. The value was returned from the function \code{\link{identifyLncGraphW}}.}
  \item{detail}{ A logical. If true, gene lists from the function \code{\link{identifyLncGraphW}} are converted into strings, which are used to display and write results with genes. }
}
\value{
A data.frame. Columns include pathwayId, pathwayName, annMoleculeRatio, annBgRatio,annWeight, pvalue, 'fdr', annMoleculeList, annBgMoleculeList. Detailed information is provided in \code{\link{identifyLncGraphW}}.
}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{\code{\link{identifyLncGraphW}}}
\examples{
\dontrun{
### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  inteUMGraph<-getInteUMGraph(LncGenePairs)
### get user-interested lncRNAs and genes sets.
  geneLnc<-GetExampleData(exampleData="geneLnc")
# get locate subpathways.
  sub<-getLocSubGraphLnc(geneLnc,inteUMGraph,type="gene_lncRNA",n=1,s=8)
  SubcodeLncResult<-identifyLncGraphW(geneLnc,sub,type="gene_lncRNA",bet=1)
  resultT<-printGraphW(SubcodeLncResult,detail=TRUE)


}
}
\keyword{ file }

