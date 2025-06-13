\name{getInteGraphList}
\alias{getInteGraphList}
\title{Get the reconstructed signal pathway graphs}
\description{
 Get the reconstructed KEGG signal pathway graphs embedded by lncRNAs of competitive regulation.
}
\usage{
getInteGraphList(graphList,relations)
}
\arguments{
  \item{graphList}{A graphList. There nodes must be represented by genes.}
  \item{relations}{A data frame. It contains two columns, the first is lncRNA names and the second is its target names.}
}
\details{
The argument "relations" represents user-interested lncRNA-target interactions, which can be returned from the \code{GetExampleData}.
 }
\value{
A graph list. 
}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{ \code{\link{plotGraphL}}, \code{\link{getLocSubGraphLnc}}, \code{\link{GetExampleData}}}
\examples{
\dontrun{
### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  g2<-GetExampleData(exampleData="g2")
  interUMGraph<-getInteGraphList(g2,LncGenePairs)
  #interUMGraph<-getInteUMGraph(LncGenePairs)
### get user-interested lncRNAs and genes sets.
  #geneLnc<-GetExampleData(exampleData="geneLnc")
# get locate subpathways.
  #sub<-getLocSubGraphLnc(geneLnc,interUMGraph,type="gene_lncRNA",n=1,s=8)
  #SubcodeLncResult<-identifyLncGraphW(geneLnc,sub,type="gene_lncRNA",bet=1)
  #resultT<-printGraphW(SubcodeLncResult,detail=TRUE)

 }
}
\keyword{ file }