\name{plotAnnGraph}
\alias{plotAnnGraph}
\title{Visualize annotated pathway graphs}
\description{
  Visualize annotated pathway graphs.
}
\usage{
	plotAnnGraph(pathwayId,graphList,ann,gotoKEGG=FALSE,orgSpecific=TRUE,
	displayInR=TRUE,match=TRUE,vertex.frame.color="red",...)
}
\arguments{
  \item{pathwayId}{A character vector of pathway identifier, e.g., c("path:00010","path:00020").}
  \item{graphList}{A graph list.}
  \item{ann}{A list, e.g., the return value of the function \code{\link{identifyLncGraphW}}.}
  \item{gotoKEGG}{A logical. If TRUE, go to KEGG web site. The identified nodes are colored red.}
  \item{orgSpecific}{A logical. }
  \item{match}{A logical.}
  \item{displayInR}{A logical. If TRUE, display pathway graphs on the device.}
  \item{vertex.frame.color}{A character defining the frame color of the nodes which represent the annotated nodes in the pathway.}
  \item{...}{ The arguments passed to or from methods. See \code{\link{plotGraphL}}, \code{\link{plot.igraph}} and see \code{\link{plot}}.}  
}
\details{
The function can visualize the annotated pathway (or subpathways) graphs. The red nodes in the result graph represent the nodes which include the submitted molecules.
}
\author{ Xinrui Shi, Chunquan Li, Xia Li}
\seealso{\code{\link{plotGraphL}},\code{\link{identifyLncGraphW}},\code{\link{printGraphW}}}

\examples{
\dontrun{
### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  g2<-GetExampleData(exampleData="g2")
  interUMGraph<-getInteGraphList(g2,LncGenePairs)
  interUMGraph<-getInteUMGraph(LncGenePairs)
### get user-interested lncRNAs and genes sets.
  geneLnc<-GetExampleData(exampleData="geneLnc")
# get locate subpathways.
  sub<-getLocSubGraphLnc(geneLnc,interUMGraph,type="gene_lncRNA",n=1,s=8)
  SubcodeLncResult<-identifyLncGraphW(geneLnc,sub,type="gene_lncRNA",bet=1)
  resultT<-printGraphW(SubcodeLncResult,detail=TRUE)
  plotAnnGraph("path:04020_1",sub,SubcodeLncResult,gotoKEGG=FALSE)

}
}
\keyword{ file }

