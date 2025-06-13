\name{getLocSubGraphLnc}
\alias{getLocSubGraphLnc}
\title{Get the located signal subpathways
}
\description{
Locate signal subpathways competitively regulated by lncRNAs.
}
\usage{
getLocSubGraphLnc(moleculeList, graphList, type="gene_lncRNA", 
   n=1, s=8, method = "shortestPaths")
}
\arguments{
  \item{moleculeList}{A character vector. Such as competitively regulated lncRNAs and/or differentially expressed genes under disease phenotypes. Gene should be geneid.}
  \item{graphList}{A graph list. There nodes must be represented by genes. } 
  \item{type}{A character string. Should be one of "gene", "lncRNA" or "gene_lncRNA".}
  \item{n}{An integer. The maximum acceptable quantities of non-signature node at the shortest path between each two interested molecules.}  
  \item{s}{An integer. The minimum acceptable quantities of nodes in located subpathways.}  
  \item{method}{A character string. In which the shorest path algorithms will be used.} 
}
\details{
We apply lenient distance similarity method to locate signal subpathways competitively regulated by miRNAs. We first map user competitively regulated lncRNAs and/or interested genes to pathways as signatures. For a given pathway, we compute the shortest path between any two signatures. In shortest path, if the number of non-signature nodes between two signatures is no more than n, then these two signature nodes and other nodes at the shortest path are added into the same node set. We extract the corresponding subgraph in the pathway graph according to each node set. We finally define these subgraphs with node number >= s as the subpathway regions of the pathway. 
The argument \code{n} is maximum number of permitted non-signature nodes at the shortest path between signature nodes. The default parameter n=1.
The argument \code{s} is used to filter subpathways in which the number of nodes are less than the parameter s. The default parameter s=8.
The argument \code{method} determines which shortest path algorithms will be used. We set the default value as "get.shortest.paths". 
}
\value{ A list of graphs.}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{\code{\link{getInteGraphList}}, \code{\link{identifyLncGraphW}}}
\examples{
\dontrun{

### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  interUMGraph<-getInteUMGraph(LncGenePairs)
### get user-interested lncRNAs and genes sets.
##geneLnc<-c(getBackground(type="gene")[1:3000],unique(LncGenePairs[1,]))
  geneLnc<-GetExampleData(exampleData="geneLnc")
# get locate subpathways.
  sub<-getLocSubGraphLnc(geneLnc,interUMGraph,type="gene_lncRNA",n=1,s=8)
  
}
}
\keyword{ file }