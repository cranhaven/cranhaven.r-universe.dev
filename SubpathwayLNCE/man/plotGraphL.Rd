\name{plotGraphL}
\alias{plotGraphL}
\title{Visualize a pathway graph}
\description{
  Visualize a pathway graph.
}
\usage{
plotGraphL(graph,margin=0,vertex.label.cex=0.6,vertex.label.font=1,
  vertex.size=8,vertex.size2=6,edge.arrow.size=0.2,
  edge.arrow.width=3,vertex.label=V(graph)$graphics_name,
  vertex.shape=V(graph)$graphics_type,layout=getLayout(graph),
  vertex.label.color="black",vertex.color=V(graph)$graphics_bgcolor,
  vertex.frame.color="dimgray",edge.color="dimgray",
  edge.label=getEdgeLabel(graph),edge.label.cex=0.6,
  edge.label.color="dimgray",edge.lty=getEdgeLty(graph),
  axes=FALSE,xlab="",ylab="",sub=NULL,main=NULL,...)
 
}
\arguments{
  \item{graph}{ The igraph object of a pathway graph.}
  \item{margin}{ A numeric. The value is usually between -0.5 and 0.5, which is able to zoom in or out a pathway graph. The default is 0.} 
  \item{vertex.label.cex}{ A numeric vector of node label size.}
  \item{vertex.label.font}{ A numeric vector of label font.} 
  \item{vertex.size}{ A numeric vector of Node size. See \code{\link{plot.igraph}}} 
  \item{vertex.size2}{ A numeric vector of Node size.}
  \item{edge.arrow.size}{ Edge arrow size.The default is 0.2.}
  \item{edge.arrow.width}{ Edge arrow width. The default is 3.}
  \item{vertex.label}{ A vector of node label. The default is graphics_name.}  
  \item{vertex.shape}{ A vector of node shape. The default is graphics_type.} 
  \item{layout}{ A matrix of x-y coordinates with two dims. Determine the placement of the nodes for drawing a graph. The default is the KEGG node coordinates that are originally obtained from the KGML file.}  
  \item{vertex.label.color}{ A vector of node label colors. The default is black.}    
  \item{vertex.color}{ A vector of node colors. The default is the KEGG node color.}  
  \item{vertex.frame.color}{ A vector of node frame color. The default is dimgray.}
  \item{edge.color}{ A vector of edge color. The default is dimgray.}
  \item{edge.label}{ A vector of edge label.}  
  \item{edge.label.cex}{ Edge label size.}  
  \item{edge.label.color}{ A vector of edge label color. The default is dimgray.}  
  \item{edge.lty}{ A vector of line type for the edges.}    
  \item{axes}{ A logical. whether to plot axes. The default is FALSE.}
  \item{xlab}{ A character string. The label of the horizontal axis. The default is the empty string.}
  \item{ylab}{ A character string. The label of the vertical axis. The default is the empty string.}
  \item{sub}{ A character string of subtitle. }
  \item{main}{ A character string of main title. }
  \item{...}{ The arguments passed to or from methods. See \code{\link{plot}}.}  
}
\details{
The function \code{plotGraphL} is able to display a pathway graph. 

The argument \code{layout} is used to determine the placement of the nodes for drawing a graph. There are mainly two preprocessed methods to determine the placement of the nodes for drawing a pathway graph: the KEGG pathway layout and \code{\link{layout}} provided in the function \code{\link{plot.igraph}} of the \code{igraph} package. The default layout is the KEGG layout, for which the coordinates of nodes in KEGG is used to determine the placement of the nodes for drawing a graph. Therefore, the returned figure by the function may be very similar to the KEGG pathway graph when information in the pathway graph is complete relatively. The layouts provided in \code{igraph} include \code{layout.reingold.tilford}, \code{layout.random}, \code{layout.circle}, \code{layout.sphere},..., .

}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{\code{\link{plot}}, \code{\link{layout.random}}}

\examples{
\dontrun{
###  get  signal pathway graphs   ###

g<- GetExampleData(exampleData="g2")

# visualize the graph 
plotGraphL(g[[9]],layout=layout.random)

}
}
\keyword{ file }

