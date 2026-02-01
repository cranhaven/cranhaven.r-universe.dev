\name{plot.netgwasmap}
\alias{plot.netgwasmap}

\title{ plot for S3 class "netgwasmap" }

\description{
Plot the graph associated with constructed linkage map via function \code{\link{netmap}}.
}

\usage{
\method{plot}{netgwasmap}(x, vis= NULL, layout= NULL, vertex.size= NULL, label.vertex =
		"none", label.size= NULL, vertex.color= NULL, edge.color = "gray29",
		sel.ID = NULL, ... )
}

\arguments{
	\item{x}{An object from "netgwasmap" class.}
	\item{vis}{Visualizing in four options: (i) "summary" plots the related network, conditional dependence relationships between markers before and after ordering markers; (ii) "interactive" plots the associated network, where it opens a new windows with interactive graph drawing facility; (iii) "unordered markers" plots the conditional dependence relationships between markers before ordering markers; (iv) "ordered markers" plots conditional dependence relationships between markers after ordering markers. Default is "summary".}
	\item{layout}{The vertex placement algorithm which is according to \pkg{igraph} package. The default layout is Fruchterman-Reingold layout. Other possible layouts are, for example, layout_with_kk, circle, and Reingold-Tilford graph in \pkg{igraph} package. }
	\item{vertex.size}{ Optional integer to adjust vertex size in graph G. Default is 5.}
	\item{label.vertex}{Assign names to the vertices. There are three options: "none", "some", "all". (i) Specifying "none" omits vertex labels in the graph, (ii) using \code{label.vertex} = "some" you need to provide a vector of vertex IDs or a single vertex ID to the \code{sel.ID} argument, which you would like to be shown in the graph. \code{label.vertex} = "some" is only applicable for \code{vis} = "interactive", (iii) Specifying "all" includes all vertex labels in the graph. Default is "none".}
	\item{label.size}{Optional integer to adjust the size of node's label in graph G. Applicable when vertex.label is TRUE. Default is 0.8.}
	\item{vertex.color}{Optional integer vectors giving colors to the vertices. }
	\item{edge.color}{Optional integer vectors giving colors to edges. }
	\item{sel.ID}{ONLY applicable when \code{vis}= "interactive". A vector of vertex IDs or a single vertex ID, which you would like to be shown in the graph. ONLY applicable when \code{label.vertex}="some".}
	\item{\dots}{ONLY applicable when \code{vis}= "CI". System reserved (No specific usage)}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}
}

\references{
1. Behrouzi, P., and Wit, E. C. (2018). De novo construction of polyploid linkage maps using discrete graphical models. Bioinformatics. \cr
2. Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}


\seealso{\code{\link{netmap}}, \code{\link{buildMap}}.}

