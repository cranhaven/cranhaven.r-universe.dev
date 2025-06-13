\name{SubpathwayLNCE-internal}
\alias{initialize}
\alias{Getenvir}
\alias{getEdgeLabel}
\alias{getEdgeLty}
\alias{getLayout}
\alias{ExpProcess}
\alias{getKGeneFromGene}
\alias{getNodeLabel}
\alias{getOneNodePath}
\alias{getSymbolFromGene}
\title{SubpathwayLNCE internal functions}
\description{
  These functions internally called by other functions in the package \code{SupathwayLNCE} and not meant to be called by the user.
}

\usage{

    initialize()
    Getenvir(envData)
	getEdgeLabel(graph)
    getEdgeLty(graph)
	getLayout(graph)
	ExpProcess(ExpMatrix,pert)
	getKGeneFromGene(geneList)
	getNodeLabel(graph,type="symbol",displayNumber=1)
	getOneNodePath(current_node,other_nodes,pathway,
	n,all_shortest_paths_length,directed,method="shortestPaths")
	getSymbolFromGene(geneList)
	
}

\author{ Xinrui Shi, Chunquan Li, Xia Li}

\keyword{internal}