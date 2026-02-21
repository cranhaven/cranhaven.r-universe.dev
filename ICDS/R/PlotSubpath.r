#' @title PlotSubpathway
#' @description PlotSubpathway:plot a network graph when user input a list of gene
#' @param subpID gene list of a interested subpathway
#' @param pathway.name name of the interested subpathway
#' @param zz z-score of each gene
#' @param Pathway the name of the pathway database
#' @param layout The layout specification(\code{\link[igraph]{layout_}}). It must be a call to a layout specification function.
#' @return Network graph
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph simplify
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom methods is
#' @importFrom graphite edges
#' @importFrom graphite pathways
#' @importFrom utils installed.packages
#' @import org.Hs.eg.db
#' @export
#' @examples
#' require(graphite)
#'
#' subpID<-unlist(strsplit("ACSS1/ALDH3B2/ADH1B/ADH1A/ALDH2/DLAT/ACSS2","/"))
#' pathway.name="Glycolysis / Gluconeogenesis"
#' zzz<- GetExampleData("zzz")
#' \donttest{PlotSubpathway(subpID=subpID,pathway.name=pathway.name,zz=zzz)}
PlotSubpathway<-function(subpID,pathway.name,zz,Pathway="kegg",layout=layout.fruchterman.reingold){
  dbpkg <- "org.Hs.eg.db"
  pkgs <- installed.packages()[,1]
  if (!dbpkg %in% pkgs) {
    stop("You should have org.Hs.eg.db package installed before using PlotSubpathway...")
  }
  require(dbpkg, character.only=TRUE)
  p<-match(pathway.name,names(pathways("hsapiens", Pathway)))
	edges(convertIdentifiers(pathways("hsapiens", Pathway)[[p]],"symbol"))[,c("src","dest")]->Pairs;

	graph.data.frame(unique(Pairs,stringsAsFactors=FALSE), directed=FALSE)->graph;
	simpleGraph<-simplify(graph);

  
	g2<-induced_subgraph(simpleGraph,subpID);
	nz<-zz[match(names(V(g2)),rownames(zz)),"z_score"];
	myc<-colorRampPalette(c("#F4BEBE", "#F90D0D"))(length(nz))[order(order(nz))];
	plot(g2,vertex.color=myc,layout=layout,vertex.label.dist=1,edge.width=1.5);
}
