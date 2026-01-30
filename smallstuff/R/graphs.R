#########1#########2#########3#########4#########5#########6#########7#########8
#' Create an adjacency matrix from a multigraph according to the definition
#'
#' Create an adjacency matrix using the definition, i.e. an entry equals 1 if
#' there is an edge from the vertex in the column to the vertex in the row, and
#' cycles are counted twice.
#'
#' @param g the graph (an igraph object)
#' @param ... additional arguments to be passed to the igraph function
#' \code{as_adj}
#' @return Adjacency matrix for graph \code{g}
#' @examples
#' g=igraph::graph_from_literal(1-2,2-2:3:3:4,3-4:5:6,5-1:1:1,6-6,simplify=FALSE)
#' as_adj_def(g)
#' @export
################################################################################
as_adj_def<-function(g,...) {
  if (!inherits(g,"igraph")) stop("Must input a graph")
  m=as.matrix(igraph::as_adj(g,...))
  if (igraph::is.directed(g)) t(m) else m+diag(diag(m))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Add Graph Attributes to a Graph from a Data Frame
#'
#' Add graph attributes to a graph from a data frame where each column
#' represents an attribute. Note that only the first row of the data frame
#' is used.
#'
#' @param g the graph (an igraph object) to which the graph attributes should
#' be added
#' @param df data frame, or an object that can be converted to a data
#' frame, where the first row contains a graph attribute in each column
#' @return Graph \code{g} with the graph attributes in \code{df} added.
#' @examples
#' g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
#' df=data.frame(name="Test Graph",descr="A graph")
#' graph_attr_from_df(g,df)
#' @export
################################################################################
graph_attr_from_df<-function(g,df) {
  if (!inherits(g,"igraph")) stop("Must input a graph")
  df=as.data.frame(df)
  for (i in 1:ncol(df)) {
    if (inherits(df[,i],"factor")) df[,i]=as.character(df[,i])
    g=igraph::set_graph_attr(g,names(df)[i],df[1,i])
  }
  g
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Weight Distribution of a Graph
#'
#' Obtain the weight distribution of a graph, indicating for each strength from
#' zero to the maximum strength of any vertex, the proportion of vertices with
#' such a strength. This assumes positive integer weights.
#'
#' @param g the graph (an igraph object)
#' @param cumulative \code{TRUE} if cumulative weights are to be used; default
#' is \code{FALSE}
#' @param ... additional parameters to be passed to the igraph function
#' \code{strength}
#' @return A vector with the weighted degree distribution for the graph
#' \code{g}.
#' @examples
#' g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
#' igraph::E(g)$weight=c(1,2,1,4,2,1,1)
#' table(igraph::strength(g))/6
#' weight_distribution(g)
#' @export
################################################################################
weight_distribution<-function(g,cumulative = FALSE,...) {
  if (!inherits(g,"igraph")) stop("Must input a graph")
  cs <- igraph::strength(g,...)
  hi <- graphics::hist(cs, -1:max(cs), plot = FALSE)$density
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  names(res)=0:(length(res)-1)
  res
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Split a Graph into Subgraphs
#'
#' Split a graph into subgraphs using the values in a vector to indicate which
#' vertices belong together.
#'
#' @param g the graph (an igraph object)
#' @param split a vector with a value for each vertex in \code{g}
#' @return A list of graphs, where each graph is a subgraph of \code{g}
#' containing the vertices with the same value in \code{split}.
#' @examples
#' g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
#' split=c("A","A","B","B","A","B")
#' igraph::V(g);split
#' igraph::V(get_subgraphs(g,split)[[1]])
#' igraph::V(get_subgraphs(g,split)[[2]])
#' @export
################################################################################
get_subgraphs<-function(g,split) {
  if (!inherits(g,"igraph")) stop("Must input a graph")
  if (length(split)!=igraph::gorder(g)) stop("split must have a value for each vertex")
  split=as.factor(split)
  subs=list()
  for (i in 1:length(levels(split))) {
    subs[[i]]=igraph::induced_subgraph(g,which(as.numeric(split)==i))
  }
  subs
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Find Edge Crossings
#'
#' Determine if edges in a graph cross groups or stay within groups. This is
#' similar to the crossings function in igraph, but uses a vector for the split
#' rather than a communities object.
#'
#' @param split a vector with a value for each vertex in \code{g}, indicating
#' the group each vertex belongs to
#' @param g an igraph object
#' @return A logical vector indicating for each edge if it crosses groups or 
#'         not. For each edge that crosses, it is TRUE, otherwise it is FALSE.
#' @examples
#' g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
#' split=c("A","A","B","B","A","B")
#' igraph::V(g);split
#' igraph::E(g);crossing2(split,g)
#' @export
################################################################################
crossing2 <- function (split, g) 
{
  if (!inherits(g,"igraph")) stop("Must input a graph")
  if (length(split)!=igraph::gorder(g)) stop("split must have a value for each vertex")
  el <- igraph::as_edgelist(g, names = FALSE)
  m1 <- split[el[,1]]
  m2 <- split[el[,2]]
  res <- m1 != m2
  if (!is.null(names(m1))) {
    names(res) <- paste(names(m1), names(m2), sep = "|")
  }
  res
}
