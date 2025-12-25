#####################################################
## Utility functions to process and analyze graphs ##
#####################################################

#' @title Build graph levels
#' @description Group a set of nodes in according to their maximum depth in the graph. Firstly, it inverts the weights
#' of the graph and then it applies the Bellman Ford algorithm to find the shortest path, achieving in this way the longest path.
#' @param g an object of class \code{graphNEL}.
#' @param root name of the class that it is on the top-level of the hierarchy (\code{def. root="00"}).
#' @return A list of the nodes grouped w.r.t. the distance from the root: the first element of the list corresponds to the root node (level 0),
#' the second to nodes at maximum distance 1 (level 1), the third to the node at maximum distance 3 (level 2) and so on.
#' @export
#' @examples
#' data(graph);
#' root <- root.node(g);
#' lev <- graph.levels(g, root=root);
graph.levels <- function(g, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("root is not the right root node of g. Use the function root.node(g) to find the root node of g");
    ed <- edges(g);
    ew <- edgeWeights(g);
    for(i in 1:length(ed)){
        l <- length(ew[[i]]);
        if(l!=0)
            ew[[i]][1:l] <- -1;
    }
    edL <- vector(mode="list", length=length(ed));
    names(edL) <- names(ed);
    for(i in 1:length(ed)){
        edL[[i]] <- list(edges=ed[[i]], weights=ew[[i]]);
    }
    G <- graphNEL(nodes=nodes(g), edgeL=edL, edgemode="directed");
    depth.G <- bellman.ford.sp(G,root)$distance;
    depth.G <- -depth.G
    levels <- vector(mode="list", length=max(depth.G)+1);
    names(levels) <- paste(rep("level", max(depth.G)+1), 0:max(depth.G), sep="_");
    for(i in 1:(max(depth.G)+1))
        levels[[i]] <- names(which(depth.G==i-1));
    return(levels);
}

#' @title Flip graph
#' @description Compute a directed graph with edges in the opposite direction.
#' @param g a \code{graphNEL} directed graph
#' @return A graph (as an object of class \code{graphNEL}) with edges in the opposite direction w.r.t. \code{g}.
#' @export
#' @examples
#' data(graph);
#' g.flipped <- compute.flipped.graph(g);
compute.flipped.graph <- function(g){
    ed <- edges(g);
    ndL <- vector(mode="list", length=length(ed));
    names(ndL) <- names(ed);
    for(i in 1:length(ed)){
        children <- ed[[i]];
        parent   <- names(ed[i]);
        if(length(children)!=0){
            for(j in 1:length(children))
                ndL[[children[j]]] <- c(ndL[[children[j]]],parent);
        }
    }
    for (i in 1:length(ndL))
        ndL[[i]] <- list(edges=ndL[[i]]);
    og <- graphNEL(nodes=nodes(g), edgeL=ndL, edgemode="directed");
    return(og);
}

#' @name build.parents
#' @title Build parents
#' @description Build parents for each node of a graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that it is on the top-level of the hierarchy (\code{def. root="00"}).
#' @param levels a list of character vectors. Each component represents a graph level and the elements of any component correspond to nodes.
#' The level 0 represents the root node.
#' @examples
#' data(graph);
#' root <- root.node(g)
#' parents <- build.parents(g, root=root);
#' lev <- graph.levels(g, root=root);
#' parents.tod <- build.parents.top.down(g, lev, root=root);
#' parents.bup <- build.parents.bottom.up(g, lev, root=root);
#' parents.tsort <- build.parents.topological.sorting(g, root=root);
#' @rdname build.parents
#' @return \code{build.parents} returns a named list of character vectors. Each component corresponds to a node \eqn{x} of the graph (i.e. child node)
#' and its vector is the set of its parents (the root node is not included).
#' @export
build.parents <- function(g, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("root is not the right root node of g. Use the function root.node(g) to find the root node of g");
    nd <- nodes(g)
    ndL <- vector(mode="list", length=length(nd));
    names(ndL) <- nd;
    ed <- edges(g);
    for(i in 1:length(ed)){
        children <- ed[[i]];
        parent   <- names(ed[i]);
        if(length(children)!=0){
            for(j in 1:length(children))
                ndL[[children[j]]] <- c(ndL[[children[j]]],parent);
        }
    }
    ndL <- ndL[-which(names(ndL)==root)];
    return(ndL);
}

#' @rdname build.parents
#' @return \code{build.parents.top.down} returns a named list of character vectors. Each component corresponds to a node \eqn{x} of the graph (i.e. child node)
#' and its vector is the set of its parents. The order of nodes follows the levels of the graph from root (excluded) to leaves.
#' @export
build.parents.top.down <- function(g,levels, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("root is not the right root node of g. Use the function root.node(g) to find the root node of g");
    ord.nd <- unlist(levels);
    ndL <- vector(mode="list", length=length(ord.nd));
    names(ndL) <- ord.nd;
    ed <- edges(g);
    for(i in 1:length(ed)){
        children <- ed[[i]];
        parent   <- names(ed[i]);
        if(length(children)!=0){
            for(j in 1:length(children))
                ndL[[children[j]]] <- c(ndL[[children[j]]],parent);
        }
    }
    ndL <- ndL[-which(names(ndL)==root)];
    return(ndL);
}

#' @rdname build.parents
#' @return \code{build.parents.bottom.up} returns a named list of character vectors. Each component corresponds to a node \eqn{x} of the
#' graph (i.e. child node) and its vector is the set of its parents. The nodes are ordered from leaves to root (excluded).
#' @export
build.parents.bottom.up <- function(g,levels, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("root is not the right root node of g. Use the function root.node(g) to find the root node of g");
    flip.ord.nd <- rev(unlist(levels));
    ndL <- vector(mode="list", length=length(flip.ord.nd));
    names(ndL) <- flip.ord.nd;
    ed <- edges(g);
    for(i in 1:length(ed)){
        children <- ed[[i]];
        parent   <- names(ed[i]);
        if(length(children)!=0){
            for(j in 1:length(children))
                ndL[[children[j]]] <- c(ndL[[children[j]]],parent);
        }
    }
    ndL <- ndL[-which(names(ndL)==root)];
    return(ndL);
}

#' @rdname build.parents
#' @return \code{build.parents.topological.sorting} a named list of character vectors. Each component corresponds to a node \eqn{x} of the graph (i.e. child node)
#' and its vector is the set of its parents. The nodes are ordered according to a topological sorting, i.e. parents node come before children node.
#' @export
build.parents.topological.sorting <- function(g, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("root is not the right root node of g. Use the function root.node(g) to find the root node of g");
    ord.nd <- tsort(g);
    ndL <- vector(mode="list", length=length(ord.nd));
    names(ndL) <- ord.nd;
    ed <- edges(g);
    for(i in 1:length(ed)){
        children <- ed[[i]];
        parent   <- names(ed[i]);
        if(length(children)!=0){
            for(j in 1:length(children))
                ndL[[children[j]]] <- c(ndL[[children[j]]],parent);
        }
    }
    ndL <- ndL[-which(names(ndL)==root)];
    return(ndL);
}

#' @name build.descendants
#' @title Build descendants
#' @description Build descendants for each node of a graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param levels a list of character vectors. Each component represents a graph level and the elements of any component correspond to nodes.
#' The level 0 coincides with the root node.
#' @examples
#' data(graph);
#' root <- root.node(g);
#' desc <- build.descendants(g);
#' lev <- graph.levels(g, root=root);
#' desc.tod <- build.descendants.per.level(g,lev);
#' desc.bup <- build.descendants.bottom.up(g,lev);
#' @rdname build.descendants
#' @return \code{build.descendants} returns a named list of vectors. Each component corresponds to a node \eqn{x} of the graph, and its vector
#' is the set of its descendants including also \eqn{x}.
#' @export
build.descendants <- function(g){
    name.nodes <- nodes(g);
    g2 <- transitive.closure(g);
    desc <- edges(g2);
    for(x in name.nodes)
        desc[[x]] <- c(desc[[x]],x);
    return(desc);
}

#' @rdname build.descendants
#' @return \code{build.descendants.per.level} returns a named list of vectors.
#' Each component corresponds to a node \eqn{x} of the graph and its vector is the set of its descendants including also \eqn{x}.
#' The nodes are ordered from root (included) to leaves.
#' @export
build.descendants.per.level <- function(g,levels){
    ord.nd <- unlist(levels);
    g2 <- transitive.closure(g);
    desc <- edges(g2)[ord.nd];
    for(x in ord.nd)
        desc[[x]] <- c(desc[[x]],x);
    return(desc);
}

#' @rdname build.descendants
#' @return \code{build.descendants.bottom.up} returns a named list of vectors. Each component corresponds to a node \eqn{x} of
#' the graph and its vector is the set of its descendants including also \eqn{x}. The nodes are ordered from leaves to root (included).
#' @export
build.descendants.bottom.up <- function(g,levels) {
    flip.ord.nd <- rev(unlist(levels));
    g2 <- transitive.closure(g);
    desc <- edges(g2)[flip.ord.nd];
    for(x in flip.ord.nd)
        desc[[x]] <- c(desc[[x]],x);
    return(desc);
}

#' @name build.children
#' @title Build children
#' @description Build children for each node of a graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param levels a list of character vectors. Each component represents a graph level and the elements of any
#' component correspond to nodes. The level 0 coincides with the root node.
#' @examples
#' data(graph);
#' root <- root.node(g);
#' children <- build.children(g);
#' lev <- graph.levels(g, root=root);
#' children.tod <- build.children.top.down(g,lev);
#' children.bup <- build.children.bottom.up(g,lev);
#' @rdname build.children
#' @return \code{build.children} returns a named list of vectors. Each component corresponds to a node \eqn{x} of the graph and its vector
#' is the set of its children.
#' @export
build.children <- function(g){
    return(edges(g));
}

#' @rdname build.children
#' @return \code{build.children.top.down} returns a named list of character vectors. Each component corresponds to a node \eqn{x}
#' of the graph (i.e. parent node) and its vector is the set of its children. The nodes are ordered from root (included) to leaves.
#' @export
build.children.top.down <- function(g,levels){
    child <- build.children(g)
    nd <- c();
    for(i in 1:length(levels)){
        level.nodes <- levels[[i]];
        nd <- append(nd,child[level.nodes]);
    }
    return(nd);
}

#' @rdname build.children
#' @return \code{build.children.bottom.up} returns a named list of character vectors. Each component corresponds to a node \eqn{x}
#' of the graph (i.e. parent node) and its vector is the set of its children. The nodes are ordered from leaves (included) to root.
#' @export
build.children.bottom.up <- function(g,levels){
    flip.ord.nd <- rev(unlist(levels));
    ed <- edges(g);
    nd <- ed[flip.ord.nd];
    return(nd);
}

#' @name build.ancestors
#' @title Build ancestors
#' @description Build ancestors for each node of a graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param levels a list of character vectors. Each component represents a graph level and the elements of any component correspond to nodes.
#' The level 0 coincides with the root node.
#' @examples
#' data(graph);
#' root <- root.node(g);
#' anc <- build.ancestors(g);
#' lev <- graph.levels(g, root=root);
#' anc.tod <-build.ancestors.per.level(g,lev);
#' anc.bup <- build.ancestors.bottom.up(g,lev);
#' @rdname build.ancestors
#' @return \code{build.ancestos} returns a named list of vectors. Each component corresponds to a node \eqn{x} of the graph and its vector
#' is the set of its ancestors including also \eqn{x}.
#' @export
build.ancestors <- function(g){
    og <- compute.flipped.graph(g);
    names.nodes <- nodes(og);
    og2 <- transitive.closure(og);
    anc <- edges(og2);
    for(x in names.nodes)
        anc[[x]] <- c(anc[[x]],x);
    return(anc);
}

#' @rdname build.ancestors
#' @return \code{build.ancestors.per.level} returns a named list of vectors. Each component corresponds to a node \eqn{x}
#' of the graph and its vector is the set of its ancestors including also \eqn{x}. The nodes are ordered from root (included) to leaves.
#' @export
build.ancestors.per.level <- function(g,levels){
    og <- compute.flipped.graph(g);
    ord.nd <- unlist(levels);
    og2 <- transitive.closure(og);
    anc <- edges(og2)[ord.nd];
    for(x in ord.nd)
        anc[[x]] <- c(anc[[x]],x);
    return(anc);
}

#' @rdname build.ancestors
#' @return \code{build.ancestors.bottom.up} a named list of vectors. Each component corresponds to a node \eqn{x} of the
#' graph and its vector is the set of its ancestors including also \eqn{x}. The nodes are ordered from leaves to root (included).
#' @export
build.ancestors.bottom.up <- function(g,levels){
    og <- compute.flipped.graph(g);
    flip.ord.nd <- rev(unlist(levels));
    og2 <- transitive.closure(og);
    anc <- edges(og2)[flip.ord.nd];
    for(x in flip.ord.nd)
        anc[[x]] <- c(anc[[x]],x);
    return(anc);
}

#' @title Root node
#' @description Find the root node of a directed graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @return Name of the root node.
#' @export
#' @examples
#' data(graph);
#' root <- root.node(g);
root.node <- function(g){
    d <- degree(g);
    root <- names(which(d$inDegree==0));
    return(root);
}

#' @title Leaves
#' @description Find leaves of a directed graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @return A vector with the names of the leaves of g.
#' @export
#' @examples
#' data(graph);
#' leaves <- find.leaves(g);
find.leaves <- function(g){
    d <- degree(g);
    leaves <- names(which(d$outDegree==0));
    return(leaves);
}

#' @title Distances from leaves
#' @description Compute the minimum distance of each node from one of the leaves of the graph.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @return A named vector. The names are the names of the nodes of the graph \code{g}, and their values represent the distance from the leaves.
#' A value equal to 0 is assigned to the leaves, 1 to nodes with distance 1 from a leaf and so on.
#' @export
#' @examples
#' data(graph);
#' dist.leaves <- distances.from.leaves(g);
distances.from.leaves <- function(g){
    leaves <- find.leaves(g);
    n.leaves <- length(leaves);
    og <- compute.flipped.graph(g);
    og <- addNode("root", og);
    og <- addEdge(rep("root",n.leaves), leaves, og, rep(1,n.leaves));
    dist <- acc(og,"root")[[1]]-1;
    return(dist);
}

#' @title Constraints matrix
#' @description Return a matrix with two columns and as many rows as there are edges. The entries of the first columns are the index of the node the edge
#' comes from (i.e. children nodes), the entries of the second columns indicate the index of node the edge is to (i.e. parents nodes).
#' Referring to a dag this matrix defines a partial order.
#' @param g a graph of class \code{graphNELL}. It represents the hierarchy of the classes.
#' @return A constraints matrix w.r.t the graph \code{g}.
#' @export
#' @examples
#' data(graph);
#' m <- constraints.matrix(g);
constraints.matrix <- function(g){
    eM <- edgeMatrix(g);
    eM <- cbind(eM[2,],eM[1,]);
    nd <- nodes(g);
    dimnames(eM) <- list(nd[eM[,2]], c("child","parent"))
    return(eM);
}

#' @title Build subgraph
#' @description Build a subgraph with only the supplied nodes and any edges between them.
#' @param nd a vector with the nodes for which the subgraph must be built.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param edgemode can be "directed" or "undirected".
#' @return A subgraph with only the supplied nodes.
#' @export
#' @examples
#' data(graph);
#' anc <- build.ancestors(g);
#' nd <- anc[["HP:0001371"]];
#' subg <- build.subgraph(nd, g, edgemode="directed");
build.subgraph <- function(nd, g, edgemode="directed"){
    ed <- edges(g);
    ed.sel <- ed[nd];
    ndL <- vector(mode="list", length=length(ed.sel));
    names(ndL) <- names(ed.sel);
    for(i in 1:length(ed.sel)){
        parent   <- names(ed.sel[i]);
        children <- ed.sel[[i]];
        if(length(children!=0)){
            children.map <- children[children %in% nd]
            ndL[[i]] <- append(ndL[[i]],children.map);
        }
    }
    for (i in 1:length(ndL))
        ndL[[i]] <- list(edges=ndL[[i]]);
    G <- graphNEL(nodes=nd, edgeL=ndL, edgemode=edgemode);
    return(G);
}

#' @title DAG checker
#' @description Check the integrity of a dag.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that is on the top-level of the hierarchy (\code{def. root="00"}).
#' @return If all the nodes are accessible from the root "dag is ok" is printed, otherwise a message error and the list of
#' the not accessible nodes is printed on the stdout.
#' @export
#' @examples
#' data(graph);
#' root <- root.node(g);
#' check.dag.integrity(g, root=root);
check.dag.integrity <- function(g, root="00"){
    if(sum(nodes(g) %in% root)==0)
        stop("root node not found in g. Insert the root node");
    if(root.node(g)!=root)
        stop("the supplied root node is not the right root node of g. Use the function root.node(g) to find the root node of g");
  all.nodes <- nodes(g);
  acc.nodes <- names(acc(g,root)[[1]]);
  if((length(all.nodes) - length(acc.nodes)) > 1) {
        n <- setdiff(all.nodes,c(acc.nodes,root));
        cat("not all nodes accessible from root\n");
        cat("nodes not accessible from root:\n");
        cat(n,"\n");
    }else{
        cat("dag is ok\n")
    };
}

#' @name hierarchical.checkers
#' @title Hierarchical constraints checker
#' @description Check if the true path rule is violated or not. In other words this function checks if the score of a parent or an ancestor node is
#' always larger or equal than that of its children or descendants nodes.
#' @param y.hier vector of scores relative to a single example. It must be a named numeric vector (names are functional classes).
#' @param S.hier the matrix with the scores of the classes corrected in according to hierarchy. It must be a named matrix: rows are examples and
#' columns are functional classes.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that is on the top-level of the hierarchy (\code{def. root="00"}).
#' @return A list of 3 elements:
#' \itemize{
#'  \item status:
#'   \itemize{
#'    \item \code{OK} if none hierarchical constraints have bee broken;
#'    \item \code{NOTOK} if there is at least one hierarchical constraints broken;
#'   }
#'  \item hierarchy_constraints_broken:
#'   \itemize{
#'    \item TRUE: example did not respect the hierarchical constraints;
#'    \item FALSE: example broke the hierarchical constraints;
#'   }
#'  \item hierarchy_constraints_satisfied: how many terms satisfied the hierarchical constraint;
#' }
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' root <- root.node(g);
#' S.hier <- htd(S,g,root);
#' S.hier.single.example <- S.hier[sample(ncol(S.hier),1),];
#' check.hierarchy.single.sample(S.hier.single.example, g, root=root);
#' check.hierarchy(S.hier, g, root);
check.hierarchy.single.sample <- function(y.hier,g, root="00"){
    if(!(root %in% names(y.hier))){
        max.score <- max(y.hier);
        y.hier <- c(max.score,y.hier);
        names(y.hier)[1] <- root;
    }
    par <- build.parents(g,root);
    v <- c()
    for(i in 1:length(par)){
        child <- y.hier[names(par[i])];
        parents <- y.hier[par[[i]]]
        x <- parents >= child
        y <- any(x==0)
        v <- append(v,y)
    }
    names(v) <- names(par)
    violated <- any(v==TRUE);
    if(violated)
        status = "NOTOK"
    else
        status = "OK";
    h <- as.factor(v);
    k <- summary(h);
    l <- list(status=status, hierarchy.constraints.broken=v, hierarchy.constraints.satisfied=k);
    return(l);
}

#' @rdname hierarchical.checkers
#' @export
check.hierarchy <- function(S.hier,g, root="00"){
    if(!(root %in% colnames(S.hier))){
        max.score <- max(S.hier);
        z <- rep(max.score,nrow(S.hier));
        S.hier <- cbind(z,S.hier);
        colnames(S.hier)[1] <- root;
    }
    par <- build.parents(g,root);
    v <- c()
    for(i in 1:length(par)){
        child <- S.hier[,names(par[i])];
        parents <- S.hier[,par[[i]]]
        x <- parents >= child
        y <- any(x==0)
        v <- append(v,y)
    }
    names(v) <- names(par)
    violated <- any(v==TRUE);
    if(violated)
        status = "NOTOK"
    else
        status = "OK";
    h <- as.factor(v);
    k <- summary(h);
    l <- list(status=status, hierarchy.constraints.broken=v, hierarchy.constraints.satisfied=k);
    return(l);
}

#' @title Lexicographical topological sorting
#' @description Nodes of a graph are sorted according to a lexicographical topological ordering.
#' @details A topological sorting is a linear ordering of the nodes such that given an edge from \code{u} to \code{v}, the node \code{u} comes before
#' node \code{v} in the ordering. Topological sorting is not possible if the graph \code{g} contains self-loop.
#' To implement the topological sorting algorithm we applied the Kahn’s algorithm.
#' @param g an object of class \code{graphNEL}.
#' @return A vector in which the nodes of the graph \code{g} are sorted according to a lexicographical topological order.
#' @export
#' @examples
#' data(graph);
#' T <- lexicographical.topological.sort(g);
lexicographical.topological.sort <- function(g){
    ## check self-loop: graph with self-loop cannot be processed
    indegree <- degree(g)$inDegree;
    if(!(any(indegree==0))){
        stop("input graph g contains self-loop"); ## self-loop detect
    }
    T <- c();
    indegree <- degree(g)$inDegree;
    while(length(indegree)!=0){
        queue <- names(which(indegree==0));
        if(length(queue)==0)
            stop("input graph g contains self-loop"); ## check self-loop
        queue <- queue[order(queue, decreasing=FALSE)];
        T <- append(T, queue[1]);
        indegree <- indegree[-which(names(indegree)==queue[1])];
        processed <- adj(g, queue)[[1]];
        s <- indegree[processed] - 1;
        indegree[processed] <- s;
    }
    return(T);
}

#' @title Build consistent graph
#' @description Build a graph in which all nodes are reachable from root.
#' @details All nodes not accessible from root (if any) are removed from the graph and printed on stdout.
#' @param g an object of class \code{graphNEL}.
#' @param root name of the class that is on the top-level of the hierarchy (\code{def. root="00"}).
#' @return A graph (as an object of class \code{graphNEL}) in which all nodes are accessible from root.
#' @export
#' @examples
#' data(graph);
#' root <- root.node(g);
#' G <- graph::addNode(c("X","Y","Z"), g);
#' G <- graph::addEdge(c("X","Y","Z"), c("HP:0011844","HP:0009810","HP:0012385"), G);
#' G <- build.consistent.graph(G, root=root);
build.consistent.graph <- function(g=g, root="00"){
    nd <- nodes(g);
    if(length(root.node(g))>1){
        dk.sp <- dijkstra.sp(g, start=root)$distance;
        nd <- nd[which(dk.sp!=Inf)];
        ndinc <- names(dk.sp[which(dk.sp==Inf)]);
        cat("removed nodes not accessible from root:", paste(1:length(ndinc), "\t", ndinc), sep="\n");
    }
    g <- build.subgraph(nd, g);
    return(g);
}

#' @title Weighted adjacency matrix
#' @description Build a symmetric weighted adjacency matrix (wadj matrix) of a graph.
#' @param file name of the plain text file to be read (\code{def. edges}). The format of the file is a sequence of rows.
#' Each row corresponds to an edge represented through a pair of vertexes (blank separated) and the weight of the edges.
#' For instance: \code{nodeX nodeY score}. The file extension can be plain (".txt") or compressed (".gz").
#' @return A named symmetric weighted adjacency matrix of the graph.
#' @export
#' @examples
#' edges <- system.file("extdata/edges.txt.gz", package="HEMDAG");
#' W <- weighted.adjacency.matrix(file=edges);
weighted.adjacency.matrix <- function(file="edges.txt"){
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        m <- read.table(gzfile(file), colClasses="character", stringsAsFactors=FALSE);
    }else{
        m <- as.matrix(read.table(file, colClasses="character", stringsAsFactors=FALSE));
    }
    nodesname <- as.vector(as.matrix((m[,1:2])));
    charcheck <- any(suppressWarnings(is.na(as.numeric(nodesname))));
    if(charcheck){
        nodes <- sort(unique(as.vector(as.matrix(m[,1:2])))); ##NB: df must be converted as matrix to make as.vector working..
    }else{
        nodes <- as.character(sort(as.numeric(unique(as.vector(m[,1:2])))));
    }
    n.nodes <- length(nodes);
    # building the adjacency matrix
    W <- matrix(0, nrow=n.nodes, ncol=n.nodes);
    dimnames(W) <- list(nodes,nodes);
    W[cbind(m[,1], m[,2])] <- as.numeric(m[,3]);
    W[cbind(m[,2], m[,1])] <- as.numeric(m[,3]);
    return(W);
}

#' @title Tupla matrix
#' @description Transform a named score matrix in a tupla, i.e. in the form \code{nodeX nodeY score}.
#' @details Only the \emph{non-zero} interactions are kept, while the \emph{zero} interactions are discarded.
#' @param m a named score matrix. It can be either a \code{m x n} matrix (where \code{m} are example and \code{n}
#' are functional terms, e.g. GO terms) or it can be a square named matrix \code{m x m}, where \code{m} are examples.
#' @param output.file name of the file on which the matrix must be written.
#' @param digits number of digits to be used to save scores of \code{m} (\code{def. digits=3}).
#' The extension of the file can be plain (".txt") or compressed (".gz").
#' @return A tupla score matrix stored in output.file.
#' @export
#' @examples
#' data(wadj);
#' file <- tempfile();
#' tupla.matrix(W, output.file=file, digits=3);
tupla.matrix <- function(m, output.file="net.file.gz", digits=3){
    im <- which(m!=0, arr.ind=TRUE);
    rows <- rownames(im);
    ## degenerate case when m is symmetric and some interactions are zero
    if(isSymmetric(m) && any(rowSums(m)==0)){
        colrep.names <- intersect(colnames(m), rows);
    }else{
        colrep.names <- colnames(m);
    }
    colrep.times <- table(im[,2]);
    cols <- rep(colrep.names, times=colrep.times);
    df <- data.frame(row=rows, col=cols, score=m[im]);
    tmp <- strsplit(output.file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        write.table(format(df,digits=digits), file=gzfile(output.file), quote=FALSE, row.names=FALSE, col.names=FALSE);
    }else{
        write.table(format(df,digits=digits), file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE);
    }
}

#' @name build.scores.matrix
#' @title Build scores matrix
#' @description Build a scores matrix from file
#' @param file name of the text file to be read. The matrix of the input file can be either a list (e.g in the form \code{example nodeX|score}),
#' or a tupla (i.e. in the form \code{example nodeX score}).The file extension can be plain (".txt") or compressed (".gz").
#' @param split character vector containing a regular expression use for splitting.
#' @return A named scores matrix.
#' @export
#' @examples
#' file.list  <- system.file("extdata/scores.list.txt.gz", package="HEMDAG");
#' file.tupla <- system.file("extdata/scores.tupla.txt.gz", package="HEMDAG");
#' S <- build.scores.matrix.from.list(file.list, split="[(\t,|)]");
#' S <- build.scores.matrix.from.tupla(file.tupla);
build.scores.matrix.from.list <- function(file="scores.list.txt", split="[(\t,|)]"){ ## split=[(,=)]
    ## read scores file and build a matrix with 3 columns: gene/annotations/scores
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        con <- gzfile(file);
        line <- readLines(con);
        close(con);
    }else{
        line <- readLines(file);
    }
    tmp  <- strsplit(line, split=split, perl=TRUE);
    gene.names <- sapply(tmp, `[[`, 1);
    ## scores and ontology terms list
    scores.list <- ann.list <- vector(mode="list", length=length(gene.names));
    names(scores.list) <- gene.names;
    names(ann.list) <- gene.names;
    for(i in 1:length(tmp)){
        x <- suppressWarnings(as.numeric(tmp[[i]][-1])); ## NA by coercion -> first element is the gene/protein name
        y <- x[!is.na(x=x)];
        scores.list[[i]] <- y;
        z <- is.na(x);
        ann.list[[i]] <- tmp[[i]][-1][z]
    }
    num.edges <-  length(unlist(ann.list)); ## check: num.score <- length(unlist(scores.list)); num.edges==num.score; ## TRUE
    num.v <- length(ann.list);
    m <- matrix(character(num.edges*3), ncol=3);
    count <- 0;
    node1 <- names(ann.list);
    for (i in 1:num.v) {
        x <- ann.list[[i]];
        y <- scores.list[[i]]
        len.x <- length(x);
        if (len.x!=0){
            for (j in 1:len.x) {
                count <- count + 1;
                m[count,] <- c(node1[i], x[j], y[j]);
            }
        }
    }
    ## build the hierarchical scores matrix in R
    samplename <- m[,1];
    charcheck <- any(suppressWarnings(is.na(as.numeric(samplename))));
    if(charcheck){
        genes <- sort(unique(samplename));
    }else{
        genes <- as.character(sort(as.numeric(unique(samplename))));
    }
    ngene <- length(genes);
    feat  <- sort(unique(m[,2]));
    nfeat <- length(feat);
    ## return scores matrix
    S <- matrix(0, nrow=ngene, ncol=nfeat);
    dimnames(S) <- list(genes, feat);
    S[cbind(m[,1], m[,2])] <- as.numeric(m[,3]);
    return(S);
}

#' @rdname build.scores.matrix
#' @export
build.scores.matrix.from.tupla <- function(file="scores.tupla.txt"){
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        m <- read.table(gzfile(file), colClasses="character", stringsAsFactors=FALSE);
    }else{
        m <- as.matrix(read.table(file, colClasses="character", stringsAsFactors=FALSE));
    }
    prsname <- as.vector(as.matrix((m[,1])));
    charcheck <- any(suppressWarnings(is.na(as.numeric(prsname))));
    if(charcheck){
        prs <- sort(unique(as.vector(as.matrix(m[,1])))); ##NB:df must be converted as matrix to make as.vector working..
    }else{
        prs <- as.character(sort(as.numeric(unique(as.vector(m[,1])))));
    }
    terms <- sort(unique(as.vector(as.matrix(m[,2]))));
    n.prs <- length(prs);
    n.obo <- length(terms);
    # building scores matrix
    S <- matrix(0, nrow=n.prs, ncol=n.obo);
    dimnames(S) <- list(prs,terms);
    S[cbind(m[,1], m[,2])] <- as.numeric(m[,3]);
    return(S);
}


#' @title Parse an HPO obo file
#' @description Read an HPO obo file (\href{http://human-phenotype-ontology.github.io/}{HPO}) and write the edges of the dag on a plain text file.
#' The format of the file is a sequence of rows and each row corresponds to an edge represented through a pair of vertexes separated by blank.
#' @details A faster and more flexible parser to handle \emph{obo} file can be found \href{https://github.com/marconotaro/obogaf-parser}{here}.
#' @param obofile an HPO obo file. The extension of the obofile can be plain (".txt") or compressed (".gz").
#' @param file name of the file of the edges to be written. The extension of the file can be plain (".txt") or compressed (".gz").
#' @return A text file representing the edges in the format: source  destination (i.e. one row for each edge).
#' @export
#' @examples
#' \dontrun{
#' hpobo <- "http://purl.obolibrary.org/obo/hp.obo";
#' build.edges.from.hpo.obo(obofile=hpobo, file="hp.edge");}
build.edges.from.hpo.obo <- function(obofile="hp.obo", file="edge.file"){
    tmp <- strsplit(obofile, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        con <- gzfile(obofile);
        line <- readLines(con);
        close(con);
    }else{
        line <- readLines(obofile);
    }
    n.lines <- length(line);
    m <- matrix(character(1000000*2), ncol=2);
    colnames(m) <- c("source", "destination");
    i <- 1;
    j <- 0; # number of edges;
    while(i<=n.lines){
        while((i<=n.lines) && (line[i]!="[Term]")){
            i <- i + 1;
        }
        if(i>=n.lines){break();}
        i <- i + 1; # id
        destination <- strsplit(line[i], split="[ ]")[[1]][2];
        while( (line[i]!="") && (strsplit(line[i], split="[ ]")[[1]][1]!="is_a:") ){ # checking first is_a entry
            i <- i + 1;
        }
        if (line[i] == ""){next();}  # we are at the end of the record and is_a has been found
        source <- strsplit(line[i], split="[ ]")[[1]][2];
        j <- j + 1;
        i <- i + 1;
        m[j,]<-c(source,destination);
        while( (line[i]!="") && (strsplit(line[i], split="[ ]")[[1]][1]=="is_a:") ){# checking successive is_a entry
            source <- strsplit(line[i], split="[ ]")[[1]][2];
            i <- i + 1;
            j <- j + 1;
            m[j,]<-c(source,destination);
        }
    }
    m <- m[1:j,];
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        write.table(m, file=gzfile(file), quote=FALSE, row.names=FALSE, col.names=FALSE);
    }else{
        write.table(m, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE);
    }
}

#' @title Write a directed graph on file
#' @description Read an object of class \code{graphNEL} and write the graph as sequence of rows on a plain text file.
#' @param g a graph of class \code{graphNEL}.
#' @param file name of the file to be written. The extension of the file can be plain (".txt") or compressed (".gz").
#' @return A plain text file representing the graph. Each row corresponds to an edge represented through a pair of vertexes separated by blank.
#' @export
#' @examples
#' data(graph);
#' file <- tempfile();
#' write.graph(g, file=file);
write.graph <- function(g, file="graph.txt.gz"){
    num.edges <- length(unlist(edges(g)));
    num.v <- numNodes(g);
    m <- matrix(character(num.edges*2), ncol=2);
    res <- edges(g);
    count <- 0;
    node1 <- names(res);
    for (i in 1:num.v) {
        x <- res[[i]];
    len.x <- length(x);
    if (len.x!=0)
        for (j in 1:len.x) {
            count <- count + 1;
            m[count,] <- c(node1[i],x[j]);
        }
    }
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        write.table(m, file=gzfile(file), quote=FALSE, row.names=FALSE, col.names=FALSE);
    }else{
        write.table(m, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE);
    }
}

#' @title Read a directed graph from a file
#' @description Read a directed graph from a file and build a \code{graphNEL} object.
#' @param file name of the file to be read. The format of the file is a sequence of rows and each row corresponds to an edge represented
#' through a pair of vertexes separated by blanks. The extension of the file can be plain (".txt") or compressed (".gz").
#' @return An object of class \code{graphNEL}.
#' @export
#' @examples
#' ed <- system.file("extdata/graph.edges.txt.gz", package= "HEMDAG");
#' g <- read.graph(file=ed);
read.graph <- function(file="graph.txt.gz"){
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        m <- as.matrix(read.table(gzfile(file), colClasses="character"));
    }else{
        m <- as.matrix(read.table(file, colClasses="character"));
    }
    thenodes<-sort(unique(as.vector(m))); # nodes
    n.nodes <- length(thenodes);
    n.edges <- nrow(m);
    # building the graph
    edL <- vector("list", length=n.nodes);
    names(edL) <- thenodes;
    for(i in 1:n.nodes)
        edL[[i]]<-list(edges=NULL);
    g <- graphNEL(nodes=thenodes, edgeL=edL, edgemode="directed");
    g <- addEdge(m[1:n.edges,1], m[1:n.edges,2], g, rep(1,n.edges));
    return(g);
}

#' @title Read an undirected graph from a file
#' @description Read a graph from a file and build a \code{graphNEL} object. The format of the input file is a sequence of rows.
#' Each row corresponds to an edge represented through a pair of vertexes (blank separated) and the weight of the edge.
#' @param file name of the file to be read. The extension of the file can be plain (".txt") or compressed (".gz").
#' @return A graph of class \code{graphNEL}.
#' @export
#' @examples
#' edges <- system.file("extdata/edges.txt.gz", package="HEMDAG");
#' g <- read.undirected.graph(file=edges);
read.undirected.graph <- function(file="graph.txt.gz"){
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        m <- as.matrix(read.table(gzfile(file), colClasses="character"));
    }else{
        m <- as.matrix(read.table(file, colClasses="character"));
    }
    thenodes<-sort(unique(as.vector(m[,1:2]))); # nodes
    n.nodes <- length(thenodes);
    n.edges <- nrow(m);
    # building the graph
    edL <- vector("list", length=n.nodes);
    names(edL) <- thenodes;
    for(i in 1:n.nodes)
        edL[[i]]<-list(edges=NULL);
    g <- graphNEL(nodes=thenodes, edgeL=edL, edgemode="undirected");
    g <- addEdge(m[1:n.edges,1], m[1:n.edges,2], g, as.numeric(m[1:n.edges,3]));
    return(g);
}
