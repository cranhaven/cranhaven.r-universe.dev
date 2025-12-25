##########
## GPAV ##
##########

#' @title Binary upper triangular adjacency matrix
#' @description Compute a binary square upper triangular matrix where rows and columns correspond to the nodes' name of the graph \code{g}.
#' @details The nodes of the matrix are topologically sorted (by using the \code{tsort} function of the \pkg{RBGL} package).
#' Let's denote with \code{adj} our adjacency matrix. Then \code{adj} represents a partial order data set in which the class \code{j}
#' dominates the class \code{i}. In other words, \code{adj[i,j]=1} means that \code{j} dominates \code{i}; \code{adj[i,j]=0} means that there
#' is no edge between the class \code{i} and the class \code{j}. Moreover the nodes of \code{adj} are ordered such that \code{adj[i,j]=1}
#' implies \eqn{i < j}, i.e. \code{adj} is upper triangular.
#' @param g a graph of class \code{graphNELL} representing the hierarchy of the class.
#' @return An adjacency matrix which is square, logical and upper triangular.
#' @export
#' @examples
#' data(graph);
#' adj <- adj.upper.tri(g);
adj.upper.tri <- function(g){
    ## 0. write the graph g as pair parent-child (source- destination)
    num.ed <- numEdges(g);
    num.nd <- numNodes(g);
    m <- matrix(character(num.ed*2), ncol=2);
    ed <- edges(g);
    count <- 0;
    par <- names(ed);
    for(i in 1:num.nd){
        children <- ed[[i]];
        len.x <- length(children);
        if(len.x!=0){
            for(j in 1:len.x){
                count <- count + 1;
                m[count,] <- c(par[i], children[j]);
            }
        }
    }
    ## 1. topological sorting: nodes are ordering in a linear way such as vertex u comes before v.
    tsort.nd <- tsort(g);
    ## 2. map each node of the graph to a integer number (in a decreasing order)
    source <- mapvalues(m[,1], from=tsort.nd[1:length(tsort.nd)], to=length(tsort.nd):1, warn_missing=F);
    destination  <- mapvalues(m[,2], from=tsort.nd[1:length(tsort.nd)], to=length(tsort.nd):1, warn_missing=F);
    ## 3. build upper triangular logical adjacency constraints matrix. this matrix should be sparse
    eM <- cbind(from=as.numeric(destination), to=as.numeric(source));
    adj <- matrix(0, nrow=num.nd, ncol=num.nd);
    rev.nd <- tsort.nd[num.nd:1];  ## reverse topological sorting for a right mapping with number
    dimnames(adj) <- list(rev.nd, rev.nd);
    adj[eM] <- 1;
    return(adj);
}

#' @title Generalized Pool-Adjacent Violators (GPAV)
#' @description Implementation of \code{GPAV} (Generalized Pool-Adjacent Violators) algorithm.
#' (\cite{Burdakov et al., In: Di Pillo G, Roma M, editors. An O(n2) Algorithm for Isotonic Regression. Boston, MA: Springer US; 2006.
#' p. 25–33. Available from: \doi{10.1007/0-387-30065-1_3}}
#' @details Given the constraints adjacency matrix of the graph, a vector of scores \eqn{\hat{y} \in R^n} and a vector of strictly positive
#' weights \eqn{w \in R^n}, the \code{GPAV} algorithm returns a vector \eqn{\bar{y}} which is as close as possible, in the least-squares sense,
#' to the response vector \eqn{\hat{y}} and whose components are partially ordered in accordance with the constraints matrix \code{adj}.
#' In other words, \code{GPAV} solves the following problem:
#' \deqn{
#'  \bar{y} = \left\{
#'   \begin{array}{l}
#'    \min \sum_{i \in V} (\hat{y}_i - \bar{y}_i )^2\\\\
#'    \forall i, \quad  j \in par(i) \Rightarrow  \bar{y}_j  \geq \bar{y}_i
#'   \end{array}
#' \right.
#'}
#' where \eqn{V} are the number of vertexes of the graph.
#' @param Y vector of scores relative to a single example. \code{Y} must be a numeric named vector, where names
#' correspond to classes' names, i.e. nodes of the graph \code{g} (root node included).
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that
#' \code{W} is a unitary vector of the same length of the columns' number of the matrix \code{S} (root node included).
#' @param adj adjacency matrix of the graph which must be sparse, logical and upper triangular. Number of columns of \code{adj} must be
#' equal to the length of \code{Y} and \code{W}.
#' @return A list of 3 elements:
#' \itemize{
#'  \item \code{YFit}: a named vector with the scores of the classes corrected according to the \code{GPAV} algorithm.
#'  \item \code{blocks}: list of vectors, containing the partitioning of nodes (represented with an integer number) into blocks;
#'  \item \code{W}: vector of weights.
#' }
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' Y <- S[3,];
#' adj <- adj.upper.tri(g);
#' Y.gpav <- gpav(Y,W=NULL,adj);
gpav <- function(Y, W=NULL, adj){
    if(is.null(W))
        W <- rep(1,ncol(adj));
    nW <- length(W);
    nY <- length(Y);
    nadj <- ncol(adj);
    Ynames <- names(Y);
    if(nY!=nadj)
        stop("mismatch between the number of classes between Y and adj");
    if(nW!=nadj)
        stop("mismatch between the number of classes between Y and adj");
    ## sort nodes in the same order of adj matrix (i.e. in a topologically ordered)
    Y <- Y[colnames(adj)];
    ## just for clearnnes: we play with index and not with name...
    Y <- unname(Y);
    W <- unname(W);
    ## assign to each term an integer number, i.e. index's term
    N <- ncol(adj);
    corr <- 1:N;
    gpavres <- .C("gpav_cpp", as.double(W), as.double(adj), as.integer(N), as.double(Y), as.integer(corr));
    YFit <- gpavres[[4]];
    corr <- gpavres[[5]];
    YFit <- YFit[corr];
    names(YFit) <- colnames(adj);
    YFit <- YFit[Ynames];
    return(YFit);
}

#' @title GPAV over examples
#' @description Compute \code{GPAV} across all the examples.
#' @seealso \code{\link{gpav.parallel}}
#' @param S a named flat scores matrix with examples on rows and classes on columns (root node included).
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that
#' \code{W} is a unitary vector of the same length of the columns' number of the matrix \code{S} (root node included).
#' @return A named matrix with the scores of the classes corrected according to the \code{GPAV} algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' S.gpav <- gpav.over.examples(S,W=NULL,g);
gpav.over.examples <- function(S, g, W=NULL){
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    adj <- adj.upper.tri(g);
    M <- c();
    for(i in 1:nrow(S))
        M <- rbind(M, gpav(S[i,], W=W, adj));
    rownames(M) <- rownames(S);
    S <- M; rm(M);
    return(S);
}
#' @title GPAV over examples -- parallel implementation
#' @description Compute \code{GPAV} across all the examples (parallel implementation).
#' @param S a named flat scores matrix with examples on rows and classes on columns (root node included).
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that
#' \code{W} is a unitary vector of the same length of the columns' number of the matrix \code{S} (root node included).
#' @param ncores number of cores to use for parallel execution (\code{def. 8}).
#' If \code{ncores=0}, the maximum number of cores minus one are used.
#' @return A named matrix with the scores of the classes corrected according to the \code{GPAV} algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' if(Sys.info()['sysname']!="Windows"){
#'    S.gpav <- gpav.parallel(S,W=NULL,g,ncores=2);
#' }
gpav.parallel <- function(S, g, W=NULL, ncores=8){
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    prnames <- rownames(S);
    adj <- adj.upper.tri(g);
    if(ncores == 0){
        n.cores <- detectCores();
        if(n.cores > 3)
            ncores <- n.cores - 1;
    }
    registerDoParallel(cores=ncores);
    res.list <- foreach(i=1:nrow(S), .inorder=FALSE) %dopar% {
        res <- gpav(S[i,], W=W, adj);
        list(protein=prnames[i], scores=res);
    }
    prnames <- unlist(lapply(res.list, '[[', 1));
    hierscores <- lapply(res.list, '[[', 2);
    S <- do.call(rbind, hierscores);
    rownames(S) <- prnames;
    rm(res.list);
    return(S);
}

#' @title GPAV vanilla
#' @description Correct the computed scores in a hierarchy according to the \code{GPAV} algorithm.
#' @param S a named flat scores matrix with examples on rows and classes on columns (root node included).
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that
#' \code{W} is a unitary vector of the same length of the columns' number of the matrix \code{S} (root node included).
#' @param parallel a boolean value. Should the parallel version \code{GPAV} be run?
#' \itemize{
#'  \item \code{TRUE}: execute the parallel implementation of \code{GPAV} (\code{\link{gpav.parallel}});
#'  \item \code{FALSE} (\code{def.}): execute the sequential implementation of \code{GPAV} (\code{\link{gpav.over.examples}});
#' }
#' @param ncores number of cores to use for parallel execution. Set \code{ncores=1} if \code{parallel=FALSE},
#' otherwise set \code{ncores} to the desired number of cores.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A named matrix with the scores of the classes corrected according to the \code{GPAV} algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' S.gpav <- gpav.vanilla(S, g, W=NULL, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL);
gpav.vanilla <- function(S, g, W=NULL, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL){
    ## parameters check
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type are inconsistent");
    if(parallel==TRUE && ncores<2)
        warning("increase number of cores to exploit the gpav parallel version");
    if(parallel==FALSE && ncores>=2)
        warning("set parallel to TRUE to exploit the gpav parallel version");
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## check root scores before running gpav
    root <- root.node(g);
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score, nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## gpav correction
    if(parallel){
        S <- gpav.parallel(S, g, W=W, ncores=ncores);
    }else{
        S <- gpav.over.examples(S, g, W=W);
    }
    cat("gpav correction: done\n");
    return(S);
}

#' @title GPAV holdout
#' @description Correct the computed scores in a hierarchy according to the \code{GPAV} algorithm by applying a classical holdout procedure.
#' @param S a named flat scores matrix with examples on rows and classes on columns (root node included).
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param testIndex a vector of integer numbers corresponding to the indexes of the elements (rows) of the scores matrix \code{S} to be used in the test set.
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that
#' \code{W} is a unitary vector of the same length of the columns' number of the matrix \code{S} (root node included).
#' @param parallel a boolean value. Should the parallel version \code{GPAV} be run?
#' \itemize{
#'  \item \code{TRUE}: execute the parallel implementation of \code{GPAV} (\code{\link{gpav.parallel}});
#'  \item \code{FALSE} (\code{def.}): execute the sequential implementation of \code{GPAV} (\code{\link{gpav.over.examples}});
#' }
#' @param ncores number of cores to use for parallel execution. Set \code{ncores=1} if \code{parallel=FALSE},
#' otherwise set \code{ncores} to the desired number of cores.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A named matrix with the scores of the classes corrected according to the \code{GPAV} algorithm. Rows of the matrix are shrunk to \code{testIndex}.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(test.index);
#' S.gpav <- gpav.holdout(S, g, testIndex=test.index, norm=FALSE, norm.type=NULL);
gpav.holdout <- function(S, g, testIndex, W=NULL, parallel=FALSE, ncores=1, norm=TRUE, norm.type=NULL){
    ## parameters check
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type are inconsistent");
    if(parallel==TRUE && ncores<2)
        warning("increase number of cores to exploit the gpav parallel version");
    if(parallel==FALSE && ncores>=2)
        warning("set parallel to TRUE to exploit the gpav parallel version");
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## check root scores before running gpav
    root <- root.node(g);
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score, nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## shrink flat scores matrix to test test
    S <- S[testIndex,];
    ## gpav correction
    if(parallel){
        S <- gpav.parallel(S, g, W=W, ncores=ncores);
    }else{
        S <- gpav.over.examples(S, g, W=W);
    }
    cat("gpav correction: done\n");
    return(S);
}
