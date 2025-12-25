#############
## HTD-DAG ##
#############

#' @title HTD-DAG
#' @description Implementation of the top-down procedure to correct the scores of the hierarchy according to the constraints that the score of a
#' node cannot be greater than a score of its parents.
#' @details The \code{HTD-DAG} algorithm modifies the flat scores according to the hierarchy of a DAG \eqn{G} through a unique run across
#' the nodes of the graph. For a given example \eqn{x}, the flat predictions \eqn{f(x) = \hat{y}} are hierarchically corrected to
#' \eqn{\bar{y}}, by per-level visiting the nodes of the DAG from top to bottom according to the following simple rule:
#' \deqn{
#'  \bar{y}_i := \left\{
#'   \begin{array}{lll}
#'    \hat{y}_i  & {\rm if} \quad i \in root(G) \\
#'    \min_{j \in par(i)} \bar{y}_j & {\rm if} \quad \min_{j \in par(i)} \bar{y}_j < \hat{y}_i \\
#'    \hat{y}_i & {\rm otherwise}
#'   \end{array}
#'  \right.
#' }
#' The node levels correspond to their maximum path length from the root.
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that it is the top-level of the hierarchy (\code{def:00}).
#' @return A matrix with the scores of the classes corrected according to the \code{HTD-DAG} algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' root <- root.node(g);
#' S.htd <- htd(S,g,root);
htd <- function(S, g, root="00"){
    levels <- graph.levels(g,root);
    # a dummy root is added if it does not exist
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    # nodes are scanned from top to bottom: a list par.tod with the parents for each node (ordered from top to bottom) is obtained
    par.tod <- build.parents.top.down(g,levels,root)
    for(i in 1:length(par.tod)){
        child <- S[,names(par.tod[i])];
        parents <- as.matrix(S[,par.tod[[i]]]);
        # Note: the version with an apply and an ifelse statement is slower ...
        for(j in 1:length(child)){
            x <- min(parents[j,]);
            if(x < child[j])
                child[j] <- x;
        }
        S[,names(par.tod[i])] <- child;
    }
    return(S);
}

#' @title HTD-DAG vanilla
#' @description Correct the computed scores in a hierarchy according to the \code{HTD-DAG} algorithm.
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A matrix with the scores of the classes corrected according to the \code{HTD-DAG} algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' S.htd <- htd.vanilla(S, g, norm=FALSE, norm.type=NULL);
htd.vanilla <- function(S, g, norm=FALSE, norm.type=NULL){
    ## check
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type are inconsistent");
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## htd correction
    root <- root.node(g);
    S <- htd(S, g, root);
    cat("htd-dag correction: done\n");
    return(S);
}

#' @title HTD-DAG holdout
#' @description Correct the computed scores in a hierarchy according to the \code{HTD-DAG} algorithm applying a classical holdout procedure.
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param testIndex a vector of integer numbers corresponding to the indexes of the elements (rows) of the scores matrix \code{S} to be used in the test set.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A matrix with the scores of the classes corrected according to the \code{HTD-DAG} algorithm. Rows of the matrix are shrunk to \code{testIndex}.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(test.index);
#' S.htd <- htd.holdout(S, g, testIndex=test.index, norm=FALSE, norm.type=NULL);
htd.holdout <- function(S, g, testIndex, norm=FALSE, norm.type=NULL){
    ## check
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type are inconsistent");
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## shrinking scores matrix to test test
    S <- S[testIndex,];
    ## hierarchical top-down
    root <- root.node(g);
    S <- htd(S, g, root);
    cat("htd-dag correction: done\n");
    return(S);
}
