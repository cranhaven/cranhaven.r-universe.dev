###################################
##  Obozinski Heuristic Methods  ##
###################################

#' @name obozinski.heuristic.methods
#' @aliases obozinski.max
#' @aliases obozinski.and
#' @aliases obozinski.or
#' @title Obozinski heuristic methods
#' @description Implementation of the Obozinski's heuristic methods \code{Max}, \code{And}, \code{Or} (\cite{Obozinski et al., Genome Biology, 2008,
#' \doi{10.1186/gb-2008-9-s1-s6}}).
#' @details Obozinski's heuristic methods:
#' \enumerate{
#'  \item \bold{Max}: reports the largest logistic regression (LR) value of self and all descendants: \eqn{p_i = max_{j \in descendants(i)} \hat{p_j}};
#'  \item \bold{And}: reports the product of LR values of all ancestors and self. This is equivalent to computing the probability that all
#'   ancestral terms are "on" assuming that, conditional on the data, all predictions are independent: \eqn{p_i = \prod_{j \in ancestors(i)} \hat{p_j}};
#'  \item \bold{Or}: computes the probability that at least one of the descendant terms is "on" assuming again that, conditional on the data,
#'   all predictions are independent: \eqn{1 - p_i = \prod_{j \in descendants(i)} (1 - \hat{p_j})};
#' }
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that it is the top-level of the hierarchy (\code{def:00}).
#' @return A matrix with the scores of the classes corrected according to the chosen Obozinski's heuristic algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' root  <- root.node(g);
#' S.max <- obozinski.max(S,g,root);
#' S.and <- obozinski.and(S,g,root);
#' S.or  <- obozinski.or(S,g,root);
obozinski.max <- function(S, g, root="00"){
    if(!(root %in% colnames(S))) {
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    desc <- build.descendants(g);
    for(i in 1:length(desc)){
        m <- as.matrix(S[,desc[[i]]]);
        S[,names(desc[i])] <- apply(m, 1, max);
    }
    return(S);
}

#' @rdname obozinski.heuristic.methods
#' @export
obozinski.and <- function(S, g, root="00"){
    if(!(root %in% colnames(S))) {
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    S.hier <- S;
    anc <- build.ancestors(g);
    for(i in 1:length(anc)){
        m <- as.matrix(S[,anc[[i]]]);
        idx <- which(apply(m,1,sum)>0); ## consider only examples with scores greater than 0
        m.idx <- as.matrix(m[idx,]); ## handle case with one descendant -> apply needs a matrix
        S.hier[idx,names(anc[i])] <- apply(m.idx, 1, prod);
    }
    rm(S); gc();
    return(S.hier);
}

#' @rdname obozinski.heuristic.methods
#' @export
obozinski.or <- function(S, g, root="00"){
    if(!(root %in% colnames(S))) {
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    S.hier <- S;
    desc <- build.descendants(g);
    for(i in 1:length(desc)){
        m <- as.matrix(S[,desc[[i]]]);
        idx <- which(apply(m,1,sum)>0); ## consider only examples with scores greater than 0
        m.idx <- as.matrix(m[idx,]); ## handle case with one descendant -> apply needs a matrix
        S.hier[idx,names(desc[i])] <- 1-(apply(1-m.idx,1,prod));
    }
    rm(S); gc();
    return(S.hier);
}

#' @title Obozinski's heuristic methods calling
#' @description Compute the Obozinski's heuristic methods \code{Max}, \code{And}, \code{Or} (\cite{Obozinski et al., Genome Biology, 2008}).
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param heuristic a string character. It can be one of the following three values:
#' \enumerate{
#'  \item "max": run the method \code{obozinski.max};
#'  \item "and": run the method \code{obozinski.and};
#'  \item "or": run the method \code{obozinski.or};
#' }
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A matrix with the scores of the classes corrected according to the chosen heuristic algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' S.and <- obozinski.methods(S, g, heuristic="and", norm=TRUE, norm.type="maxnorm");
obozinski.methods <- function(S, g, heuristic="and", norm=FALSE, norm.type=NULL){
    ## check
    if(heuristic!="max" && heuristic!="and" && heuristic!="or")
        stop("the chosen heuristic method is not among those available or it has been misspelled");
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type are inconsistent");
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## Obozinski's hierarchical heuristic methods
    root <- root.node(g);
    if(heuristic=="and")
        S <- obozinski.and(S, g, root);
    if(heuristic=="max")
        S <- obozinski.max(S, g, root);
    if(heuristic=="or")
        S <- obozinski.or(S, g, root);
    cat("Obozinski's heuristic", heuristic, "correction: done\n");
    return(S);
}

#' @title Obozinski's heuristic methods -- holdout
#' @description Compute the Obozinski's heuristic methods \code{Max}, \code{And}, \code{Or} (\cite{Obozinski et al., Genome Biology, 2008})
#' applying a classical holdout procedure.
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param testIndex a vector of integer numbers corresponding to the indexes of the elements (rows) of the scores matrix \code{S} to be used in the test set.
#' @param heuristic a string character. It can be one of the following three values:
#' \enumerate{
#'  \item "max": run the method \code{heuristic.max};
#'  \item "and": run the method \code{heuristic.and};
#'  \item "or": run the method \code{heuristic.or};
#' }
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @return A matrix with the scores of the classes corrected according to the chosen heuristic algorithm. Rows of the matrix are shrunk to \code{testIndex}.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(test.index);
#' S.and <- obozinski.holdout(S, g, testIndex=test.index, heuristic="and", norm=FALSE, norm.type=NULL);
obozinski.holdout <- function(S, g, testIndex, heuristic="and", norm=FALSE, norm.type=NULL){
    ## check
    if(heuristic!="max" && heuristic!="and" && heuristic!="or")
        stop("the chosen heuristic method is not among those available or it has been misspelled");
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
    ## Obozinski's hierarchical heuristic methods
    root <- root.node(g);
    if(heuristic=="and")
        S <- obozinski.and(S, g, root);
    if(heuristic=="max")
        S <- obozinski.max(S, g, root);
    if(heuristic=="or")
        S <- obozinski.or(S, g, root);
    cat("Obozinski's heuristic", heuristic, "correction: done\n");
    return(S);
}
