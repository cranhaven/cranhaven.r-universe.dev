###########################
## Normalization Methods ##
###########################

#' @title Max normalization
#' @description Normalize the scores of a scores matrix by dividing the score values of each class for the maximum score of the class.
#' @param S a scores matrix. Rows are examples and columns are classes.
#' @return A scores matrix with the scores normalized.
#' @export
#' @examples
#' data(scores);
#' maxnorm <- normalize.max(S);
normalize.max <- function(S){
    classes <- colnames(S);
    maximum <- apply(S,2,max);
    for(class in classes){
        if(maximum[class] != 0){
            S[,class] <- S[,class]/maximum[class];
        }
    }
    return(S);
}

#' @title Scores normalization function
#' @description Normalize a scores matrix w.r.t. max normalization (\code{maxnorm}) or quantile normalization (\code{qnorm})
#' @details To apply the quantile normalization the \pkg{preprocessCore} package must be properly installed.
#' @param norm.type can be one of the following two values:
#' \itemize{
#'  \item maxnorm (\code{def.}): each score is divided w.r.t. the max of each class;
#'  \item qnorm: a quantile normalization is applied. Package preprocessCore is used;
#' }
#' @param S A named flat scores matrix with examples on rows and classes on columns.
#' @return The matrix of the scores flat normalized w.r.t. \code{maxnorm} or \code{qnorm}.
#' @export
#' @examples
#' data(scores);
#' norm.types <- c("maxnorm","qnorm");
#' for(norm.type in norm.types){
#'     scores.normalization(norm.type=norm.type, S=S);
#' }
scores.normalization <- function(norm.type="maxnorm", S){
    if(norm.type=="maxnorm"){
        ## Max Normalization
        S <- normalize.max(S);
    }else if(norm.type=="qnorm"){
        ## Quantile Normalization
        ## note: normalize.quantiles function returns a unnamed matrix. colnames are essential for hierarchical algorithms
        S.norm <- normalize.quantiles(S);
        dimnames(S.norm) <- list(rownames(S),colnames(S));
        S <- S.norm;
        rm(S.norm);
    }else{
        stop("the chosen normalization method is not among those available or it was misspelled");
    }
    return(S);
}
