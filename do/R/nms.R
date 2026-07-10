#' Return numeric names in matrix or dataframe
#'
#' @param df dataframe or matrix
#'
#' @return numeric names vectors
#' @export
#'
numeric.nms <- function(df){
    if (!(is.data.frame(df) | is.matrix(df))) stop('data must be dataframe or matrix')
    x=sapply(names(df), function(i) is.numeric(df[,i]))
    names(x)[x]
}
#' Return factor names in matrix or dataframe
#'
#' @param df dataframe or matrix
#'
#' @return factor names vectors
#' @export
#'
factor.nms <- function(df){
    if (!(is.data.frame(df) | is.matrix(df))) stop('data must be dataframe or matrix')
    x=sapply(names(df), function(i) is.factor(df[,i]))
    names(x)[x]
}
#' Return character names in matrix or dataframe
#'
#' @param df dataframe or matrix
#'
#' @return character names vectors
#' @export
#'
character.nms <- function(df){
    if (!(is.data.frame(df) | is.matrix(df))) stop('data must be dataframe or matrix')
    x=sapply(names(df), function(i) is.character(df[,i]))
    names(x)[x]
}




#' Names with different letters
#'
#' @param df datafame or matrix
#' @param least names with at least different letters, which means >=
#' @param most names with at most different letters, which means <=
#'
#' @return names
#' @export
names_n <- function(df,most=NULL,least=NULL){
    x=sapply(names(df), function(i) length(unique_no.NA(as.character(df[,i]))))
    if (!is.null(least) & is.null(most)){
        y=names(x)[x>=least]
        y
    }else if (is.null(least) & !is.null(most)){
        y=names(x)[x<=most]
        y
    }else if (is.null(least) & is.null(most)){
        stop('least and most can not both be NULL')
    }else if (!is.null(least) & !is.null(most)){
        stop('least and most can not both be given at the same time')
    }
}

