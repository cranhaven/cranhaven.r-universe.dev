#' Grep for vectors
#'
#' @param x vectors with one or more characters
#' @param patterns vectors with one or more characters
#' @name grep
#' @return vectors
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('b','1')
#' grep_or(x,patterns)
grep_or <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    x[res>0]
}
#' @rdname grep
#' @export
#'
#' @examples
#' x=c('a12','a1','b')
#' patterns=c('a','1')
#' grep_and(x,patterns)
grep_and <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    x[res==length(patterns)]
}
#' @rdname grep
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('a','1')
#' grep_not_and(x,patterns)
grep_not_and <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    x[res!=length(patterns)]
}
#' @rdname grep
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('a','1')
#' grep_not_or(x,patterns)
grep_not_or <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    x[res==0]
}
