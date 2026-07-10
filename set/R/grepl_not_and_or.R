#' Grepl for vectors
#'
#' @param x vectors with one or more characters
#' @param patterns vectors with one or more characters
#' @name grepl
#' @return vectors
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('b','1')
#' grepl_or(x,patterns)
grepl_or <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    res>0
}
#' @rdname grepl
#' @export
#'
#' @examples
#' x=c('a12','a1','b')
#' patterns=c('a','1')
#' grepl_and(x,patterns)
grepl_and <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    res==length(patterns)
}
#' @rdname grepl
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('a','1')
#' grepl_not_and(x,patterns)
grepl_not_and <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    res!=length(patterns)
}
#' @rdname grepl
#' @export
#'
#' @examples
#' x=c('a12','a','b')
#' patterns=c('a','1')
#' grepl_not_or(x,patterns)
grepl_not_or <- function(x,patterns){
    lp=lapply(patterns, function(i) grepl(i,x))
    res=0
    for (i in lp) res=res+i
    res==0
}
