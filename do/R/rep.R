#' Replicate Each Elements of Vectors
#'
#' @param x vectors
#' @param each one or more numbers for replication
#' @name replicate
#' @return replicated vectors
#' @export
#'
#' @examples
#' rep_n(c('ab','cde','k','op'),5)
#' rep_n(c('ab','cde','k','op'),c(4,6))
#' rep_n(c('ab','cde','k','op'),c(1,2,3,4))
#' 
#' rep_character(c('ab','cde','k','op'),5)
#' rep_character(c('ab','cde','k','op'),c(4,6))
#' rep_character(c('ab','cde','k','op'),c(1,2,3,4))
rep_n <- function(x,each){
    len.x=length(x)
    if (length(each)==1) each=rep(each,len.x)
    if ((len.x %% length(each) ==0) &
        (len.x != length(each))) each=rep(each,len.x/length(each))
    if (length(each) != len.x) stop('times must be one or the same length with x or times length of x')
    x2=lapply(1:len.x, function(i) rep(x[i],each[i]))
    x3=sapply(x2, function(i) paste0(i,collapse = ''))
    if (is.numeric(x)) x3=is.numeric(x3)
    x3
    
}
#' @name replicate
#' @export
rep_character <- function(x,each){
    len.x=length(x)
    if (length(each)==1) each=rep(each,len.x)
    if ((len.x %% length(each) ==0) &
        (len.x != length(each))) each=rep(each,len.x/length(each))
    if (length(each) != len.x) stop('times must be one or the same length with x or times length of x')
    x2=strsplit(x,'')
    x3=sapply(1:length(x2), function(i) rep(x2[[i]],each=each[i]))
    x4=sapply(x3, function(i) paste0(i,collapse = ''))
    if (is.numeric(x)) x4=is.numeric(x4)
    x4
}
