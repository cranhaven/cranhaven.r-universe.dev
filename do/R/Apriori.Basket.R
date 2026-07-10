#' Convert vector to sparse matrix
#' @description Convert vector or dataframe to sparse matrix.
#' @param x a vector
#' @param sep one separator
#' @param dup.delete whether to delete duplicated values in the same row, default is FALSE
#'
#' @return a sparse matrix
#' @export
#'
#' @examples
#' # convert a vector to sparse matrix
#' g=c('a,b,a,,','a,b,c,d','d,c,f,g,h')
#' Apriori.Basket(x=g,sep = ',')
#'
#' # convert a dataframe to sparse matrix
#' library(data.table)
#' df=fread(text = '
#' t1 t2 t3
#' a NA d
#' g a j')
#' Apriori.Basket(x=df,sep = ',')
Apriori.Basket <- function(x,sep=';',dup.delete=FALSE){
    if (is.data.frame(x) | is.matrix(x)){
        x[is.na(x)]=''
        x=do::paste0_columns(x,collapse = sep)
    }
    sp=strsplit(x,sep)
    sp=lapply(sp, function(i) i[nchar(i)>0])
    x=lapply(sp, function(i) as.data.frame(t(as.matrix(table(i)))))
    x=do.call(plyr::rbind.fill,x)
    x[is.na(x)]=0
    if (dup.delete) x[x>1]=1
    x
}

