#' Change data type
#'
#' @param x dataframe
#' @param value column names
#'
#' @return factor or numeric columns in a dataframe
#' @export
#' @name columntrans
#' @examples
#' str(mtcars)
#' factor.it(mtcars,c("cyl", "vs", "am", "gear"))
#' factor.it(mtcars)=c("cyl", "vs", "am", "gear")
#' str(mtcars)
#' 
#' numeric.it(mtcars,c("cyl", "vs", "am", "gear"))
#' numeric.it(mtcars)=c("cyl", "vs", "am", "gear")
#' str(mtcars)
`factor.it<-` <- function(x,value){
    if (!is.data.frame(x)) stop('x must be a dataframe')
    if (any(! value %in% colnames(x))) stop('value must be column names in x')
    for (i in value) {
        x[,i]=factor(x[,i])
    }
    x
}
#' @export
#' @rdname columntrans
factor.it <- function(x,value){
    if (!is.data.frame(x)) stop('x must be a dataframe')
    if (any(! value %in% colnames(x))) stop('value must be column names in x')
    for (i in value) {
        x[,i]=factor(x[,i])
    }
    x
}
#' @export
#' @rdname columntrans
numeric.it <- function(x,value){
    if (!is.data.frame(x)) stop('x must be a dataframe')
    if (any(! value %in% colnames(x))) stop('value must be column names in x')
    for (i in value) {
        x[,i]=as.numeric(as.character(x[,i]))
    }
    x
}
#' @export
#' @rdname columntrans
`numeric.it<-` <- function(x,value){
    if (!is.data.frame(x)) stop('x must be a dataframe')
    if (any(! value %in% colnames(x))) stop('value must be column names in x')
    for (i in value) {
        x[,i]=as.numeric(as.character(x[,i]))
    }
    x
}