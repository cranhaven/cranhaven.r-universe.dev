#' Create dump matrix for a vector
#'
#' @param ... one vector
#' @param include.name logical, default is TRUE, wether to include name of variable
#' @return a dump matix contains 0 and 1
#' @export
#'
#' @examples
#' x=c('a','b','c','a','a')
#' dump.it(x)
#' dump.it(mtcars$am)
#' dump.it(mtcars[,'am'])
dump.it <- function(...,include.name=TRUE){
    x=list(...)[[1]]
    x2=unique(x[!is.na(x)])
    x3=lapply(x2, function(i) as.numeric(x==i))
    x4=do.call(cbind,x3)
    colnames(x4)=x2
    rownames(x4)=NULL
    if (include.name){
        name<<-(do::get_names(...))
        name=do::Replace0(name,c('.*\\$','.*, \\"','\\".*','.*, ','\\].*'))
        colnames(x4)=paste0(name,'.',colnames(x4))
    }
    data.frame(x4,check.names = FALSE)
}


