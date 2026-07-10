#' Extract Some String
#' @param x string
#' @param ... patterns of c('begin','after')
#' @param type any left characters of character or list
#'
#' @return characters
#' @export
#'
#' @examples
#' x='abdghtyu'
#' take_out(x,c('a','d'),c('h','u'))
take_out <- function(x,...,type='c'){
    value<-list(...)
    for (i in 1:length(value)) {
        if (i==1) res=list()
        before=value[[i]][1]
        after=value[[i]][2]
        if (before==after){
            xs=sub(before,paste0(before,'before'),x)
            before=paste0(before,'before')
        }else{
            xs=x
        }
        x1=Replace0(xs,paste0('.*',before))
        x2=Replace0(x1,paste0(after,'.*'))
        res=c(res,list(x2))
    }
    if (left('character',nchar(type))==type){
        do.call(c,res)
    }else{
        res
    }
}