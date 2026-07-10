#' Replace Exactly
#'
#' @param x vector, dataframe or matrix
#' @param from replaced stings
#' @param to replacements
#' @param pattern a special pattern, see examples for detail
#'
#' @return replaced data
#' @export
#'
#' @examples
#' a=c(1,2,3,1,4)
#' Replace_ex(x = a,from = c(1,2),to=5)
#' Replace_ex(x=a,pattern = c('1:5','2:5'))
#' Replace_ex(x=a,pattern = '[12]:5')
#' 
#' 
#' a=data.frame(v=c(1,2,3,2,4),
#'              b=c(7,8,9,4,6))
#' Replace_ex(x = a,from = c(1,2),to=5)
#' Replace_ex(x=a,pattern = c('1:5','2:5'))
Replace_ex <- function(x,from,to,pattern){
    #for vector
    re.vector<-function(x,from,to){
        for (i in 1:length(from)) {
            x[x==from[i]]=to
        }
        return(x)
    }
    #for from and to
    if (all(!missing(from),!missing(to))){
        if (is.vector(x)){
            x=re.vector(x,from,to)
        }else{
            if (any(is.data.frame(x),is.matrix(x))){
                for (i in 1:ncol(x)) {
                    x[,i]=re.vector(x[,i],from,to)
                }
            }
        }
    }
    #for pattern
    if (!missing(pattern)){
        for (i in 1:length(pattern)) {
            from=Replace0(pattern[i],':.*')
            to=Replace0(pattern[i],'.*:')
            if (is.vector(x)){
                x=re.vector(x,from,to)
            }else{
                if (any(is.data.frame(x),is.matrix(x))){
                    for (i in 1:ncol(x)) {
                        x[,i]=re.vector(x[,i],from,to)
                    }
                }
            }
        }
    }
    return(x)
}