#' Expand Data by Weight
#'
#' @param x dataframe or matrix
#' @param weight weight column names or index
#'
#' @return expanded data
#' @export
#'
#' @examples
#' df=data.frame(v=c(1,2,3),
#'                x=c(7,8,9),
#'                n=c(2,3,4))
#' expand(x = df,weight = 3)
#' expand(x = df,weight = 'n')
expand <- function(x,weight){
    freq_xf=x[,weight]
    res=c()
    for (i in 1:nrow(x)){
        timesrep=freq_xf[i]
        temp.res=x[rep(i,timesrep),]
        res=rbind(res,temp.res)
    }
    rownames(res)=NULL
    return(res)
}
