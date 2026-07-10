#' Row Frequency
#'
#' @param x dataframe or matrix
#'
#' @return data with frequency column
#' @export
#'
#' @examples
#' row.freq(x=mtcars[,8:11])
row.freq <- function(x){
    if (!any(is.data.frame(x),is.matrix(x))){
        stop('x must be dataframe or matrix')
    }
    x.u=unique(x)
    for (i in 1:ncol(x)) {
        if (i==1) res=NULL
        res=paste0(res,x[,i])
    }
    res.tab=table(res)
    res.count=res.tab[unique(res)]
    cbind(x.u,res.count)[,-(ncol(x.u)+1)]
}