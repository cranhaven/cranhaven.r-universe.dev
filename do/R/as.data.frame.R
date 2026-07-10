#' Transform to dataframe
#' rules object or calibrate object
#' @param x data with rules class for package 'arules'
#' @param row.names ignore
#' @param optional ignore
#' @param ... ignore
#' @name as.data.frame
#' @importFrom methods as
#' @method as.data.frame rules
#' @return a dataframe
#' @export
#'
as.data.frame.rules <- function(x, row.names = NULL, optional = FALSE, ...){
    if (attr(attributes(x)$class,'package') != 'arules') stop('as.data.frame.rules can only used for arules package')
    df=as(x,'data.frame')
    if (nrow(df) !=0){
        dfi=as.data.frame(do.call(rbind,lapply(strsplit(df$rules,'\\} {0,}=> {0,}\\{'),function(i) Trim(i,c('{','}')))))
        colnames(dfi)=c('lhs','rhs')
        cbind(dfi,df[-1])
    }else{
        df
    }
}
#' @rdname as.data.frame
#' @method as.data.frame calibrate
#' @export
#'
as.data.frame.calibrate <- function(x, row.names = NULL, optional = FALSE, ...){
    utils::head(x,length(x[,1]))
}