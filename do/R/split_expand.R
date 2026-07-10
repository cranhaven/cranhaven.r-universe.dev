#' Split One Column and Expand
#'
#' @param data dataframe or matrix
#' @param variable one column name with connected values
#' @param sep seperated symbol, which can be one or more
#'
#' @return expanded dataframe or matrix
#' @export
#'
#' @examples
#' df=data.frame(a=c(1,0),
#'               b=c('a','n'),
#'            cyl=c('6;6;4;4;4',
#'                  '6;8;'))
#' split_expand(data=df,variable='cyl',sep=';')
split_expand <- function(data,variable,sep){
    left.check=0
    left.names=not(colnames(data),variable)
    if (length(left.names)==1){
        if (is.factor(data[,left.names])){
            left.names.level=levels(data[,left.names])
            data[,left.names]=as.character(data[,left.names])
            left.check=1
        }
    }
    if (length(sep)>1){
        data[,variable]=Replace(data = data[,variable],from = sep[-1],to=sep[1])
        sep=sep[1]
    }
    res=NULL
    for (i in 1:nrow(data)) {
        df.i=t(col_split(data[i,variable],sep))
        rownames(df.i)=NULL
        df.rbind=suppressWarnings(cbind(data[i,not(colnames(data),variable)],df.i))
        res=rbind(res,df.rbind)
    }
    rownames(res)=NULL
    colnames(res)=c(not(colnames(data),variable),variable)
    if (is.matrix(data)){
        if (left.check==1){
            res[,left.names]=factor(x = res[,left.names],levels = left.names.level)
        }
        return(res)
    }else if(is.data.frame(data)){
        res=data.frame(res,check.rows = FALSE)
        if (left.check==1){
            res[,left.names]=factor(x = res[,left.names],levels = left.names.level)
        }
        return(res)
    }
}
