#' Split A Vector into Columns
#'
#' @param x a vector
#' @param split one or more characters. Split exactly
#' @param reg_expr character. Split by regular expressions
#' @param colnames optional. Column names for outcome
#' @param cat logical, whether to show message
#' @return A dataframe with several columns.
#' @export
#'
#' @examples
#' x=c('1a2','3a4','4a4')
#' col_split(x,split='a')
#' col_split(x = x,reg_expr = '[a-z]')
#' 
#' #two splits
#' df=data.frame(result=c('A, B-C',
#'                        'A, C-D',
#'                        'E, F-G'))
#' col_split(x = df[,1],split = c(',','-'))
col_split <- function(x,split,reg_expr,colnames,cat=TRUE){
    if (any(is.data.frame(x),is.matrix(x),is.array(x))){
        stop('x must be a vector')
    }
    x=as.vector(x)
    if (!missing(split)){
        if (length(split)>1){
            x=Replace(data = x,from = split[-1],to=split[1])
            split=split[1]
        }
        f=strsplit(x = x,split = split)
    }else if (!missing(reg_expr)){
        f=strsplit(x = x,split = reg_expr,fixed = FALSE)
    }
    f.df <- function(x){
        df=data.frame(t(x))
        colnames(df)=paste0('x',1:ncol(df))
        df
    }
    f2=lapply(f, function(x) f.df(x))
    f3=do.call(plyr::rbind.fill,f2)
    if (missing(colnames)){
        return(f3)
    }else{
        if (length(colnames)>ncol(f3)){
            if (cat) message('\n',length(colnames),' colnames Vs. ',ncol(f3),' splited columns\n')
            colnames(f3)=colnames[1:ncol(f3)]
        }else{
            colnames(f3)[1:length(colnames)]=colnames
        }
        return(f3)
    }
}
