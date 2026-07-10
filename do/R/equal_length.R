#' Equal Length
#'
#' @param x can be number, strings, verctors, dataframe or matrix.
#' @param suffix suffix
#' @param nchar maximun length
#' 
#' @param colname a logistic value, default is FALSE
#' @param rowname a logistic value, default is FALSE
#'
#' @return equal length results
#' @export
#'
#' @examples 
#' a=c(123,1,24,5,1.22554)
#' equal_length(a,0)
#' 
#' df = data.frame(
#'     a=c(12,1,1.23),
#'     b=c('a','abcd','d')
#' )
#' equal_length(x = df,suffix = 'x')
#' 
#' equal_length(x = df,suffix = 0,nchar =5)

equal_length <- function(x,suffix=" ",nchar,colname=FALSE,rowname=FALSE){
    if (all(colname,rowname)){
        max_length=max(max(Nchar(x)),
                       max(Nchar(colnames(x))),
                       max(Nchar(rownames(x))))
    }else if (all(colname,!rowname)){
        max_length=max(max(Nchar(x)),max(Nchar(colnames(x))))
    }else if (all(!colname,rowname)){
            max_length=max(max(Nchar(x)),max(Nchar(rownames(x))))
    }else if (all(!colname,!rowname)){
        max_length=max(max(Nchar(x)))
    }
    if (!missing(nchar)){
        if (nchar < max_length) stop('nchar must be more than the maximum character in x.')
        max_length=nchar
    }else{
        max_length=max_length
    }
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            x[,i]=as.character(x[,i])
            for (j in 1:nrow(x)) {
                x[j,i]=paste0(x[j,i],
                          inner_Add_Symbol(rep(suffix,max_length-Nchar(x[j,i])),""))
            }
        }
        if (colname){
            #CN is short for colnames
            CN=colnames(x)
            for (i in 1:length(CN)) {
                CN[i]=paste0(CN[i],
                             inner_Add_Symbol(rep(suffix,max_length-Nchar(CN[i])),""))
            }
            colnames(x)=CN
        }
        if (rowname){
            #RN is short for rownames
            RN=rownames(x)
            max_length_RN=max(Nchar(RN))
            for (i in 1:length(RN)) {
                RN[i]=paste0(RN[i],
                             inner_Add_Symbol(rep(suffix,max_length_RN-Nchar(RN[i])),""))
            }
            rownames(x)=RN
        }
    }else{
        for (i in 1:length(x)) {
            x[i]=paste0(x[i],
                         inner_Add_Symbol(rep(suffix,max_length-Nchar(x[i])),""))
        }
    }
    x
}
