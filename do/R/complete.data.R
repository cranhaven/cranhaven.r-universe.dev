#' Complete data
#' @description Removing rows with NA in dataframe or matrix. Removing NA atomic.
#' @param x dataframe or matrix or atomic
#' @importFrom stats complete.cases
#' @return complete data
#' @export
#'
#' @examples
#' x=c(1,NA,2)
#' complete.data(x)
#' 
#' x=data.frame(a=c(1,NA))
#' complete.data(x)
complete.data <- function(x){
    if (is.data.frame(x) | is.matrix(x)){
        na.row=sum(!complete.cases(x))
        if (na.row==0){
            message('Now Missing Value')
            return(x)
        }else{
            message(na.row,' Missing Row (',round(na.row/nrow(x)*100,2),'%)')
            return(x[complete.cases(x),,drop=FALSE])
        }
    }else if (is.atomic(x)){
        x[!is.na(x)]
    }
}
