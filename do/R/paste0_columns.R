#' Paste Columns Together
#' @description Paste each column in a dataframe together.
#' @param df a dataframe
#' @param collapse collapse, default is comma
#'
#' @return a character
#' @export
#'
#' @examples
#' df=data.frame(a=c(1,2,30),
#'               b=c('x','y','z'))
#' paste0_columns(df)
#' 
#' df=data.frame(a=c(1,2,30),b=c('x','y','z'),c=c(1,7,8))
#' paste0_columns(df)
#' 
paste0_columns <- function(df,collapse=','){
    if (ncol(df)==1){
        df
    }else{
        apply(df,1,paste0,collapse=collapse)
    }
}


