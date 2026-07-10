#' Delete and Move Up the Rest Values
#'
#' @param x dataframe or matrix
#' @param delete one delete object
#'
#' @return dataframe or matrix
#' @export
#'
#' @examples
#' a=c(1,NA,7,NA)
#' b=c(NA,2,2,7)
#' d=c(1,NA,40,7)
#' df=data.frame(a,b,d)
#' 
#' delete_up(x = df,delete = NA)
delete_up <- function(x,delete){
    mtr=data.frame(matrix(NA,nrow = nrow(x),ncol = ncol(x)),check.names = FALSE)
    colnames(mtr)=colnames(x)
    rownames(mtr)=rownames(x)
    for (i in 1:ncol(x)) {
        x.i=x[,i]
        if (is.na(delete)){
            x.yes=x.i[!is.na(x.i)]    
        }else{
            x.yes=x.i[x.i!=delete]
        }
        mtr[1:length(x.yes),i]=x.yes
    }
    mtr1=mtr[rowSums(mtr,na.rm = TRUE) != 0,]
    mtr2=mtr1[,colSums(mtr1,na.rm = TRUE) != 0]
    mtr2
}
#' Delete and Move Left the rest Values
#'
#' @param x dataframe or matrix
#' @param delete one delete object
#'
#' @return dataframe or matrix
#' @export
#'
#' @examples
#' a=c(1,NA,7,NA)
#' b=c(NA,2,2,7)
#' d=c(1,NA,40,7)
#' df=data.frame(a,b,d)
#' delete_left(x=df,NA)
delete_left <- function(x,delete){
    mtr=data.frame(matrix(NA,nrow = nrow(x),ncol = ncol(x)),check.names = FALSE)
    colnames(mtr)=colnames(x)
    rownames(mtr)=rownames(x)
    for (i in 1:nrow(x)) {
        x.i=x[i,]
        if (is.na(delete)){
            x.yes=x.i[!is.na(x.i)]    
        }else{
            x.yes=x.i[x.i!=delete]
        }
        mtr[i,1:length(x.yes)]=x.yes
    }
    mtr1=mtr[rowSums(mtr,na.rm = TRUE) != 0,]
    mtr2=mtr1[,colSums(mtr1,na.rm = TRUE) != 0]
    mtr2
}