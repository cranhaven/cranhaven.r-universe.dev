#' Judge for Included Character
#'
#' @param pattern one or more vectors
#' @param x one or more vectors
#'
#' @return a matrix with logical words
#' @details '%or%' and '%and%' can be used in pattern, see examples.
#' @export
#'
#' @examples
#' 
#' a=c('abcd','agj','abcu')
#' 
#' # Grepl for one vector
#' pat1='b'
#' Grepl(pat1,a)
#' 
#' # Grepl for two vectors
#' pat2=c('c','d')
#' Grepl(pat2,a)
#' 
#' # use %or% in pattern
#' pat3=c('a%or%c','d')
#' Grepl(pat3,a)
#' 
#' # use %and% in pattern
#' pat4=c('a%and%c','d')
#' Grepl(pat4,a)
Grepl <- function(pattern,x){
    res_x=list()
    for (j in x) {
        res_x=c(res_x,list(Grepl_i(pattern,j)))
    }
    last=do.call(rbind,res_x)
    rownames(last)=1:nrow(last)
    last
}
Grepl_i <- function(pattern,x){
    res_i=list()
    for (i in pattern) {
        if (grepl("%or%",i)){
            many=strsplit(i,"%or%")[[1]]
            if (any(sapply(many,function(k) grepl(k,x)))){
                res_i=c(res_i,TRUE)
            }else{
                res_i=c(res_i,FALSE)
            }
        }else if (grepl("%and%",i)){
            many=strsplit(i,"%and%")[[1]]
            if (all(sapply(many,function(k) grepl(k,x)))){
                res_i=c(res_i,TRUE)
            }else{
                res_i=c(res_i,FALSE)
            }
        }else{
            res_i=c(res_i,grepl(i,x))
        }
    }
    mt=do.call(cbind,res_i)
    colnames(mt)=pattern
    mt
}
