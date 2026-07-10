#' Comine xml_nodeset
#'
#' @param ... one or more xml_nodeset
#'
#' @return xml_nodeset
#' @export
#' @method c xml_nodeset
c.xml_nodeset <- function(...){
    pattern <- list(...)
    k=1
    for(i in 1:length(pattern)){
        if (length(pattern[[i]])==0) next(i)
        if (k==1){
            res=pattern[[i]]
            k=k+1
        }else{
            res[(length(res)+1):(length(res)+length(pattern[[i]]))]=pattern[[i]]
            k=k+1
        }
    }
    res
}
