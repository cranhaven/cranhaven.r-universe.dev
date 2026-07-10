#' UTF8 Code for Chinese
#'
#' @param x chinese characters
#' @importFrom utils capture.output
#' @return an expression with UTF8 code.
#' @export
#'
chinese_utf8 <- function(x){
    for (i in 1:length(x)) {
        if (i==1) res=list()
        f=capture.output(tmcn::catUTF8(x[i]))
        t=paste0('tmcn::toUTF8("',f,'")')
        res=c(res,list(parse(text=t)))
    }
    names(res)=x
    res
}