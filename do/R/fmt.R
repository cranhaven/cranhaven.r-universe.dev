#' Formatting Replacement
#'
#' @param x format with slash number and one space, which is like "/1 ". 
#'     Number means replacement order.
#' @param ... values to be passed into x
#'
#' @return replaced string
#' @export
#'
#' @examples
#' 'whwdzg, ykybnfg'
#' fmt('/ hwdzg, ykybnfg',
#'     'w')
#' fmt('/ h/ dzg, ykybnfg',
#'     'w')
#' fmt('/1 h/ dzg, ykybnfg',
#'     'w')
#' fmt('/1 h/ dzg, ykybnfg',
#'     'w','-w-')
#' 
#' fmt('/ h/1 dzg, ykybnfg',
#'     'w','-w-')
#' 
#' fmt('/1 h/0 dzg, ykybnfg',
#'     'w','-w-')
#' 
#' 
#' '|w|' |> 
#'     fmt(x = '/ h/ dzg, ykybnfg')
#' 
#' 
#' '|w|' |> 
#'     fmt(x = '/ h/ dzg, ykybnfg',
#'         '-w-')
#' '|w|' |> 
#'     fmt(x = '/ h/1 dzg, ykybnfg',
#'         '-w-')
fmt <- function(x,...){
    x <- x
    to <- c(...)
    # pos number
    pos <- stringr::str_extract_all(x,'/[0-9]{1,} ')[[1]] |> 
        unique()
    if (length(pos) >0){
        pos_number <- pos  |> 
            stringr::str_extract_all('[0-9]{1,}') |> 
            unlist()|> 
            as.numeric()
        pos_order <- pos[order(pos_number)]
        if (length(pos_order) != length(to)){
            len <- min(length(to),length(pos_order))
        }else{
            len <- length(to)
        }
        for (i in seq(len)) {
            x <- gsub(pos_order[i],to[i],x)
        }
        to <- to[-seq(len)]
    }
    if (length(to)==0) return(x)
    # pos space
    pos0 <- stringr::str_extract_all(x,'/ ')[[1]]
    if (length(pos0)>0){
        if (length(to)==1){
            # one x to other
            x <- gsub('/ ',to,x)
        }else if (length(to)==length(pos0)){
            for (i in seq_range(pos0)) {
                x <- sub('/ ',to[i],x)    
            }
        }else if (length(pos0) > length(to) & length(pos0) %% length(to) == 0){
            to <- rep(to,length(pos0) %/% length(to))
            for (i in seq_range(pos0)) {
                x <- sub('/ ',to[i],x)    
            }
        }else{
            if (cnOS()){
                msg <- tmcn::toUTF8("\u66FF\u6362\u5B57\u7B26\u548C\u66FF\u6362\u4F4D\u7F6E\u4E0D\u5339\u914D")
            }else{
                msg <- 'position and characters are not match'
            }
            stop(msg)
        }
    }
    x
}
