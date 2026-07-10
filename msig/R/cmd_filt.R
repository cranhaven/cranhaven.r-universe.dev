
#' Filt data by key words
#' Case insensitive
#' @param x data from msig package
#' @param ... one or more key words
#' @return filted results with high light information.
#' @export
#' @examples
#' \donttest{
#' browse_msig('immune') |>
#'     msig_filt('response')
#' }
msig_filt <- function(x,...){
    hl <- c(...)
    if ('browse_msig' %in% names(attributes(x))){
        sr <- attr(x,"browse_msig")
        xl <- lapply(hl, function(i) grepl(i,x,ignore.case = TRUE))
        x <- x[rowSums(data.frame(xl)) == length(xl)]
        attr(x,"browse_msig") <- unique(c(sr,hl))
    }else if ('search_msig' %in% names(attributes(x))){
        sr <- attr(x,"search_msig")
        xp <- do::paste0_columns(x,'---')
        xl <- lapply(hl, function(i) grepl(i,xp,ignore.case = TRUE))
        x <- x[rowSums(data.frame(xl)) == length(xl),]
        x$link <- paste0(x$link,' AND ',paste0(hl,recycle0 = ' AND '))
        attr(x,"search_msig") <- unique(c(sr,hl))
    }else if ('similarity_geneset' %in% names(attributes(x))){
        sr <- attr(x,"similarity_geneset")
        xp <- do::paste0_columns(x[,seq_len(2)],'---')
        xl <- lapply(hl, function(i) grepl(i,xp,ignore.case = TRUE))
        x <- x[rowSums(data.frame(xl)) == length(xl),]
        attr(x,"similarity_geneset") <- unique(c(sr,hl))
    }else if ('related_geneset' %in% names(attributes(x))){
        sr <- attr(x,"related_geneset")
        for (i in seq_len(length(x))) {
            xi = x[[i]]
            xl <- lapply(hl, function(i) grepl(i,xi,ignore.case = TRUE))
            x[[i]] <- xi[rowSums(data.frame(xl)) == length(xl)]
            message(names(x)[i])
            cat('left: ',length(x[[i]]),'\n\n')
            names(x)[i] <- paste0(names(x)[i],' --- left: ',length(x[[i]]))
        }
        attr(x,"related_geneset") <- unique(c(sr,hl))
    }else if('msig_gene' %in% names(attributes(x))){
        sr <- attr(x,"msig_gene")
        for (i in seq_len(length(x))) {
            xi = do::paste0_columns(x[[i]][,seq_len(4)],'-----')
            xl <- lapply(hl, function(i) grepl(i,xi,ignore.case = TRUE))
            x[[i]] <- x[[i]][rowSums(data.frame(xl)) == length(xl),]
            message(names(x)[i])
            cat('left: ',nrow(x[[i]]),'\n\n')
            names(x)[i] <- paste0(names(x)[i],' --- left: ',nrow(x[[i]]))
        }
        attr(x,"msig_gene") <- unique(c(sr,hl))
    }
    x
}
