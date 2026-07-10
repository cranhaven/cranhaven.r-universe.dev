#' @export
#' @method msig_gene data.frame
#' @rdname msig_gene
msig_gene.data.frame <- function(...,list=TRUE,info=TRUE){
    x <- list(...)[[1]]
    ck <- msigdb$standard_name %in% x$standard_name
    gene <- msigdb$gene[ck]
    sdn <- msigdb$standard_name[ck]
    sdnlen <- max(nchar(sdn))
    x <- lapply(seq_len(length(gene)), function(i){
        xi <- do::col_split(gene[i],'\\|') |>
            as.character() |>
            col_split2(',',colnames = c('Ensembl','Genesymbol','EntrezId'))
        if (info){
            Ensembl_n <- sum(nchar(xi$Ensembl)  >0 & !is.na(xi$Ensembl))
            Genesymbol_n <- sum(nchar(xi$Genesymbol)  >0 & !is.na(xi$Genesymbol))
            cat(paste0(crayon::red(sdn[i]),do::rep_n(' ',sdnlen-nchar(sdn[i])+1)),
                'Ensembl:',paste0(Ensembl_n,','),
                'Genesymbol: ',Genesymbol_n,'\n')
        }

        xi
    })
    names(x) <- msigdb$standard_name[ck]
    if (!list){
        x <- lapply(seq_len(length(x)), function(i){
            j <- x[[i]]
            j$geneset <- names(x)[i]
            j
        })
        x <- do.call(rbind,x)
        x <- x[,c('geneset', "Ensembl", "Genesymbol", "EntrezId")]
    }
    attr(x,'msig_gene') <- names(x)
    x
}


col_split2 <- function (x, split, reg_expr, colnames){
    if (any(is.data.frame(x), is.matrix(x), is.array(x))) {
        stop("x must be a vector")
    }
    x = as.vector(x)
    if (!missing(split)) {
        if (length(split) > 1) {
            x = do::Replace(data = x, from = split[-1], to = split[1])
            split = split[1]
        }
        f = strsplit(x = x, split = split)
    }
    else if (!missing(reg_expr)) {
        f = strsplit(x = x, split = reg_expr, fixed = FALSE)
    }
    f.df <- function(x) {
        df = data.frame(t(x))
        colnames(df) = paste0("x", seq_len(ncol(df)))
        df
    }
    f2 = lapply(f, function(x) f.df(x))
    f3 = do.call(plyr::rbind.fill, f2)
    if (missing(colnames)) {
        return(f3)
    }
    else {
        if (length(colnames) > ncol(f3)) {
            colnames(f3) = colnames[seq_len(ncol(f3))]
        }
        else {
            colnames(f3)[seq_len(length(colnames))] = colnames
        }
        return(f3)
    }
}
