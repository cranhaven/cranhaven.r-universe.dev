

#' Retrieve gene by Gene Symbol from MsigDB
#' @param local logical, default is FALSE, whether to extract gene symbol from local database
#' @param ... one or more geneset names, which can be little or capital.
#' @return gene symbol
#' @name msig_geneSymbol
#' @export
#'
#' @examples
#' \donttest{
#' genes <- msig_geneSymbol('izadpanah_stem_cell_adipose_vs_bone_dn')
#' genes <- msig_geneSymbol('izadpanah_stem_cell_adipose_vs_bone_dn',
#'                    'REACTOME_DEGRADATION_OF_AXIN')
#' }

msig_geneSymbol <- function(...,local=FALSE) UseMethod('msig_geneSymbol')

#' @rdname msig_geneSymbol
#' @export
#' @method msig_geneSymbol list
msig_geneSymbol.list <- function(...,local=FALSE){
    x <- list(...)[[1]]
    if (length(x)==1){
        x[[1]]$Genesymbol[nchar(x[[1]]$Genesymbol)>0]
    }else{
        lapply(x, function(i) i$Genesymbol[nchar(i$Genesymbol)>0])
    }
}
#' @rdname msig_geneSymbol
#' @export
#' @method msig_geneSymbol data.frame
msig_geneSymbol.data.frame <- function(...,local=FALSE){
    x <- list(...)[[1]]
    x |> msig_gene(info=FALSE) |> msig_geneSymbol()
}
#' @rdname msig_geneSymbol
#' @export
#' @method msig_geneSymbol character
msig_geneSymbol.character <- function(...,local=FALSE){
    gsn <- c(...)
    if (local){
        local_msig(geneset=paste0(gsn,collapse = '|')) |>
            msig_geneSymbol()
    }else{
        url <- 'http://www.gsea-msigdb.org/gsea/msigdb/download_geneset.jsp?geneSetName=%s&fileType=txt'
        url <- sprintf(url,gsn)
        genes <- lapply(url, function(i){
            suppressWarnings(readLines(i))[-c(1,2)]
        })
        if (length(url)==1){
            return(genes[[1]])
        }else{
            names(genes) <- gsn
            return(genes)
        }
    }
}
