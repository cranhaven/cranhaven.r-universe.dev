

#' Retrieve gene by Gene set Name
#'
#' @param ... one or more geneset names, which can be little or capital.
#' @param list logical, default is FALSE, whether to show result by list.
#' @param info logical, whether to show information about gene set.
#' @return Print detail information about the geneset, number of genes and return all gene names.
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @name msig_gene
#' @examples
#' \donttest{
#' genes <- msig_gene('izadpanah_stem_cell_adipose_vs_bone_dn',
#'                    'REACTOME_DEGRADATION_OF_AXIN')
#' genes |>
#'     msig_view()
#' }
#'
#'
msig_gene <- function(...,list=TRUE,info=TRUE) UseMethod('msig_gene')

