#' Retrieve Gene set Names from MSigDB database
#' Retrieve gene set names from MSigDB database by the gene set name and collection.
#' the search filed is gene name.
#'
#' @param geneSetName one keyword for gene set name, default is empty
#' @param collection one collection, default is empty
#'
#' @return gene set names
#' @export
#'
#' @examples
#' \donttest{
#' # missing genSetName and collection to get all gene set names
#' x <- browse_msig()
#' # search for gene names include immune
#' x <- browse_msig('immune')
#'
#' x |>
#'     msig_view('cells','response','to','m')
#'
#' # search for gene names include immune in c8
#' browse_msig('immune','c8')
#'
#' # gene names in c8
#' browse_msig('immune','c8')
#' }
#'
#'
browse_msig <- function(geneSetName='',collection=''){
    if (length(geneSetName) >1) stop('geneSetName must be one character')
    if (length(collection) >1) stop('collection must be one character')
    url <- 'http://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp?geneSetName=%s&collection=%s'
    url <- sprintf(url,geneSetName,collection)
    html <- xml2::read_html(url)
    x <- html |>
        rvest::html_nodes(xpath='//a[@href]') |>
        set::grep_and(c('msigdb','cards')) |>
        rvest::html_text() |>
        tolower()
    attr(x,'browse_msig') <- geneSetName
    message('gene sets: ',length(x))
    x
}

