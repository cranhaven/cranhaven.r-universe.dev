#' Show collection of MSigDB database
#'
#' @return show all collection in MSigDB in web page http://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp.
#'     For chromosome, we should treat as collection together.
#' @export
#'
#' @examples
#' \donttest{
#' browse_show_collection()
#' }
#'
browse_show_collection <- function(){
    url='http://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp'
    html <- xml2::read_html(url)
    html |>
        rvest::html_nodes(xpath='//a[@href]') |>
        set::grep_and(c("collection",'msigdb','genesets','jsp')) |>
        rvest::html_attr('href') |>
        do::Replace0('.*collection=')
}
