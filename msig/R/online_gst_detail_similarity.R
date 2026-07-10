#' Query similarity gene sets
#'
#' @param geneSetName one gene set name
#'
#' @return similarity gene sets
#' @export
#'
#' @examples
#' \donttest{
#' x <- similarity_geneset('REACTOME_DEGRADATION_OF_AXIN')
#' x |>
#'     msig_view()
#'
#' }

similarity_geneset <- function(geneSetName){
    if (length(geneSetName) >1) stop('geneSetName must be 1')
    url <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                   geneSetName)
    html <- xml2::read_html(url)
    numb <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Filtered by similarity') |>
        rvest::xml_nodes(xpath='td/a') |>
        rvest::html_text() |>
        do::Replace0(c('.*show ','\\)\n.*'))
    if (length(numb) == 0){
        message('no related gene set')
    }else{
        numb |>
            paste0(collapse = '\n') |>
            message()
        div <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Filtered by similarity') |>
            rvest::xml_nodes(xpath='td') |>
            rvest::html_table() |>
            as.data.frame()
        colnames(div) <- c('External_ID','External_Name')
        link <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Filtered by similarity') |>
            rvest::xml_nodes(xpath='td/div//a[@href]') |>
            rvest::html_attr('href')
        div$link <- link
        attr(div,'similarity_geneset') <- geneSetName
        div
    }

}
