#' Query related gene sets
#'
#' @param geneSetName one gene set name
#'
#' @return related gene sets from gene set detailed information table
#' @export
#'
#' @examples
#' \donttest{
#' x <- related_geneset('AAANWWTGC_UNKNOWN')
#' x |>
#'     msig_filt('unknown') |>
#'     msig_view('ttt')
#' }
#'

related_geneset <- function(geneSetName){
    if (length(geneSetName) >1) stop('geneSetName must be 1')
    url <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                   geneSetName)
    html <- xml2::read_html(url)
    numb <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Related gene sets') |>
        rvest::xml_nodes(xpath='td/a') |>
        rvest::html_text() |>
        do::Replace0(c('.*show ','\\).*'))
    if (length(numb) == 0){
        message('no related gene set')
    }else{
        numb |>
            paste0(collapse = '\n') |>
            message()
        div <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Related gene sets') |>
            rvest::xml_nodes(xpath='td/div')
        for (i in seq_len(length(numb))) {
            if (i==1) rt <- list()
            ri <- div[i] |>
                rvest::xml_nodes(xpath='a') |>
                rvest::html_text() |>
                tolower() |>
                list()
            names(ri) <- numb[i]
            rt <- c(rt,ri)
        }
        attr(rt,'related_geneset')=geneSetName
        rt
    }


}
