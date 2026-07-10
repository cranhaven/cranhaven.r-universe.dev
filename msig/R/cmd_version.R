

#' version information of MSigDB database
#'
#' @return version dataframe
#' @export
#'
msig_version <- function(){
    message('\nhttps://data.broadinstitute.org/gsea-msigdb/msigdb/release/\n')

    html <- xml2::read_html('https://data.broadinstitute.org/gsea-msigdb/msigdb/release/')
    html |>
        rvest::html_nodes(xpath='//table//tr') |>
        set::grep_and(c('valign','alt')) |>
        set::grep_not_or('Parent Directory') |>
        rvest::html_text() |>
        do::Replace0('\u00A0') |>
        do::Replace0(' {1,}- {1,}') |>
        do::col_split('/',colnames = c('Name','Last modified'))

}
