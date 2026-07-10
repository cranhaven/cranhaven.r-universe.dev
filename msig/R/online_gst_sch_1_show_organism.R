
#' Show organism for msigdb_search()
#'
#' @param email email that registered for MSigDB database.
#' @return organisms from MsigDB website.
#'
#' @export
#'
#' @examples
#' \donttest{
#' search_show_organism("your email")
#' # or
#' email <- 'your email'
#' search_show_organism()
#' }
search_show_organism <- function(email){
    if (missing(email)) email = get('email',envir = .GlobalEnv)
    h <- httr::GET(URLencode('http://www.gsea-msigdb.org/gsea/msigdb/search.jsp'),
                   httr::authenticate(email, "password"))
    x <- httr::content(h)
    x |>
        rvest::html_nodes(xpath='//select[@name="organism"]//option') |>
        rvest::html_attr('value') |>
        .nchar1()
}
