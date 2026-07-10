
#' Show contributor for msigdb_search()
#'
#' @param email email that registered for MSigDB database.
#' @return contributors from MsigDB website.
#'
#' @export
#'
#' @examples
#' \donttest{
#' search_show_contributor("your email")
#' # or
#' email <- 'your email'
#' search_show_contributor()
#' }
search_show_contributor <- function(email){
    if (missing(email)) email = get('email',envir = .GlobalEnv)
    h <- httr::GET(URLencode('http://www.gsea-msigdb.org/gsea/msigdb/search.jsp'),
                   httr::authenticate(email, "password"))
    x <- httr::content(h)
    x |>
        rvest::html_nodes(xpath='//select[@name="contributor"]//option') |>
        rvest::html_attr('value') |>
        .nchar1()
}
