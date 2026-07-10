
#' Show collctions for msigdb_search()
#'
#' @param email email that registered for MSigDB database.
#'
#' @return collections from MsigDB website.
#' @export
#'
#' @examples
#' \donttest{
#' search_show_collection("your email")
#' # or
#' email <- 'your email'
#' search_show_collection()
#' }
search_show_collection <- function(email){
    if (missing(email)) email = get('email',envir = .GlobalEnv)
    h <- httr::GET(URLencode('http://www.gsea-msigdb.org/gsea/msigdb/search.jsp'),
                   httr::authenticate(email, "password"))
    x <- httr::content(h)
    x |>
        rvest::html_nodes(xpath='//select[@name="collection"]//option') |>
        rvest::html_attr('value') |>
        .nchar1()
}
.nchar1 <- function(x) x[nchar(x)>=1]
