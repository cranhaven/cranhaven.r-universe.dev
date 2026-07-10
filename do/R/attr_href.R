#' Get hypertext reference attributes
#'
#' @param x A document (from read_html()), node set (from html_elements()), node (from html_element()), or session (from session()).
#'
#' @return hypertext reference attributes
#' @export
#'
attr_href <- function(x){
    rvest::html_attr(x,'href')
}