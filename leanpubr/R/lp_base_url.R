#' Leanpub Base URL
#'
#' @param secure Should https be used (may be necessary)
#'
#' @return Character vector (length 1) of URL
#' @export
#'
#' @examples
#' lp_base_url()
lp_base_url = function(secure = TRUE) {
  url = "http"
  if (secure) {
    url = paste0(url, "s")
  }
  url = paste0(url, "://leanpub.com")
  return(url)
}
