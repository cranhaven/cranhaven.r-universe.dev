#' Leanpub Sales Information
#'
#' @inheritParams lp_get_wrapper
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @export
#'
lp_sales = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = lp_get_wrapper(
    slug = slug,
    endpoint = "/sales",
    api_key = api_key,
    secure = secure,
    verbose = verbose, ...)
  return(L)

}

#' @rdname lp_sales
#' @export
#' @param page page to extract for sales
#' @examples \dontrun{
#' if (lp_have_api_key()) {
#' res1 = lp_all_sales(slug = "biostatmethods")
#' nurl = lp_next_url(res1)
#' res_next = lp_next(res1)
#'
#' }
#' }
lp_all_sales = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  page = NULL,
  ...) {

  args = list(
    slug = slug,
    endpoint = "/individual_purchases",
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    ...)
  if (!is.null(page)) {
    query = args$query
    query$page = page
    args$query = query
  }
  L = do.call(lp_get_wrapper, args = args)
  return(L)
}


#' @rdname lp_sales
#' @param result an object of class `lp_results`, which must have
#' a `response` slot in a list
#' @export
lp_next_url = function(result) {
  url = result$response$url
  parsed_url = httr::parse_url(url)
  page = parsed_url$query$page
  if (is.null(page)) {
    page = 1
  }
  page = as.numeric(page)
  page = page + 1
  parsed_url$query$page = page
  url = httr::build_url(parsed_url)
  return(url)
}

#' @rdname lp_sales
#' @export
lp_next = function(result, ...) {
  url = lp_next_url(result = result)
  res = get_results(
    url = url, ...)
  return(res)
}
