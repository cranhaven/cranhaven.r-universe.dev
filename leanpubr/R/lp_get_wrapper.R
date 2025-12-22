#' Leanpub Wrapper for GET/POST statements
#'
#' @param slug slug of the project
#' @param endpoint call to the api endpoint
#' @param api_key API key for Leanpub, passed to \code{\link{lp_api_key}}
#' @param secure passed to \code{\link{lp_base_url}} for https
#' @param verbose print diagnostic messages
#' @param ... additional options to pass to \code{\link[httr]{GET}}
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @export
#'
#' @importFrom httr GET stop_for_status warn_for_status content
#' @importFrom httr POST content_type_json content_type
#' @importFrom xml2 read_html
#' @examples
#' if (lp_have_api_key()) {
#' stat = lp_get_wrapper(slug = "neuroimagingforstatisticians",
#'     endpoint = "/job_status", nonstop = TRUE, error = FALSE)
#' }
lp_get_wrapper = function(
  slug,
  endpoint,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = list(...)
  if ("error" %in% names(L)) {
    error = L$error
  } else {
    error = TRUE
  }
  L$error = NULL
  api_key = lp_api_key(api_key = api_key, error = error)

  url = lp_base_url(secure = secure)
  path = paste0("/", slug, endpoint)
  ending = ".json"
  if ("add_json" %in% names(L)) {
    add_json = L$add_json
    L$add_json = NULL
    if (!add_json) {
      ending = ""
    }
  }
  path = paste0(path, ending)

  url = paste0(url, path)

  if ("query" %in% names(L)) {
    query = L$query
  } else {
    query = list()
  }
  query$api_key = api_key
  L$query = query
  L$verbose = verbose
  L$url = url

  L = do.call(get_results, args = L)
  return(L)
}

#' @rdname lp_get_wrapper
#' @importFrom jsonlite toJSON
#' @export
lp_post_wrapper = function(
  slug,
  endpoint,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {

  args = list(...)
  if ("error" %in% names(args)) {
    error = args$error
    args$error = NULL
  } else {
    error = TRUE
  }
  api_key = lp_api_key(api_key = api_key, error = error)

  url = lp_base_url(secure = secure)
  path = paste0("/", slug, endpoint)
  path = paste0(path, ".json")
  url = paste0(url, path)
  if (verbose > 1) {
    message(paste0("URL is: ", url))
  }

  if ("body" %in% names(args)) {
    body = args$body
  } else {
    body = list()
  }
  body$api_key = api_key
  args$body = body
  args$url = url
  args$verbose = verbose

  L = do.call(post_type, args = args)
    # url = url,
    # body = body,
    # verbose = verbose,
    # ...)

  return(L)

}
