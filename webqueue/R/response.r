
#' Compile an HTTP response.
#' 
#' If your WebQueue's `handler` function returns a list, json object, character
#' vector, or scalar integer, `response()` will be used to transform that 
#' result into an HTTP response.\cr\cr
#' You may also call `response()` within your handler to better customize the 
#' HTTP response. Or, return a result of class 'AsIs' to have that object 
#' passed directly on to 'httpuv'.
#' 
#' @param body      The content. A list will be encoded as JSON. A scalar 
#'                  integer will be interpreted as a status. A character vector
#'                  will be concatenated with no separator.
#' @param status    A HTTP response status code.
#' @param headers   A named character vector of HTTP headers. A list-like
#'                  object is acceptable if all elements are simple strings.
#' @param ...       Objects created by `header()` and/or `cookie()`. Or 
#'                  key-value pairs to add to `headers`.
#' 
#' @return A `<response/AsIs>` object. Essentially a list with elements named
#'         `body`, `status`, and `headers` formatted as 'httpuv' expects.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     response(list(name = unbox('Peter'), pi = pi))
#'          
#'     response(307L, Location = '/new/file.html')
#'     
#'     # The `body` and `status` slots also handle header objects.
#'     response(cookie(id = 123, http_only = TRUE))
#'     
#'     # Allow javascript to access custom headers.
#'     uid <- header('x-user-id'    = 100, expose = TRUE)
#'     sid <- header('x-session-id' = 303, expose = TRUE)
#'     response(uid, sid)
#'     

response <- function (body = NULL, status = 200L, headers = NULL, ...) {
  
  headers <- c(as.list(headers), ...)
  if (inherits(status, 'wq_header')) { headers <- c(status, headers); status <- 200L }
  if (inherits(body,   'wq_header')) { headers <- c(body,   headers); body   <- NULL }
  
  for (i in seq_along(headers)) {
    headers[[i]] <- as.character(headers[[i]])
    stopifnot(
      valid_string(names(headers)[[i]], ok = '-_'),
      valid_string(headers[[i]], ok = '_ :;.,\\/"\'?!(){}[]@<>=-+*#$&`|~^%')
    )
  }
  
  if (is_int(body)) { status <- body; body <- NULL }
  status <- as.integer(status)
  stopifnot(status >= 100L, status < 600L)
  
  if (!inherits(body, c('list', 'json', 'character', 'numeric', 'NULL', 'ls_str')))
    cli_abort('`body` must be of class list, character, json, or NULL, not {.type {body}}.')
  
  # Set content-type unless it's already set.
  if (!'content-type' %in% tolower(names(headers)))
    headers[['Content-Type']] <- {
      if (inherits(body, c('list', 'json'))) { 'application/json; charset=utf-8' }
      else if (inherits(body, 'character'))  { 'text/html; charset=utf-8'        }
    }
  
  # Combine multiple 'Access-Control-Expose-Headers' into one.
  aceh <- tolower(names(headers)) == 'access-control-expose-headers'
  if (sum(aceh) > 1) {
    merged  <- paste(collapse = ', ', headers[aceh])
    headers <- headers[!aceh]
    headers[['Access-Control-Expose-Headers']] <- merged
  }
  
  if (inherits(body, 'ls_str'))             body <- paste(capture.output(body), collapse = '\n')
  if (inherits(body, 'list'))               body <- toJSON(body, null = 'null')
  if (inherits(body, c('json', 'numeric'))) body <- as.character(body)
  if (length(body) > 0) { body <- paste0(body, collapse = '')         }
  else                  { body <- code_to_msg[[as.character(status)]] }
  
  resp <- structure(
    .Data = list(body = body, status = status, headers = headers),
    class = c('wq_response', 'AsIs') )
  
  return (resp)
}



#' Assemble an HTTP header.
#' 
#' See https://developer.mozilla.org/en-US/docs/Glossary/Response_header for 
#' example response headers and their purpose.
#' 
#' @param ...       A single key-value pair.
#' @param expose    Allow javascript to read this header.
#' @param name      Explicitly set the name (key) in the key-value pair.
#' @param value     Explicitly set the value in the key-value pair.
#' 
#' @return A 'header' object that can be passed to `response()`.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     header(name = 'Location', value = '/index.html')
#'     
#'     Location <- '/index.html'
#'     header(Location)
#'     
#'     response(307L, header(Location = '/index.html'))
#'     
#'     # Allow javascript to access a header value
#'     header('x-user-id' = 100, expose = TRUE)
#'     

header <- function (..., expose = FALSE, name = ...names(), value = ..1) {
  
  header_name  <- name[[1]] %||% as.character(substitute(...))
  header_value <- as.character(value)
  
  stopifnot(
    valid_string(header_name,  ok = '-_'),
    valid_string(header_value, ok = '_ :;.,\\/"\'?!(){}[]@<>=-+*#$&`|~^%')
  )
  
  hdr        <- list(header_value)
  names(hdr) <- header_name
  
  if (isTRUE(expose))
    hdr <- c(hdr, list('Access-Control-Expose-Headers' = header_name))
  
  class(hdr) <- 'wq_header'
  return (hdr)
  
}



#' Assemble an HTTP cookie.
#' 
#' See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie for 
#' a more in-depth description of each parameter's purpose.
#' 
#' @param ...          A single key-value pair.
#' @param max_age      The number of seconds until expiration. Omit to create a 
#'                     session cookie. `Inf` is mapped to 34560000L (400 days).
#' @param domain       Send with requests to this host.
#' @param path         Send with requests to this path.
#' @param same_site    `'Strict'`, `'Lax'`, or `'None'`.
#'                     `secure` required for `'None'`.
#' @param secure        Only send over HTTPS.
#' @param http_only     Disallow javascript access.
#' @param partitioned   Use partitioned storage. `secure` required.
#' @param name          Explicitly set the name (key) in the key-value pair.
#' @param value         Explicitly set the value in the key-value pair.
#' 
#' @return A 'header' object that can be passed to `response()`.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     cookie(xyz = 123, max_age = 3600, http_only = TRUE)
#'     
#'     token <- 'randomstring123'
#'     cookie(token)
#'     
#'     response(cookie(token = 'randomstring123'))
#'     

cookie <- function (
    ...,
    max_age = NULL, domain = NULL, path = NULL, same_site = 'Lax', 
    secure = FALSE, http_only = FALSE, partitioned = FALSE, 
    name = ...names(), value = ..1 ) {
  
  cookie_name  <- name[[1]] %||% as.character(substitute(...))
  cookie_value <- as.character(value)
  
  if (identical(max_age, Inf)) max_age <- 34560000L
  
  stopifnot(
    valid_string(cookie_name,  no = ' ()<>@,;:\\"/[]?={}'),
    valid_string(cookie_value, no = ' ",;\\'),
    is.null(max_age) || is_int(max_age <- as.integer(max_age)),
    valid_string(domain, ok = '.-',                 null_ok = TRUE),
    valid_string(path,   ok = ".-_~!$&'()*+,=:@%/", null_ok = TRUE),
    same_site %in% c('None', 'Lax', 'Strict'),
    is_bool(secure),
    is_bool(http_only),
    is_bool(partitioned)
  )
  
  cookie_string <- paste(collapse = '; ', c(
    paste0(cookie_name, '=', cookie_value),
    if (!is.null(max_age))  paste0('Max-Age=',  max_age),
    if (!is.null(domain))   paste0('Domain=',   domain),
    if (!is.null(path))     paste0('Path=',     path),
    if (same_site != 'Lax') paste0('SameSite=', same_site),
    if (secure)             'Secure',
    if (http_only)          'HttpOnly',
    if (partitioned)        'Partitioned'
  ))
  
  hdr <- header(name = 'Set-Cookie', value = cookie_string)
  
  return (hdr)
}



#' Ensure a list becomes a JSON object.
#' 
#' This function returns a list that `jsonlite::toJSON()` will always encode as 
#' `{}`.
#' 
#' @param x   A list, or list-like object.
#' 
#' @return A list with the names attribute set.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     updates <- list()
#'     
#'     response(list(updates = updates))
#'     
#'     response(list(updates = js_obj(updates)))
#'     

js_obj <- function (x = list()) {
  x <- as.list(x, all.names = TRUE)
  if (is.null(names(x)))
    names(x) <- character(length(x))
  return (x)
}


#' Print a response object.
#' 
#' @param x   An object of class `response`.
#' @param ...   Not used.
#' 
#' @noRd
#' @export
print.wq_response <- function (x, ...) {
  
  stopifnot(
    is.list(x),
    length(x) == 3,
    is_int(x$status),
    is.list(x$headers),
    all_named(x$headers),
    is_string(x$body, null_ok = TRUE),
    all(sapply(x$headers, is_string))
  )
  
  msg <- code_to_msg[[as.character(x$status)]]
  cat(style_bold(paste('HTTP/1.1', x$status, msg)), '\n', sep = '')
  
  for (i in seq_along(x$headers))
    cat(paste0(names(x$headers)[[i]], ': ', x$headers[[i]]), '\n', sep = '')
  
  if (!is.null(x$body)) {
    
    cat('\n')
    
    if (nchar(x$body) > 50) {
      ct <- which(tolower(names(x$headers)) == 'content-type')
      ct <- if (length(ct) > 0) tolower(x$headers[[ct[[1]]]]) else ''
      if (startsWith(ct, 'application/json')) x$body <- prettify(x$body)
    }
    
    x$body <- strsplit(x$body, '\n', fixed = TRUE)[[1]]
    width  <- min(getOption('width', 100L), 100L) - 5L
    
    for (i in seq_len(min(length(x$body), 10))) {
      line <- x$body[[i]]
      post <- if (nchar(line) > width) style_italic(col_grey('...'))
      cat(substr(line, 1, width), post, '\n', sep = '')
    }
    
    if (length(x$body) > 10) {
      line <- paste('___', length(x$body) - 10, 'more lines.', '___')
      cat(style_italic(col_grey(line)))
    }
    
  }
  
  invisible()
}




#' Print a header object.
#' 
#' @param x   An object of class `header`.
#' @param ...   Not used.
#' 
#' @noRd
#' @export
print.wq_header <- function (x, ...) {
  for (i in seq_along(x))
    cat(names(x)[[i]], ': ', x[[i]], '\n', sep = '')
}
