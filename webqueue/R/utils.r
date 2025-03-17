is_int <- function (x) {
  if (!is.integer(x)) return (FALSE)
  if (length(x) != 1) return (FALSE)
  if (is.na(x))       return (FALSE)
  return (TRUE)
}

is_string <- function (x, null_ok = FALSE) {
  if (is.null(x) && null_ok) return (TRUE)
  if (!is.character(x))      return (FALSE)
  if (length(x) != 1)        return (FALSE)
  if (is.na(x))              return (FALSE)
  if (!nzchar(x))            return (FALSE)
  return (TRUE)
}

is_bool <- function (x) {
  if (!is.logical(x)) return (FALSE)
  if (length(x) != 1) return (FALSE)
  if (is.na(x))       return (FALSE)
  return (TRUE)
}

all_named <- function (x) {
  if (length(x) == 0)      return (TRUE)
  if (is.null(names(x)))   return (FALSE)
  if (any(names(x) == '')) return (FALSE)
  return (TRUE)
}

valid_string <- function (x, ok = '', no = '', null_ok = FALSE) {
  
  if (is.null(x) && null_ok) return (TRUE)
  if (!is_string(x))         return (FALSE)
  
  if (nzchar(ok)) {
    ok <- c(as.raw(c(48:57, 65:90, 97:122)), charToRaw(ok))
    return (all(charToRaw(x) %in% ok))
  }
  else {
    no <- c(as.raw(c(0:31, 127)), charToRaw(no))
    return (!any(charToRaw(x) %in% no))
  }
  
}


code_to_msg <- list(
  '100' = "Continue",
  '101' = "Switching Protocols",
  '102' = "Processing",
  '103' = "Early Hints",
  '200' = "OK",
  '201' = "Created",
  '202' = "Accepted",
  '203' = "Non-Authoritative Information",
  '204' = "No Content",
  '205' = "Reset Content",
  '206' = "Partial Content",
  '207' = "Multi-Status",
  '208' = "Already Reported",
  '226' = "IM Used",
  '300' = "Multiple Choices",
  '301' = "Moved Permanently",
  '302' = "Found",
  '303' = "See Other",
  '304' = "Not Modified",
  '307' = "Temporary Redirect",
  '308' = "Permanent Redirect",
  '400' = "Bad Request",
  '401' = "Unauthorized",
  '402' = "Payment Required",
  '403' = "Forbidden",
  '404' = "Not Found",
  '405' = "Method Not Allowed",
  '406' = "Not Acceptable",
  '407' = "Proxy Authentication Required",
  '408' = "Request Timeout",
  '409' = "Conflict",
  '410' = "Gone",
  '411' = "Length Required",
  '412' = "Precondition Failed",
  '413' = "Content Too Large",
  '414' = "URI Too Long",
  '415' = "Unsupported Media Type",
  '416' = "Range Not Satisfiable",
  '417' = "Expectation Failed",
  '418' = "I'm a teapot",
  '421' = "Misdirected Request",
  '422' = "Unprocessable Content",
  '423' = "Locked",
  '424' = "Failed Dependency",
  '425' = "Too Early",
  '426' = "Upgrade Required",
  '428' = "Precondition Required",
  '429' = "Too Many Requests",
  '431' = "Request Header Fields Too Large",
  '451' = "Unavailable For Legal Reasons",
  '500' = "Internal Server Error",
  '501' = "Not Implemented",
  '502' = "Bad Gateway",
  '503' = "Service Unavailable",
  '504' = "Gateway Timeout",
  '505' = "HTTP Version Not Supported",
  '506' = "Variant Also Negotiates",
  '507' = "Insufficient Storage",
  '508' = "Loop Detected",
  '510' = "Not Extended",
  '511' = "Network Authentication Required"
)
