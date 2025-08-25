#' @title Build URL Query String
#'
#' @description 
#' Takes named arguments (...) and creates a URL query string starting
#' with 'prefix' with each named pair of arguments separated with 'sep'
#' and each item within a value that is a vector separated by 'value_sep'.
#'
#' Each value part of the named arguments must be coercible to character.
#' For example, list(key1 = "value1", key2 = 42, key3 = c(1, 2, 4)) would
#' be acceptable as it can all be coerced to characters
#'
#' @usage build_url_query_string(prefix = "?", sep = "&", value_sep = ",", ...)
#'
#' @param prefix Prefix to returned URL. Defaults to "?". 
#' @param sep . String separating used to separate in the URL output 
#' each named argument in (...). Defaults to "&".
#' @param value_sep String separating each item in value if the 
#' value part of a named argument is a vector. Defaults to ",".
#' @param ... Any number of named argument pairs where the value must be 
#' coercible to character
#'
#' @examples
#' build_url_query_string()
#' ""
#' build_url_query_string(field1 = "option1")
#' "?field1=option1"
#' build_url_query_string(field1 = "option1", field2 = 42, field3 = c(1, 2, 3))
#' "?field1=option1&field2=42&field3=1,2,3"
#' build_url_query_string(prefix = "/query?", sep = "|", value_sep = ":", 
#'                        field1 = 1, field2 = c(7, 8, 9))
#' "/query?field1=1|field2=7:8:9"
#'
#' @returns A string containing the named arguments parsed into the URL query string format
#' @export

build_url_query_string <- function(prefix = "?", sep = "&", value_sep = ",", ...) {
  # Coerce values in list(...) to character. If value is a vector then coerce and 
  # collapse to string with 'value_sep' separator. I.e. c(1, 2, 3) -> "1,2,3"
  args <- lapply(list(...), function (x) {
    paste0(x, collapse = value_sep)
  })

  # Create url separators depending on number of named arguments
  # If length(args) == 0 -> c(rep("?", 0), rep("&", 0)) -> character(0)
  # If length(args) == 1 -> c(rep("?", 1), rep("&", 0)) -> c("?")
  # If length(args)  > 1 -> c(rep("?", 1), rep("&", length(args) - 1)) -> c("?", "&", ...)
  seps <- c(rep(prefix, length(args) > 0), rep(sep, length(args) - (length(args) > 0)))

  # Paste out the vector of options in the format: "?name1=value1&name2=value2..."
  # Empty string ("") if no named arguments provided
  return(paste0(seps, names(args), rep("=", length(seps)), args, collapse = ""))
}

#' @title Build constituency section of get_boundary_areaname query string
#'
#' @description 
#' Create correctly formatted 'where' part of url query string used by
#' get_boundary_areaname to fetch contituency boundaries
#' 
#' @usage build_constituency_query_string(constituency_list, ...)
#'
#' @param constituency_list Vector of constituency names
#' @param ... Additional strings to be added to the query string
#'
#' @examples
#' build_constituency_query_string("Westminster", "LAD22NM", "3D")
#' "%20(%20'LAD22NM%20%3D%20'Westminster')%20"
#'
#' build_constituency_query_string(c("Westminster", "Tower Hamlets"), "LAD22NM", "3D")
#' "%20(%20'LAD22NM%20%3D%20'Westminster'%20OR%20LAD22NM%20%3D%20'Tower%20Hamlets')%20"
#'
#' @returns A string formatted for 'where' portion of url for querying Open Geography portal
#' @export

build_constituency_query_string <- function(constituency_list, ...) {
  no_spaces_w_quotes <- paste0("'", gsub("\\s", "%20", constituency_list), "'")
  query_string <- paste(paste(..., sep = "%20%"), no_spaces_w_quotes, sep = "%20", collapse = "%20OR%20")

  return(paste0("%20(", query_string, ")%20"))
}

