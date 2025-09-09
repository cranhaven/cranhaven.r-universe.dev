
#' Combine str_match and str_locate
#'
#' For every pattern, return the index of the first match of pattern in strings
#'
#' @param patterns Character vector of patterns
#' @param strings Character vector of strings
#'
#' @return Integer vector of length(patterns) where entry i gives the index in strings where pattern i first matched
#' @export
str_locate_match <- function(patterns, strings)
{
  matches <- rep_along(patterns, na_int)
  for (i in seq_along(patterns))
  {
    str_matches <- str_locate(strings, patterns[[i]])
    for (j in seq_along(strings))
    {
      if (!is.na(str_matches[j,"start"]))
      {
        matches[[i]] <- j
        break
      }
    }
  }
  return (matches)
}

#' Format as percentage for output
#'
#' Vectorised conversion
#' @param x Numeric vector
#' @param decimal_places Decimal places to display
#' @param include_plus_sign prepend a "+" to the output if positive (if negative, a "-" must be prepended of course)
#'
#' @return Character vector
#' @export
#'
#' @examples
#' as_percentage_label(0.746) # gives "74.6%"
as_percentage_label <- function(x,
                                decimal_places = 1,
                                include_plus_sign = F)
{
  str_c(
    if_else(include_plus_sign & x>0, "+", ""),
    format(round(x*100, decimal_places), nsmall = decimal_places),
    "%")
}


#' Format numeric value for output
#'
#' Vectorised conversion
#'
#' @param x Numeric vector
#' @param decimal_places Decimal places to display
#' @param remove_trailing_zeroes If the required decimal places are less than decimal places,
#'  should resulting trailing zeros be removed?
#'
#' @return Character vector
#' @export
#'
#' @examples
#' as_formatted_number(0.74167, 2) # gives "0.74"
as_formatted_number <- function(x,
                                decimal_places = 1,
                                remove_trailing_zeroes = T)
{
  x <- trimws(format(round(x, decimal_places), nsmall=decimal_places, scientific=F))
  if (remove_trailing_zeroes)
    x <- str_replace(x, "([0-9])0+$", "\\1")
  x
}

#' Formatting p values
#'
#' Vectorised conversion
#'
#' @param x Numeric vector
#' @param decimal_places Decimal places to display
#' @param prefix Prefix to prepend (default "p=")
#' @param less_than_cutoff Cut-off for small p values. Values smaller than this will be displayed like "p<..."
#' @param remove_trailing_zeroes If the required decimal places are less than decimal places,
#'  should resulting trailing zeros be removed?
#' @param alpha Cut-off for assuming significance, usually 0.05
#' @param ns_replacement If p value is not significant (is > alpha), it will be replace by this string (e.g. "n.s.")
#' If NULL (default), no replacement is performed.
#'
#' Vectorised (in parallel) over x, prefix, less_than_cutoff, alpha and ns_replacement.
#'
#' @return Character vector
#' @export
#'
#' @examples
#' as_formatted_p_value(0.02) # "p=0.02"
#' as_formatted_p_value(0.00056) # "p<0.001"
as_formatted_p_value <- function(x,
                                 decimal_places = 3,
                                 prefix = "p",
                                 less_than_cutoff = 0.001,
                                 remove_trailing_zeroes = T,
                                 alpha = 0.05,
                                 ns_replacement = NULL)
{
  replace_ns <- !is_null(ns_replacement)
  ns_replacement <- if(replace_ns) as.character(ns_replacement) else ""

  if_else(replace_ns & x > alpha,
          ns_replacement,
          if_else(x < less_than_cutoff,
                  str_c(prefix,
                        "<",
                        as_formatted_number(less_than_cutoff,
                                            min(20, 1-floor(log10(less_than_cutoff))))
                  ),
                  str_c(prefix,
                        if_else(str_length(prefix)>0, "=", ""),
                        as_formatted_number(x,
                                            decimal_places = decimal_places,
                                            remove_trailing_zeroes = remove_trailing_zeroes)
                  )
          )
  )
}

