#' Get or set the system locale
#'
#' Wrappers to \code{Sys.getlocale} and \code{Sys.setlocale} for getting and
#' setting the system locale.
#'
#' @param simplify If \code{TRUE}, the locale settings are returned as a 
#' character vector, otherwise a list.
#' @param remove_empty_categories if \code{TRUE}, don't include empty categories.
#' @param ... Name-value pairs of locale categories to set.
#' @param l A list, as an alternative method of passing local categories to set.
#' @return A named list or vector giving the system locale names. 
#' \code{sys_set_locale} invisibly returns the locale settings *before* making 
#' changes (like \code{setwd} and \code{options} do).
#' @examples
#' (current_locale <- sys_get_locale())
#' 
#' # Output simplified to character vector
#' sys_get_locale(simplify = TRUE)
#' \dontrun{
#' # Not run since it (temporarily) affects system settings
#' english <- if(is_windows()) "English.United_Kingdom" else 
#'   if(is_mac()) "en_GB" else 
#'   if(is_linux()) "en_GB.utf8" else
#'   "en"
#' sys_set_locale(LC_MONETARY = english)
#' sys_get_locale()
#' sys_set_locale(l = current_locale)  #restore everything
#' }
#' @seealso \code{\link[base]{Sys.getlocale}}.
#' @export
sys_get_locale <- function(simplify = FALSE, remove_empty_categories = TRUE)
{
  categories <- c(
    "LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_NUMERIC", 
    "LC_TIME", "LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"
  )
  names(categories) <- categories
  
  locale <- lapply(categories, Sys.getlocale)
  if(remove_empty_categories)
  {
    locale <- locale[nzchar(locale)]
  }
  if(simplify) 
  {
    unlist(locale)
  } else 
  {
    locale
  }
}

#' @rdname sys_get_locale
#' @importFrom assertive.base merge_dots_with_list
#' @export
sys_set_locale <- function(..., l = list())
{
  old_locale <- sys_get_locale()
  values <- merge_dots_with_list(..., l = l)
  categories <- names(values)
  categories <- match.arg(
    categories,
    locale_categories(),
    several.ok = TRUE
  )
  
  for(i in seq_along(values))
  {
    Sys.setlocale(categories[i], values[[i]])
  }
  invisible(old_locale)
}

#' Allowed locale categories.
#'
#' The categories of locale that can be gotten/set.
#'
#' @param include_all If \code{TRUE}, the value \code{LC_ALL} is included.
#' @param include_unix If \code{TRUE}, the extra unix-only values are included.
#' @return A character vector of locale categories.
#' @seealso \code{\link{sys_get_locale}}.
#' @noRd
locale_categories <- function(include_all = TRUE, include_unix = is_unix())
{
  allowed_categories <- c(
    if(include_all) "ALL",
    "COLLATE", "CTYPE", "MONETARY", "NUMERIC", "TIME",
    if(include_unix) c("MESSAGES", "PAPER", "MEASUREMENT")
  )
  paste0("LC_", allowed_categories)
}
