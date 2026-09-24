

#' @title 10-digit US phone number
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param sep \link[base]{character} scalar
#' 
#' @details
#' Function [phone10] converts all US and Canada (+1) phone numbers to 10-digit.
#' 
#' @returns
#' Function [phone10] returns a \link[base]{character} \link[base]{vector} of \link[base]{nchar}-10.
#' 
#' @examples
#' x = c(
#'  '+1(800)275-2273', # Apple
#'  '1-888-280-4331', # Amazon
#'  '000-000-0000'
#' )
#' phone10(x)
#' phone10(x, sep = '-')
#' 
#' @export 
phone10 <- function(x, sep = '') {
  x <- as.character(x)
  xok <- !is.na(x)
  x0 <- x[xok]
  x1 <- gsub('\\ |\\+|\\(|\\)|-', replacement = '', x = x0)
  nc <- nchar(x1)
  if (!all(nc %in% 10:11)) stop('US phone number should be 10 or 11 digits')
  if (any(id11 <- (nc == 11L))) {
    if (!all(substr(x1[id11], start = 1L, stop = 1L) == '1')) stop('US international code must be 1')
    x1[id11] <- substr(x1[id11], start = 2L, stop = 11L)
  } # else do nothing
  x1[x1 %in% c(
    paste(rep('0', times = 10), collapse = '')
  )] <- NA_character_
  
  if (nzchar(sep)) {
    x1 <- paste(
      substr(x1, start = 1L, stop = 3L),
      substr(x1, start = 4L, stop = 6L),
      substr(x1, start = 7L, stop = 10L), sep = sep)
  }
  
  x[xok] <- x1
  return(x)
}




#' @title 5-digit US Zip Code
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @details
#' Function [zip5] converts all US zip codes to 5-digit.
#' 
#' @returns
#' Function [zip5] returns a \link[base]{character} \link[base]{vector} of \link[base]{nchar}-5.
#' 
#' @examples
#' zip5(c('14901', '41452-1423'))
#' @export 
zip5 <- function(x) {
  x <- as.character(x)
  xok <- !is.na(x)
  x0 <- x[xok]
  if (any(nchar(x0) < 5L)) stop('US zip code must be 5 or 9 digits')
  x[xok] <- substr(x0, start = 1L, stop = 5L)
  return(x)
}

