


#' @title Hexavigesimal (Base 26L) and Excel Columns
#' 
#' @description
#' Convert between decimal, hexavigesimal in C-style, and hexavigesimal in Excel-style.
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}, 
#' which consists of (except missingness)
#' only letters `A` to `Z` and `a` to `z`.
#'  
#' @details
#' Convert between decimal, hexavigesimal in C-style, and hexavigesimal in Excel-style.
#' 
#' \tabular{lrrrrrrrrrr}{
#' Decimal \tab 0 \tab 1 \tab 25 \tab 26 \tab 27 \tab 51 \tab 52 \tab 676 \tab 702 \tab 703 \cr
#' Hexavigesimal; C \tab `0` \tab `1` \tab `P` \tab `10` \tab `11` \tab `1P` \tab `20` \tab `100` \tab `110` \tab `111` \cr
#' Hexavigesimal; Excel \tab `0` \tab `A` \tab `Y` \tab `Z` \tab `AA` \tab `AY` \tab `AZ` \tab `YZ` \tab `ZZ` \tab `AAA` \cr
#' }
#' 
#' Function [Excel2C] converts 
#' from hexavigesimal in Excel-style 
#' to hexavigesimal in C-style.
#' 
#' Function [Excel2int] converts 
#' from hexavigesimal in Excel-style 
#' to decimal, using function [Excel2C] and \link[base]{strtoi}.
#' 
#' @returns 
#' 
#' Function [Excel2int] returns an 
#' \link[base]{integer} \link[base]{vector}.
#' 
#' Function [Excel2C] returns a 
#' \link[base]{character} \link[base]{vector}.
#' 
#' @references 
#' \url{http://mathworld.wolfram.com/Hexavigesimal.html}
#' 
#' @examples 
#' int1 = c(NA_integer_, 1L, 25L, 26L, 27L, 51L, 52L, 676L, 702L, 703L)
#' Excel1 = c(NA_character_, 'A', 'Y', 'Z', 'AA', 'AY', 'AZ', 'YZ', 'ZZ', 'AAA')
#' C1 = c(NA_character_, '1', 'P', '10', '11', '1P', '20', '100', '110', '111')
#' stopifnot(identical(int1, Excel2int(Excel1)), identical(int1, strtoi(C1, base = 26L)))
#' 
#' int2 = c(NA_integer_, 1L, 4L, 19L, 37L, 104L, 678L)
#' Excel2 = c(NA_character_, 'a', 'D', 's', 'aK', 'cZ', 'Zb')
#' stopifnot(identical(int2, Excel2int(Excel2)))
#' Excel2C(Excel2)
#' 
#' head(swiss[Excel2int('A')])
#' @name hexavigesimalExcel
#' @seealso \link[base]{as.hexmode}
#' @export
Excel2int <- function(x) {
  xok <- !is.na(x)
  x <- Excel2C(x)
  z <- strtoi(x, base = 26L)
  if (any(is.na(z[xok]) | z[xok] < 0)) stop('should not happen')
  return(z)
}



#' @rdname hexavigesimalExcel
#' @export
Excel2C <- function(x) {
  ret <- rep(NA_character_, time = length(x))
  
  xok <- !is.na(x)
  x1 <- toupper(x[xok])
  if (any(id <- grepl(pattern = '[^A-Z]', x = x1))) stop(paste0(sQuote(x1[id]), collapse = ', '), ' contains characters that are not A-Z')
  
  x2 <- strsplit(x1, split = '', fixed = TRUE)
  
  excel2c <- function(x_) {
    # x_ = x2[[1L]]
    id <- match(x_, table = LETTERS)
    if (anyNA(id)) stop('wont happen')
    id <- c(0L, id) 
    while (length(i26 <- which(id == 26L))) {
      id[i26] <- 0L
      id[i26 - 1L] <- id[i26 - 1L] + 1L
    }
    tmp <- character(length(id))
    i9 <- (id <= 9L)
    tmp[i9] <- as.character(id[i9])
    tmp[!i9] <- letters[id[!i9] - 9L]
    out <- paste(tmp, collapse = '')
    return(gsub(pattern = '^0+', replacement = '', x = out))
    # ?base::strtoi handles preceeding '0' correctly; i.e., 
    # stopifnot(identical(strtoi('01', base = 26L), strtoi('1', base = 26L)))
    # I remove leading 0's just for pretty output
  }
  
  ret[xok] <- vapply(x2, FUN = excel2c, FUN.VALUE = '')
  return(ret)
}


