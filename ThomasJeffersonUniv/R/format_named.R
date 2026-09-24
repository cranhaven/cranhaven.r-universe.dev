

#' @title format_named
#' 
#' @param x \link[base]{character} \link[base]{vector}, 
#' or a \link[base]{list} of \link[base]{character} object.
#' Input `x` must be named
#' 
#' @param sep \link[base]{character} scalar, see \link[base]{paste}
#' 
#' @param colored \link[base]{logical} scalar, whether use two different color
#' to separate each element, default `TRUE`
#' 
#' @returns
#' Function [format_named] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' x1 = c(a = 'a1', bc = '2\n3')
#' cat(format_named(x1), sep = '\n')
#' noout = lapply(format_named(x1), FUN = message)
#' 
#' x2 = list(a = '1\n2', b = character(), cd = '3\n4', efg = '5\n6\n7')
#' noout = lapply(format_named(x2, colored = FALSE), FUN = message)
#' 
#' x3 = c(a = '1\n2')
#' noout = lapply(format_named(x3), FUN = message)
#' 
#' @keywords internal
#' @export
format_named <- function(x, sep = ': ', colored = TRUE) {
  
  x0 <- trimws(vapply(x, FUN = paste, collapse = ' ', FUN.VALUE = ''))
  x1 <- x0[nzchar(x0)]
  if (!length(nm <- names(x1))) stop('input must be named')
  if (!all(nzchar(nm))) stop('do not allow empty name!')
  
  x2 <- strsplit(x1, split = '\n')
  if (all((nx <- lengths(x2)) == 1L)) { 
    colour_times <- rep(1, times = length(x1))
  } else { # some element(s) contains '\n'
    x1 <- unlist(x2, use.names = FALSE)
    tmp <- character(length = length(x1))
    c_nx <- cumsum(nx)
    tmp[c_nx] <- nm
    nm <- tmp # now allow zchar in `nm`
    colour_times <- c(c_nx[1L], diff(c_nx))
  } 
  
  x_nm <- format.default(nm, justify = 'right')
  
  if (!colored) return(paste(x_nm, x1, sep = sep))
  
  ANSI_head <- if (length(colour_times) == 1L) {
    #rep('\033[1;4;32m', times = colour_times) # bold, underline
    rep('\033[1;32m', times = colour_times) # bold
  #} else suppressWarnings(mapply(rep, c('\033[1;4;32m', '\033[1;4;36m'), times = colour_times))
  } else suppressWarnings(mapply(rep, c('\033[1;32m', '\033[1;36m'), times = colour_times))
  # base::suppressWarnings on length not integer times haha
  return(paste0(
    unlist(ANSI_head, use.names = FALSE),
    x_nm, 
    # '\033[24m', # underline off
    sep, 
    '\033[22m', # bold off
    x1, '\033[0m'
  ))
  
}

# https://gist.github.com/upsilun/4a85ab3bc7cf92e8acde720c6eb7ddea

