# DIN Paper formats

#' The DIN A paper formats
#' @name dina
NULL

#' @rdname dina
#' @return A named list (0-10) of named vectors (long, short) of unit objects with the size in inches of the DIN A paper formats
#' @seealso \link[grid]{unit}
#' @export
dinAFormat <- function()
{
  list(
    "0"=c(long=46.81, short=33.11),
    "1"=c(long=33.11, short=23.39),
    "2"=c(long=23.39, short=16.54),
    "3"=c(long=16.54, short=11.69),
    "4"=c(long=11.69, short=8.27),
    "5"=c(long=8.27,  short=5.83),
    "6"=c(long=5.83,  short=4.13),
    "7"=c(long=4.13,  short=2.91),
    "8"=c(long=2.91,  short=2.05),
    "9"=c(long=2.05,  short=1.46),
    "10"=c(long=1.46, short=1.02)
  )
}

#' @rdname dina
#' @export
dinA_format <- dinAFormat


#' @rdname dina
#' @param n DIN A paper format index (0-10)
#'
#' @return named unit vector (long, short) with the size in inches of the requested DIN A paper format
#' @export
dinA <- function(n)
{
  if (n < 0 || n > 10)
    stop("DIN A ", n, ": value not defined")
  dinAFormat()[[n+1]]
}

#' @rdname dina
#' @return the long side / width in landscape as a unit object in inches
#' @export
dinAWidth <- function(n)
{
  return(dinA(n)[1])
}

#' @rdname dina
#' @export
dinA_width <- dinAWidth


#' @rdname dina
#' @return the short side / height in landscape as a unit object in inches
#' @export
dinAHeight <- function(n)
{
  return(dinA(n)[2])
}

#' @rdname dina
#' @export
dinA_height <- dinAHeight

