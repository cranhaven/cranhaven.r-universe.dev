#' @rdname is_windows
#' @export
is_64_bit_os <- function()
{
  .Deprecated("is_64_bit")
  is_64_bit()
}

#' @rdname is_windows
#' @export
is_32_bit <- function()
{
  is_xx_bit(32)
}

#' @rdname is_windows
#' @export
is_64_bit <- function()
{
  is_xx_bit(64)
}

is_xx_bit <- function(n)
{
  bits <- 8 * .Machine$sizeof.pointer
  if(bits != n)
  {
    return(false(gettextf("R is %d bit.", bits)))
  }
  TRUE
}

