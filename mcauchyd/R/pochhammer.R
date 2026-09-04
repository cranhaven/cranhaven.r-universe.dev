pochhammer <- function(x, n) {
  #' Pochhammer Symbol
  #'
  #' Computes the Pochhammer symbol.
  #'
  #' @aliases pochhammer
  #'
  #' @usage pochhammer(x, n)
  #' @param x numeric.
  #' @param n positive integer.
  #' @return Numeric value. The value of the Pochhammer symbol.
  #' @details The Pochhammer symbol is given by:
  #' \deqn{ \displaystyle{ (x)_n = \frac{\Gamma(x+n)}{\Gamma(x)} = x (x+1) ... (x+n-1) } }
  #' @author Pierre Santagostini, Nizar Bouhlel
  #'
  #' @examples
  #' pochhammer(2, 0)
  #' pochhammer(2, 1)
  #' pochhammer(2, 3)
  #'
  #' @export

  # Arguments:
  #   - x: numeric
  #   - n: integer

  if (n < 0) {
    stop("n must be non negative")
  }

  if (n == 0) {
    return(1)
  }

  if (n > 0) {
    return(prod(x + (1:n) - 1))
  }
}
