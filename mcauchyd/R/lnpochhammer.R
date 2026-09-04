lnpochhammer <- function(x, n) {
  #' Logarithm of the Pochhammer Symbol
  #'
  #' Computes the logarithm of the Pochhammer symbol.
  #'
  #' @aliases lnpochhammer
  #'
  #' @usage lnpochhammer(x, n)
  #' @param x numeric.
  #' @param n positive integer.
  #' @return Numeric value. The logarithm of the Pochhammer symbol.
  #' @details The Pochhammer symbol is given by:
  #' \deqn{ \displaystyle{ (x)_n = \frac{\Gamma(x+n)}{\Gamma(x)} = x (x+1) ... (x+n-1) } }
  #' So, if \eqn{n > 0}:
  #' \deqn{ \displaystyle{ log\left((x)_n\right) = log(x) + log(x+1) + ... + log(x+n-1) } }
  #' 
  #' If \eqn{n = 0}, \eqn{\displaystyle{ log\left((x)_n\right) = log(1) = 0}}
  #' @seealso [pochhammer()]
  #' @author Pierre Santagostini, Nizar Bouhlel
  #'
  #' @examples
  #' lnpochhammer(2, 0)
  #' lnpochhammer(2, 1)
  #' lnpochhammer(2, 3)
  #'
  #' @export
  
  # Arguments:
  #   - x: numeric
  #   - n: integer
  
  if (n < 0) {
    stop("n must be non negative")
  }
  
  if (n == 0) {
    return(0)
  }
  
  if (n > 0) {
    y <- x + (1:n) - 1
    return(sum(log(abs(y)) + (y < 0)*pi*1i))
    # y <- matrix(x, nrow = n, ncol = length(x), byrow = TRUE) +
    #   matrix(1:n, nrow = n, ncol = length(x), byrow = FALSE) - 1
    # return(colSums(log(abs(y)) + (y < 0)*pi*1i))
  }
}
