#' @title Calculate The pH of weak acid/base
#' @description Calculate the pH of weak acid or base.
#' @param  ka ionization constant.
#' @param  c concentration.
#' @param  digits digit of the output.
#' @return monoa() will return the pH of weak acid,
#' the monob() will return the pH of weak base.
#' And you can also use the mono() function to
#' replace the monoa()  function and monob() function.
#' @examples
#' monoa(1.4 - 6, 2.35e-2)
#' monoa(2.78e-8, 0.01)
#' monob(1.35e-5, 0.01)
#' monob(2.4 - 6, 1e-4)
#' @export
monoa <- function(ka, c, digits = 2) {
  mono(ka, c, digits = digits, TRUE)
}

#' @rdname monoa
#' @export
monob <- function(ka, c, digits = 2) {
  mono(ka, c, digits = digits, FALSE)
}


#' @rdname monoa
#' @export
#' @param  acid if TRUE, it is equivalent
#' to monoa function; if FALSE, it is
#' equivalent to monob function.
#' @param kw the default is 1e-14
mono <- function(ka, c, digits = 2, acid = TRUE,
                 kw = 1e-14) {
  kw <- kw
  while (ka * c >= 20 * kw & c / ka >= 500) {
    H <- sqrt(ka * c)
    pH <- -log10(H)
    if (acid == FALSE) pH <- 14 - pH
    return(print(pH, digits = digits))
  }
  while (ka * c >= 20 * kw & c / ka <= 500) {
    H <- 0.5 * (-ka + sqrt(ka^2 + 4 * ka * c))
    pH <- -log10(H)
    if (acid == FALSE) pH <- 14 - pH
    return(print(pH, digits = digits))
  }
  while (ka * c <= 20 * kw & c / ka >= 500) {
    H <- sqrt(ka * c + kw)
    pH <- -log10(H)
    if (acid == FALSE) pH <- 14 - pH
    return(print(pH, digits = digits))
  }
}
