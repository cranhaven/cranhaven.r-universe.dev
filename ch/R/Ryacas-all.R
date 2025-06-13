#' @title tex format and $$...$$
#' @importFrom  Ryacas tex
#' @author Chai
#' @description output to Console and clipboard.
#' You can better check the correctness of the output.
#' @return lat_fmt() will output to Console.
#' latex_fmt() will output to clipboard.
#' @param  x  symbol object,for more see \code{\link[Ryacas]{ysym}}.
#' @export
lat_fmt <- function(x) {
  x <- Ryacas::tex(x)
  x <- paste("$$", x, "$$")
  cat(x)
}


#' @rdname lat_fmt
#' @export
latex_fmt <- function(x) {
  x <- Ryacas::tex(x)
  x <- paste("$$", x, "$$")
  cat(x, file = "clipboard")
}



#' @title Extracting coefficients from polynomials
#' @importFrom  Ryacas ysym y_fn as_r
#' @importFrom magrittr %>%
#' @importFrom pracma polyroots
#' @importFrom  polynom polynomial
#' @author Chai
#' @description sym2ploy can extract coefficients from
#' polynomials and gives the roots of polynomials.
#' The roots is  calculated from \code{\link[base]{polyroot}} and
#' the \code{\link[pracma]{polyroots}} function.
#' @return sym2poly() returns Coefficients and the roots.
#' sym2coef() only returns coefficients.
#' @param  x  The polynomials,for examples,
#' '3*x^2 + 6*x^6 + 2 + 25*x'.
#' @param  var, The var from polynomials, for examples,
#' the var of '3*x^2 + x^6 + x*8 + x*5^2' is 'x'.
#' @examples
#' sym2poly("3*x^2 + x^5 + x*8")
#' @export
sym2poly <- \(x, var = "x") {
  if (class(x) == "character") {
    x <- Ryacas::ysym(x)
  } else if (class(x) == "yac_symbol") {
    x <- x
  }
  else {
    return("x must be polynomial character or yac_symbol")
  }
  x %>%
    y_fn("Degree", var) -> h0
    paste0("0 .. ", h0) -> h1
  x %>%
    y_fn("Coef", var, h1) %>%
    as_r() -> x1
  polynom::polynomial(x1) -> p1
  x1 %>% polyroot() -> y1
  x1 %>%
    rev() %>%
    pracma::polyroots() -> y2
  list(
    coeffs = x1,
    polynomial = p1,
    polyroot = y1,
    pracma_polyroots = y2
  )
}

#' @rdname sym2poly
#' @export
 sym2coef <- function(x,var = 'x'){
   if (class(x) == "character") {
     x <- Ryacas::ysym(x)
   } else if (class(x) == "yac_symbol") {
     x <- x
   }
   else {
     return("x must be polynomial character or yac_symbol")
   }
   x %>%
     y_fn("Degree", var) -> h0
   paste0("0 .. ", h0) -> h1
   x %>%
     y_fn("Coef", var, h1) %>%
     as_r() -> x1
   x1
 }