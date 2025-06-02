#' @rdname g 
#'
#' @param x Covariate on which the smooth is applied.
#' 
#' @export
s <- function(x, fcons = NULL, s_opts = list()){
  cl <- match.call()
  if (!is.null(fcons)){ 
    fcons <- match.arg(fcons, c("inc", "dec", "cvx", "ccv", 
      "inccvx", "deccvx", "incccv", "decccv"))
  }
  # obt_opts <- deparse(substitute(s_opts))
  # opts_form <- substr(obt_opts, 6, nchar(obt_opts) - 1)
  attributes(x) <- c(attributes(x),
    list(fcons = fcons, s_opts = s_opts, label = deparse(cl$x)))
  return(x)
}