#' Distributes a function across a call tree
#'
#' @description
#' Applies the function `f` to all symbols at the leaves of a call tree.
#' 
#' @param x An expression
#' @param f An symbol. The name of the function to distribute across `x`
#' @param supp_args A list of symbols or literals containing other arguments to `f`
#'
#' @noRd
distribute <- function(x, f, supp_args = NULL) {

  switch_expr(
    x,

    constant = x,
    symbol = rlang::call2(f, x, !!!supp_args),

    pairlist = ,
    call = as.call(c(
      x[[1]],
      purrr::map(x[-1], ~ distribute(.x, f, supp_args))
    ))
  )
}
