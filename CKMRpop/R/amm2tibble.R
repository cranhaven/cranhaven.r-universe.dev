#' convert an ancestry-matching matrix to a ggplot-able tibble
#'
#' This merely converts a matrix to a tibble that can be plotted
#' easily using ggplot.
#' @param M an ancestry-match matrix
#' @return `amm2tibble()` returns a tibble with three columns:
#'    * `x`: the 1-based index of the row of input matrix,
#'    * `y`: the 1-based index of the column of the input matrix,
#'    * `amm`: the logical value (TRUE/FALSE) of the (x,y)-th cell of the input matrix.
#' @export
#' @examples
#' # convert one of the simple example AMMs to a tibble
#' amm2tibble(example_amms$Father_Offspring_2gen)

amm2tibble <- function(M) {
  tibble(
    x = rep(1:nrow(M), ncol(M)),
    y = rep(1:ncol(M), each = nrow(M)),
    amm = as.vector(M)
  )
}
