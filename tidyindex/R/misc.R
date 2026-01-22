#' A ggplot2 theme for benchmarking the index series
#'
#' @param yintercept intercept
#' @param linetype linetype
#'
#' @return a ggplot2 object
#' @export
#' @examples
#' if (require("ggplot2", quietly = TRUE) ){
#' dplyr::tibble(x = 1:100, y = rnorm(100, sd = 2)) |>
#'   ggplot(aes(x = x, y =y )) +
#'   geom_line() +
#'   theme_benchmark()
#' }
theme_benchmark <- function(yintercept = -2, linetype = "dashed"){

  list(
    geom_hline(yintercept = yintercept, linetype = linetype),
    theme_bw(),
    theme(panel.grid = element_blank(),
          legend.position = "bottom")
  )
}
