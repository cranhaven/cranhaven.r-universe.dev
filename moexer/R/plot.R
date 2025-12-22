#' Plot Candles
#'
#' @param x A candles tibble as returned by [get_candles()]
#' @param ... For compatibility with the generic; ignored
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' \dontrun{
#' get_candles(secid = 'SBER', from = '2020-01-01') |>
#'     plot()
#' }
plot.MoexCandles <- function(x, ...) {
    x |>
        mutate(
            direction = factor(case_when(
                open <= close ~
                    'up',
                TRUE ~
                    'down'
            ))
        ) |>
        ggplot() +
        geom_boxplot(
            aes(
                x = begin,
                lower = pmin(open, close),
                upper = pmax(open, close),
                ymin = low,
                ymax = high,
                middle = low,
                group = begin,
                fill = direction
            ),
            stat = 'identity',
            fatten = 0
        ) +
        facet_grid(cols = vars(secid))
}
