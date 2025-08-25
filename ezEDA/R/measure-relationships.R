#' Plot the relationship between two measures and optionally highlight a category
#'
#' @param data A data frame or tibble
#' @param measure1,measure2 Unquoted column names of measures
#' @param category Unquoted name of a category (can be factor, character or numeric)
#' @return A ggplot plot object
#' @export
#' @examples
#' \donttest{
#' two_measures_relationship(ggplot2::diamonds, carat, price)
#' two_measures_relationship(ggplot2::diamonds, carat, depth)
#' }
#' two_measures_relationship(ggplot2::mpg, displ, hwy)
#' two_measures_relationship(ggplot2::mpg, cty, hwy)
#' two_measures_relationship(ggplot2::mpg, displ, hwy, class)
two_measures_relationship <- function(data, measure1, measure2, category = NULL) {
    m1 <- rlang::enquo(measure1)
    m2 <- rlang::enquo(measure2)
    cat <- rlang::enquo(category)
    cat_unspecified <- rlang::quo_is_null(cat)
    if (!cat_unspecified) { data <- col_to_factor(data, cat) }
    g = ggplot(data, aes(!!m1, !!m2))
    if (cat_unspecified)
        g + geom_point() + geom_smooth()
    else g + geom_point(mapping = aes(color = !!cat)) +
      geom_smooth() +
      facet_wrap(vars(!!cat))
}

#' Plot the relationship between many measures
#'
#' @param data A data frame or tibble
#' @param ... Unquoted column names of numeric columns (measures)
#' @return A ggplot plot object
#' @export
#' @examples
#' multi_measures_relationship(ggplot2::mpg, hwy, displ)
#' multi_measures_relationship(ggplot2::mpg, cty, hwy, displ)
multi_measures_relationship <- function(data, ...) {
    cols <- rlang::enquos(...)
    colnames <- unlist(lapply(cols, rlang::as_label))
    ggpairs(data[, colnames])
}
