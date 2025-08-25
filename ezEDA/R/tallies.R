#' Plot counts of a category
#'
#' @param data A data frame or tibble
#' @param category_column Unquoted column name of category (can be factor, character or numeric)
#' @return A ggplot plot object
#' @export
#' @examples
#' category_tally(ggplot2::mpg, class)
#' category_tally(ggplot2::diamonds, cut)
category_tally <- function(data, category_column) {
    cat <- rlang::enquo(category_column)
    col_name <- rlang::as_label(cat)
    tab <- table(data[[col_name]])
    data[[col_name]] <- factor(data[[col_name]], levels = names(sort(tab)))
    ggplot(data, aes(!!cat)) + geom_bar() + coord_flip()
}

#' Plot counts of combinations of two category columns
#'
#' @param data A data frame or tibble
#' @param main_category,sub_category Unquoted column names of two categories (can be factor, character or numeric)
#' @param separate Boolean indicating whether the plot should be faceted or not
#' @param position "stack" or "dodge"
#' @return A ggplot plot object
#' @export
#' @examples
#' two_category_tally(ggplot2::mpg, class, drv)
#' two_category_tally(ggplot2::mpg, class, drv, position = "dodge")
#' two_category_tally(ggplot2::mpg, class, drv, separate = TRUE)
#' two_category_tally(ggplot2::diamonds, cut, clarity)
#' two_category_tally(ggplot2::diamonds, cut, clarity, separate = TRUE)
two_category_tally <- function(data, main_category, sub_category,
    separate = FALSE, position = "stack") {
    cat <- rlang::enquo(main_category)
    col_name <- rlang::as_label(cat)
    tab <- table(data[[col_name]])
    data[[col_name]] <- factor(data[[col_name]], levels = names(sort(tab)))
    sub_cat <- rlang::enquo(sub_category)
    data <- col_to_factor(data, sub_cat)
    p <- ggplot(data, aes(!!cat, fill = !!sub_cat)) + geom_bar(position = position) +
        coord_flip()
    if (separate)
        p + guides(fill = FALSE) + facet_wrap(vars(!!sub_cat)) else p
}
