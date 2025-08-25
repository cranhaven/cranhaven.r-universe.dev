#' Plot the contribution of different categories to a measure
#'
#' @param data A data frame or tibble
#' @param category Unquoted name of category (can be factor, character or numeric)
#' @param measure Unquoted name of measure
#' @return A ggplot plot object
#' @export
#' @examples
#' category_contribution(ggplot2::diamonds, cut, price)
#' category_contribution(ggplot2::diamonds, clarity, price)
category_contribution <- function(data, category, measure) {
    cat <- rlang::enquo(category)
    meas <- rlang::enquo(measure)
    name = paste("total_", rlang::as_label(meas), sep = "")
    name_sym = rlang::sym(name)
    data %>% group_by(!!cat) %>% summarize(`:=`(!!name_sym, sum(!!meas))) %>%
        mutate(`:=`(!!cat, reorder(!!cat, !!name_sym))) %>% ggplot(aes(!!cat,
        !!name_sym)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) +
        coord_flip()
}

#' Plot the contribution to a measure by combinations of two categories
#'
#' @param data A data frame or tibble
#' @param category1,category2 Unquoted names of category columns (can be factor, character or numeric)
#' @param measure Unquoted name of measure
#' @param separate Boolean to indicate whether the plots for different combinations should be in different facets
#' @return A ggplot plot object
#' @export
#' @examples
#' two_category_contribution(ggplot2::diamonds, cut, clarity, price)
#' two_category_contribution(ggplot2::diamonds,  clarity, cut, price, separate = TRUE)
two_category_contribution <- function(data, category1, category2, measure,
    separate = FALSE) {
  tot <- NULL
    cat_1 <- rlang::enquo(category1)
    cat_2 <- rlang::enquo(category2)
    data <- col_to_factor(data, cat_2)
    meas <- rlang::enquo(measure)

    dat_1 <- data %>% group_by(!!cat_1) %>% summarize(tot = sum(!!meas)) %>%
        mutate(fac = reorder(!!cat_1, tot))

    name = paste("total_", rlang::as_label(meas), sep = "")
    name_sym = rlang::sym(name)
    dat_2 <- data %>% mutate(`:=`(!!cat_1, factor(!!cat_1, levels = levels(dat_1$fac)))) %>%
        group_by(!!cat_1, !!cat_2) %>% summarize(`:=`(!!name_sym, sum(!!meas)))

    g <- ggplot(dat_2, aes(!!cat_1, !!name_sym, fill = !!cat_2)) + geom_bar(stat = "identity") +
        ## scale_y_continuous(labels = scales::comma) +
    coord_flip()

    if (separate)
        g + facet_wrap(vars(!!cat_2)) + guides(fill = "none") else g
}
