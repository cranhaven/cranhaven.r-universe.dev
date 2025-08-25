#' Plot the distribution of a numeric (measure) column
#'
#' @param data A data frame or tibble
#' @param measure Unquoted column name of containing numbers (measure)
#' @param type Histogram ("hist") or Boxplot ("box")
#' @param bwidth width of bin for histogram (by default uses binwidth for 30 bins)
#' @return A ggplot plot object
#' @export
#' @examples
#' measure_distribution(ggplot2::diamonds, price)
#' measure_distribution(ggplot2::mpg, hwy)
#' measure_distribution(ggplot2::mpg, hwy, bwidth = 2)
#' measure_distribution(ggplot2::mpg, hwy, "hist")
#' measure_distribution(ggplot2::mpg, hwy, "box")

measure_distribution <- function(data, measure, type = "hist", bwidth = NULL) {
    m <- rlang::enquo(measure)
    if (!(type %in% c("hist", "box"))) {
        message("Type must be 'hist' or 'box'")
        return(1)
    }
    g <- ggplot(data)
    if (type == "hist") {
        if (is.null(bwidth)) message("Using binwidth corresponding to 30 bins. Use bwidth argument to fix if needed.")
        g + geom_histogram(aes(!!m), binwidth = bwidth)
    } else {
        g + geom_boxplot(aes(x = 1, y = !!m)) + theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) + xlab("") + scale_x_continuous(breaks = seq(1,
            1, 1)) + coord_flip()
    }
}

#' Plot the distribution of a numeric (measure) column differentiated by a category
#'
#' @param data A data frame or tibble
#' @param measure Unquoted column name of measure (containing numbers)
#' @param category Unquoted column name of category (can be factor, character or numeric)
#' @param type Histogram ("hist") or Boxplot ("box")
#' @param separate Boolean specifying whether to plot each category in a separate facet
#' @param bwidth width of bin for histogram (by default uses binwidth for 30 bins)
#' @return A ggplot plot object
#' @export
#' @examples
#' measure_distribution_by_category(ggplot2::diamonds, price, cut)
#' measure_distribution_by_category(ggplot2::mpg, hwy, class)
#' measure_distribution_by_category(ggplot2::diamonds, price, cut, separate = TRUE)
#' measure_distribution_by_category(ggplot2::mpg, hwy, class, separate = TRUE)
#' measure_distribution_by_category(ggplot2::mpg, hwy, class, "box")

measure_distribution_by_category <- function(data, measure, category, type = "hist",
    separate = FALSE, bwidth = NULL) {
    m <- rlang::enquo(measure)
    cat <- rlang::enquo(category)
    data <- col_to_factor(data, cat)
    if (!(type %in% c("hist", "box"))) {
        message("Type must be 'hist' or 'box'")
        return(1)
    }
    g <- ggplot(data)
    if (type == "hist") {
        if (is.null(bwidth)) message("Using binwidth corresponding to 30 bins. Use bwidth argument to fix if needed.")
        g <- g + geom_histogram(aes(!!m, fill = !!cat), binwidth = bwidth)
        if (separate == FALSE) {
            g
        } else {
            g + facet_wrap(vars(!!cat))
        }
    } else {
        g + geom_boxplot(aes(x = !!cat, y = !!m)) + coord_flip()
    }
}

#' Plot the distribution of a numeric (measure) column differentiated by two categories
#'
#' @param data A data frame or tibble
#' @param measure Unquoted column name of containing numbers (measure)
#' @param category1,category2 Unquoted column names of categories (can be factor, character or numeric)
#' @param bwidth width of bin for histogram (by default uses binwidth for 30 bins)
#' @return A ggplot plot object
#' @export
#' @examples
#' measure_distribution_by_two_categories(ggplot2::mpg, hwy, class, fl)
#' measure_distribution_by_two_categories(ggplot2::diamonds, carat, cut, clarity)
measure_distribution_by_two_categories <- function(data, measure, category1,
    category2, bwidth = NULL) {
    c1 <- rlang::enquo(category1)
    c2 <- rlang::enquo(category2)
    m <- rlang::enquo(measure)
    if (is.null(bwidth)) message("Using binwidth corresponding to 30 bins. Use bwidth argument to fix if needed.")
    g = ggplot(data) + geom_histogram(mapping = aes(x = !!m), binwidth = bwidth) + facet_grid(vars(!!c1),
        vars(!!c2))
    g
}

#' Plot the change of distribution of a numeric (measure) column over time
#'
#' @param data A data frame or tibble
#' @param measure Unquoted column name of containing numbers (measure)
#' @param time Unquoted name of column containing the time object
#' @param bwidth width of bin for histogram (by default uses binwidth for 30 bins)
#' @return A ggplot plot object
#' @export
#' @examples
#' h1 <- round(rnorm(50, 60, 8), 0)
#' h2 <- round(rnorm(50, 65, 8), 0)
#' h3 <- round(rnorm(50, 70, 8), 0)
#' h <- c(h1, h2, h3)
#' y <- c(rep(1999, 50), rep(2000, 50), rep(2001, 50))
#' df <- data.frame(height = h, year = y)
#' measure_distribution_over_time(df, h, year)
measure_distribution_over_time <- function(data, measure, time, bwidth = NULL) {
    m <- rlang::enquo(measure)
    cat <- rlang::enquo(time)
    if (is.null(bwidth)) message("Using binwidth corresponding to 30 bins. Use bwidth argument to fix if needed.")
    ggplot(data) + geom_histogram(aes(!!m), binwidth = bwidth) + facet_wrap(vars(!!cat), ncol = 1)
}
