## -------------------------------------------------------------------

##' Plot Discrete Variables
##'
##' Creates a grid of bar plots for variables with at most
##' \code{nvalues} (15 is the default) different values.
##' @param x Data frame, tibble or matrix with data.
##' @param nvalues Integer. It creates a bar plot for each variable
##'     with at most this amount of different values. The default
##'     value is 15.
##' @param sort_by_frequency Logical. It casts every variable to
##'     factor and then reorders the levels by frequency before
##'     generating the plot.
##' @return A GGplot object.
##' @importFrom ggplot2 ggplot theme_void theme_bw coord_flip qplot
##'     xlab ylab
##' @importFrom purrr map map2
##' @importFrom dplyr select_if
##' @importFrom forcats fct_inorder
##' @examples
##' library(glancedata)
##'
##' plot_discrete_vars(mtcars)
##' plot_discrete_vars(mtcars, sort_by_frequency = TRUE)
##' plot_discrete_vars(iris)
##'
##' ## The following two examples give a warning because they don't
##' ## have any variable with fewer than 15 different values.
##' plot_discrete_vars(cars)
##' plot_discrete_vars(state.x77)
##' @author Guillermo Basulto-Elias
##' @export
plot_discrete_vars <- function(x,
                               nvalues = 15,
                               sort_by_frequency = FALSE) {

    ## Cast to dataframe if x is a matrix
    if (is.matrix(x)) x <- as.data.frame(x)


    ## Check x is dataframe
    if (!is.data.frame(x)) {
        stop("'plot_discrete_vars' must receive a data frame.")
    }

    ## Keep variables with few levels.
    x <- select_if(x, ~ length(unique(.x)) < nvalues + 1)

    ## Column names
    nam <- colnames(x)

    ## I
    if (ncol(x) == 0) {
        msg <- paste("No variable has fewer than", nvalues,
                     "values.")
        warning(msg)
        return (ggplot() + theme_void())
    }

    ## Convert dataframe to list
    x <- as.list(x)

    ## Sort each variableby frequency
    if (sort_by_frequency) {
        x <- map(x, ~ fct_inorder(factor((.x))))
    }

    ## Generate list of plots
    x <- x %>%
        map(~ qplot(.x, geom = "bar")) %>%
        map(~ .x + theme_bw() + coord_flip())

    ## Add names
    x <- map2(x, nam, function(u, v) u + xlab(v) + ylab(''))


    x <- gridExtra::grid.arrange(grobs = x,
                                 ncol = ceiling(sqrt(length(nam))),
                                 bottom = "count")

    return (x)
}



## plot_discrete_vars(mtcars)
## plot_discrete_vars(mtcars, sort_by_frequency = TRUE)
## plot_discrete_vars(iris)
##
## Warnings
## plot_discrete_vars(cars)
## plot_discrete_vars(state.x77)


