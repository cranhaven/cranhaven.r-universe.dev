## -------------------------------------------------------------------

##' Plot Continuous Variables
##'
##' Graphical summaries of numerical variables using functions from
##' \code{ggplot2} and \code{GGally}.
##' @param x Data frame which may include continuous and discrete
##'     variables. Non-continuous variables are ignored.
##' @param plot_type Plot type. Current options: \describe{
##'
##' \item{\code{"pairwise"}}{Calls \code{\link[GGally]{ggpairs}} to
##' get plots of pairwise differences. Avoid it if there are too many
##' numerical variables.}
##'
##' \item{\code{"density"}}{Calls \code{\link[ggplot2]{geom_density}}
##' and \code{\link[ggplot2]{geom_rug}}.}
##'
##' \item{\code{"histogram"}}{Calls
##' \code{\link[ggplot2]{geom_histogram}} and
##' \code{\link[ggplot2]{geom_rug}}.}
##'
##' \item{\code{"violin"}}{Calls \code{\link[ggplot2]{geom_violin}}
##' and \code{\link[ggplot2]{geom_jitter}}.}
##'
##' \item{\code{"boxplot"}}{Calls
##' \code{\link[ggplot2]{geom_boxplot}}.}
##'
##' \item{\code{"qqplot"}}{Calls
##' \code{\link[ggplot2]{stat_qq_line}} and
##' \code{\link[ggplot2]{stat_qq}}.}
##' }
##' @return A ggplot object.
##' @importFrom dplyr select_if mutate
##' @importFrom tidyr gather
##' @importFrom GGally ggpairs
##' @importFrom ggplot2 ggplot geom_density geom_boxplot geom_violin
##'     geom_histogram facet_wrap coord_flip theme aes element_blank
##'     geom_rug geom_jitter theme_bw stat_qq stat_qq_line
##' @examples
##' library(glancedata)
##'
##' plot_numerical_vars(iris, "pairwise")
##' plot_numerical_vars(iris, "density")
##' plot_numerical_vars(iris, "boxplot")
##' plot_numerical_vars(iris, "violin")
##' plot_numerical_vars(iris, "histogram")
##' plot_numerical_vars(iris, "qqplot")
##' @author Guillermo Basulto-Elias
##' @export
plot_numerical_vars <- function(x, plot_type) {

    ## Number of rows
    n <- nrow(x)

    ## Keep continuous variables only
    x <- select_if(x, is.numeric)

    ## Set to NULL to pass test
    value <- NULL

    ## Get transparency for rug and jitter
    trans <- min(c(20/n, 1))

    ## Use long format required for some plots and add dummy variable
    ## to generate plots.
    if (plot_type %in% c("density", "histogram", "violin",
                         "boxplot", "qqplot")) {
        x  <- gather(x)
        xxxx <- NULL                    # To pass R CMD CHECK
        x  <- mutate(x, xxxx = "")
    }

    out <-
        switch(plot_type,
               pairwise = ggpairs(x),
               density = {
                   x %>%
                       ggplot(aes(value)) +
                       geom_density(fill = "grey92") +
                       geom_rug(alpha = trans) +
                       theme(axis.title.x = element_blank())
               },
               histogram = {
                   x %>%
                       ggplot(aes(value)) +
                       geom_histogram() +
                       geom_rug(alpha = trans) +
                       theme(axis.title.x = element_blank())
               },
               violin =  {
                   x %>%
                       ggplot(aes(xxxx, value)) +
                       geom_violin() +
                       geom_jitter(alpha = trans) +
                       coord_flip() +
                       theme(axis.title.x = element_blank())
               },
               boxplot = {
                   x %>%
                       ggplot(aes(xxxx, value)) +
                       geom_boxplot(fill = "grey92") +
                       coord_flip()
               },
               qqplot = {
                   x %>%
                       ggplot(aes(sample = value)) +
                       stat_qq_line(col = "blue") +
                       stat_qq(alpha = 0.3)
               }
               )

    ## Split by columns
    if (plot_type %in% c("density", "histogram",
                         "violin", "boxplot", "qqplot")) {
        out <- out + facet_wrap(~key, scales = "free")
    }


    if (is.null(out)) stop ("Plot type not defined.")

    out + theme_bw()
}
