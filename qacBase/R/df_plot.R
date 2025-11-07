#' @title Visualize a data frame
#'
#' @description
#' \code{df_plot} visualizes the variables in a data frame.
#'
#' @details
#' For each variable, the plot displays
#' \itemize{
#'   \item{type (\code{numeric},
#'               \code{integer},
#'               \code{factor},
#'               \code{ordered factor},
#'               \code{logical}, or \code{date})}
#'   \item{percent of available (and missing) cases}
#' }
#' Variables are sorted by type and the total number of variables
#' and cases are printed in the caption.
#' @param data a data frame.
#' @import ggplot2
#' @export
#' @return a \code{ggplot2} graph
#' @seealso For more descriptive statistics on
#' a data frame see \link{contents}.
#' @examples
#' df_plot(cars74)

df_plot <- function(data){
  if (!is.data.frame(data))
    stop('data must be a data.frame', call.=FALSE)

  classes <- vector(mode="character",
                    length=length(data))
  for (i in seq_along(data)){
    classes[i] <- class(data[[i]])[1]
  }

  pct_n <-  100 *sapply(data, function(x){sum(!is.na(x))/length(x)})

  df <- data.frame(var = names(data),
                   classes = classes,
                   pct_n = pct_n,
                   classes_n = as.numeric(as.factor(classes)))

  ggplot(df,
         aes(x=stats::reorder(.data[["var"]], .data[["classes_n"]]),
             y=.data[["pct_n"]], fill=.data[["classes"]])) +
    geom_bar(stat="identity") +
    labs(x="", y="Percent Available",
         title=paste(as.character(deparse(substitute(data)))),
         caption=paste(nrow(data), "cases",
                        ncol(data), "variables"),
         fill="Type") +
    guides(fill = guide_legend(reverse=TRUE)) +
    scale_y_continuous(breaks=seq(0, 100, 20)) +
    coord_flip() +
    theme_minimal()
}
