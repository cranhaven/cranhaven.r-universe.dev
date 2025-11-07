#' @title Plot a tab object
#' @description Plot a frequency or cumulative frequency table
#' @param x An object of class \code{tab}
#' @param fill Fill color for bars
#' @param size numeric. Size of bar text labels.
#' @param ... Parameters passed to a function
#' @importFrom stats reorder
#' @return a ggplot2 graph
#' @examples
#' tbl1 <- tab(cars74, carb)
#' plot(tbl1)
#'
#' tbl2 <- tab(cars74, carb, sort = TRUE)
#' plot(tbl2)
#'
#' tbl3 <- tab(cars74, carb, cum=TRUE)
#' plot(tbl3)
#' @rdname plot.tab
#' @import ggplot2
#' @export

plot.tab <- function(x, fill="deepskyblue2", size=3.5, ...) {
  if(!inherits(x, "tab")) stop("Must be class 'tab'")
  x$ord <- 1:nrow(x)
  vname <- attr(x, "vname")
  if (length(x)==4){
    p <- ggplot(x, aes(x=reorder(.data[["level"]], .data[["ord"]]),
                        y=.data[["percent"]])) +
      geom_bar(stat="identity", fill=fill) +
      labs(x=vname, y="percent") + coord_flip() +
      geom_text(aes(label = paste0(round(.data[["percent"]]), "%")),
                hjust=1, size=size, color="grey30")
  }
  if (length(x) == 6){
    p <- ggplot(x,
                aes(x=reorder(.data[["level"]], .data[["ord"]]),
                    y=.data[["cum_percent"]])) +
      geom_bar(fill="grey", alpha=.6, stat="identity") +
      geom_bar(aes(x=reorder(.data[["level"]], .data[["ord"]]),
                   y=.data[["percent"]]),
               fill=fill, stat="identity") +
      labs(x=vname, y="cumulative percent") + coord_flip() +
      geom_text(aes(label = paste0(round(.data[["cum_percent"]]), "%")),
              hjust=1, size=size, color="grey30")
  }

  return(p)
}

