#' Rank Slopegraph
#'
#' Create a slopegraph or bump chart from a data frame of ranks.
#'
#' @param df A data frame of records.
#' @param names The name of the column having the names of the records.
#' @param group Optional. The name of the column with a grouping variable.
#' @param force.grouping If \code{TRUE}, the column specified in the argument
#'   \code{names} will be considered as a grouping variable for plotting the
#'   slopegraphs. (Each record will be represented by a different colour).
#'   Default is \code{TRUE}.
#' @param line.size Size of lines plotted. Must be numeric.
#' @param line.alpha Transparency of lines plotted. Must be
#'   numeric.
#' @param line.col Default is \code{TRUE}. Overrides colouring by
#'   \code{force.grouping} argument.
#' @param point.size Size of points plotted. Must be numeric.
#' @param point.alpha Transparency of points plotted. Must be
#'   numeric.
#' @param point.col Default is \code{TRUE}. Overrides colouring by
#'   \code{force.grouping} argument.
#' @param text.size Size of text annotations plotted. Must be
#'   numeric.
#' @param legend.position Position of the legend in the plot.
#'
#' @references
#'
#' \insertRef{tufte_visual_1986}{ammistability}
#'
#' @return The slopegraph as a \code{ggplot2} grob.
#' @importFrom stats ave
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
#'
#' @examples
#' library(agricolae)
#' data(soil)
#'
#' dec <- c("pH", "EC")
#' inc <- c("CaCO3", "MO", "CIC", "P", "K", "sand",
#'          "slime", "clay", "Ca", "Mg", "K2", "Na", "Al_H", "K_Mg", "Ca_Mg",
#'          "B", "Cu", "Fe", "Mn", "Zn")
#'
#' soilrank <- rankdf(soil, increasing = inc, decreasing = dec)
#' soilrank
#' soilslopeg <- rankslopegraph(soilrank, names = "place")
#' soilslopeg
rankslopegraph <- function(df, names, group, force.grouping = TRUE,
                           line.size = 1, line.alpha = 0.5, line.col = NULL,
                           point.size = 1, point.alpha = 0.5, point.col = NULL,
                           text.size = 2, legend.position = "bottom"){
  # check if names and group present in df

  if (missing(group) || is.null(group)) {
    ids <- names
  } else {
    ids <- c(names, group)
  }

  dfmelt <- reshape2::melt(df, id.vars = ids)

  # # consolidate duplicated ranks
  # dfmelt2 <- aggregate(. ~value+variable, data=dfmelt,
  #                     function(x) paste(unique(x), collapse = "\n"))

  dfmelt[, names] <- as.character(dfmelt[, names])

  dfmelt <- transform(dfmelt, lab1 = ave(get(names), variable, value,
                                         FUN = toString))
  dfmelt$lab1 <- gsub(", ", "\n", dfmelt$lab1)
  # dfmelt$lab1 <- dfmelt[, names]
  dfmelt$lab2 <- dfmelt$value

  dpcheck <- dfmelt[, c("variable", "value")]

  if (any(duplicated(dpcheck))) {
    dfmelt[duplicated(dpcheck), ]$lab1 <- NA
    dfmelt[duplicated(dpcheck), ]$lab2 <- NA
  }

  if (missing(group) || is.null(group)) {
    if (!force.grouping) {
      gp <- NULL
    } else{
      gp <- names
    }
  } else {
    gp <- group
    dfmelt[, gp] <- as.factor(dfmelt[, gp])
  }

  slopeg <- ggplot(data = dfmelt, aes_string(x = "variable", y = "value",
                                             group = names))

  if (missing(line.col) || is.null(line.col)) {
    slopeg <- slopeg +
      geom_line(aes_string(color = gp), size = line.size, alpha = line.alpha)
  } else {
    slopeg <- slopeg +
      geom_line(aes_string(color = gp), size = line.size, alpha = line.alpha,
                colour = line.col)
  }

  if (missing(point.col) || is.null(point.col)) {
    slopeg <- slopeg +
      geom_point(aes_string(color = gp), size = point.size, alpha = point.alpha)
  } else {
    slopeg <- slopeg +
      geom_point(aes_string(color = gp), size = point.size, alpha = point.alpha,
                 colour = point.col)
  }

  slopeg <- slopeg +
    geom_text(aes_string(label = "lab1"), size = text.size,
              vjust = 0 + 0, na.rm = TRUE, nudge_y = point.size / 5) +
    geom_text(aes_string(label = "lab2"), size = text.size,
              vjust = 1 - 0, na.rm = TRUE, nudge_y = -point.size / 10) +
    scale_y_reverse(breaks = 1:max(dfmelt$value)) +
    ylab("Rank") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          legend.position = legend.position)

  return(slopeg)
}


#' Ranks in a data.frame
#'
#' @param df A data frame.
#' @param increasing A character vector of column names of the data frame to be
#'   ranked in increasing order.
#' @param decreasing A character vector of column names of the data frame to be
#'   ranked in decreasing order.
#' @param ... Additional arguments to be passed on to
#'   \code{\link[base:rank]{rank()}}.
#'
#' @return A data frame with the ranks computed in the columns specified in
#'   arguments \code{increasing} and \code{decreasing}.
#' @export
#'
#' @examples
#' library(agricolae)
#' data(soil)
#'
#' dec <- c("pH", "EC")
#' inc <- c("CaCO3", "MO", "CIC", "P", "K", "sand",
#'          "slime", "clay", "Ca", "Mg", "K2", "Na", "Al_H", "K_Mg", "Ca_Mg",
#'          "B", "Cu", "Fe", "Mn", "Zn")
#'
#' soilrank <- rankdf(soil, increasing = inc, decreasing = dec)
#' soilrank
rankdf <-
function(df, increasing = NULL, decreasing = NULL, ...) {
   if((missing(decreasing) || is.null(decreasing)) &&
      (missing(increasing) || is.null(increasing))) {
     stop('Both "increasing" and "decreasing" are missing')
   }

  if(!missing(decreasing) || !is.null(decreasing)) {
    df[, decreasing] <- lapply(df[, decreasing, drop = FALSE],
                               function(x) rank(-x, ...))
  }
  if(!missing(increasing) || !is.null(increasing)) {
    df[, increasing] <- lapply(df[, increasing, drop = FALSE],
                               function(x) rank(x, ...))
  }

  return(df)
}
