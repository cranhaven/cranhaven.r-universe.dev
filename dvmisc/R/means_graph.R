#' Graph Means Across a Grouping Variable
#'
#' Creates plot showing mean of Y variable across levels of a grouping variable,
#' with customizable error bars. Observations with missing values for \code{y} 
#' and/or \code{group} are dropped.
#' 
#' @param y Numeric vector of values for the continuous variable.
#' @param group Vector of values indicating what group each \code{y} observation 
#' belongs to. Function plots group levels across x-axis in same order as 
#' \code{table(group)}.
#' @param error.bars Character string indicating what the error bars should 
#' represent. Possible values are \code{"sd"} for +/- one standard deviation, 
#' \code{"se"} for +/- one standard error, \code{"t.ci"} for 95\% confidence 
#' interval based on t distribution, \code{"z.ci"} for 95\% confidence interval 
#' based on Z distribution, and \code{"none"} for no error bars.
#' @param alpha Numeric value indicating what alpha should be set to for 
#' confidence intervals. Only used if \code{error.bars} is \code{"t.ci"} or 
#' \code{"z.ci"}.
#' @param p.legend If \code{TRUE}, p-value (from \code{\link[stats]{t.test}} 
#' function if group has 2 levels, otherwise \code{\link[stats]{aov}} function) 
#' is printed in a legend.
#' @param plot.list Optional list of inputs to pass to 
#' \code{\link[graphics]{plot}} function.
#' @param lines.list Optional list of inputs to pass to 
#' \code{\link[graphics]{lines}} function.
#' @param axis.list Optional list of inputs to pass to 
#' \code{\link[graphics]{axis}} function.
#' @param legend.list Optional list of inputs to pass to 
#' \code{\link[graphics]{legend}} function.
#' @param ... Additional arguments to pass to \code{\link[stats]{t.test}} or 
#' \code{\link[stats]{aov}}.
#' 
#' @return Plot showing mean of \code{y} across levels of \code{group}.
#' 
#' @export
means_graph <- function(y, group, error.bars = "t.ci", alpha = 0.05,
                        p.legend = TRUE,
                        plot.list = NULL,
                        lines.list = NULL,
                        axis.list = NULL,
                        legend.list = NULL,
                        ...) {
  
  # Get name of y and group variables for axis labels
  y.varname <- deparse(substitute(y))
  group.varname <- deparse(substitute(group))
  
  # Drop missing values
  locs.missing <- which(is.na(y) | is.na(group))
  if (length(locs.missing) > 0) {
    y <- y[-locs.missing]
    group <- group[-locs.missing]
  }
  
  # Get levels of groups variable for tick labels
  group.levels <- names(table(group))
  
  # Get group means
  means <- tapply(X = y, INDEX = group, FUN = mean)
  
  # Create error bars
  if (error.bars == "sd") {
    
    sds <- tapply(X = y, INDEX = group, FUN = sd)
    lower.bars <- means - sds
    upper.bars <- means + sds
    y.label <- paste(y.varname, " (Mean +/- 1 SD)", sep = "")
    
  } else if (error.bars == "se") {
    
    ses <- tapply(X = y, INDEX = group, FUN = function(x)
      sd(x) / sqrt(length(x)))
    lower.bars <- means - ses
    upper.bars <- means + ses
    y.label <- paste(y.varname, " (Mean +/- 1 SE)", sep = "")
    
  } else if (error.bars == "t.ci") {
    
    ns <- tapply(X = y, INDEX = group, FUN = length)
    sds <- tapply(X = y, INDEX = group, FUN = sd)
    t.crit <- qt(p = 1 - alpha/2, df = ns - 1)
    lower.bars <- means - t.crit * sds / sqrt(ns)
    upper.bars <- means + t.crit * sds / sqrt(ns)
    y.label <- paste(y.varname, " (Mean +/- 95% CI)", sep = "")
    
  } else if (error.bars == "z.ci") {
    
    ns <- tapply(X = y, INDEX = group, FUN = length)
    sds <- tapply(X = y, INDEX = group, FUN = sd)
    z.crit <- qnorm(p = 1 - alpha/2)
    lower.bars <- means - z.crit * sds / sqrt(ns)
    upper.bars <- means + z.crit * sds / sqrt(ns)
    y.label <- paste(y.varname, " (Mean +/- 95% CI)", sep = "")
    
  } else if (error.bars == "none") {
    
    lower.bars <- NULL
    upper.bars <- NULL
    y.label <- paste(y.varname, " (Mean)", sep = "")
    
  }
  
  # Figure out ylim values
  if (!is.null(lower.bars)) {
    max.error <- max(upper.bars)
    min.error <- min(lower.bars)
    span.error <- max.error - min.error
    y1 <- min.error - 0.1 * span.error
    y2 <- max.error + 0.1 * span.error
  } else {
    range.means <- range(means)
    span.means <- diff(range.means)
    y1 <- range.means[1] - 0.1 * span.means
    y2 <- range.means[2] + 0.1 * span.means
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <-
    list_override(list1 = list(x = means, type = "p", pch = 16, xaxt = "n",
                               main = paste("Mean ", y.varname, " by ",
                                            group.varname, sep = ""),
                               cex.main = 1.25,
                               xlab = group.varname, ylab = y.label,
                               xlim = c(0.5, length(group.levels) + 0.5),
                               ylim = c(y1, y2)),
                  list2 = plot.list)
  axis.list <-
    list_override(list1 = list(side = 1, at = 1: length(group.levels),
                               labels = group.levels),
                  list2 = axis.list)
  
  # Create graph
  do.call(plot, plot.list)
  
  # Add error bars
  if (error.bars != "none") {
    for (ii in 1:length(lower.bars)) {
      
      end.points <- c(lower.bars[ii], upper.bars[ii])
      do.call(lines, c(list(x = rep(ii, 2), y = end.points), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03),
                            y = rep(end.points[1], 2)), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03),
                            y = rep(end.points[2], 2)), lines.list))
      
    }
  }
  
  # Add legend
  if (p.legend) {
    
    # Perform t-test/ANOVA
    if (length(unique(group)) == 2) {
      pval <- t.test(y ~ group, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "T-test P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("T-test P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("T-test P = ", sprintf("%.2f", pval), sep = "")
      }
    } else {
      pval <- summary(aov(y ~ group, ...))[[1]][[1, "Pr(>F)"]]
      if (pval < 0.001) {
        pval.text <- "ANOVA P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("ANOVA P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("ANOVA P = ", sprintf("%.2f", pval), sep = "")
      }
    }
    
    # Add user inputs to legend, if any
    legend.list <- list_override(list1 = list(x = "topleft",
                                              legend = pval.text),
                                 list2 = legend.list)
    
    # Add legend
    do.call(legend, legend.list)
    
  }
  
  # Add labels
  do.call(axis, axis.list)
  
}