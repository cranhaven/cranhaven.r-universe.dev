#' Graph Log-Odds of Binary Variable Across A Grouping Variable
#' 
#' Creates plot showing sample log-odds of binary Y variable across levels of a
#' grouping variable, with customizable error bars. Observations with missing 
#' values for \code{y} and/or \code{group} are dropped.
#' 
#' @param y Vector of values for binary response variable. Must take on 2 
#' values, but can be any type (e.g. numeric, character, factor, logical). 
#' Function plots log-odds of second value returned by \code{table(y)}.
#' @param group Vector of values indicating what group each \code{y} observation 
#' belongs to. Function plots group levels across x-axis in same order as 
#' \code{table(group)}.
#' @param error.bars Character string indicating what the error bars should 
#' represent. Possible values are \code{"exact.ci"} for exact 95\% confidence 
#' interval based on binomial distribution, \code{"z.ci"} for approximate 95\% 
#' confidence interval based on Z distribution, and \code{"none"} for no error 
#' bars.
#' @param alpha Numeric value indicating what alpha should be set to for 
#' confidence intervals. Only used if \code{error.bars} is \code{"exact.ci"} or 
#' \code{"z.ci"}.
#' @param p.legend Character string controlling what p-value is printed in a 
#' legend. Possible values are \code{"chi"} for Chi-square test of association, 
#' \code{"fisher"} for Fisher's exact test, and \code{"none"} for no legend at 
#' all.
#' @param plot.list Optional list of inputs to pass to 
#' \code{\link[graphics]{plot}} function.
#' @param lines.list Optional list of inputs to pass to 
#' \code{\link[graphics]{lines}} function.
#' @param axis.list Optional list of inputs to pass to 
#' \code{\link[graphics]{axis}} function.
#' @param legend.list Optional list of inputs to pass to 
#' \code{\link[graphics]{legend}} function.
#' @param ... Additional arguments to pass to \code{\link[stats]{chisq.test}} or
#' \code{\link[stats]{fisher.test}} functions.
#' 
#' @return Plot showing log-odds of \code{y} across levels of \code{group}.
#' 
#' @export
logodds_graph <- function(y, group, error.bars = "none", alpha = 0.05, 
                          p.legend = "chi", 
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
  
  # Create contingency table
  group.y.table <- table(group, y)
  x <- 1: nrow(group.y.table)
  
  # Get levels of groups variable for tick labels
  group.levels <- rownames(group.y.table)
  
  # Exclude rows of 0
  locs.0 <- which(apply(group.y.table, 1, sum) == 0)
  if (length(locs.0) > 0) {
    group.y.table <- group.y.table[-locs.0, ]
    x <- x[-locs.0]
  }
  
  # Get outcome probabilities and log-odds for each level
  probs <- apply(group.y.table, 1, function(x) x[2] / sum(x))
  logodds <- log(probs / (1 - probs))
  
  # Create error bars
  if (error.bars == "exact.ci") {
    
    probs.ci <- apply(group.y.table, 1, function(x)
      binom.test(x = rev(x), conf.level = 1 - alpha)$conf.int)
    logodds.ci <- log(probs.ci / (1 - probs.ci))
    lower.bars <- logodds.ci[1, ]
    upper.bars <- logodds.ci[2, ]
    y.label <- paste(y.varname, " (log-odds +/- 95% CI)", sep = "")
    
  } else if (error.bars == "z.ci") {
    
    probs.ci <- apply(group.y.table, 1, function(x)
      prop.test(x = x[2], n = sum(x), conf.level = 1 - alpha)$conf.int)
    logodds.ci <- log(probs.ci / (1 - probs.ci))
    lower.bars <- logodds.ci[1, ]
    upper.bars <- logodds.ci[2, ]
    y.label <- paste(y.varname, " (log-odds +/- 95% CI)", sep = "")
    
  } else if (error.bars == "none") {
    
    lower.bars <- NULL
    upper.bars <- NULL
    y.label <- paste(y.varname, " (log-odds)", sep = "")
    
  }
  
  # Figure out ylim values
  if (!is.null(lower.bars)) {
    max.error <- max(upper.bars[!is.infinite(upper.bars)])
    min.error <- min(lower.bars[!is.infinite(lower.bars)])
    span.error <- max.error - min.error
    y1 <- min.error - 0.1 * span.error
    y2 <- max.error + 0.1 * span.error
    upper.bars[upper.bars == Inf] <- max.error + span.error
    lower.bars[lower.bars == -Inf] <- min.error - span.error
  } else {
    range.logodds <- range(logodds[!is.infinite(logodds)])
    span.logodds <- diff(range.logodds)
    y1 <- range.logodds[1] - 0.1 * span.logodds
    y2 <- range.logodds[2] + 0.1 * span.logodds
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <-
    list_override(list1 = list(x = x, y = logodds,
                               type = "p", pch = 16, xaxt = "n",
                               main = paste("Log-odds ", y.varname, " by ",
                                            group.varname, sep = ""),
                               cex.main = 1.25,
                               xlab = group.varname, ylab = y.label,
                               xlim = c(0.5, length(group.levels) + 0.5),
                               ylim = c(y1, y2)),
                  list2 = plot.list)
  cex.axis.value <- ifelse(length(group.levels) <= 3, 1,
                           ifelse(length(group.levels) >= 8, 0.5,
                                  1 - 0.1 * (length(group.levels) - 3)))
  axis.list <-
    list_override(list1 = list(side = 1, at = 1: length(group.levels),
                               labels = group.levels,
                               cex.axis = cex.axis.value),
                  list2 = axis.list)
  
  # Create graph
  do.call(plot, plot.list)
  
  # Add error bars
  if (error.bars != "none") {
    for (ii in 1:length(lower.bars)) {
      
      end.points <- c(lower.bars[ii], upper.bars[ii])
      do.call(lines, c(list(x = rep(x[ii], 2), y = end.points), lines.list))
      do.call(lines, c(list(x = c(x[ii] - 0.03, x[ii] + 0.03),
                            y = rep(end.points[1], 2)), lines.list))
      do.call(lines, c(list(x = c(x[ii] - 0.03, x[ii] + 0.03),
                            y = rep(end.points[2], 2)), lines.list))
      
    }
  }
  
  # Add labels
  do.call(axis, axis.list)
  
  # Add legend
  if (p.legend != "none") {
    
    if (p.legend == "chi") {
      
      # Perform Chi-square test for association
      pval <- chisq.test(x = group, y = y, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "Chi-square P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("Chi-square P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("Chi-square P = ", sprintf("%.2f", pval), sep = "")
      }
      
    } else if (p.legend == "fisher") {
      
      # Perform Fisher's exact test
      pval <- fisher.test(x = group, y = y, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "Fisher P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("Fisher P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("Fisher P = ", sprintf("%.2f", pval), sep = "")
      }
      
    }
    
    # Add user inputs to legend, if any
    legend.list <-
      list_override(list1 = list(x = "topleft", legend = pval.text),
                    list2 = legend.list)
    
    # Add legend
    do.call(legend, legend.list)
    
  }
  
}
