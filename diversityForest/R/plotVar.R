# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

##' This function allows to visualise the (estimated) distributions of a variable \code{x} for each of the categories of a categorical variable \code{y}.
##' This allows to study the dependency structure of \code{y} on \code{x}.
##' Two types of visualisations are available: density plots and boxplots.
##'
##' See the 'Details' section of \code{\link{plotMcl}}.
##' 
##' @title Plot of the (estimated) dependency structure of a variable \code{x} on a categorical variable \code{y}
##' @param x Metric variable or ordered categorical variable that has at least as many unique values as \code{y}
##' @param y Factor variable with at least three categories.
##' @param plot_type Plot type, one of the following: "both" (the default), "density", "boxplot".  If "density", a \code{"density"} plot is produced, if "boxplot", a \code{"boxplot"} is produced, and if "both", both a \code{"density"} plot and a \code{"boxplot"} are produced. See the 'Details' section of \code{\link{plotMcl}} for details.
##' @param x_label Optional. The label of the x-axis.
##' @param y_label Optional. The label (heading) of the legend that differentiates the categories of \code{y}.
##' @param plot_title Optional. The title of the plot.
##' @param plotit This states whether the plots are actually plotted or merely returned as \code{ggplot} objects. Default is \code{TRUE}.
##' @return A list returned invisibly containing:
##' \itemize{
##'   \item Only the element \code{dens_pl} if \code{plot_type = "density"};
##'   \item Only the element \code{boxplot_pl} if \code{plot_type = "boxplot"};
##'   \item The elements \code{dens_pl}, \code{boxplot_pl}, and \code{combined_pl} if \code{plot_type = "both"}.
##' }
##' All returned plots are \code{ggplot2} objects, with \code{combined_pl} being a \code{patchwork} object.
##' @examples
##' \dontrun{
##' 
##' ## Load package:
##' 
##' library("diversityForest")
##' 
##' 
##' 
##' ## Load the "ctg" data set:
##' 
##' data(ctg)
##' 
##' 
##' ## Set seed to make results reproducible (this is necessary because
##' ## the rug plot produced by 'plotVar' does not show all observations, but
##' ## only a random subset of 1000 observations):
##' 
##' set.seed(1234)
##' 
##' 
##' ## Using a "density" plot and a "boxplot", visualise the (estimated) 
##' ## distributions of  the variable "Mean" for each of the categories of the 
##' # variable "Tendency":
##' 
##' plotVar(x = ctg$Mean, y = ctg$Tendency)
##' 
##' 
##' ## Re-create this plot with labels:
##' 
##' plotVar(x = ctg$Mean, y = ctg$Tendency, x_label = "Mean of the histogram ('Mean')",
##'         y_label = "Histogram tendency ('Tendency')", 
##'         plot_title = "Relationship between 'Mean' and 'Tendency'")
##' 
##' 
##' ## Re-create this plot, but only show the "density" plot:
##' 
##' plotVar(x = ctg$Mean, y = ctg$Tendency, plot_type = "density",
##'         x_label = "Mean of the histogram ('Mean')", 
##'         y_label = "Histogram tendency ('Tendency')", 
##'         plot_title = "Relationship between 'Mean' and 'Tendency'")
##' 
##' 
##' ## Use ggplot2 and RColorBrewer functionalities to change the line colors and
##' ## the labels of the categories of "Tendency":
##' 
##' library("ggplot2")
##' library("RColorBrewer")
##' p <- plotVar(x = ctg$Mean, y = ctg$Tendency, plot_type = "density",
##'              x_label = "Mean of the histogram ('Mean')", 
##'              y_label = "Histogram tendency ('Tendency')", 
##'              plot_title = "Relationship between 'Mean' and 'Tendency'",
##'              plotit = FALSE)$dens_pl +
##'   scale_color_manual(values = brewer.pal(n = 3, name = "Set2"),
##'                      labels = c("left asymmetric", "symmetric", 
##'                                 "right asymmetric")) +
##'   scale_linetype_manual(values = rep(1, 3),
##'                         labels = c("left asymmetric", "symmetric", 
##'                                    "right asymmetric"))
##' 
##' p
##' 
##' ## # Save as PDF:
##' ## ggsave(file="mypathtofolder/FigureXY1.pdf", width=10, height=7)
##' 
##' 
##' 
##' ## Further customizations:
##' 
##' # Create plot without plotting it:
##' 
##' plotobj <- plotVar(x = ctg$Mean, y = ctg$Tendency, 
##'                    x_label = "Mean of the histogram ('Mean')", 
##'                    y_label = "Histogram tendency ('Tendency')", 
##'                    plotit = FALSE)
##' 
##' 
##' # Customize the density plot:
##' 
##' dens_pl <- plotobj$dens_pl + theme(legend.position = "inside", 
##'                                    legend.position.inside = c(0.25, 0.9), 
##'                                    legend.title = element_text(size = 16), 
##'                                    legend.text = element_text(size = 12), 
##'                                    axis.title = element_text(size=16), 
##'                                    axis.text = element_text(size=12)) + 
##'   ylab("(Scaled) density")
##' 
##' 
##' # Customize the boxplot:
##' 
##' boxplot_pl <- plotobj$boxplot_pl + 
##'   theme(axis.text.x = element_text(color = "transparent"), 
##'         axis.ticks.x = element_line(color = "transparent"), 
##'         axis.title = element_text(size=16), 
##'         axis.text = element_text(size=12))
##' 
##' 
##' # Create a title with increased font size:
##' 
##' library("grid")
##' title_grob <- textGrob(
##'   "Title of the combined plot", 
##'   gp = gpar(fontsize = 18) 
##' )
##' 
##' 
##' # Arrange plots with title:
##' 
##' library("gridExtra")
##' p <- arrangeGrob(
##'   dens_pl, boxplot_pl, 
##'   top = title_grob,
##'   nrow = 1
##' )
##' p
##' 
##' ## # Save as PDF:
##' ## ggsave(file="mypathtofolder/FigureXY2.pdf", p, width=16, height=7)
##' 
##' }
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   }
##' @seealso \code{\link{plotMcl}}, \code{\link{plot.multifor}}
##' @encoding UTF-8
##' @importFrom ggplot2 ggplot aes geom_line geom_rug theme_bw theme labs ggtitle xlab ylab scale_x_continuous scale_y_continuous scale_color_manual scale_linetype_manual geom_boxplot element_text
##' @export
plotVar <- function(x, y, plot_type=c("both", "density", "boxplot")[1], x_label="", y_label="", plot_title="", plotit=TRUE) {
  
  # If plot_type=="density", create a density plot:
  if (plot_type=="density") {
    dens_pl <- plotVarDensity(x=x, y=y, x_label=x_label, y_label=y_label, plot_title=plot_title)$p
    if (plotit) {
      print(dens_pl)
    }
    res <- list(dens_pl=dens_pl)
    invisible(res)
  } else if (plot_type=="boxplot") {
    # If plot_type=="boxplot", create a boxplot:
    boxplot_pl <- plotVarBoxplot(x=x, y=y, x_label=x_label, y_label=y_label, plot_title=plot_title)
    if (plotit) {
      print(boxplot_pl)
    }
    res <- list(boxplot_pl=boxplot_pl)
    invisible(res)
  } else if (plot_type=="both") {
    # If plot_type=="both", create both a density plot a boxplot:
    # Create the density plot:
    dens_res <- plotVarDensity(x=x, y=y, x_label=x_label, y_label=y_label, plot_title="")
    dens_pl <- dens_res$p
    boxplot_pl <- plotVarBoxplot(x=x, y=y, x_label=x_label, y_label=y_label, plot_title="", plotres=dens_res$plotres)
    # Add the boxplot using the same colors and line types as the density plot (through 'plotres=dens_res$plotres'):
    combined_pl <- patchwork::wrap_plots(dens_pl, boxplot_pl, ncol = 2)
    combined_pl <- combined_pl +
      patchwork::plot_annotation(
        title = plot_title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
      )
    if (plotit) {
      print(combined_pl)
    }
    res <- list(combined_pl=combined_pl, dens_pl=dens_pl, boxplot_pl=boxplot_pl)
    invisible(res)
  }
  
}


plotVarDensity <- function(x, y, x_label="", y_label="", plot_title="") {
  
  classtab <- table(y)
  
  # The densities are plotted only for classes with at least two observations:
  levels_to_keep <- names(classtab[classtab >= 2])
  
  filterbool <- y %in% levels_to_keep
  
  x <- x[filterbool]
  y <- y[filterbool]
  
  if (length(unique(x)) < length(unique(y)))
    stop("The number of unique covariate values must be at least as large as the number of classes.")
  
  allclasses <- levels(y)[levels(y) %in% unique(y)]
  
  classtab <- classtab[classtab >= 2]
  classprob <- classtab/sum(classtab)
  
  # The maximum number of different colors used. If the number of classes is larger
  # than this, the different classes are differentiated visually using both
  # colors and line types:
  nmax <- min(c(length(allclasses), 7))
  
  colors <- scales::hue_pal()(nmax)
  
  if (length(allclasses) == nmax) {
    colorsvec <- colors
    linetypesvec <- rep("solid", length=length(colorsvec))
  } else {
    colorsvec <- rep(colors, length=length(allclasses))
    
    linetypesvec <- rep(c("solid", "longdash", "dotdash"), each=nmax)[1:length(colorsvec)]
    linetypesvec <- c(linetypesvec, rep("dotdash", times=length(colorsvec) - length(linetypesvec)))
  }
  
  
  # Create a density plot for a numeric covariate:
  
  if (inherits(x, "numeric")) {
    
    denstemps <- list()
    
    for(i in seq(along=allclasses)) {
      xtemp <- x[y==allclasses[i]]
      
      denstemp <- density(xtemp)
      denstemp <- data.frame(x=denstemp$x, y=denstemp$y)
      # The density values are scaled by the class sizes:
      denstemp$y <- denstemp$y*classprob[i]
      denstemps[[i]] <- denstemp
    }
    
    plotdata <- do.call("rbind", denstemps)
    plotdata$class <- factor(rep(allclasses, times=sapply(denstemps, nrow)), levels=allclasses)
    
    pointdata <- data.frame(x=x, class=y)
    pointdata$class <- droplevels(pointdata$class)
    
    # If there are more than 1000 observations, the rug plot on the lower margin
    # only shows a random subset of 1000 observations:
    if (nrow(pointdata) > 1000) {
      pointdata <- pointdata[sample(1:nrow(pointdata), size=1000),]
    }
    
    p <- ggplot(plotdata, aes(x=.data$x, color=.data$class, linetype=.data$class)) + theme_bw() + geom_line(aes(y=.data$y)) + 
      scale_color_manual(values=colorsvec) + scale_linetype_manual(values = linetypesvec) +
      ylab("(scaled) density") + geom_rug(data=pointdata, sides="b")
    
  }
  
  # Create a density plot for a factor covariate:
  
  if (inherits(x, "ordered") || inherits(x, "factor")) {
    
    if (inherits(x, "factor"))
      warning("The plot is likely not meaningful because the variable is an unordered factor..")
    
    x_levels <- levels(x)[levels(x) %in% unique(x)]
    
    # For plotting, the factor variable is transformed to a continuous variable:
    x <- as.numeric(x)
    
    denstemps <- list()
    
    for(i in seq(along=allclasses)) {
      xtemp <- x[y==allclasses[i]]
      
      denstemp <- density(xtemp)
      denstemp <- data.frame(x=denstemp$x, y=denstemp$y)
      denstemp$y <- denstemp$y*classprob[i]
      denstemps[[i]] <- denstemp
    }
    
    plotdata <- do.call("rbind", denstemps)
    plotdata$class <- factor(rep(allclasses, times=sapply(denstemps, nrow)), levels=allclasses)
    
    if (x_label=="")
      xlabadd <- theme(axis.title.x=element_blank())
    else
      xlabadd <- xlab(x_label)
    
    x_unique_sorted <- sort(unique(x))
    
    p <- ggplot(plotdata, aes(x=.data$x, y=.data$y, color=.data$class, linetype=.data$class)) + theme_bw() + geom_line() + 
      scale_color_manual(values=colorsvec) + scale_linetype_manual(values = linetypesvec) +
      # The labels of the categories of the covariate are added to the x-axis:
      scale_x_continuous(breaks=x_unique_sorted, labels=x_levels) +
      ylab("density") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  }
  
  
  # Add labels to the plot if provided:
  
  if (x_label=="")
    p <- p + theme(axis.title.x=element_blank())
  else
    p <- p + xlab(x_label)
  
  if (y_label!="")
    p <- p + labs(colour=y_label, linetype=y_label)
  
  if (plot_title!="")
    p <- p + ggtitle(plot_title)
  
  # The information on the colors and linetypes of the classes are returned too
  # because these are required by "plotVarBoxplot" in cases in which both the
  # densities and the boxplots are plotted:
  plotres <- list(allclasses=allclasses, colorsvec=colorsvec, linetypesvec=linetypesvec)
  reslist <- list(p=p, plotres=plotres)
  
  return(reslist)
  
}


plotVarBoxplot <- function(x, y, x_label="", y_label="", plot_title="", plotres=NULL) {
  
  # Create a boxplot for a numeric covariate:
  
  if (inherits(x, "numeric")) {
    
    plotdata <- data.frame(x=x, y=y)
    
    # If no information on the colors and line types of the boxplots is provided
    # (usually that returned by "plotVarDensity") boxplots with black lines are generated:
    if (is.null(plotres))
      p <- ggplot(plotdata, aes(x=.data$y, y=.data$x)) + theme_bw() + geom_boxplot() 
    else {
      # If information on the colors and line types of the boxplots is provided
      # boxplots with the specified colors and line types are generated:
      classes_dens <- plotres$allclasses
      colorsvec_dens <- plotres$colorsvec
      linetypesvec_dens <- plotres$linetypesvec
      
      classtab <- table(y)
      
      classes_present <- names(classtab[classtab >= 1])
      
      colorsvec <- linetypesvec <- rep("", length(classes_present))
      colorsvec[classes_present %in% classes_dens] <- colorsvec_dens
      linetypesvec[classes_present %in% classes_dens] <- linetypesvec_dens
      
      # Classes for which no colors or line types are provided are depicted
      # in grey:
      colorsvec[colorsvec==""] <- "grey"
      linetypesvec[linetypesvec==""] <- "solid"
      
      p <- ggplot(plotdata, aes(x=.data$y, y=.data$x, color=.data$y, linetype=.data$y)) + theme_bw() + geom_boxplot() +
        scale_color_manual(values=colorsvec) +
        scale_linetype_manual(values=linetypesvec) + theme(legend.position = "none")
    }
    
  }
  
  # Create a density plot for a factor covariate:
  
  if (inherits(x, "ordered") || inherits(x, "factor")) {
    
    if (inherits(x, "factor"))
      warning("The plot is likely not meaningful because the variable is an unordered factor.")
    
    x_levels <- levels(x)[levels(x) %in% unique(x)]
    
    # For plotting, the factor variable is transformed to a continuous variable:
    x <- as.numeric(x)
    
    plotdata <- data.frame(x=x, y=y)
    
    x_unique_sorted <- sort(unique(x))
    
    if (is.null(plotres))
      p <- ggplot(plotdata, aes(x=.data$y, y=.data$x)) + theme_bw() + geom_boxplot() +
      scale_y_continuous(breaks=x_unique_sorted, labels=x_levels)
    else {
      classes_dens <- plotres$allclasses
      colorsvec_dens <- plotres$colorsvec
      linetypesvec_dens <- plotres$linetypesvec
      
      classtab <- table(y)
      
      classes_present <- names(classtab[classtab >= 1])
      
      colorsvec <- linetypesvec <- rep("", length(classes_present))
      colorsvec[classes_present %in% classes_dens] <- colorsvec_dens
      linetypesvec[classes_present %in% classes_dens] <- linetypesvec_dens
      
      colorsvec[colorsvec==""] <- "grey"
      linetypesvec[linetypesvec==""] <- "solid"
      
      p <- ggplot(plotdata, aes(x=.data$y, y=.data$x, color=.data$y, linetype=.data$y)) + theme_bw() + geom_boxplot() +
        # The labels of the categories of the covariate are added to the x-axis:
        scale_y_continuous(breaks=x_unique_sorted, labels=x_levels) +
        scale_color_manual(values=colorsvec) +
        scale_linetype_manual(values=linetypesvec) + theme(legend.position = "none")
    }
    
  }
  
  # Add labels to the plot if provided:
  
  if (x_label=="")
    p <- p + theme(axis.title.x=element_blank())
  else
    p <- p + ylab(x_label)
  
  if (y_label=="")
    p <- p + xlab("class")
  else
    p <- p + xlab(y_label)
  
  if (plot_title!="")
    p <- p + ggtitle(plot_title)
  
  p
  
}
