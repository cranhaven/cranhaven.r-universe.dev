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

##' This function allows to visualise the (estimated) distributions of one or several variables for each of the classes of the outcomes.
##' This allows to study how exactly variables of interest are associated with the outcome, which is crucial for interpretive purposes.
##' Two types of visualisations are available: density plots and boxplots. See the 'Details' section below for further explanation.
##'
##' For the \code{"density"} plots, kernel density estimates (obtained using the 
##' \code{density()} function from base R) of the within-class distributions are 
##' plotted in the same plot using different colors and, depending on the number 
##' of classes, different line types. To account for the different number of
##' observations per class, each density is multiplied by the proportion of 
##' observations from that class. The resulting scaled densities can be interpreted 
##' in terms of the local density of the observations from each class relative to 
##' those from the other classes. For example, if a scaled density has the largest 
##' value in a particular region, this can be interpreted as the respective class 
##' being the most frequent in that region. Another example: If the scaled density 
##' of class "A" is twice as large as the scaled density of class "B" in a particular 
##' region, this can be interpreted to mean that there are twice as many observations 
##' of class "A" as of class "B" in that region.
##' 
##' In the \code{"density"} plots, only classes represented by at least two 
##' observations are considered. If the number of classes is greater than 7, 
##' the different classes are distinguished using both colors and line styles. 
##' To indicate the absolute numbers of observations in the different regions, 
##' the locations of the observations from the different classes are visualized 
##' using a rug plot on the x-axis, using the same colors and line types as for 
##' the density plots. If the number of observations is greater than 1,000, a 
##' random subset of 1,000 observations is shown in the rug plot instead of all 
##' observations for visual clarity.
##' 
##' The \code{"boxplot"} plots show the (estimated) within-class distributions 
##' side by side using boxplots. All classes are considered, even those represented 
##' by only a single observation. For the \code{plot_type="both"} option, which 
##' displays both \code{"density"} and \code{"boxplot"} plots, the boxplots are 
##' displayed using the same colors and (if applicable) line styles as the kernel 
##' density estimates, for clarity. Boxplots of classes for which no kernel density 
##' estimates were obtained (i.e., those of the classes represented by single 
##' observations) are shown in grey. 
##' 
##' Note that plots are only generated for those variables in \code{varnames} 
##' that have at least as many unique values as there are outcome classes. For 
##' categorical variables, the category labels are printed on the x- or y-axis 
##' of the \code{"density"} or \code{"boxplot"} plots, respectively. The rug plots 
##' of the \code{"density"} plots are produced only for numeric variables.
##' 
##' @title Plots of the (estimated) within-class distributions of variables
##' @param data Data frame containing the variables.
##' @param yvarname Name of outcome variable.
##' @param varnames Names of the variables for which plots should be created.
##' @param plot_type Plot type, one of the following: "both" (the default), "density", "boxplot".  If "density", \code{"density"} plot are produced, if "boxplot", \code{"boxplot"} plots are produced, and if "both", both \code{"density"} plots and \code{"boxplot"} plots are produced. See the 'Details' section below for details.
##' @param addtitles Set to \code{TRUE} (default) to add headings providing the names of the respective variables to the plots.
##' @param plotit This states whether the plots are actually plotted or merely returned as \code{ggplot} objects. Default is \code{TRUE}.
##' @return A list returned invisibly. The list has length equal to the number of elements in \code{varnames}. 
##' Each element corresponds to one variable and contains a list of \code{ggplot2} plots structured as in the output of \code{\link{plotVar}}.
##' @examples
##' \dontrun{
##'
##' ## Load package:
##' 
##' library("diversityForest")
##' 
##' 
##' 
##' ## Plot "density" and "boxplot" plots (default: plot_type = "both") for the 
##' ## first three variables in the "hars" dataset:
##' 
##' data(hars)
##' plotMcl(data = hars, yvarname = "Activity", varnames = c("tBodyAcc.mean...X", 
##'                                                          "tBodyAcc.mean...Y", 
##'                                                          "tBodyAcc.mean...Z"))
##' 
##' 
##' ## Plot only the "density" plots for these variables:
##' 
##' plotMcl(data = hars, yvarname = "Activity", 
##'         varnames = c("tBodyAcc.mean...X", "tBodyAcc.mean...Y", 
##'                      "tBodyAcc.mean...Z"), plot_type = "density")
##' 
##' ## Plot the "density" plots for these variables, but without titles of the
##' ## plots:
##' 
##' plotMcl(data = hars, yvarname = "Activity", varnames = 
##'           c("tBodyAcc.mean...X", "tBodyAcc.mean...Y", "tBodyAcc.mean...Z"), 
##'         plot_type = "density", addtitles = FALSE)
##' 
##' 
##' ## Make density plots for these variables, but only save them in a list "ps"
##' ## without plotting them ("plotit = FALSE"):
##' 
##' ps <- plotMcl(data = hars, yvarname = "Activity", varnames = 
##'                 c("tBodyAcc.mean...X", "tBodyAcc.mean...Y", 
##'                   "tBodyAcc.mean...Z"), plot_type = "density", 
##'               addtitles = FALSE, plotit = FALSE)
##' 
##' 
##' ## The plots can be manipulated later by using ggplot2 functionalities:
##' 
##' library("ggplot2")
##' p1 <- ps[[1]]$dens_pl + ggtitle("First variable in the dataset") + 
##'   labs(x="Variable values", y="my scaled density")
##' 
##' p2 <- ps[[3]]$dens_pl + ggtitle("Third variable in the dataset") + 
##'   labs(x="Variable values", y="my scaled density")
##' 
##' 
##' ## Combine both of the above plots:
##' 
##' library("ggpubr")
##' p <- ggarrange(p1, p2, ncol = 2)
##' p
##' 
##' ## # Save as PDF:
##' ## ggsave(file="mypathtofolder/FigureXY1.pdf", width=14, height=6)
##' 
##' }
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   }
##' @seealso \code{\link{plot.multifor}}, \code{\link{plotVar}}
##' @encoding UTF-8
##' @import stats 
##' @import utils
##' @export
plotMcl <- function(data, yvarname, varnames, plot_type=c("both", "density", "boxplot")[1], addtitles = TRUE, plotit = TRUE) {
  
  if (!all(varnames %in% names(data)))
    stop("Not all entries of 'varnames' are found in 'data'.")
  
  datacov <- data[,varnames, drop=FALSE]
  y_outcome <- data[,yvarname]
  
  # Plots are created only for covariates that have at least as many unique values 
  # as there are outcome classes:
  suit_inds <- which(apply(datacov, 2, function(x) length(unique(x)) >= length(unique(y_outcome))))
  
  if (length(suit_inds)==0)
    stop("None of the variables in 'varnames' have at least as many unique values as there are outcome classes. --> Nothing to plot.")
  
  if (length(suit_inds) < ncol(datacov)) {
    varnames_notused <- paste0("\"", varnames[-suit_inds], "\"")
    warning(paste0("Not all variables in 'varnames' have at least as many unique values as there are outcome classes.\n--> For the following variables in 'varnames' no plots were generated:\n", paste(varnames_notused, collapse=", ")))
    datacov <- datacov[,suit_inds]
    varnames <- varnames[suit_inds]
  }
  
  
  # Create the plots and store them in a list:
  
  ps <- list()
  for(i in 1:ncol(datacov)) {
    plot_title <- ""
    if (addtitles)
      plot_title <- varnames[i]
    p <- plotVar(datacov[,i], y_outcome, x_label=varnames[i], y_label=yvarname, plot_title=plot_title, plot_type=plot_type, plotit=plotit)
    if (plotit) {
      if(i < ncol(datacov))
        readline(prompt="Press [enter] for next plot.")
    }
    ps[[i]] <- p
  }
    
  names(ps) <- names(datacov)
  
  
  # Return the list of plots invisibly:
  
  invisible(ps)
  
}
