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

##' Plot function for \code{multifor} objects that allows to obtain a first overview of the result of the
##' class-focused VIM analysis. This function visualises the distribution of the class-focused VIM values
##' together with that of the corresponding discriminatory VIM values and
##' the estimated dependency structures of the multi-class outcome on the variables 
##' with largest class-focused VIM values. These estimated dependency structures are visualised
##' using kernel density estimate-based plots and/or boxplots.
##' 
##' In the plot showing the distribution of the class-focused VIM values along with 
##' that of the discriminatory VIM values, the discriminatory VIM values are 
##' normalized to make them comparable to the class-focused VIM values. This is 
##' achieved by dividing the discriminatory VIM values by their mean and multiplying 
##' it by that of the class-focused VIM values.\cr
##' For details on the plots of the estimated dependency structures of the 
##' multi-class outcome on the variables, see \code{\link{plotMcl}}.
##' The latter function allows to visualise these estimated dependency structures
##' for arbitrary variables in the data.
##' 
##' @title Plot method for \code{multifor} objects
##' @param x Object of class \code{multifor}.
##' @param plot_type Plot type, one of the following: "both" (the default), "density", "boxplot".  If "density", kernel density estimate-based plots are produced, if "boxplot", boxplot plots are produced, and if "both", both kernel density estimate-based plots and boxplot plots are produced. See the 'Details' section of \code{\link{plotMcl}} for details.
##' @param num_best The number of variables with largest class-focused VIM values to plot. Default is 5.
##' @param ... Further arguments passed to or from other methods.
##' @return A ggplot2 plot.
##' @examples
##' \dontrun{
##' 
##' ## Load package:
##' 
##' library("diversityForest")
##' 
##' 
##' 
##' ## Set seed to make results reproducible:
##' 
##' set.seed(1234)
##' 
##' 
##' 
##' ## Construct random forest and calculate class-focused and discriminatory VIM values:
##' 
##' data(hars)
##' model <- multifor(dependent.variable.name = "Activity", data = hars, 
##'                   num.trees = 100, probability=TRUE)
##' 
##' # NOTE: num.trees = 100 (in the above) would be likely too small for practical 
##' # purposes. This small number of trees was simply used to keep the
##' # runtime of the example short.
##' # The default number of trees is num.trees = 5000 for datasets with a maximum of
##' # 5000 observations and num.trees = 1000 for datasets larger than that.
##' 
##' 
##' 
##' ## By default the estimated class-specific distributions of the num_best=5
##' ## variables with the largest class-focused VIM values are plotted:
##' 
##' plot(model)
##' 
##' ## Consider only the 2 variables with the largest class-focused VIM values:
##' 
##' plot(model, num_best = 2)
##' 
##' ## Show only the density plots or only the boxplots:
##' 
##' plot(model, plot_type = "density", num_best = 2)
##' plot(model, plot_type = "boxplot", num_best = 2)
##' 
##' ## Show only the plot of the distributions of the class-focused and
##' ## discriminatory VIM values:
##' 
##' plot(model, num_best = 0)
##' 
##' }
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   }
##' @seealso \code{\link{plotMcl}}
##' @encoding UTF-8
##' @importFrom ggplot2 ggplot aes theme_bw geom_point scale_color_manual ylab theme element_blank
##' @importFrom rlang .data
##' @rdname plot.multifor
##' @export
plot.multifor <- function(x, plot_type=c("both", "density", "boxplot")[1], num_best=5, ...) {
  
  if (num_best < 0)
    stop("'num_best' must be an integer greater than or equal to zero.")
  
  # Extract the class-focused and discriminatory VIM values sort the values in
  # decreasing order according to the multi-clas VIM values:
  
  vim_multiclass <- x$class_foc_vim
  vim_discr <- x$discr_vim
  
  if (all(is.na(vim_multiclass)))
    stop("There are no (non-NA) class-focused VIM values.")
  
  vim_multiclass_noNA <- vim_multiclass[!is.na(vim_multiclass)]
  vim_discr_noNA <- vim_discr[!is.na(vim_multiclass)]
  
  reorderind <- order(vim_multiclass_noNA, decreasing=TRUE)
  vim_multiclass_order <- vim_multiclass_noNA[reorderind]
  vim_discr_order <- vim_discr_noNA[reorderind]
  
  
  # Rescale the discriminatory VIM values, so that they have the same
  # mean as the class-focused VIM values. This is done so that the two types of
  # VIM can be compared visually:
  
  vim_discr_order_resc <- vim_discr_order*mean(vim_multiclass_order)/mean(vim_discr_order)
  
  
  # Plot the class-focused and discriminatory VIM values:
  
  vim_multiclass_names <- names(vim_multiclass_order)
  
  datacov <- x$plotres$data[,vim_multiclass_names, drop=FALSE]
  y_outcome <- x$plotres$data[,x$plotres$yvarname]
  
  dataplot <- data.frame(x2=rep(1:length(vim_multiclass_order), 2), vim=c(vim_multiclass_order, vim_discr_order_resc),
                         type=factor(rep(c("class-focused", "discriminatory (normalized)"), each=length(vim_multiclass_order)), levels=c("class-focused", "discriminatory (normalized)")))
  
  p <- ggplot(data=dataplot, aes(x=.data$x2, y=.data$vim, color=.data$type)) + theme_bw() + geom_point() + 
    scale_color_manual(values=c("black", "grey"), name="VIM type") + ylab("VIM value") +
    theme(legend.position = c(0.95, 0.95),  # Coordinates for top-right corner
          legend.justification = c(1, 1),
          axis.title.x = element_blank())
  print(p)
  readline(prompt="Press [enter] for next plot.")
  
  if (num_best > 0) {
    
    if (num_best > ncol(datacov)) {
      warning(paste0("The value num_best=", num_best, " is larger than the number of covariates with class-focused VIM values. --> The value num_best was set to ", ncol(datacov), "."))
      num_best <- ncol(datacov)
    }
    
    
    # Make plots of the class-specific distributions of the values of the num_best
    # covariates with the largest class-focused VIM values:
    
    for(i in 1:num_best) {
      plot_title <- paste0(vim_multiclass_names[i], "  -  rank ", i, " according to the class-focused VIM")
      plotVar(datacov[,i], y_outcome, x_label=vim_multiclass_names[i], y_label = x$plotres$yvarname, plot_title=plot_title, plot_type=plot_type, plotit=TRUE)
      if(i < num_best)
        readline(prompt="Press [enter] for next plot.")
    }
    
  }
  
}
