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

##' Plot function for \code{interactionfor} objects that allows to obtain a first overview of the result of the
##' interaction forest analysis. This function visualises the distributions of the EIM values and
##' the estimated forms of the bivariable influences of the variable pairs with largest quantitative and 
##' qualitative EIM values. Further visual exploration of the result of the interaction
##' forest analysis should be conducted using \code{\link{plotEffects}}.
##' 
##' For details on the plots of the estimated forms of the bivariable influences of the variable pairs see \code{\link{plotEffects}}.
##' 
##' NOTE: The p-values shown in the plots are generally much too optimistic and \strong{MUST NOT} be reported 
##' as the result of a statistical test for significance of interaction. To obtain adjusted p-values that would correspond to
##' valid tests, it would be possible to multiply these p-values by the number of possible variable pairs, 
##' which would correspond to Bonferroni-adjusted p-values. See the 'Details' section of \code{\link{plotEffects}} for further
##' explanation and guidance. Note, however, that these Bonferroni-adjusted p-values should be interpreted
##' with caution because, stemming from bivariable models, these p-values do not take the multivariable nature 
##' of the data into account.
##' 
##' NOTE ALSO: As described in Hornung & Boulesteix (2022), in the case of data with larger numbers of variables (larger than 100, but more seriously
##' for high-dimensional data), the univariable EIM values can be biased. Therefore, it is strongly recommended to interpret the univariable EIM values
##' with caution, if the data are high-dimensional. If it is of interest to measure the univariable importance of the variables for high-dimensional data,
##' an additional conventional random forest (e.g., using the \code{ranger} package) should be constructed and the variable importance measure values
##' of this random forest be used for ranking the univariable effects.
##' 
##' @title Plot method for \code{interactionfor} objects
##' @param x Object of class \code{interactionfor}.
##' @param numpairsquant The number of pairs with largest quantitative EIM values to plot. Default is 2.
##' @param numpairsqual The number of pairs with largest qualitative EIM values to plot. Default is 2.
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
##' ## Construct interaction forest and calculate EIM values:
##' 
##' data(stock)
##' model <- interactionfor(dependent.variable.name = "company10", data = stock, 
##'                         num.trees = 20)
##' 
##' # NOTE: num.trees = 20 (in the above) would be much too small for practical 
##' # purposes. This small number of trees was simply used to keep the
##' # runtime of the example short.
##' # The default number of trees is num.trees = 20000 if EIM values are calculated
##' # and num.trees = 2000 otherwise.
##'
##'
##'
##' ## When using the plot() function without further specifications,
##' ## by default the estimated bivariable influences of the two pairs with largest quantitative
##' ## and qualitative EIM values are shown:
##'
##' plot(model)
##' 
##' # It is, however, also possible to change the numbers of
##' # pairs with largest quantitative and qualitative EIM values
##' # to be shown:
##' 
##' plot(model, numpairsquant = 4, numpairsqual = 3)
##' 
##' }
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R., Boulesteix, A.-L. (2022). Interaction forests: Identifying and exploiting interpretable quantitative and qualitative interaction effects. Computational Statistics & Data Analysis 171:107460, <\doi{10.1016/j.csda.2022.107460}>.
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   }
##' @seealso \code{\link{plotEffects}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @rdname plot.interactionfor
##' @export
plot.interactionfor <- function(x, numpairsquant=2, numpairsqual=2, ...) {
  
  if(is.null(x$eim.univ) & is.null(x$eim.quant) & is.null(x$eim.qual)) {
    cat("Nothing to plot, because the 'interactionfor' object does not feature EIM values.", "\n")
  } else {
    
    ps <- list()
    count <- 1
    
    if (!is.null(x$eim.univ)) {
      datatempuniv <- data.frame(eimuniv=x$eim.univ.sorted)
      ps[[count]] <- ggplot(data=datatempuniv, aes(x=1:nrow(datatempuniv), y=.data$eimuniv)) + theme_bw() + geom_point() + 
        labs(x="Index of variable", y="univariable EIM values") + ggplot2::ggtitle("univariable effects") +
        scale_x_continuous(breaks= scales::pretty_breaks())
      count <- count+1
    }
    if (!is.null(x$eim.quant)) {
      datatempquant <- data.frame(eimquant=x$eim.quant.sorted)
      ps[[count]] <- ggplot(data=datatempquant, aes(x=1:nrow(datatempquant), y=.data$eimquant)) + theme_bw() + geom_point() + 
        labs(x="Index of variable pair", y="quantitative EIM values") + ggplot2::ggtitle("quantitative interaction effects") +
        scale_x_continuous(breaks= scales::pretty_breaks())
      count <- count+1
    }
    if (!is.null(x$eim.qual)) {
      datatempqual <- data.frame(eimqual=x$eim.qual.sorted)
      ps[[count]] <- ggplot(data=datatempqual, aes(x=1:nrow(datatempqual), y=.data$eimqual)) + theme_bw() + geom_point() + 
        labs(x="Index of variable pair", y="qualitative EIM values") + ggplot2::ggtitle("qualitative interaction effects") +
        scale_x_continuous(breaks= scales::pretty_breaks())
      count <- count+1
    }
    
    p <- ggarrange(plotlist=ps, nrow=length(ps), ncol = 1)
    p <- annotate_figure(p, top = text_grob(ifelse(length(ps)==1, "Distribution of EIM values", "Distributions of EIM values"), face = "bold", size = 18))
    print(p)
    
    if (!is.null(x$eim.quant)) {
      ps <- plotEffects(intobj=x, type="quant", numpairs=numpairsquant, plotit=FALSE)
      if(length(ps)==1) {
        readline(prompt="Press [enter] for next plot.")
        p <- annotate_figure(ps[[1]], top = text_grob("Pairs with top quantitative EIM values", face = "bold", size = 18))
        print(p)
      } else {
        for(i in seq(along=ps)) {
          readline(prompt="Press [enter] for next plot.")
          p <- annotate_figure(ps[[i]], top = text_grob(paste("Pairs with top quantitative EIM values - ", as.roman(i), sep=""), face = "bold", size = 18))
          print(p)
        }
      }
    }
    
    if (!is.null(x$eim.qual)) {
      ps <- plotEffects(intobj=x, type="qual", numpairs=numpairsqual, plotit=FALSE)
      if(length(ps)==1) {
        readline(prompt="Press [enter] for next plot.")
        p <- annotate_figure(ps[[1]], top = text_grob("Pairs with top qualitative EIM values", face = "bold", size = 18))
        print(p)
      } else {
        for(i in seq(along=ps)) {
          readline(prompt="Press [enter] for next plot.")
          p <- annotate_figure(ps[[i]], top = text_grob(paste("Pairs with top qualitative EIM values - ", as.roman(i), sep=""), face = "bold", size = 18))
          print(p)
        }
      }
    }
  }
  
}
