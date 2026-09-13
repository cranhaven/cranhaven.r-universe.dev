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

##' This function allows to visualise the (estimated) bivariable influence of a single specific pair of variables on the outcome. The estimation
##' and plotting is performed in the same way as in \code{\link{plotEffects}}. However, \code{plotPair} does not require an \code{interactionfor} object
##' and can thus be used also without a constructed interaction forest.
##'
##' See the 'Details' section of \code{\link{plotEffects}}.
##' 
##' @title Plot of the (estimated) simultaneous influence of two variables
##' @param pair Character string vector of length two, where the first character string gives the name of the first member of the respective pair to plot and the second character string gives the name of the second member.
##' Note that the order of the two pair members in \code{pair} determines how the results are 
##' visualised: The estimated influence of the second pair member is visualised conditionally
##' on different values of the first pair member.
##' @param yvarname Name of outcome variable.
##' @param statusvarname Name of status variable, only applicable to survival data.
##' @param data Data frame containing the variables.
##' @param levelsorder1 Optional. Order the categories of the first variable should have in the plot (if it is categorical). Character string vector, where the
##' i-th entry contains the name of the category that should take the i-th place in the ordering of the categories of the first variable.
##' @param levelsorder2 Optional. Order the categories of the second variable should have in the plot (if it is categorical). Character string vector specified in an analogous
##' way as \code{levelsorder1}.
##' @param categprob Optional. Only relevant for categorical outcomes with more than two classes. 
##' Name of the class for which probabilities should be estimated. As described in \code{\link{plotEffects}}, 
##' for categorical outcomes with more than two classes, by default the probabilities
##' for the largest class (i.e., the class with the most observations) are estimated when visualising the
##' bivariable influence of the variables. Using \code{categprob} a different class can be specified for the
##' class for which probabilities should be estimated.
##' @param pvalue Set to \code{TRUE} (default) to add to the plot a p-value from a test for interaction effect obtained using a classical
##' parametric regression approach. For categorical outcomes logistic regression is used, for metric outcomes linear
##' regression and for survival outcomes Cox regression. See the 'Details' section of \code{\link{plotEffects}} for further details.
##' @param returnseparate Set to \code{TRUE} to return invisibly the two generated ggplot plots separately in the form of a list. The
##' latter option is useful, because it allows to manipulate the resulting plots (label size etc.) and makes it possible to consider
##' only one of the two plots. The default is \code{FALSE}, which results in the two plots being returned together in the form of a 
##' \code{ggarrange} object.
##' @param intobj Optional. Object of class \code{interactionfor}. If this is provided, the ordering of the categories
##' obtained when constructing the interaction forest will be used for categorical variables. See Hornung & Boulesteix (2022) for details.
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
##' ## Visualise the estimated bivariable influence of 'toothed' and 'feathers' on
##' ## the probability of type="mammal":
##' 
##' data(zoo)
##' plotPair(pair = c("toothed", "feathers"), yvarname="type", data = zoo)
##' 
##' 
##' 
##' ## Visualise the estimated bivariable influence of 'creat' and 'hgb' on
##' ## survival (more precisely, on the log hazards ratio compared to the
##' ## median effect):
##' 
##' library("survival")
##' mgus2compl <- mgus2[complete.cases(mgus2),]
##' plotPair(pair=c("creat", "hgb"), yvarname="futime", statusvarname = "death", data=mgus2compl)
##' 
##' # Problem: The outliers in the left plot make it difficult to see what is going
##' # on in the region with creat values smaller than about two even though the
##' # majority of values lie there.
##' 
##' # --> Solution: We re-run the above line setting returnseparate = TRUE, because
##' # this allows to get the two ggplot plots separately, which can then be manipulated
##' # to change the x-axis range in order to remove the outliers:
##' 
##' ps <- plotPair(pair=c("creat", "hgb"), yvarname="futime", statusvarname = "death", 
##'                data=mgus2compl, returnseparate = TRUE)
##' 
##' # Change the x-axis range:
##' library("ggplot2")
##' ps[[1]] + xlim(c(0.5,2))
##' # Save the plot:
##' # ggsave(file="mypathtofolder/FigureXY1.pdf", width=7, height=6)
##'
##' # We can, for example, also change the label sizes of the second plot:
##' # With original label sizes:
##' ps[[2]]
##' # With larger label sizes:
##' ps[[2]] +  theme(axis.title=element_text(size=15))
##' # Save the plot:
##' # library("ggplot2")
##' # ggsave(file="mypathtofolder/FigureXY2.pdf", width=7, height=6)
##' 
##' }
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R., Boulesteix, A.-L. (2022). Interaction forests: Identifying and exploiting interpretable quantitative and qualitative interaction effects. Computational Statistics & Data Analysis 171:107460, <\doi{10.1016/j.csda.2022.107460}>.
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   }
##' @seealso \code{\link{plotEffects}}, \code{\link{plot.interactionfor}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @importFrom ggplot2 aes annotate geom_jitter geom_line geom_point geom_tile ggplot labs scale_color_discrete scale_x_continuous theme_bw
##' @importFrom ggpubr ggarrange annotate_figure text_grob
##' @importFrom rlang .data
##' @export
plotPair <- function(pair, yvarname, statusvarname=NULL, data, levelsorder1=NULL, levelsorder2=NULL, categprob=NULL, pvalue=TRUE, returnseparate=FALSE, intobj=NULL) {
  
  x1name <- pair[1]
  x2name <- pair[2]
  
  if(!is.null(intobj)) {
    if(is.null(levelsorder1))
      levelsorder1 <- intobj$forest$covariate.levels[[x1name]]
    if(is.null(levelsorder2))
      levelsorder2 <- intobj$forest$covariate.levels[[x2name]]
  } else {
    if(is.null(levelsorder1))
      levelsorder1 <- levels(data[,x1name])
    if(is.null(levelsorder2))
      levelsorder2 <- levels(data[,x2name])
  }
  
  x1 <- data[,x1name]
  x2 <- data[,x2name]
  y <- data[,yvarname]
  
  if(length(class(x1))==2 & all(class(x1)==c("ordered", "factor"))) {
    class(x1) <- "factor"
  }
  if(length(class(x2))==2 & all(class(x2)==c("ordered", "factor"))) {
    class(x2) <- "factor"
  }
  
  if(inherits(y, "factor") & length(unique(y)) > 2) {
    taby <- table(y)
	if (is.null(categprob))
      categprob <- names(taby)[which.max(taby)]
    ynew <- rep(categprob, length(y))
    ynew[y!=categprob] <- paste("not", categprob)
    ynew <- factor(ynew, levels=c(categprob, paste("not", categprob)))
    y <- ynew
  }
  
  if(!is.null(statusvarname)) {
    status <- data[,statusvarname]
  } else {
    status <- NULL
  }
  
  if((class(x1) %in% c("numeric", "integer")) & (class(x2) %in% c("numeric", "integer"))) {
    return(plotNumNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, status=status, yvarname=yvarname, statusvarname=statusvarname, pvalue=pvalue, returnseparate=returnseparate))
  }
  
  if(((class(x1) %in% c("numeric", "integer")) & inherits(x2, "factor")) | (inherits(x1, "factor") & (class(x2) %in% c("numeric", "integer")))) {
    
    if((class(x1) %in% c("numeric", "integer")) & inherits(x2, "factor")) {
      x1safe <- x1
      x1namesafe <- x1name
      x1 <- x2
      x1name <- x2name
      levelsorder1 <- levelsorder2
      x2 <- x1safe
      x2name <- x1namesafe
    }
    
    return(plotCatNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, status=status, yvarname=yvarname, 
                      statusvarname=statusvarname, levelsorder1=levelsorder1, pvalue=pvalue, returnseparate=returnseparate))
  }
  
  
  if(inherits(x1, "factor") & inherits(x2, "factor")) {
    return(plotCatCat(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, status=status, yvarname=yvarname, 
                      statusvarname=statusvarname, levelsorder1=levelsorder1, levelsorder2=levelsorder2,
                      pvalue=pvalue, returnseparate=returnseparate))
  }
  
}





plotNumNum <- function(x1, x2, x1name, x2name, y, status, yvarname, statusvarname, pvalue, returnseparate)
{
  
  if(inherits(y, "factor")) {
    return(plotBinNumNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, 
                         pvalue=pvalue, returnseparate=returnseparate))
  }
  
  if(class(y) %in% c("numeric", "integer")) {
    if(is.null(statusvarname))
      return(plotMetricNumNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, 
                              pvalue=pvalue, returnseparate=returnseparate))
    else
      return(plotSurvNumNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, status=status, 
                            statusvarname=statusvarname, pvalue=pvalue, returnseparate=returnseparate))
  }
  
  
}



plotBinNumNum <- function(x1, x2, x1name, x2name, y, yvarname, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  ynum1 <- as.numeric(y)-1
  ynum <- ynum1
  ynum[ynum1==0] <- 1
  ynum[ynum1==1] <- 0
  loessfit <- suppressWarnings(loess(ynum ~ x1 * x2))
  
  # Grids for x1 and x2:
  x1qus <- quantile(x1, c(0.05, 0.95))
  if(x1qus[1]==x1qus[2])
    x1qus <- quantile(x1, c(0, 1))
  x2qus <- quantile(x2, c(0.05, 0.95))
  if(x2qus[1]==x2qus[2])
    x2qus <- quantile(x2, c(0, 1))
  x1grid <-  seq(x1qus[1], x1qus[2], length=100)
  x2grid <-  seq(x2qus[1], x2qus[2], length=100)
  
  # Common grid of x1 and x2:
  x1x2grid <-  expand.grid(x1 = x1grid, x2 = x2grid)
  
  # Delete all points from the common grid of x1 and x2
  # that are not contained in the convex hull of the
  # the points (x1, x2):
  inclbool <- (x1 >= x1qus[1] & x1 <= x1qus[2]) & (x2 >= x2qus[1] & x2 <= x2qus[2])
  ch <- grDevices::chull(x=x1[inclbool], y=x2[inclbool])
  inds <- c(ch, ch[1])
  keepbool <- sgeostat::in.chull(x0=x1x2grid$x1, y0=x1x2grid$x2, x=x1[inclbool][inds], y=x2[inclbool][inds])
  
  x1x2grid <- x1x2grid[keepbool,]
  
  
  # Feed the dataframe into the loess model and receive a matrix output with estimates of
  # acceleration for each combination of wt and hp
  attr(x1x2grid, "out.attrs") <- NULL
  mtrx3d <-  suppressWarnings(predict(loessfit, newdata = x1x2grid))
  mtrx3d[mtrx3d>1] <- 1
  mtrx3d[mtrx3d<0] <- 0
  
  x1x2grid$yprob <- mtrx3d
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() +  ggplot2::geom_contour_filled(data=x1x2grid, aes(x=.data$x1, .data$x2, z=.data$yprob)) +
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y)) + 
    ggplot2::scale_fill_viridis_d(name=paste("Est. prob. for\n'", levels(y)[1], "'", sep="")) + 
    ggplot2::scale_color_manual(name=yvarname, values=c("black", "red")) + labs(x=x1name, y=x2name)
  
  datalines <- do.call("rbind", lapply(1:9/10, function(qus) {
    dat <- x1x2grid[x1x2grid$x1==x1x2grid$x1[which.min(abs(x1x2grid$x1 - quantile(x1, qus)))],]
    dat$qu <- paste(quantile(x1, qus), " (", round(qus*100)," %)", sep="")
    return(dat)
  }))
  datalines$qu <- factor(datalines$qu, levels=paste(quantile(x1, 1:9/10), " (", round(1:9/10*100)," %)", sep=""))#paste(round(((1:9)/10)*100), "%"))
  
  p2 <- ggplot(data=datalines, aes(x=.data$x2, y=.data$yprob, color=.data$qu)) + theme_bw() + geom_line(position=ggplot2::position_dodge(width=diff(range(datalines$x2))*0.01)) + 
    ggplot2::scale_color_brewer(name=paste(x1name, "\n(deciles)", sep=""), palette = "RdYlBu") + labs(x=x2name, y=paste("Est. prob. for '", levels(y)[1], "'", sep="")) + ggplot2::ylim(c(0,1))
  
  if(pvalue) {
    pinter <- summary(glm(y ~ x1*x2, family=binomial))$coef[4,4]
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datalines$x2, na.rm=TRUE) + 0.7*diff(range(datalines$x2, na.rm=TRUE)), y=0.1, label= paste("Test for interaction\neffect using\nlogistic regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}




plotMetricNumNum <- function(x1, x2, x1name, x2name, y, yvarname, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  loessfit <- try(suppressWarnings(loess(y ~ x1 * x2)), silent=TRUE)
  if(inherits(loessfit, "try-error"))
    loessfit <- lm(y ~ x1 * x2)
  
  # Grids for x1 and x2:
  x1qus <- quantile(x1, c(0.05, 0.95))
  if(x1qus[1]==x1qus[2])
    x1qus <- quantile(x1, c(0, 1))
  x2qus <- quantile(x2, c(0.05, 0.95))
  if(x2qus[1]==x2qus[2])
    x2qus <- quantile(x2, c(0, 1))
  x1grid <-  seq(x1qus[1], x1qus[2], length=100)
  x2grid <-  seq(x2qus[1], x2qus[2], length=100)
  
  # Common grid of x1 and x2:
  x1x2grid <-  expand.grid(x1 = x1grid, x2 = x2grid)
  
  # Delete all points from the common grid of x1 and x2
  # that are not contained in the convex hull of the
  # the points (x1, x2):
  inclbool <- (x1 >= x1qus[1] & x1 <= x1qus[2]) & (x2 >= x2qus[1] & x2 <= x2qus[2])
  ch <- grDevices::chull(x=x1[inclbool], y=x2[inclbool])
  inds <- c(ch, ch[1])
  keepbool <- sgeostat::in.chull(x0=x1x2grid$x1, y0=x1x2grid$x2, x=x1[inclbool][inds], y=x2[inclbool][inds])
  
  x1x2grid <- x1x2grid[keepbool,]
  
  
  # Feed the dataframe into the loess model and receive a matrix output with estimates of
  # acceleration for each combination of wt and hp
  attr(x1x2grid, "out.attrs") <- NULL
  mtrx3d <-  suppressWarnings(predict(loessfit, newdata = x1x2grid))
  
  x1x2grid$yhat <- mtrx3d
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() +  ggplot2::geom_contour_filled(data=x1x2grid, aes(x=.data$x1, .data$x2, z=.data$yhat)) +
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y)) +
    ggplot2::scale_color_gradient(name=yvarname, low="white", high="black") + ggplot2::scale_fill_viridis_d(name=paste("Est. mean of\n", yvarname, sep="")) + 
    labs(x=x1name, y=x2name)
  
  datalines <- do.call("rbind", lapply(1:9/10, function(qus) {
    dat <- x1x2grid[x1x2grid$x1==x1x2grid$x1[which.min(abs(x1x2grid$x1 - quantile(x1, qus)))],]
    dat$qu <- paste(quantile(x1, qus), " (", round(qus*100)," %)", sep="")
    return(dat)
  }))
  datalines$qu <- factor(datalines$qu, levels=paste(quantile(x1, 1:9/10), " (", round(1:9/10*100)," %)", sep=""))#paste(round(((1:9)/10)*100), "%"))
  
  p2 <- ggplot(data=datalines, aes(x=.data$x2, y=.data$yhat, color=.data$qu)) + theme_bw() + geom_line(position=ggplot2::position_dodge(width=diff(range(datalines$x2))*0.01)) +
    ggplot2::scale_color_brewer(name=paste(x1name, "\n(deciles)", sep=""), palette = "RdYlBu") + labs(x=x2name, y=paste("Est. mean of ", yvarname, sep=""))
  
  if(pvalue) {
    pinter <- summary(lm(y ~ x1*x2))$coef[4,4]
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datalines$x2, na.rm=TRUE) + 0.7*diff(range(datalines$x2, na.rm=TRUE)), y=min(datalines$yhat, na.rm=TRUE) + 0.1*diff(range(datalines$yhat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nlinear regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}



plotSurvNumNum <- function(x1, x2, x1name, x2name, y, yvarname, status, statusvarname, pvalue, returnseparate)
{
  
  dataloess <- data.frame(y=y, status=status, x1=x1, x2=x2)
  lo <- gam::lo
  loessfit <- try(suppressWarnings(MapGAM::gamcox(survival::Surv(y, status) ~ lo(x1, x2), data=dataloess, span=0.5, loess.trace="approximate")), silent=TRUE)
  errloess <- FALSE
  if(inherits(loessfit, "try-error")) {
    errloess <- TRUE
    loessfit <- survival::coxph(survival::Surv(y, status) ~ x1*x2, data=dataloess)
    medianloghaz <- median(predict(loessfit, newdata=dataloess))
  }
  
  # Grids for x1 and x2:
  x1qus <- quantile(x1, c(0.05, 0.95))
  if(x1qus[1]==x1qus[2])
    x1qus <- quantile(x1, c(0, 1))
  x2qus <- quantile(x2, c(0.05, 0.95))
  if(x2qus[1]==x2qus[2])
    x2qus <- quantile(x2, c(0, 1))
  x1grid <-  seq(x1qus[1], x1qus[2], length=100)
  x2grid <-  seq(x2qus[1], x2qus[2], length=100)
  
  # Common grid of x1 and x2:
  x1x2grid <-  expand.grid(x1 = x1grid, x2 = x2grid)
  
  # Delete all points from the common grid of x1 and x2
  # that are not contained in the convex hull of the
  # the points (x1, x2):
  inclbool <- (x1 >= x1qus[1] & x1 <= x1qus[2]) & (x2 >= x2qus[1] & x2 <= x2qus[2])
  ch <- grDevices::chull(x=x1[inclbool], y=x2[inclbool])
  inds <- c(ch, ch[1])
  keepbool <- sgeostat::in.chull(x0=x1x2grid$x1, y0=x1x2grid$x2, x=x1[inclbool][inds], y=x2[inclbool][inds])
  
  x1x2grid <- x1x2grid[keepbool,]
  
  
  # Feed the dataframe into the loess model and receive a matrix output with estimates of
  # acceleration for each combination of wt and hp
  attr(x1x2grid, "out.attrs") <- NULL
  if(!errloess)
    mtrx3d <-  suppressWarnings(MapGAM::predict.gamcox(loessfit, newdata = x1x2grid)$pred)
  else
    mtrx3d <- suppressWarnings(predict(loessfit, newdata=x1x2grid)) - medianloghaz
  
  x1x2grid$loghazrat <- mtrx3d
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)[status==1,]
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  mypalette <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdYlGn")))
  p1 <- ggplot() + theme_bw() +  ggplot2::geom_contour_filled(data=x1x2grid, aes(x=.data$x1, .data$x2, z=.data$loghazrat)) +
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y)) + geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2), shape=21) +
    ggplot2::scale_color_continuous(name=yvarname, type="viridis") + ggplot2::discrete_scale("fill", "manual", mypalette, name="Log hazards ratio\ncompared to the\nmedian effect") +
    labs(x=x1name, y=x2name)

  datalines <- do.call("rbind", lapply(1:9/10, function(qus) {
    dat <- x1x2grid[x1x2grid$x1==x1x2grid$x1[which.min(abs(x1x2grid$x1 - quantile(x1, qus)))],]
    dat$qu <- paste(quantile(x1, qus), " (", round(qus*100)," %)", sep="")
    return(dat)
  }))
  datalines$qu <- factor(datalines$qu, levels=paste(quantile(x1, 1:9/10), " (", round(1:9/10*100)," %)", sep=""))
  
  p2 <- ggplot(data=datalines, aes(x=.data$x2, y=.data$loghazrat, color=.data$qu)) + theme_bw() + geom_line(position=ggplot2::position_dodge(width=diff(range(datalines$x2))*0.01)) +
    ggplot2::scale_color_brewer(name=paste(x1name, "\n(deciles)", sep=""), palette = "RdYlBu") + labs(x=x2name, y="Log hazards ratio compared to the median effect")
  
  if(pvalue) {
    pinter <- summary(survival::coxph(survival::Surv(y, status) ~ x1*x2, data =  dataloess))$coef[3,"Pr(>|z|)"]
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datalines$x2, na.rm=TRUE) + 0.7*diff(range(datalines$x2, na.rm=TRUE)), y=min(datalines$loghazrat, na.rm=TRUE) + 0.1*diff(range(datalines$loghazrat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nCox regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}





plotCatNum <- function(x1, x2, x1name, x2name, y, status, yvarname, statusvarname, levelsorder1, pvalue, returnseparate)
{
  
  if(inherits(y, "factor")) {
    return(plotBinCatNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, levelsorder1=levelsorder1, pvalue=pvalue, 
                         returnseparate=returnseparate))
  }
  
  if(class(y) %in% c("numeric", "integer")) {
    if(is.null(statusvarname))
      return(plotMetricCatNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, levelsorder1=levelsorder1,
                              pvalue=pvalue, returnseparate=returnseparate))
    else
      return(plotSurvCatNum(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, status=status, statusvarname=statusvarname, 
                            levelsorder1=levelsorder1, pvalue=pvalue, returnseparate=returnseparate))
  }
  
}



plotBinCatNum <- function(x1, x2, x1name, x2name, y, yvarname, levelsorder1, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  ynum1 <- as.numeric(y)-1
  ynum <- ynum1
  ynum[ynum1==0] <- 1
  ynum[ynum1==1] <- 0
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  
  x1 <- as.numeric(x1cat)
  categsx1 <- sort(unique(x1))
  
  x2ranges <- sapply(1:length(categsx1), function(x) quantile(x2[x1==x], c(0.05, 0.95)))
  x2rangeall <- range(x2ranges)
  x2gridall <- seq(x2rangeall[1], x2rangeall[2], length=length(categsx1)*100)
  x1x2grid <- expand.grid(x2=x2gridall, x1=categsx1)[,2:1]
  x1x2grid$yprob <- NA
  
  for(i in 1:length(categsx1)) {
    loessdat <- data.frame(x=x2[x1==categsx1[i]], y=ynum[x1==categsx1[i]])
    loessfit <- suppressWarnings(loess(y ~ x, data=loessdat))
    x1x2gridtemp <- x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]
    names(x1x2gridtemp)[2] <- "x"
    preds <- try(suppressWarnings(predict(loessfit, newdata=x1x2gridtemp)), silent=TRUE)
    if(inherits(preds, "try-error")) {
      lmfit <- lm(y ~ x, data=loessdat)
      preds <- predict(lmfit, newdata=x1x2gridtemp)
    }
    preds[preds < 0] <- 0
    preds[preds > 1] <- 1
    x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]$yprob <- preds
  }
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$yprob), width=0.8) +
    geom_jitter(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), width=0.2, height=0) + ggplot2::scale_fill_viridis_c(name=paste("Est. prob. for\n'", levels(y)[1], "'", sep=""), na.value=NA) + 
    ggplot2::scale_color_manual(name=yvarname, values=c("black", "red")) + 
    scale_x_continuous(breaks=1:length(categsx1), labels=levelsorder1) + labs(x=x1name, y=x2name)
  
  datatemp <- x1x2grid[complete.cases(x1x2grid),]
  
  p2 <- ggplot(data=datatemp, aes(x=.data$x2, y=.data$yprob, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + 
    geom_line() +
    scale_color_discrete(name=x1name) + labs(x=x2name, y=paste("Est. prob. for '", levels(y)[1], "'", sep="")) + ggplot2::ylim(c(0,1))
  
  if(pvalue) {
    coefs <- summary(glm(y ~ x1cat*x2, family=binomial))$coef
    pvals <- coefs[(length(levels(x1cat))+2):nrow(coefs), "Pr(>|z|)"]
    pinter <- min(p.adjust(pvals, method = "holm"))
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datatemp$x2, na.rm=TRUE) + 0.7*diff(range(datatemp$x2, na.rm=TRUE)), y=0.1, label= paste("Test for interaction\neffect using\nlogistic regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}


plotMetricCatNum <- function(x1, x2, x1name, x2name, y, yvarname, levelsorder1, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  
  x1 <- as.numeric(x1cat)
  categsx1 <- sort(unique(x1))
  
  x2ranges <- sapply(1:length(categsx1), function(x) quantile(x2[x1==x], c(0.05, 0.95)))
  x2rangeall <- range(x2ranges)
  x2gridall <- seq(x2rangeall[1], x2rangeall[2], length=length(categsx1)*100)
  x1x2grid <- expand.grid(x2=x2gridall, x1=categsx1)[,2:1]
  x1x2grid$yhat <- NA
  
  for(i in 1:length(categsx1)) {
    loessdat <- data.frame(x=x2[x1==categsx1[i]], y=y[x1==categsx1[i]])
    if(length(unique(loessdat$x)) > 1) {
      loessfit <- suppressWarnings(loess(y ~ x, data=loessdat))
      x1x2gridtemp <- x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]
      names(x1x2gridtemp)[2] <- "x"
      preds <- try(suppressWarnings(predict(loessfit, newdata=x1x2gridtemp)), silent=TRUE)
      if(inherits(preds, "try-error")) {
        lmfit <- lm(y ~ x, data=loessdat)
        preds <- predict(lmfit, newdata=x1x2gridtemp)
      }
    } else {
      preds <- mean(loessdat$y)
    }
    if(sum(x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i]) > 0)
      x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]$yhat <- preds
    else {
      tempval <- mean(c(x2ranges[1,i], x2ranges[2,i]))
      gridval <- x1x2grid$x2[which.min(abs(x1x2grid$x2 - tempval))]
      x1x2grid[x1x2grid$x2==gridval & x1x2grid$x1==categsx1[i],]$yhat <- preds
    }
  }
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$yhat), width=0.8) +
    geom_jitter(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), width=0.2, height=0) + ggplot2::scale_fill_viridis_c(name=paste("Est. mean of\n", yvarname, sep=""), na.value=NA) + 
    ggplot2::scale_color_gradient(name=yvarname, low="white", high="black") + 
    scale_x_continuous(breaks=1:length(categsx1), labels=levelsorder1) + labs(x=x1name, y=x2name)
  
  datatemp <- x1x2grid[complete.cases(x1x2grid),]
  
  p2 <- ggplot(data=datatemp, aes(x=.data$x2, y=.data$yhat, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + 
    geom_line() +
    scale_color_discrete(name=x1name) + labs(x=x2name, y=paste("Est. mean of ", yvarname, sep=""))
  
  if (pvalue) {
    coefs <- summary(lm(y ~ x1cat*x2))$coef
    pvals <- coefs[(length(levels(x1cat))+2):nrow(coefs), "Pr(>|t|)"]
    pinter <- min(p.adjust(pvals, method = "holm"))
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datatemp$x2, na.rm=TRUE) + 0.7*diff(range(datatemp$x2, na.rm=TRUE)), y=min(datatemp$yhat, na.rm=TRUE) + 0.1*diff(range(datatemp$yhat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nlinear regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}



plotSurvCatNum <- function(x1, x2, x1name, x2name, y, yvarname, status, statusvarname, levelsorder1, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  
  x1 <- as.numeric(x1cat)
  categsx1 <- sort(unique(x1))
  
  x2ranges <- sapply(1:length(categsx1), function(x) quantile(x2[x1==x], c(0.05, 0.95)))
  x2rangeall <- range(x2ranges)
  x2gridall <- seq(x2rangeall[1], x2rangeall[2], length=length(categsx1)*100)
  x1x2grid <- expand.grid(x2=x2gridall, x1=categsx1)[,2:1]
  x1x2grid$loghazrat <- NA
  
  for(i in 1:length(categsx1)) {
    x1x2gridtemp <- x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]
    names(x1x2gridtemp)[2] <- "x2"
    loessdat <- data.frame(y=y[x1==categsx1[i]], status=status[x1==categsx1[i]], x2=x2[x1==categsx1[i]])
    # if(length(unique(loessdat$x)) > 1) {
    invisible(capture.output(loessfit <- try(suppressWarnings(rms::cph(survival::Surv(y, status) ~ rcs(x2, 4), data=loessdat)), silent=TRUE)))
    if(class(loessfit)[1]!="try-error") {
      medianloghaz <- median(rms::predictrms(loessfit, newdata = loessdat) + loessfit$center)
      preds <- suppressWarnings(rms::predictrms(loessfit, newdata = x1x2gridtemp) + loessfit$center - medianloghaz)
    } else {
      coxmodel <- survival::coxph(survival::Surv(y, status) ~ x2, data=loessdat)
      medianloghaz <- median(predict(coxmodel, newdata=loessdat))
      preds <- predict(coxmodel, newdata=x1x2gridtemp) - medianloghaz
    }
    x1x2grid[x1x2grid$x2 >= x2ranges[1,i] & x1x2grid$x2 <= x2ranges[2,i] & x1x2grid$x1==categsx1[i],]$loghazrat <- preds
  }
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)[status==1,]
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$loghazrat), width=0.8) +
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), position = ggplot2::position_jitter(width=0.2, height=0, seed = 1234)) + 
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2), shape=21, position = ggplot2::position_jitter(width=0.2, height=0, seed = 1234)) +
    ggplot2::scale_fill_distiller(name="Log hazards ratio\ncompared to the\nmedian effect", palette = "RdYlGn", direction=-1, na.value=NA) +
    ggplot2::scale_color_continuous(name=yvarname, type="viridis") + 
    scale_x_continuous(breaks=1:length(categsx1), labels=levelsorder1) + labs(x=x1name, y=x2name)
  
  datatemp <- x1x2grid[complete.cases(x1x2grid),]
  
  p2 <- ggplot(data=datatemp, aes(x=.data$x2, y=.data$loghazrat, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + 
    geom_line() +
    scale_color_discrete(name=x1name) + labs(x=x2name, y="Log hazards ratio compared to the median effect")
  
  if (pvalue) {
    coefs <- summary(survival::coxph(survival::Surv(y, status) ~ x1cat*x2))$coef
    pvals <- coefs[(length(levels(x1cat))+1):nrow(coefs), "Pr(>|z|)"]
    pinter <- min(p.adjust(pvals, method = "holm"))
	cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
    issmall <- pinter < 0.0001
    pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    
    p2 <- p2 + annotate("text", x=min(datatemp$x2, na.rm=TRUE) + 0.7*diff(range(datatemp$x2, na.rm=TRUE)), y=min(datatemp$loghazrat, na.rm=TRUE) + 0.1*diff(range(datatemp$loghazrat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nCox regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}









plotCatCat <- function(x1, x2, x1name, x2name, y, status, yvarname, statusvarname, levelsorder1, levelsorder2, pvalue, returnseparate)
{
  
  if(inherits(y, "factor")) {
    return(plotBinCatCat(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, levelsorder1=levelsorder1, levelsorder2=levelsorder2, pvalue=pvalue, returnseparate=returnseparate))
  }
  
  if(class(y) %in% c("numeric", "integer")) {
    if(is.null(statusvarname))
      return(plotMetricCatCat(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, levelsorder1=levelsorder1, levelsorder2=levelsorder2, pvalue=pvalue, returnseparate=returnseparate))
    else
      return(plotSurvCatCat(x1=x1, x2=x2, x1name=x1name, x2name=x2name, y=y, yvarname=yvarname, status=status, statusvarname=statusvarname, levelsorder1=levelsorder1, 
                            levelsorder2=levelsorder2, pvalue=pvalue, returnseparate=returnseparate))
  }
  
}


plotBinCatCat <- function(x1, x2, x1name, x2name, y, yvarname, levelsorder1, levelsorder2, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  ynum1 <- as.numeric(y)-1
  ynum <- ynum1
  ynum[ynum1==0] <- 1
  ynum[ynum1==1] <- 0
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  x2cat <- factor(as.character(x2), levels=levelsorder2)
  
  x1 <- as.numeric(x1cat)
  x2 <- as.numeric(x2cat)
  
  
  # Common grid of x1 and x2:
  x1x2grid <-  expand.grid(x2 = sort(unique(x2)), x1 = sort(unique(x1)))[,2:1]
  
  x1x2grid$yprob <- apply(x1x2grid, 1, function(x) mean(ynum[x1==x[1] & x2==x[2]]==1))
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$yprob, width=0.8, height=0.8)) +
    geom_jitter(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), width=0.25, height=0.25) + 
    ggplot2::scale_fill_viridis_c(name=paste("Est. prob. for\n'", levels(y)[1], "'", sep=""), na.value=NA) + 
    ggplot2::scale_color_manual(name=yvarname, values=c("black", "red")) + 
    ggplot2::scale_x_continuous(breaks=1:length(unique(x1)), labels=levelsorder1) + 
    ggplot2::scale_y_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + labs(x=x1name, y=x2name)
  
  p2 <- ggplot(data=x1x2grid, aes(x=.data$x2, y=.data$yprob, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + geom_point() + geom_line() + labs(x=x2name, y=paste("Est. prob. for '", levels(y)[1], "'", sep="")) +
    scale_x_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + scale_color_discrete(name=x1name) + ggplot2::ylim(c(0,1))
  
  if (pvalue) {
    coefs <- summary(glm(y ~ x1cat*x2cat, family=binomial))$coef
    if(nrow(coefs)==(length(levels(x1cat)) + length(levels(x2cat)) - 1))
      pchar <- "p = NA"
    else {
      pvals <- coefs[(length(levels(x1cat))+length(levels(x2cat))):nrow(coefs), "Pr(>|z|)"]
      pinter <- min(p.adjust(pvals, method = "holm"))
	  cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
      issmall <- pinter < 0.0001
      pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    }
    
    p2 <- p2 + annotate("text", x=min(x1x2grid$x2, na.rm=TRUE) + 0.7*diff(range(x1x2grid$x2, na.rm=TRUE)), y=0.1, label= paste("Test for interaction\neffect using\nlogistic regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}


plotMetricCatCat <- function(x1, x2, x1name, x2name, y, yvarname, levelsorder1, levelsorder2, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  x2cat <- factor(as.character(x2), levels=levelsorder2)
  
  x1 <- as.numeric(x1cat)
  x2 <- as.numeric(x2cat)
  
  
  # Common grid of x1 and x2:
  x1x2grid <-  expand.grid(x2 = sort(unique(x2)), x1 = sort(unique(x1)))[,2:1]
  
  x1x2grid$yhat <- apply(x1x2grid, 1, function(x) mean(y[x1==x[1] & x2==x[2]]))
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$yhat, width=0.8, height=0.8)) +
    geom_jitter(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), width=0.25, height=0.25) + 
    ggplot2::scale_fill_viridis_c(name=paste("Est. mean of\n", yvarname, sep=""), na.value=NA) + 
    ggplot2::scale_color_gradient(name=yvarname, low="white", high="black") + 
    ggplot2::scale_x_continuous(breaks=1:length(unique(x1)), labels=levelsorder1) + 
    ggplot2::scale_y_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + labs(x=x1name, y=x2name)
  
  p2 <- ggplot(data=x1x2grid, aes(x=.data$x2, y=.data$yhat, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + geom_point() + geom_line() + labs(x=x2name, y=paste("Est. mean of ", yvarname, sep="")) +
    scale_x_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + scale_color_discrete(name=x1name)
  
  if (pvalue) {
    coefs <- summary(lm(y ~ x1cat*x2cat))$coef
    if(nrow(coefs)==(length(levels(x1cat)) + length(levels(x2cat)) - 1))
      pchar <- "p = NA"
    else {
      pvals <- coefs[(length(levels(x1cat))+length(levels(x2cat))):nrow(coefs), "Pr(>|t|)"]
      pinter <- min(p.adjust(pvals, method = "holm"))
	  cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
      issmall <- pinter < 0.0001
      pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    }
    
    p2 <- p2 + annotate("text", x=min(x1x2grid$x2, na.rm=TRUE) + 0.7*diff(range(x1x2grid$x2, na.rm=TRUE)), y=min(x1x2grid$yhat, na.rm=TRUE) + 0.1*diff(range(x1x2grid$yhat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nlinear regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}



plotSurvCatCat <- function(x1, x2, x1name, x2name, y, yvarname, status, statusvarname, levelsorder1, 
                           levelsorder2, pvalue, returnseparate)
{
  
  # Fit loess to data to obtained estimated probabilities:
  
  x1cat <- factor(as.character(x1), levels=levelsorder1)
  x2cat <- factor(as.character(x2), levels=levelsorder2)
  
  x1 <- as.numeric(x1cat)
  x2 <- as.numeric(x2cat)
  
  
  # Common grid of x1 and x2:
  x1x2gridcat <-  expand.grid(x2cat = levelsorder2, x1cat = levelsorder1, stringsAsFactors = FALSE)[,2:1]
  x1x2gridcat$x1cat <- factor(x1x2gridcat$x1cat, levels=levelsorder1)
  x1x2gridcat$x2cat <- factor(x1x2gridcat$x2cat, levels=levelsorder2)
  
  coxmodel <- survival::coxph(survival::Surv(y, status) ~ x1cat*x2cat)
  medianloghaz <- median(predict(coxmodel, newdata=data.frame(x1cat, x2cat)))
  
  x1x2grid <-  expand.grid(x2 = sort(unique(x2)), x1 = sort(unique(x1)))[,2:1]
  x1x2grid$loghazrat <- predict(coxmodel, newdata=x1x2gridcat) - medianloghaz
  
  dataplot <- data.frame(x1=x1, x2=x2, y=y)[status==1,]
  if(nrow(dataplot) > 300)
    dataplot <- dataplot[sample(1:nrow(dataplot), size=300),]
  p1 <- ggplot() + theme_bw() + geom_tile(data=x1x2grid, aes(x=.data$x1, y=.data$x2, fill=.data$loghazrat, width=0.8, height=0.8)) +
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2, color=.data$y), position = ggplot2::position_jitter(width=0.25, height=0.25, seed = 1234)) + 
    geom_point(data=dataplot, aes(x=.data$x1, y=.data$x2), shape=21, position = ggplot2::position_jitter(width=0.25, height=0.25, seed = 1234)) +
    ggplot2::scale_fill_distiller(name="Log hazards ratio\ncompared to the\nmedian effect", palette = "RdYlGn", direction=-1, na.value=NA) +
    ggplot2::scale_color_continuous(name=yvarname, type="viridis") + 
    ggplot2::scale_x_continuous(breaks=1:length(unique(x1)), labels=levelsorder1) + 
    ggplot2::scale_y_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + labs(x=x1name, y=x2name)

  p2 <- ggplot(data=x1x2grid, aes(x=.data$x2, y=.data$loghazrat, color=factor(levelsorder1[x1], levels=levelsorder1))) + theme_bw() + geom_point() + geom_line() + labs(x=x2name, y="Log hazards ratio compared to the median effect") +
    scale_x_continuous(breaks=1:length(unique(x2)), labels=levelsorder2) + scale_color_discrete(name=x1name)
  
  if (pvalue) {
    coefs <- summary(coxmodel)$coef
    if(nrow(coefs)==(length(levels(x1cat)) + length(levels(x2cat)) - 1))
      pchar <- "p = NA"
    else {
      pvals <- coefs[(length(levels(x1cat))+length(levels(x2cat))-1):nrow(coefs), "Pr(>|z|)"]
      pinter <- min(p.adjust(pvals, method = "holm"))
	  cat(paste("(Unadjusted) p-value: p =", pinter), "\n")
      issmall <- pinter < 0.0001
      pchar <- ifelse(issmall, "p < 0.0001", paste("p = ", format(round(pinter, 4), nsmall=4, scientific=FALSE), sep=""))
    }
    
    p2 <- p2 + annotate("text", x=min(x1x2grid$x2, na.rm=TRUE) + 0.7*diff(range(x1x2grid$x2, na.rm=TRUE)), y=min(x1x2grid$loghazrat, na.rm=TRUE) + 0.1*diff(range(x1x2grid$loghazrat, na.rm=TRUE)), label= paste("Test for interaction\neffect using\nCox regression:\n", pchar, sep=""))
  }
  
  if(!returnseparate)
    return(ggarrange(p1, p2, nrow = 1, ncol = 2))
  else
    invisible(list(p1, p2))
  
}
