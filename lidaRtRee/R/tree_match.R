# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' 3D matching of detected tree top positions with reference positions
#'
#' First computes a matching index for each potential pair associating a detected
#' with a reference tree. This index is the 3D distance between detected and
#' reference points, divided by a maximum matching distance set by user-defined
#' parameters. Pairs with the lowest index are then iteratively associated.
#'
#' @param lr data.frame or matrix. 3D coordinates (X Y Height) of reference positions
#' @param ld data.frame or matrix. 3D coordinates (X Y Height) of detected positions
#' @param delta_ground numeric. buffer around trunk position : absolute value
#' @param h_prec numeric. buffer around apex position : proportion of reference
#' tree height
#' @param stat boolean. should matching stats be computed
#' @return A data.frame with matched pairs (row of reference positions in first
#' column, and row of detected positions in second column) and corresponding 3D
#' distances
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain
#' forests mapping: Support vector regression for stand parameters estimation
#' and unsupervised training for treetop detection. Ph.D. thesis. University of
#' Grenoble, France. pp. 53-55 \url{https://theses.hal.science/tel-00652698/document}
#'
#' Monnet, J.-M., Mermin, E., Chanussot, J., Berger, F. 2010. Tree top detection
#' using local maxima filtering: a parameter sensitivity analysis. Silvilaser 2010,
#' the 10th International Conference on LiDAR Applications for Assessing Forest
#' Ecosystems, September 14-17, Freiburg, Germany, 9 p. \url{https://hal.science/hal-00523245/document}
#' @seealso \code{\link{plot_matched}}, \code{\link{hist_detection}}
#' @examples
#' # create reference and detected trees
#' ref_trees <- cbind(c(1, 4, 3, 4, 2), c(1, 1, 2, 3, 4), c(15, 18, 20, 10, 11))
#' def_trees <- cbind(c(2, 2, 4, 4), c(1, 3, 4, 1), c(16, 19, 9, 15))
#' #
#' # match trees
#' match1 <- tree_matching(ref_trees, def_trees)
#' match2 <- tree_matching(ref_trees, def_trees, delta_ground = 2, h_prec = 0)
#' match1
#' match2
#'
#' # 2D display of matching result
#' plot_matched(ref_trees, def_trees, match1, xlab = "X", ylab = "Y")
#' plot_matched(ref_trees, def_trees, match2, xlab = "X", ylab = "Y")
#' @export
tree_matching <- function(lr, ld, delta_ground = 2.1, h_prec = 0.14, stat = TRUE) {
  # convert to data.frame
  lr <- as.data.frame(lr)
  ld <- as.data.frame(ld)
  # coefficients for max matching squared radius ( rmax = delta_ground + h_prec * HEIGHT -> rmax^2 = delta_ground^2 + 2* h_prec * delta_ground * HEIGHT + h_prec^2 * HEIGHT^2)
  d2max0 <- delta_ground^2
  d2max1 <- 2 * h_prec * delta_ground
  d2max2 <- h_prec^2
  # number of positions
  nr <- nrow(lr)
  nd <- nrow(ld)
  #
  # max distance for matching (depends on reference tree height)
  norm.f <- d2max0 + d2max1 * lr[, 3] + d2max2 * lr[, 3] * lr[, 3]
  #
  # create matrix of of matching indices (row : detected position, column : reference position)
  d <- dn <- matrix(NA, nd, nr, dimnames = list(paste("d", 1:nd, sep = ""), paste("r", 1:nr, sep = "")))
  # for each reference tree
  for (i in 1:nr)
  {
    # compute matching index to detected trees
    # compute distances in each axis
    dummy <- cbind(ld[, 1] - lr[i, 1], ld[, 2] - lr[i, 2], ld[, 3] - lr[i, 3])
    # compute 3D distance
    d[, i] <- dn[, i] <- apply(dummy^2, 1, sum)
    # divided by max matching squared radius to obtain matching index
    dn[, i] <- dn[, i] / norm.f[i]
  }
  #
  # tree matching
  # replace values over the matching limit (pairs where the 3D distance is over the max matching radius of the reference tree) by the limit
  dn[dn >= 1] <- 1
  #
  # iterative matching of pairs with the lowest matching index
  matched <- data.frame()
  # add one row of "1" and one column to prevent dn from becoming a vector
  dn <- rbind(dn, rep(1, ncol(dn)))
  dn <- cbind(dn, rep(1, nrow(dn)))
  while (min(dn) < 1) # while there are values below the matching limit
  {
    # find first pair with minimum index
    coord <- t(which(dn == min(dn), arr.ind = TRUE))[1:2]
    # store pair
    matched <- rbind(matched, as.numeric(gsub("d", "", gsub("r", "", c(colnames(dn)[coord[2]], rownames(dn)[coord[1]])))))
    # remove pair from matrix
    dn <- dn[-coord[1], -coord[2]]
  }
  if (length(matched) == 0) return(NULL)
  names(matched) <- c("r", "d")
  #
  # computes stats if required
  if (stat) {
    matched$h_diff <- ld[matched[, 2], 3] - lr[matched[, 1], 3]
    matched$plan_diff <- sqrt((ld[matched[, 2], 1] - lr[matched[, 1], 1])^2 + (ld[matched[, 2], 2] - lr[matched[, 1], 2])^2)
  }
  return(matched)
}

#-------------------------------------------------------------------------------
#' Plot of matched pairs of detected and reference trees
#'
#' @param lr data.frame or matrix. 3D coordinates (X Y Height) of reference 
#' positions
#' @param ld data.frame or matrix. 3D coordinates (X Y Height) of detected 
#' positions
#' @param matched data.frame. contains pair indices, typically returned by \code{\link{tree_matching}}
#' @param chm raster object. raster for background display
#' @param plot_border sf or SpatVector object. plot boundaries for display
#' @param ... Additional arguments to be used by \code{\link{plot}}
#' @return no return
#' @seealso \code{\link{tree_matching}}, \code{\link{hist_detection}}
#' @examples
#' # create reference and detected trees
#' ref_trees <- cbind(c(1, 4, 3, 4, 2), c(1, 1.5, 2, 3, 4), c(15, 18, 20, 10, 11))
#' def_trees <- cbind(c(2, 2, 4, 4), c(1, 3, 4, 1), c(16, 19, 9, 15))
#' #
#' # compute matching
#' match1 <- tree_matching(ref_trees, def_trees)
#' match2 <- tree_matching(ref_trees, def_trees, delta_ground = 2, h_prec = 0)
#'
#' # 2D display of matching results
#' plot_matched(ref_trees, def_trees, match1, xlab = "X", ylab = "Y")
#' plot_matched(ref_trees, def_trees, match2, xlab = "X", ylab = "Y")
#' @export
plot_matched <- function(lr, ld, matched, chm = NULL, plot_border = NULL, ...) {
  # colors
  # convert to data.frame with same names to avoid further issues
  lr <- as.data.frame(lr)
  ld <- as.data.frame(ld)
  names(lr)[1:3] <- names(ld)[1:3] <- c("x", "y", "z")
  couleur_lr <- rep("red", nrow(lr))
  couleur_ld <- rep("red", nrow(ld))
  couleur_lr[matched[, "r"]] <- "blue"
  couleur_ld[matched[, "d"]] <- "blue"
  #
  # display raster background or not
  if (!is.null(chm)) {
    if(!inherits(chm, "SpatRaster")) chm <- convert_raster(chm)
    terra::plot(chm, ...)
    # add points of detected and reference trees
    graphics::points(rbind(lr[, 1:2], ld[, 1:2]), 
                     pch = c(rep(1, nrow(lr)), rep(2, nrow(ld))), 
                     cex = c(lr[, 3], ld[, 3]) / 20, 
                     col = c(couleur_lr, couleur_ld))
  } else {
    # add points of detected and reference trees
    graphics::plot(rbind(lr[, 1:2], ld[, 1:2]), 
                   pch = c(rep(1, nrow(lr)), rep(2, nrow(ld))), 
                   asp = 1, 
                   cex = c(lr[, 3], ld[, 3]) / 20, 
                   col = c(couleur_lr, couleur_ld), ...)
  }
  #
  # draw lines between pairs
  for (i in 1:nrow(matched))
  {
    graphics::lines(rbind(lr[matched[i, 1], 1:2], ld[matched[i, 2], 1:2]), col = "blue")
  }
  # add plot boundary
  if (!is.null(plot_border)) {
    if(inherits(plot_border, "SpatVector")) plot_border <- sf::st_as_sf(plot_border)
    plot(sf::st_geometry(plot_border), add = TRUE)
  }
  #
  # legend
  graphics::legend("topleft", c("reference", "detected", "matched", "not matched"), 
                   col = c("black", "black", "blue", "red"), 
                   pch = c(1, 2, 15, 15))
}

#-------------------------------------------------------------------------------
#' Histogram of detection
#'
#' Displays the histogram of tree heights of three categories: true detections, omissions, and false detections.
#'
#' @param lr data.frame or matrix. 3D coordinates (X Y Height) of reference positions
#' @param ld data.frame or matrix. 3D coordinates (X Y Height) of detected positions
#' @param matched data.frame. contains pair indices, typically returned by \code{\link{tree_matching}}
#' @param plot boolean. should the histogram be displayed or not
#' @return A list with three numerics: numbers of true detections, omissions and false detections
#' @seealso \code{\link{tree_matching}}
#' @examples
#' # create reference and detected trees
#' ref_trees <- cbind(c(1, 4, 3, 4, 2), c(1, 1, 2, 3, 4), c(15, 18, 20, 10, 11))
#' def_trees <- cbind(c(2, 2, 4, 4), c(1, 3, 4, 1), c(16, 19, 9, 15))
#' #
#' # tree matching with different buffer size
#' match1 <- tree_matching(ref_trees, def_trees)
#' match2 <- tree_matching(ref_trees, def_trees, delta_ground = 2, h_prec = 0)
#' #
#' # corresponding number of detections
#' hist_detection(ref_trees, def_trees, match1)
#' hist_detection(ref_trees, def_trees, match2)
#' @export
hist_detection <- function(lr, ld, matched, plot = TRUE) {
  # convert to data.frame
  lr <- as.data.frame(lr)
  ld <- as.data.frame(ld)
  # create output list
  dummy <- list()
  # true detections
  dummy[[1]] <- lr[matched[, 1], 3]
  # true omissions
  dummy[[2]] <- lr[-matched[, 1], 3]
  # false detections
  dummy[[3]] <- ld[-matched[, 2], 3]
  if (plot) {
    # draw histogram
    hist_stack(dummy, breaks = seq(
      from = 0, 
      to = ceiling(max(lr[, 3], ld[, 3]) / 5) * 5, 
      by = 5), 
      col = c("green", "red", "blue"),
      yaxt = "n")
    graphics::axis(2, mgp = c(0.5, 0.5, 0))
    graphics::mtext(side = 2, text = "Number of trees", line = 1.3)
    graphics::mtext(side = 1, text = "Height classes (m)", line = 1.3)
    graphics::legend("topright", 
                     c("True positive", "False negative", "False positive"), 
                     fill = c("green", "red", "blue"))
  }
  #
  list(true_detections = length(dummy[[1]]), 
       false_detections = length(dummy[[3]]), 
       omissions = length(dummy[[2]]))
}

#-------------------------------------------------------------------------------
#' Stacked histogram
#'
#' @param x list of vectors. values for each category
#' @param breaks vector. breaks for histogram bins
#' @param col vector. colors for each category
#' @param breaksFun function for breaks display
#' @param ... arguments to be passed to methods, as in \code{\link[graphics]{plot}}
#' @return no return
#' @export
#'
hist_stack <- function(x, breaks, col = NULL, breaksFun = paste, ...) {
  if (!is.list(x)) {
    stop("'x' must be a list.")
  }
  if (is.null(col)) {
    col <- 1:length(x)
  }
  bars <- NULL
  for (i in 1:length(x)) {
    if (!is.numeric(x[[i]])) {
      paste("Element", i, "of 'x' is not numeric.")
    }
    h <- graphics::hist(x[[i]], breaks = breaks, plot = FALSE)
    bars <- rbind(bars, h$counts)
  }
  graphics::barplot(bars, names.arg = NULL, col = col, space = 0, ...)
  at <- seq(along = h$breaks) - 1
  modulo <- ceiling(length(at) / 10)
  sel <- (at %% modulo == 0)
  graphics::axis(side = 1, at = at[sel], labels = breaksFun(h$breaks)[sel], mgp = c(0.5, 0.5, 0))
}

#-------------------------------------------------------------------------------
#' Regression of detected heights VS reference heights
#'
#' Computes a linear regression model between the reference heights and the 
#' detected heights of matched pairs.
#'
#' @param lr data.frame or matrix. 3D coordinates (X Y Height) of reference 
#' positions
#' @param ld data.frame or matrix. 3D coordinates (X Y Height) of detected 
#' positions
#' @param matched data.frame. contains pair indices, typically returned by \code{\link{tree_matching}}
#' @param plot boolean. indicates whether results should be plotted
#' @param species vector of strings. species for standardized color use by call 
#' to \code{\link{species_color}}
#' @param ... arguments to be passed to methods, as in \code{\link[graphics]{plot}}
#' @return A list with two elements. First one is the linear regression model, 
#' second one is a list with stats (root mean square error, bias and standard 
#' deviation of detected heights compared to reference heights).
#' @seealso \code{\link{tree_matching}}
#' @examples
#' # create tree locations and heights
#' ref_trees <- cbind(c(1, 4, 3, 4, 2), c(1, 1, 2, 3, 4), c(15, 18, 20, 10, 11))
#' def_trees <- cbind(c(2, 2, 4, 4), c(1, 3, 4, 1), c(16, 19, 9, 15))
#'
#' # tree matching
#' match1 <- tree_matching(ref_trees, def_trees)
#'
#' # height regression
#' reg <- height_regression(ref_trees, def_trees, match1,
#'   species = c("ABAL", "ABAL", "FASY", "FASY", "ABAL"),
#'   asp = 1, xlim = c(0, 21), ylim = c(0, 21)
#' )
#' summary(reg$lm)
#' reg$stats
#' @export
#'
height_regression <- function(lr, ld, matched, plot = TRUE, species = NULL, ...) {
  # convert to data.frame
  lr <- as.data.frame(lr)
  ld <- as.data.frame(ld)
  # build data.frame with pairs
  app <- data.frame(Hm = lr[matched[, 1], 3], Hl = ld[matched[, 2], 3])
  # fit regression
  reg <- stats::lm(Hm ~ Hl, data = app)
  #
  if (plot) {
    # retrieve dots
    dots <- list(...)
    if (!methods::hasArg("add")) dots$add <- FALSE
    # apply species-specific colors
    if (!is.null(species)) {
      # load palette
      color <- species_color()
      # extract corresponding colors
      col1 <- color[as.character(species[matched[, 1]]), c("col", "abvr")]
      # add /overwrite in dots arguments
      dots$col <- col1$col
    } else {
      # if user-specified colors are not present
      if (!methods::hasArg("col")) dots$col <- "black"
    }
    # if user-specified asp is not present
    if (!methods::hasArg("asp")) dots$asp <- 1
    # if user-specified xlab is not present
    if (!methods::hasArg("xlab")) dots$xlab <- "Detected height (m)"
    # if user-specified ylab is not present
    if (!methods::hasArg("ylab")) dots$ylab <- "Reference height (m)"
    #
    args <- list(x = app$Hl, y = app$Hm)
    # call plot with those arguments and those in dots except "add"
    dots$add <- NULL
    do.call(graphics::plot, c(args, dots))
    # add 1:1 line
    graphics::abline(c(0, 1), lty = 1)
    # add regression line
    graphics::abline(c(reg$coefficients[1], reg$coefficients[2]), lty = 2)
    #
    # add legend
    if (!is.null(species)) {
      texte <- sort(unique(col1$abvr))
      graphics::legend("topleft", texte, col = color[texte, "col"], pch = 15, cex = 1, y.intersp = 1)
    }
  }
  list(lm = reg, stats = list(rmse = sqrt(sum((app$Hm - app$Hl)^2) / nrow(app)), 
                              bias = mean(app$Hl - app$Hm), 
                              sd = stats::sd(app$Hl - app$Hm)))
}
