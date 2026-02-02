#' Plot alignment cost density and warping path
#'
#' Plot alignment cost density and warping path, optionally with the two snow profiles plotted in the margins
#' along the axes.
#'
#' @param alignment object from [dtwSP]
#' @param localCost plot *local* cost matrix, otherwise plot accumulated global cost.
#' @param labelHeight plot axes in units of height (cm) or in unitless (i.e., layer index).
#' @param marginalPros plot profiles in margins along the axes. default TRUE
#' @param pathCol color of warping path
#' @param target draw horizontal & vertical lines from matrix cells to corresponding layers in the (marginal) profiles.
#' Provide either a vector of length 1 (i.e., index of warping path) or length 2 (i.e., x, y coordinates in terms of
#' layer indices), or a matrix with 2 columns, specifying (x, y) if you desire multiple 'targets'
#' @param movingTarget Do you want to draw the warping path only partially, from the origin to the target cross?
#' Only possible if target cross is given as a scalar! default = FALSE (Useful to create GIF animations of a moving path)
#' @param tlty target lty
#' @param tlwd target lwd
#' @param tcol target col
#' @param tcex target cex
#' @param cex.lab cex of axis labels (cf. [par])
#' @param xlab x-axis label to change default labeling
#' @param ylab y-axis label to change default labeling
#' @param ... forwarded to [par]
#'
#' @note If you can't see the axis labels, try e.g., `par(oma = c(3, 3, 0, 0))` before calling the function. Note, there
#' seems to be a problem (only sometimes) with the left-hand labels that are for some reason not plotted parallel
#' to the axis. Also, the routine is not bulletproof with respect to drawing 'targets'. Apologies for any inconveniences!
#'
#' @author fherla
#'
#' @examples
#'
#' ## first align profiles:
#' dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)
#'
#' ## then plot cost density:
#' plotCostDensitySP(dtwAlignment)
#'
#' ## label height instead of layer index, and don't show warping path:
#' plotCostDensitySP(dtwAlignment, labelHeight = TRUE, pathCol = "transparent")
#'
#' ## draw lines to the cell that corresponds to the DH and SH layers
#' plotCostDensitySP(dtwAlignment, target = c(191, 208))
#'
#' ## "moving target", i.e., draw warping path only from origin to target:
#' plotCostDensitySP(dtwAlignment, target = 200, movingTarget = TRUE)
#' plotCostDensitySP(dtwAlignment, target = 266, movingTarget = TRUE)
#'
#'
#' ## A cool GIF can be created from frames like those
#' create_GIF <- FALSE
#' if (create_GIF){
#'   nPath <- length(dtwAlignment$index1)
#'   resolution <- 100  # i.e. super low, make value smaller for smoother GIF
#'   for (k in seq(1, nPath, by = resolution)) {
#'     plotCostDensitySP(dtwAlignment, target = k, movingTarget = TRUE)
#'   }
#' }
#'
#' @export
plotCostDensitySP <- function(alignment, localCost = TRUE, labelHeight = FALSE, marginalPros = TRUE, pathCol = "black",
                              target = FALSE, movingTarget = FALSE, tlty = "dotted", tlwd = 1.5, tcol = "black", tcex = 1.5,
                              cex.lab = 1, xlab = NULL, ylab = NULL, ...) {

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (!identical(target, FALSE)) {
    online <- FALSE
    if (length(target) == 1) online <- TRUE
    else if (length(target) == 2) target <- matrix(target, ncol = 2)
    else {
      if (!is.matrix(target)) stop("target needs to be a vector of length 1 or 2, or a matrix with 2 columns!")
    }
  }

  ## Color Palette:
  if (localCost) {
    greens <- colorRampPalette(c("#B8E186", "#276419"))
    pinks <- colorRampPalette(c("#F1B6DA", "#8E0152"))
    colpal <- c(rev(greens(10)), pinks(10))
    # rev(c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA",
    #       "#B8E186", "#7FBC41", "#4D9221", "#276419"))

    colpal <- rev(c('#c00000', '#c91005', '#d11c0b', '#d92712', '#e13018', '#e83a1f', '#ef4327', '#f54d2e', '#fa5737', '#ff6140', '#ff863f', '#ff863f', '#fda63f', '#fda63f', '#f8c43f', '#f8c43f', '#efe141', '#efe141', '#daff53',  '#daff53'))
  } else {
    # colpal <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0",
    #             "#225EA8", "#253494", "#081D58")
    colfunc <- colorRampPalette(c("#E0F3DB", "#0868AC"))
    colpal <- colfunc(20)

  }
  oldpalette <- palette()
  on.exit(palette(oldpalette))
  palette(colpal)
  ## plot:
  # dtwPlotDensity(align$alignment)
  cm <- alignment$costMatrix
  if (localCost) cm <- alignment$localCostMatrix
  if (labelHeight) {
    xlab <- ifelse(is.null(xlab), "Query height (cm)", xlab)
    ylab <- ifelse(is.null(ylab), "Reference height (cm)", ylab)
    ## subfunction: assure monotonically increasing vector
    hackMonotonicity <- function(x) {
      stat <- diff(x) == 0
      while (any(stat)) {
        il <- which(stat)
        x[il + 1] <- x[il] + 0.001
        stat <- diff(x) == 0
      }
      return(x)
    }
    x <- alignment$query$layers$height
    y <- alignment$reference$layers$height
    if (any(diff(x) == 0)) x <- hackMonotonicity(x)
    if (any(diff(y) == 0)) y <- hackMonotonicity(y)
    ## convert target into Height coordinates
    if (!identical(target, FALSE)) {
      if (!online) target <- matrix(c(x[target[, 1]], y[target[, 2]]), ncol = 2, byrow = F)
    }
  } else {
    xlab <- ifelse(is.null(xlab), "Query layer index", xlab)
    ylab <- ifelse(is.null(ylab), "Reference layer index", ylab)
    x <- seq(dim(cm)[1])
    y <- seq(dim(cm)[2])
  }

  ## base R density plot:
  if (!marginalPros) {
    image(cm, col = 1:length(palette()), x = x, y = y,
          xlab = xlab, ylab = ylab)
    par(...)
    if (!localCost) contour(cm, x = x, y = y, add = TRUE)
    lines(x[alignment$index1], y[alignment$index2], col = pathCol, lwd = 2.5)
  } else {

    ## gg density plot:
    # cmMelted <- reshape2::melt(cm)
    #
    # dPlot <- ggplot2::ggplot(cmMelted, aes(x = Var2, y = Var1, z = value)) +
    #   stat_contour(geom = "polygon", aes(fill = ..level..)) +
    #   geom_tile(aes(fill = value)) +
    #   stat_contour(bins = 15) +
    #   xlab("Query") +
    #   ylab("Reference") +
    #   guides(fill = guide_colorbar(title = "Cost"))
    #
    # dPlotAnnotated <- directlabels::direct.label(dPlot, "bottom.pieces")
    #
    # browser()


    ## base R three-panel plot:
    ## layout definition - SPs at top and right
    zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
    ## alternatively SPs at bottom and right (need to change plotting order also, first yPro then xPro)
    # zones=matrix(c(1,2,3,0), ncol=2, byrow=TRUE)
    # layout(zones, widths=c(4/5,1/5), heights=c(4/5,1/5))

    ## main plot panel
    par(mar=c(4,4,1,1), ...)
    # par(mar=c(1,3,3,1))  # if xPro at bottom
    image(cm, col = 1:length(palette()), x = x, y = y, xlab = "", ylab = "")
    # contour(cm, x = x, y = y, add = TRUE)
    ## draw warping path
    if (movingTarget) {
      lines(x[alignment$index1[1:target]], y[alignment$index2[1:target]], col = pathCol, lwd = 1.5)
    } else {
      lines(x[alignment$index1], y[alignment$index2], col = pathCol, lwd = 2)  # 1.5
    }

    ## draw target cross:
    if (!identical(target, FALSE)) {
      if (online) target <- matrix(c(x[alignment$index1[target]], y[alignment$index2[target]]), ncol = 2, byrow = F)
      points(target[, 1], target[, 2], pch = 19, cex = tcex)
      for (i in nrow(target)) {
        lines(c(target[i, 1], max(x)), rep(target[i, 2], times = 2), lty = tlty, lwd = tlwd, col = tcol)
        lines(rep(target[i, 1], times = 2), c(target[i, 2], max(y)), lty = tlty, lwd = tlwd, col = tcol)
      }
      ## find indices to emphasize corresponding layers:
      ql <- which(x - target[, 1] == 0)
      emph <- which(alignment$query$layers$gtype == alignment$query$layers$gtype[ql] &
                      alignment$query$layers$hardness == alignment$query$layers$hardness[ql])
      mask <- diff(emph)
      iql <- which(emph == ql)
      qbins <- sort(c(0, which(mask != 1) + 1, length(emph), iql))
      range <- qbins[c(which(qbins == iql)[1]-1, which(qbins == iql)[1]+1)]
      emphTHOSE_qu <- emph[seq(ifelse(range[1]==0, 1, range[1]), ifelse(range[2]>0, range[2]-1, range[2]))]

      rl <- which(y - target[, 2] == 0)
      emph <- which(alignment$reference$layers$gtype == alignment$reference$layers$gtype[rl] &
                      alignment$reference$layers$hardness == alignment$reference$layers$hardness[rl])
      mask <- diff(emph)
      irl <- which(emph == rl)
      rbins <- sort(c(0, which(mask != 1) + 1, length(emph), irl))
      range <- rbins[c(which(rbins == irl)[1]-1, which(rbins == irl)[1]+1)]
      emphTHOSE_r <- emph[seq(ifelse(range[1]==0, 1, range[1]), ifelse(range[2]>0, range[2]-1, range[2]))]
    }

    ## plot xPro (= query)
    par(mar=c(0,4,1,1))  # margins if at top
    # par(mar=c(1,3,0,1))  # margins if at bottom
    # barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
    barplot(alignment$query$layers$hardness,
            width = c(x[1], diff(x)),
            col = sapply(alignment$query$layers$gtype, getColoursGrainType),
            horiz = F, border = NA, space = 0,
            axes = F, xaxs = 'i', yaxs = 'i'
    )
    axis(2, at = 1:5, labels = c('F', '4F', '1F', 'P', 'K'))
    ## overdraw to emphasize:
    if (!identical(target, FALSE)) {
      hardness_mod <- alignment$query$layers$hardness
      hardness_mod[emphTHOSE_qu] <- 0
      barplot(hardness_mod,
              width = c(x[1], diff(x)),
              col = rep(paste0("#FFFFFF95"), times = length(hardness_mod)),
              horiz = F, border = NA, space = 0,
              axes = F, xaxs = 'i', yaxs = 'i',
              add = TRUE
      )
    }

    ## plot yPro (= ref)
    par(mar=c(4,0,1,1))
    # par(mar=c(1,0,3,1))  # xPro at bottom
    # barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
    barplot(alignment$reference$layers$hardness,
            width = c(y[1], diff(y)),
            col = sapply(alignment$reference$layers$gtype, getColoursGrainType),
            horiz = TRUE, border = NA, space = 0,
            axes = FALSE, xaxs = 'i', yaxs = 'i'
    )
    axis(1, at = 1:5, labels = c('F', '4F', '1F', 'P', 'K'))
    ## overdraw to emphasize:
    if (!identical(target, FALSE)) {
      hardness_mod <- alignment$reference$layers$hardness
      hardness_mod[emphTHOSE_r] <- 0
      barplot(hardness_mod,
              width = c(y[1], diff(y)),
              col = rep(paste0("#FFFFFF95"), times = length(hardness_mod)),
              horiz = TRUE, border = NA, space = 0,
              axes = FALSE, xaxs = 'i', yaxs = 'i',
              add = TRUE
      )
    }

    # par(oma=c(0,0,0,0))
    mtext(xlab, side=1, line=-1.5, outer=TRUE, adj=0,
          at=(.7 * (mean(x) - min(x))/(max(x)-min(x))), cex = cex.lab)
    mtext(ylab, side=2, line=-1.5, outer=TRUE, adj=0,
          at=(.7 * (mean(y) - min(y))/(max(y) - min(y))), cex = cex.lab)

    mtext(expression(Hardness), side=1, line=-1.5, outer=TRUE, adj=0,  # ~bold(r)^h
          at=(1.65 * (mean(x) - min(x))/(max(x)-min(x))), cex = cex.lab)
    mtext(expression(Hardness), side=2, line=-1.5, outer=TRUE, adj=0,  # ~bold(q)^h
          at=(1.65 * (mean(y) - min(y))/(max(y) - min(y))), cex = cex.lab)

  }
}

