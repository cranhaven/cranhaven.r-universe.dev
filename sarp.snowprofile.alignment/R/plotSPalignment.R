#' Align and plot two snow profiles using DTW
#'
#' This is a plotting routine for the DTW alignment of two snow profiles. Either provide two snow profiles or
#' a `dtwSP` alignment object. Don't resize the figure, otherwise the plotted alignment segments will not be
#' in correct place anymore! If you need a specific figure size, use `grDevices::png` with a width/height aspect
#' ratio of about 5/3.
#'
#' @importFrom grid viewport pushViewport popViewport grid.lines gpar
#' @importFrom graphics plot
#' @import sarp.snowprofile
#'
#' @param query The query snowprofile to be warped
#' @param ref The reference snowprofile to be warped against
#' @param dtwAlignment `dtwSP` object (optional)
#' @param keep.alignment Return `dtwSP` object with resampled query, ref and warped query? boolean
#' @param segCol Color of alignment segments. Passed to [gpar], default = "gray70"
#' @param segLty Linestyle of alignment segments. Passed to [gpar], default = "dotted"
#' @param segLwd Linewidth of alignment segments, default = 1
#' @param segTidy Tidy up alignment segments, if profiles have not been resampled? boolean,
#' default FALSE i.e. one segment line per (synthetic) layer interface -> supports visual understanding of alignment, but is also often confusing
#' (segTidy currently only implemented for tidying up to gtype and hardness interfaces)
#' @param segInd Index vector of query layers that will get alignment segments drawn. Note, that the profiles might get resampled, so pre-calculate your correct indices!
#' @param segEmph Index vector of query layers, the alignment segments of which will be emphasized (thick and red). Note, that the profiles might get resampled, so pre-calculate your correct indices!
#' @param cex font size, cf. `par`
#' @param plot.costDensity First graph, [plotCostDensitySP] with warping path? boolean, default = FALSE
#' @param plot.warpedQuery plot warped query additionally to query, ref and alignment segments? (i.e. three pane plot) boolean, default = TRUE
#' @param label.ddate Label deposition date in profiles? (Only possible if `ddate` is given in 'dims', cf [distanceSPlayers])
#' @param mainQu subtitle for query subfigure
#' @param mainRef subtitle for reference subfigure
#' @param mainQwarped subtitle for warped query subfigure
#' @param emphasizeLayers_qu emphasize Layers in query, see [sarp.snowprofile::plot.snowprofile]
#' @param emphasizeLayers_ref emphasize Layers in reference, see [sarp.snowprofile::plot.snowprofile]
#' @param failureLayers_qu draw arrow to failure layers (see [sarp.snowprofile::plot.snowprofile])? provide height vector.
#' @param failureLayers_qu_col color of arrow(s) (individual color string or vector, see [sarp.snowprofile::plot.snowprofile])
#' @param ... Arguments passed to \code{\link{distanceSPlayers}} and \code{\link{dtwSP}}
#'
#' @return dtw object with the resampled '$query' and '$reference', as well as the warped query '$queryWarped'
#' (only if keep.alignment is TRUE)
#'
#' @author fherla
#'
#' @examples
#'
#' plotSPalignment(SPpairs$B_modeled1, SPpairs$B_modeled2)
#'
#' plotSPalignment(SPpairs$B_modeled1, SPpairs$B_modeled2, dims = c("gtype"), weights = c(1))
#'
#' ## alternatively keep alignment:
#' alignment <- plotSPalignment(SPpairs$B_modeled1, SPpairs$B_modeled2, keep.alignment = TRUE)
#' print(paste("Similarity between profiles:", alignment$sim))
#'
#' ## alternatively, with precomputed alignment and emphasized layer matches:
#' dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)
#' plotSPalignment(dtwAlignment = dtwAlignment, segEmph = c(190, 192))
#'
#' ## directly after plotting, add text to figure:
#' grid::grid.text("Profiles SPpairs$A (modeled/manual)", x = 0.5, y = 0.8,
#'                 gp = grid::gpar(fontsize=12, col="grey"))
#'
#'
#' @export
plotSPalignment <- function(query, ref, dtwAlignment = NULL, keep.alignment = FALSE,
                            plot.costDensity = FALSE, plot.warpedQuery = TRUE, label.ddate = FALSE,
                            segCol = "gray70", segLty = "dotted", segLwd = 1, segTidy = FALSE,
                            segInd = TRUE, segEmph = NA,
                            cex = 1,
                            mainQu = "query", mainRef = "reference", mainQwarped = "warped query",
                            emphasizeLayers_qu = FALSE, emphasizeLayers_ref = FALSE,
                            failureLayers_qu = FALSE, failureLayers_qu_col = "red",
                            ...) {

  ## --- subfunction, assertion, initialization ----
  ## local subfunction to draw line segments
  draw_segments <- function(point_list, gp) {
    grid.lines(x = c(point_list["fromX"], point_list["toX"]), y = c(point_list["fromY"], point_list["toY"]),
               gp = gp)
  }

  ## --- calculate dtwAlignment ----
  if (!is.null(dtwAlignment)) {
    ## dtw object is provided
    if (!inherits(dtwAlignment, "dtwSP")) stop("dtwAlignment needs to be a dtw object.")
  } else {

    if (!is.snowprofile(query) | !is.snowprofile(ref)) stop("query and ref need to be two snowprofile objects.")

    ## calculate dtw object with call to dtwSP (resampling is done there!)
    dtwAlignment <- dtwSP(query, ref, keep.internals = TRUE, ...)
  }


  ## --- warp query onto ref and create new snowprofile object ----
  ## that code has been ported to the function 'warpSP'
  ## it is executed in dtwSP if keep.internals == TRUE:
  qmod <- dtwAlignment$queryWarped


  ## --- plot ----
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(cex = cex, cex.axis = cex, cex.main = cex, cex.lab = cex,
      mar = c(5, 5, 4, 1))
  ## cost density plot:
  if (plot.costDensity) {
    par(mfrow = c(1, 1))
    plotCostDensitySP(dtwAlignment)
  }
  ## calculate segment endpoints - native units:
  ## (see 'dtwPlotTwoWay' to learn how to use the index instances in dtwAlignment)
  idx <- seq(length(dtwAlignment$index1))
  xr <- rep(0, length(idx))  # dtwAlignment$reference$layers$hardness[dtwAlignment$index2[idx]]
  yr <- dtwAlignment$reference$layers$height[dtwAlignment$index2[idx]]
  xq <- dtwAlignment$query$layers$hardness[dtwAlignment$index1[idx]]
  yq <- dtwAlignment$query$layers$height[dtwAlignment$index1[idx]]
  if (segTidy) {  # clean up segment lines:
    DF <- data.frame(xr, yr, xq, yq)
    yrTidy <- mergeIdentLayers(dtwAlignment$reference)$layers$height
    yqTidy <- mergeIdentLayers(dtwAlignment$query)$layers$height
    Ir <- which(yr %in% yrTidy)
    Iq <- which(yq %in% yqTidy)
    DF <- DF[DF$yr %in% unique(yr[Ir]) & DF$yq %in% unique(yq[Iq]), ]
    xr <- DF$xr
    yr <- DF$yr
    xq <- DF$xq
    yq <- DF$yq
  }

  ## plot subplots and convert native viewport units into global units:
  ymx <- max(tail(dtwAlignment$reference$layers$height, n = 1),
              tail(dtwAlignment$query$layers$height, n = 1),
              tail(qmod$layers$height, n = 1))

  if (plot.warpedQuery) par(mfrow = c(1, 3))
  else par(mfrow = c(1, 2))
  # 1st subplot
  par(cex = cex)
  plot(dtwAlignment$query, main = mainQu, ymax = ymx, emphasizeLayers = emphasizeLayers_qu,
       failureLayers = failureLayers_qu, failureLayers.cex = 6, failureLayers.col = failureLayers_qu_col,
       ylab = "Snow height (cm)")  # ; vps3 <- do.call(vpStack, baseViewports())
  if (label.ddate) {
    xText <- matrix(0.7, nrow = nrow(dtwAlignment$query$layers), ncol = 1)
    yText <-  dtwAlignment$query$layers$height - 0.5 * diff(c(0, dtwAlignment$query$layers$height))
    labelsText = as.Date(dtwAlignment$query$layers$ddate)
    ispaced <- which(!duplicated(labelsText))
    text(xText[ispaced], yText[ispaced], labelsText[ispaced])
  }
  Xq <- sapply(xq, function(x) grconvertX(x, "user", "ndc"))
  Yq <- sapply(yq, function(x) grconvertY(x, "user", "ndc"))
  # 2nd subplot
  plot(dtwAlignment$reference, main = mainRef, ymax = ymx, emphasizeLayers = emphasizeLayers_ref, xlab = "Hardness")  # ; vps1 <- do.call(vpStack, baseViewports())
  if (label.ddate) {
    xText <- matrix(0.7, nrow = nrow(dtwAlignment$reference$layers), ncol = 1)
    yText <-  dtwAlignment$reference$layers$height - 0.5 * diff(c(0, dtwAlignment$reference$layers$height))
    labelsText = as.Date(dtwAlignment$reference$layers$ddate)
    ispaced <- which(!duplicated(labelsText))
    text(xText[ispaced], yText[ispaced], labelsText[ispaced])
  }
  Xr <- sapply(xr, function(x) grconvertX(x, "user", "ndc"))
  Yr <- sapply(yr, function(x) grconvertY(x, "user", "ndc"))
  # 3rd subplot
  if (plot.warpedQuery) {
    plot(qmod, main = mainQwarped, ymax = ymx,
         highlightUnobservedBasalLayers = FALSE)  # ; vps2 <- do.call(vpStack, baseViewports())
  }

  ## plot line segments:
  bind_points <- suppressWarnings(cbind(fromX=Xr, fromY=Yr, toX=Xq, toY=Yq))
  pushViewport(viewport())
  if (is.numeric(segInd)) bind_points <- bind_points[which(yq %in% dtwAlignment$query$layers$height[segInd]), ]
  apply(bind_points, 1, function(x, gp) draw_segments(x, gp), gp = gpar(col = segCol, lty = segLty, lwd = segLwd))
  if (!all(is.na(segEmph))) {
    bind_points <- bind_points[which(yq %in% dtwAlignment$query$layers$height[segEmph]), ]
    apply(bind_points, 1, function(x, gp) draw_segments(x, gp), gp = gpar(col = "red", lty = segLty, lwd = 2*segLwd))
  }
  # popViewport()  ## not to popViewport means stuff can be added to whole Fig after function finishes


  if (keep.alignment) {
    return(dtwAlignment)
  }
}


## --- old unused code, that could maybe still be handy ----

# # convert to npc units
# pushViewport(vps1)
# Yr <- sapply(yr, function(x) convertY(unit(x,"native"), "npc"))
# Xr <- sapply(xr, function(x) convertX(unit(x,"native"), "npc"))
# pushViewport(vps2)
# Yqw <- sapply(yqw, function(x) convertY(unit(x,"native"), "npc"))
# Xqw <- sapply(xqw, function(x) convertX(unit(x,"native"), "npc"))
# popViewport(3)
# # bind points
# bind_points_nat <- suppressWarnings(cbind(fromX=xr, fromY=yr, fromVPS=NA, toX=xqw, toY=yqw, toVPS=NA))


# draw_segments_old <- function(point_list) {
#   color <- "red"  # "gray70"
#   #lstyle = 3
#   grid.move.to(x = point_list$fromX, y = point_list$fromY)  # , vp = point_list$fromVPS)
#   grid.line.to(x = point_list$toX, y = point_list$toY,
#                gp = gpar(col = color))  # , lty = lstyle  #  vp = point_list$toVPS,
# }


