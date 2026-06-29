#' Plot the densities of multiple ``bsl'' class objects.
#'
#' @description The function \code{combinePlotsBSL} can be used to plot multiple
#'   BSL densities together, optionally with the true values for the parameters.
#' @param objectList     A list of ``bsl'' class objects.
#' @param label          A string vector indicating the labels to be shown in
#'   the plot legend. The default is \code{NULL}, which uses the names from
#'   \code{objectList}.
#' @param legendPosition One of the three string arguments, ``auto'', ``right''
#'   or ``bottom'', indicating the legend position. The default is ``auto'',
#'   which automatically choose from ``right'' and ``bottom''. Only used when
#'   \code{which} is \code{1L}.
#' @param legendNcol     An integer argument indicating the number of columns of
#'   the legend. The default, \code{NULL}, put all legends in the same row or
#'   column depending on \code{legendPosition}. Only used when \code{which} is
#'   \code{1L}.
#' @param col            A vector argument containing the plotting color for
#'   each density curve. Each element of the vector will be passed into
#'   \code{lines}. Only used when \code{which} is \code{1L}.
#' @param lty            A vector argument containing the line type for each
#'   density curve. Each element of the vector will be passed into \code{lines}.
#'   Only used when \code{which} is \code{1L}.
#' @param lwd            A vector argument containing the line width for each
#'   density curve. Each element of the vector will be passed into \code{lines}.
#'   Only used when \code{which} is \code{1L}.
#' @param cex.lab        The magnification to be used for x and y labels
#'   relative to the current setting of cex. To be passed into \code{plot}. Only
#'   used when \code{which} is \code{1L}.
#' @param cex.axis       The magnification to be used for axis annotation
#'   relative to the current setting of cex. To be passed into \code{plot}. Only
#'   used when \code{which} is \code{1L}.
#' @param cex.legend     The magnification to be used for legend annotation
#'   relative to the current setting of cex. Only used when \code{which} is
#'   \code{1L}.
#' @param top            A string argument of the combined plot title. Only used
#'   when \code{which} is \code{2L}.
#' @param options.color  A list of additional arguments to pass into function
#'   \code{ggplot2::scale_color_manual}. Only used when \code{which} is
#'   \code{2L}.
#' @param options.linetype A list of additional arguments to pass into function
#'   \code{ggplot2::scale_linetype_manual}. Only used when \code{which} is
#'   \code{2L}.
#' @param options.size A list of additional arguments to pass into function
#'   \code{ggplot2::scale_size_manual}. Only used when \code{which} is
#'   \code{2L}.
#' @inheritParams BSL-class
#'
#' @return No return value, called for the plots produced.
#'
#' @examples
#' \dontshow{
#' toy_sim <- function(n, theta) matrix(rnorm(2*n, theta), nrow = n)
#' toy_sum <- ma2_sum
#'
#' model <- newModel(fnSimVec = toy_sim, fnSum = toy_sum, sumArgs = list(epsilon = 2), theta0 = 0)
#'
#' result1 <- bsl(y = 1:2, n = 50, M = 10, model = model, covRandWalk = matrix(1),
#'                method = "BSL")
#' result2 <- bsl(y = 1:2, n = 50, M = 10, model = model, covRandWalk = matrix(1),
#'                method = "uBSL")
#' result3 <- bsl(y = 1:2, n = 50, M = 10, model = model, covRandWalk = matrix(1),
#'                method = "semiBSL")
#' combinePlotsBSL(list(result1, result2, result3), label = c("BSL","uBSL","semiBSL"), thin = 2)
#' }
#' \dontrun{
#' toy_sim <- function(n, theta) matrix(rnorm(2*n, theta), nrow = n)
#' toy_sum <- ma2_sum
#'
#' model <- newModel(fnSimVec = toy_sim, fnSum = toy_sum, sumArgs = list(epsilon = 2), theta0 = 0)
#'
#' result1 <- bsl(y = 1:2, n = 100, M = 5e3, model = model, covRandWalk = matrix(1),
#'                method = "BSL", plotOnTheFly = TRUE)
#' result2 <- bsl(y = 1:2, n = 100, M = 5e3, model = model, covRandWalk = matrix(1),
#'                method = "uBSL", plotOnTheFly = TRUE)
#' result3 <- bsl(y = 1:2, n = 100, M = 5e3, model = model, covRandWalk = matrix(1),
#'                method = "semiBSL", plotOnTheFly = TRUE)
#' combinePlotsBSL(list(result1, result2, result3), label = c("BSL","uBSL","semiBSL"), thin = 20)
#' }
#'
#' @seealso \code{\link{ma2}}, \code{\link{cell}}, \code{\link{mgnk}} and
#'   \code{\link{toad}} for examples.
#' @export
combinePlotsBSL <- function(objectList, which = 1L, thin = 1, burnin = 0, thetaTrue = NULL, label = NULL,
                            legendPosition = c('auto','right','bottom')[1],
                            legendNcol = NULL, col = NULL, lty = NULL, lwd = NULL, cex.lab = 1, cex.axis = 1, cex.legend = 0.75,
                            top = 'Approximate Marginal Posteriors', options.color = list(), options.linetype = list(),
                            options.size = list(), options.theme = list()) {
  if (which == 1L) {
    if (length(options.color) != 0 || length(options.linetype) != 0 || length(options.size) != 0 || length(options.theme) != 0) {
      warning('"options.color", "options.linetype", "options.size" and "options.theme" are ignored when which = 1')
    }
    multiPlotDefault(objectList, thin, burnin, thetaTrue, label, legendPosition, legendNcol, col, lty, lwd, cex.lab, cex.axis, cex.legend)
  } else if (which == 2L) {
    multiPlotGgplot(objectList, thin, burnin, thetaTrue, label, top, options.color, options.linetype, options.size, options.theme)
  } else {
    stop('Indicate a supported plot number, 1 for R default density plot or 2 for ggplot density plot')
  }
}

multiPlotDefault <- function(objectList, thin = 1, burnin = 0, thetaTrue = NULL, label = NULL, legendPosition = c('auto','right','bottom')[1],
                             legendNcol = NULL, col = NULL, lty = NULL, lwd = NULL, cex.lab = 1, cex.axis = 1, cex.legend = 0.75) {
  nList <- length(objectList)
  p <- ncol(objectList[[1]]@theta)
  a <- floor(sqrt(p))
  b <- ceiling(p / a)

  if (is.null(col)) {
    col <- 1 : nList
  } else {
    if (length(col) != nList) {
      stop ('length of "col" must match "objectList"')
    }
  }
  if (is.null(lty)) {
    lty <- 1 : nList
  } else {
    if (length(col) != nList) {
      stop ('length of "col" must match "objectList"')
    }
  }
  if (is.null(lwd)) {
    lwd <- rep(1, nList)
  } else {
    if (length(col) != nList) {
      stop ('length of "col" must match "objectList"')
    }
  }

  thetaNames <- objectList[[1]]@model@thetaNames
  if (length(thin) == 1L) {
    thin <- rep(thin, nList)
  }
  if (length(burnin) == 1L) {
    burnin <- rep(burnin, nList)
  }

  if (legendPosition == 'auto') {
    if (a*b == p) { # legend at bottom
      layoutM <- matrix(c(1:p,rep(p+1,b)), nrow = a+1, ncol = b, byrow = TRUE)
      layout(mat = layoutM, heights = c(rep(0.85/a,a),0.15))
      legendx <- 'center'
      legendHorz <- TRUE
    } else { # legend at corner
      layoutM <- matrix(c(1:p,rep(p+1,a*b-p)),nrow = a, ncol = b, byrow = TRUE)
      layout(mat = layoutM, heights = rep.int(1,a))
      legendx <- 'center'
      legendHorz <- FALSE
    }
  } else if (legendPosition == 'right') {
    if (a*b == p) {
      layoutM <- cbind(matrix(c(1:p), nrow = a, ncol = b, byrow = TRUE), rep(p+1,a))
      layout(mat = layoutM, widths = c(rep(0.85/b,b),0.15))
      legendx <- 'center'
      legendHorz <- FALSE
      mfg <- c(a, b+1)
    } else {
      layoutM <- cbind(matrix(c(1:p,rep(p+2,a*b-p)),nrow = a, ncol = b, byrow = TRUE), rep(p+1,a))
      layout(mat = layoutM, widths = c(rep(0.85/b,b),0.15))
      legendx <- 'center'
      legendHorz <- FALSE
    }
  } else if (legendPosition == 'bottom') {
    if (a*b == p) {
      layoutM <- matrix(c(1:p,rep(p+1,b)), nrow = a+1, ncol = b, byrow = TRUE)
      layout(mat = layoutM, heights = c(rep(0.85/a,a),0.15))
      legendx <- 'center'
      legendHorz <- TRUE
    } else {
      layoutM <- rbind(matrix(c(1:p,rep(p+2,a*b-p)),nrow = a, ncol = b, byrow = TRUE), rep(p+1,b))
      layout(mat = layoutM, heights = rep.int(1,a))
      legendx <- 'center'
      legendHorz <- TRUE
    }
  } else {
    stop('"legendPosition" must be "auto" or "right" or "bottom"')
  }

  if (is.null(legendNcol)) {
    legendNcol <- ifelse(legendHorz, nList, 1)
  }

  idx <- lapply(1:nList, FUN = function(i) seq((burnin[i] + 1), objectList[[i]]@M, thin[i]))
  theta <- d <- xRange <- yRange <- vector('list', p)
  for (k in 1:p) {
    theta[[k]] <- lapply(1:nList, FUN = function(i) objectList[[i]]@theta[idx[[i]], k])
    d[[k]] <- lapply(theta[[k]], FUN = density)
    xRange[[k]] <- range(sapply(1:nList, FUN = function(i) range(d[[k]][[i]]$x)))
    yRange[[k]] <- c(0, max(sapply(1:nList, FUN = function(i) max(d[[k]][[i]]$y))))
  }

  for (k in 1:p) {
    # par(mar = c(5.1,5.1,2,2))
    plot(0, type = 'n', main = NA, xlab = thetaNames[k], ylab = 'density', cex.lab = cex.lab,
         xlim = xRange[[k]], ylim = yRange[[k]], cex.axis = cex.axis)
    for (i in 1 : nList) {
      lines(d[[k]][[i]], col = col[i], lty = lty[i], lwd = lwd[i])
    }
    if (!is.null(thetaTrue)) {
      abline(v = thetaTrue[k], col = 'forestgreen', lty = 3)
    }
  }
  oldpar <- par(no.readonly = TRUE)    # get current user par settings
  on.exit(par(oldpar))            # reset current user par settings
  par(mar=c(0,0,0,0), cex = cex.legend)
  plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
  if (is.null(label)) {
    if (is.null(names(objectList))) {
      label <- paste0('result', 1 : nList)
    } else {
      label <- names(objectList)
    }
  }
  legend(x = legendx, legend = label, col = col, lty = lty, lwd = lwd, ncol = legendNcol)
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
}


multiPlotGgplot <- function(objectList, thin = 1, burnin = 0, thetaTrue = NULL, label = NULL, top = 'Approximate Marginal Posteriors',
                            options.color = list(), options.linetype = list(), options.size = list(), options.theme = list()) {
  nList <- length(objectList)
  p <- ncol(objectList[[1]]@theta)
  a <- floor(sqrt(p))
  b <- ceiling(p / a)
  if (!is.null(thetaTrue) & length(thetaTrue) != p) {
    stop('Length of thetaTrue does not match the number of parameters.')
  }
  thetaNames <- objectList[[1]]@model@thetaNames
  if (is.null(label)) {
    if (is.null(names(objectList))) {
      label <- names(objectList) <- paste0('result', 1 : nList)
    } else {
      label <- names(objectList)
    }
  }
  samples <- array(list(), nList)
  for (i in 1 : nList) {
    theta <- getTheta(objectList[[i]], burnin = burnin, thin = thin)
    samples[[i]] <- data.frame(theta, label = label[i])
  }
  samples <- do.call('rbind', samples)

  plist <- array(list(), p)
  for (i in 1 : p) {
    plist[[i]] <- ggplot(samples, aes_string(x = colnames(samples)[i])) +
      geom_density(aes(color = label, linetype = label, size = label)) + {
        if (length(options.color) != 0) {
          do.call(scale_color_manual, options.color)
        }
      } + {
        if (length(options.linetype) != 0) {
          do.call(scale_linetype_manual, options.linetype)
        }
      } + {
        if (!'values' %in% names(options.size)) {
          options.size$values <- rep(1, nList)
        }
        do.call(scale_size_manual, options.size)
      } +
      geom_hline(yintercept = 0, colour = "grey", size = 0.75) + {
        if (!is.null(thetaTrue)) {
          geom_vline(xintercept = thetaTrue[i], color = 'forestgreen', linetype = 'dashed', size = 0.5)
        }
      } +
      labs(x = thetaNames[i], y = 'density') + {
        if (!'plot.margin' %in% names(options.theme)) {
          options.theme$plot.margin <- unit(rep(0.08,4), "npc")
        }
        if (!'legend.title' %in% names(options.theme)) {
          options.theme$legend.title <- element_blank()
        }
        options.theme$legend.position <- 'none'
        do.call(theme, options.theme)
      }
  }
  g <- ggplotGrob(plist[[1]] + theme(legend.position = 'bottom'))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  combined <- arrangeGrob(do.call(arrangeGrob, c(plist, nrow = a, ncol = b, top = top)), legend, nrow = 2, heights = unit.c(unit(1, "npc") - lheight, lheight))
  grid.newpage()
  grid.draw(combined)
}
