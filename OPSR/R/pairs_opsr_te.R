#' Pairs Plot for OPSR TE Objects
#'
#' @param x an object of class `"opsr.te"`.
#' @param pch plotting 'character', i.e., symbol to use. See also [`pch`].
#' @param labels.diag labels used in the diagonal panels.
#' @param labels.reg labels for the treatment regimes.
#' @param col colour vector.
#' @param add.rug if `TRUE`, adds rugs to the lower panels.
#' @param lower.digits rounding of the digits in the lower panel.
#' @param diag.digits rounding of the digits in the diagonal panel.
#' @param lwd.dens linewidth of the densities in the diagonal panel.
#' @param diag.cex.text `cex` for the text in the diagonal panel.
#' @param upper.digits rounding of the digits in the upper panel.
#' @param upper.cex.text `cex` for the text in the upper panel.
#' @param prefix for the number plotted in the upper panel.
#' @param postfix for the number plotted in the upper panel.
#' @param lty.diag linetype for the diagonal panel.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Returns `x` invisibly.
#'
#' @details
#' Presents all potential counterfactual outcomes. The diagonal depicts
#' distributions in any given treatment regime and separate by the current
#' (factual) treatment group. The weighted mean values are shown as red numbers.
#' The lower triangular panels compare the model-implied (predicted) outcomes
#' of two treatment regimes again separate by current treatment group. The red
#' line indicates the 45-degree line of equal outcomes while the red squares
#' depict again the weighted mean values. The upper triangular panels show
#' (weighted) average treatment effects.
#'
#' @method pairs opsr.te
#' @seealso [`pairs`]
#' @example R/examples/ex-opsr_te.R
#' @export
pairs.opsr.te <- function(x, pch = 21, labels.diag = paste0("T", 1:x$nReg),
                          labels.reg = paste0("G", 1:x$nReg), col = 1:x$nReg,
                          add.rug = TRUE, lower.digits = 0, diag.digits = 0,
                          lwd.dens = 1.5, diag.cex.text = 1, upper.digits = 2,
                          upper.cex.text = 2, prefix = "", postfix = "",
                          lty.diag = 1, ...) {
  ## x is opsr.ate
  preprocess <- function() {
    dat <-
      lapply(x$ce.by.groups, function(x) {
        group <- attr(x, "group")
        df <- as.data.frame(x)
        df$group <- group
        df
      })

    dat <- Reduce(rbind, dat)
    names(dat) <- c(labels.diag, "group")
    dat$group <- factor(dat$group, labels = labels.reg)
    dat
  }

  ## g is group, w is weight
  panel.lower <- function(x, ...) {
    dots <- list(...)
    y <- dots[[1]]
    n <- length(unique(g))
    graphics::points(x, ...)
    if (add.rug) {
      graphics::rug(x, ticksize = 0.02)
      graphics::rug(y, ticksize = 0.02, side = 2)
    }
    xw <- as.numeric(
      by(x, g, function(x) stats::weighted.mean(x, w = w, na.rm = TRUE))
    )
    yw <- as.numeric(
      by(y, g, function(y) stats::weighted.mean(y, w = w, na.rm = TRUE))
    )
    graphics::abline(a = 0, b = 1, col = "red", lty = lty.diag, lwd = 2)
    graphics::points(xw, yw, pch = 15, col = "red", cex = 2 * graphics::par("cex"))
    tmp <- graphics::legend("topleft", legend = unique(g), pch = dots$pch, pt.bg = unique(dots$bg), bty = "n")
    xwr <- round(xw, lower.digits)
    ywr <- round(yw, lower.digits)
    graphics::text(tmp$rect$left + tmp$rect$w, tmp$text$y, paste0("(", xwr, ", ", ywr, ")"), col = "red", pos = 4)
  }

  ## x are actually the treatment effects
  panel.diag <- function(x, ...) {
    dots <- list(...)
    usr <- graphics::par("usr")
    graphics::par(usr = c(usr[1:2], 0, 1.5))  # sets height of ploting area!
    preprocess <- function() {
      i <- 1
      by(x, g, function(x) {
        df <- data.frame(x = stats::na.omit(x))
        df$group <- i
        df$col <- col[i]
        i <<- i + 1
        df
      })
    }
    p.density <- function(dl) {
      dfl <-
        lapply(dl, function(x) {
          d <- stats::density(x$x)
          df <- data.frame(x = d$x, y = d$y, col = unique(x$col))
        })
      maxy <- max(Reduce(rbind, dfl)$y)
      lapply(dfl, function(x) {
        ## rescale (ploting area was set via usr)
        x$y <- x$y / maxy
        graphics::lines(x$x, x$y, col = x$col, lwd = lwd.dens, ...)
      })
    }
    p.markers <- function() {
      xw <- as.numeric(
        by(x, g, function(x) stats::weighted.mean(x, w = w, na.rm = TRUE))
      )
      # points(xw, rep(0, length(xw)), pch = "|", col = "red", cex = cex.marker)
      labels <- round(xw, diag.digits)
      graphics::text(xw, y = rep(0, length(xw)), labels = labels, srt = 90, col = "red",
                     cex = diag.cex.text, adj = c(-0.1, 0.5))
    }
    dl <- preprocess()
    p.density(dl)
    p.markers()
  }

  panel.upper <- function(x, y, ...) {
    graphics::par(usr = c(0, 1, 0, 1))
    ## careful: weights do not align and need to be prepared like below
    xo <- stats::na.omit(x)
    xi <- attr(xo, "na.action")
    yo <- stats::na.omit(y)
    yi <- attr(yo, "na.action")
    x.weights <- rep(w, object$nReg)[-xi]
    y.weights <- rep(w, object$nReg)[-yi]
    xw <- stats::weighted.mean(xo, w = x.weights)
    yw <- stats::weighted.mean(yo, w = y.weights)
    te <- xw - yw
    txt <- format(c(te, 0.123456789), digits = upper.digits)[1]
    txt <- paste0(prefix, txt, postfix)
    graphics::text(0.5, 0.5, txt, cex = upper.cex.text)
  }

  dat <- preprocess()
  w <- x$weights
  g <- dat$group
  object <- x
  graphics::pairs(dat[, -ncol(dat)], pch = pch, bg = col[dat$group],
                  lower.panel = panel.lower,
                  diag.panel = panel.diag,
                  upper.panel = panel.upper,
                  ...)

  invisible(x)
}
