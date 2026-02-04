eqscplot <- function (x, y, ratio = 1, tol = 0.04, uin, ...) {
  dots <- list(...)
  nmdots <- names(dots)
  Call <- match.call()
  Call$ratio <- Call$tol <- Call$uin <- NULL
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
    if (!is.null(dn <- colnames(x))) {
      xlab0 <- dn[1L]
      ylab0 <- dn[2L]
    }
    else {
      xlab0 <- ""
      ylab0 <- ""
    }
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
    xlab0 <- "x"
    ylab0 <- "y"
  }
  else {
    xlab0 <- deparse(substitute(x))
    ylab0 <- deparse(substitute(y))
  }
  Call$x <- x
  Call$y <- y
  Call$xlab <- if ("xlab" %in% nmdots) 
    dots$xlab
  else xlab0
  Call$ylab <- if ("ylab" %in% nmdots) 
    dots$ylab
  else ylab0
  xlim <- if ("xlim" %in% nmdots) 
    dots$xlim
  else range(x[is.finite(x)])
  ylim <- if ("ylim" %in% nmdots) 
    dots$ylim
  else range(y[is.finite(y)])
  midx <- 0.5 * (xlim[2L] + xlim[1L])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2L] - xlim[1L])
  midy <- 0.5 * (ylim[2L] + ylim[1L])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2L] - ylim[1L])
  oldpin <- par("pin")
  xuin <- oxuin <- oldpin[1L]/abs(diff(xlim))
  yuin <- oyuin <- oldpin[2L]/abs(diff(ylim))
  if (missing(uin)) {
    if (yuin > xuin * ratio) 
      yuin <- xuin * ratio
    else xuin <- yuin/ratio
  }
  else {
    if (length(uin) == 1L) 
      uin <- uin * c(1, ratio)
    if (any(c(xuin, yuin) < uin)) 
      stop("'uin' is too large to fit plot in")
    xuin <- uin[1L]
    yuin <- uin[2L]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  Call$xlim <- xlim
  Call$ylim <- ylim
  Call$xaxs <- Call$yaxs <- "i"
  Call[[1L]] <- as.name("plot")
  eval.parent(Call)
}

ldahist <- function (
    data, g, nbins = 25, h, x0 = -h/1000, breaks, xlim = range(breaks), 
    ymax = 0, width, type = c("histogram", "density", "both"), 
    sep = (type != "density"), col = 5L, xlab = deparse(substitute(data)), 
    bty = "n", ...) 
{
  xlab
  type <- match.arg(type)
  data <- data[!is.na(data)]
  g <- g[!is.na(data)]
  counts <- table(g)
  groups <- names(counts)[counts > 0L]
  if (missing(breaks)) {
    if (missing(h)) 
      h <- diff(pretty(data, nbins))[1L]
    first <- floor((min(data) - x0)/h)
    last <- ceiling((max(data) - x0)/h)
    breaks <- x0 + h * c(first:last)
  }
  if (type == "histogram" || type == "both") {
    if (any(diff(breaks) <= 0)) 
      stop("'breaks' must be strictly increasing")
    if (min(data) < min(breaks) || max(data) > max(breaks)) 
      stop("'breaks' do not cover the data")
    est <- vector("list", length(groups))
    names(est) <- groups
    for (grp in groups) {
      bin <- cut(data[g == grp], breaks, include.lowest = TRUE)
      est1 <- tabulate(bin, length(levels(bin)))
      est1 <- est1/(diff(breaks) * length(data[g == grp]))
      ymax <- max(ymax, est1)
      est[[grp]] <- est1
    }
  }
  if (type == "density" || type == "both") {
    xd <- vector("list", length(groups))
    for (grp in groups) {
      if (missing(width)) 
        width <- width.SJ(data[g == grp])
      xd1 <- density(data[g == grp], n = 200L, width = width, 
                     from = xlim[1L], to = xlim[2L])
      ymax <- max(ymax, xd1$y)
      xd[[grp]] <- xd1
    }
  }
  dev.hold()
  on.exit(dev.flush())
  if (!sep) 
    plot(xlim, c(0, ymax), type = "n", xlab = xlab, ylab = "", 
         bty = bty)
  else {
    oldpar <- par(mfrow = c(length(groups), 1L))
    on.exit(par(oldpar), add = TRUE)
  }
  for (grp in groups) {
    if (sep) 
      plot(xlim, c(0, ymax), type = "n",
           xlab = paste("group", grp), ylab = "", bty = bty)
    if (type == "histogram" || type == "both") {
      n <- length(breaks)
      rect(breaks[-n], 0, breaks[-1L], est[[grp]], col = col, 
           ...)
    }
    if (type == "density" || type == "both") 
      lines(xd[[grp]])
  }
  invisible()
}

panel.lda <- function(x, y, g, cex, ...)
  text(x, y, as.character(g), cex = cex, ...)

#' Funcion LDA
#' @noRd
plot.lda <- function (x, X, g, panel = panel.lda, ..., cex = 0.7,
                      abbrev = FALSE, xlab = "LD1", ylab = "LD2") 
{
  if (abbrev) 
    levels(g) <- abbreviate(levels(g), abbrev)
  means <- colMeans(x$means)
  X <- model.matrix(x$terms, X)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L) 
    X <- X[, -xint, drop = FALSE]
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  if (ncol(X) > 2L) {
    pairs(X, panel = panel, ...)
  } else if (ncol(X) == 2L) {
    eqscplot(X[, 1L:2L], xlab = xlab, ylab = ylab, type = "n", ...)
    panel(X[, 1L], X[, 2L], g, cex, ...)
  } else {
    ldahist(X[, 1L], g, xlab = xlab, ...)
  }
  invisible(NULL)
}
