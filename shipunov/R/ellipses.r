Ellipses <- function(pts, groups, match.color=TRUE, usecolors=NULL,
 centers=FALSE, c.pch=0, c.cex=3,
 level=0.95, df=1000, prec=51,
 coords=NULL, plot=TRUE, ...) {
 ppts <- list()
 out <- seq(along=groups)
 inds <- names(table(groups))
 if (centers) cnts <- matrix(ncol=2, nrow=length(inds), dimnames=list(inds))
 for (is in inds) {
  if (match.color) { m.col <- match(is, inds) } else { m.col <- "black" }
  if (!is.null(usecolors)) m.col <- usecolors[inds == is]
  gr <- out[groups == is]
  X <- pts[gr, ]
  c.X <- apply(X, 2, median)
  if (is.null(coords)) {
   if (length(gr) > 1) {
    cov.mat <- cov(X)
    d <- sqrt(diag(cov.mat))
    dfvec <- c(2, df)
    phase <- acos(cov.mat[1, 2] / (d[1]*d[2]))
    angles <- seq(-(pi), pi, len=prec)
    mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
    xpts <- c.X[1] + d[1]*mult*cos(angles)
    ypts <- c.X[2] + d[2]*mult*cos(angles + phase)
    if(plot) lines(xpts, ypts, col=m.col, ...)
    ppts[[is]] <- cbind(xpts, ypts)
    }
   } else {
   lines(coords[[is]][, 1], coords[[is]][, 2], col=m.col, ...)
   }
  if (centers) {
   cnts[is, ] <- c(c.X[1], c.X[2])
   if(plot) points(c.X[1], c.X[2], pch=c.pch, cex=c.cex, col=m.col)
  }
 }
 if (centers) attr(ppts, "centers") <- cnts
 invisible(ppts)
}
