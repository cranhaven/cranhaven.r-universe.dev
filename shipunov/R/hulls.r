Hulls <- function(pts, groups, match.colors=TRUE, usecolors=NULL, plot=TRUE,
 centers=FALSE, c.pch=0, c.cex=3, outliers=TRUE, coef=1.5, ...)
{
ppts <- list()
out <- seq(along=groups)
inds <- names(table(groups))
for (is in inds) {
 if (match.colors) { m.col <- match(is, inds) } else { m.col <- "black" }
 if (!is.null(usecolors)) m.col <- usecolors[match(is, inds)]
 gr <- out[groups == is]
 if (length(gr) > 1) {
  X <- pts[gr, 1:2]
  hpts <- chull(X)
  ppts[[is]] <- X[hpts, ]
  rownames(ppts[[is]]) <- (1:nrow(pts))[gr][hpts]
  hpts.l <- c(hpts, hpts[1])
  if (plot & outliers) lines(X[hpts.l, ], col=m.col, ...)
  }
 }
if(!outliers) centers <- TRUE
if(centers) {
  cnts <- t(sapply(ppts, Polycenter))
  if (match.colors) { m.col <- seq_along(ppts) } else { m.col <- "black" }
  if (!is.null(usecolors)) m.col <- usecolors
  if (plot & outliers) points(cnts, pch=c.pch, cex=c.cex, col=m.col)
  attr(ppts, "centers") <- cnts
  }
if(!outliers) {
 if (plot) points(cnts, pch=c.pch, cex=c.cex, col=m.col)
 lout <- numeric(0)
 for (is in inds) {
  if (match.colors) { m.col <- match(is, inds) } else { m.col <- "black" }
  if (!is.null(usecolors)) m.col <- usecolors[match(is, inds)]
  gr <- out[groups == is]
  if (length(gr) > 1) {
   X <- pts[gr, 1:2]
   C <- cnts[match(is, inds), ]
   D <- apply(X, 1, function(.x) sqrt(sum((.x - C)^2)))
   outliers <- D %in% boxplot.stats(D, coef=coef)$out
   X <- X[!outliers, ]
   hpts <- chull(X)
   ppts[[is]] <- X[hpts, ]
   rownames(ppts[[is]]) <- (1:nrow(pts))[gr][hpts]
   hpts.l <- c(hpts, hpts[1])
   if (plot) lines(X[hpts.l, ], col=m.col, ...)
   }
  lout <- c(lout, (seq_len(nrow(pts)))[gr][outliers])
 }
 attr(ppts, "outliers") <- lout
}
invisible(ppts)
}
