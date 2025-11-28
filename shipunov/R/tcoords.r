Tcoords <- function(hcl, hang=0.1, add=0, horiz=FALSE) {
 yh <- numeric(length(hcl$labels))
 for(i in seq_len(nrow(hcl$merge))){
  sngls <- hcl$merge[i, ] < 0 # negative values are observations
  yi <- -hcl$merge[i, sngls]
  yh[yi] <- hcl$height[i]
 }
 x <- seq_len(nrow(hcl$merge) + 1L) # like in plot.hclust()
 if (hang >= 0) {
  y <- yh[hcl$order] - (diff(range(hcl$height)) * (hang + add))
  } else {
  y <- 0 - (diff(range(hcl$height)) * add)
  }
 if(!horiz) cbind(x, y) else cbind(x=y, y=x)
}

## ===

Fence <- function(hcl, fct, ex=0.05, lwd=2.5, horiz=FALSE, hang=0.1, ...) {
 pos <- Tcoords(hcl, hang=hang, horiz=horiz)
 add <- diff(range(hcl$height)) * ex
 segments(pos[, 1], pos[, 2], y1=pos[, 2] + add, col=as.factor(fct)[hcl$order], lwd=lwd, ...)
}

## ===

Tctext <- function(hcl, labels=hcl[["labels"]], hang=0.1, add=0, horiz=FALSE, xpd=TRUE, ...) {
 coords <- Tcoords(hcl, hang=hang, add=add, horiz=horiz)
 neworder <- order(hcl$order) # sort symmetrically to hcl$order
 text(coords[neworder, ], labels=labels, xpd=xpd, ...)
}
