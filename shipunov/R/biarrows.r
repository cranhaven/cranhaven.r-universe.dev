Biarrows <- function(
 deriv,
 orig,
 coeffs=NULL,
 shrink=0.45,
 closer=0.9,
 pt.col="forestgreen",
 pt.cex=1,
 pt.pch=NA, # no points
 tx=colnames(orig),
 tx.col="forestgreen",
 tx.cex=0.8,
 tx.font=1,
 tx.pos=NULL,
 tx.off=0.5,
 xpd=TRUE,
 ar.col="forestgreen",
 ar.len=0.05,
 shift="auto",
 ...) {
 deriv <- deriv[, 1:2] # all other variables discarded
 ranges <- apply(deriv, 2, function(.x) max(.x, na.rm=TRUE) - min(.x, na.rm=TRUE))
 if(is.null(coeffs)) coeffs <- cor(orig, deriv, method="pearson", use="pairwise.complete.obs")
 x <- coeffs[, 1] * ranges[1] * shrink
 y <- coeffs[, 2] * ranges[2] * shrink
 if (!is.numeric(shift) && shift == "auto") shift <- colMeans(deriv)
 points(x+shift[1], y+shift[2], col=pt.col, cex=pt.cex, pch=pt.pch)
 text(x+shift[1], y+shift[2], col=tx.col, cex=tx.cex, font=tx.font, pos=tx.pos, offset=tx.off, labels=tx, xpd=xpd)
 arrows(0+shift[1], 0+shift[2], x*closer+shift[1], y*closer+shift[2], col=ar.col, length=ar.len, ...)
}
