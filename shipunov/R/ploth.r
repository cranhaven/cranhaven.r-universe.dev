Ploth <- function(hclust, labels=hclust[["labels"]], lab.font=1, lab.col=1, col=1, pch.cex=1, pch=NA, bg=0, col.edges=FALSE, hang=-1, ...)
{
plot(dendrapply(as.dendrogram(hclust, hang=hang), function(n)
 {
 if(is.leaf(n))
  {
  at <- attributes(n)
  if (length(lab.col) > 1) lab.col <- lab.col[n]
  if (length(col) > 1) col <- col[n]
  if (length(pch.cex) > 1) pch.cex <- pch.cex[n]
  if (length(pch) > 1) pch <- pch[n]
  if (length(bg) > 1) bg <- bg[n]
  attr(n, "nodePar") <- c(at$nodePar, list(lab.col=lab.col, col=col, pch=pch, bg=bg, cex=pch.cex))
  attr(n, "label") <- labels[n]
  if (length(lab.font) > 1) lab.font <- lab.font[n]
  if (lab.font==2) attr(n, "label") <- as.expression(substitute(bold(leaf), list(leaf=attr(n, "label"))))
  if (lab.font==3) attr(n, "label") <- as.expression(substitute(italic(leaf), list(leaf=attr(n, "label"))))
  if (lab.font==4) attr(n, "label") <- as.expression(substitute(bolditalic(leaf), list(leaf=attr(n, "label"))))
  if (col.edges) attr(n, "edgePar") <- list(col=col)
  }
 n
 }), ...)
}
