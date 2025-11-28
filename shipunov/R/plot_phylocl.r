Plot.phylocl <- function(
tree, ## phylo object
cl, ## two columns classification table
strict=TRUE, ## do not join all descendants
keep.mono=FALSE, ## keep monotypic clades
what="triangles", ## also possible to use "rectangles"
col.ed="black", ## default edge color
col.td="black", ## default tips color
col.etr="transparent", ## color to suppress original edges
col.ttr="transparent", ## color to suppress original tips
col.pfl="lightgrey", ## fill color for polygons
col.pbr="black", ## border color of polygons
lty.p=1, ## line type of polygon borders
lwd.p=1, ## line width of polygon borders
col.ct="black", ## color of clade labels
ct.off=0, ## text offset of clade labels
ct.fnt=1, ## text font of clade labels
cex=par("cex"), # text size for all labels
longer="0%", ## percent to increase xlim to fit longer clade labels
... ## options to _plot.phylo()_
) {
 ## make list
 cladelist <- split(cl[, 1], cl[, 2])
 ##
 if (!keep.mono) {
 ## remove monotypic
 monoty <- sapply(cladelist, function(.x) length(.x) == 1)
 ## tell what removed
 cat("Monotypic clades removed: ", unlist(cladelist[monoty]), "\n")
 cladelist <- cladelist[!monoty]
 }
 ##
 ## set default colors
 colo <- rep(col.ed, dim(tree$edge)[1])
 tcolo <- rep(col.td, length(tree$tip.label))
 ##
 ## null plotting to determine size of tree
 pdf(file=NULL)
  tmp1 <- ape::plot.phylo(tree, plot=FALSE, ...)
 dev.off()
 longer <- as.numeric(sub("%", "", longer))
 newx <- tmp1$x.lim * (1 + longer / 100)
 ## then open device with proper size
 tmp2 <- ape::plot.phylo(tree, plot=FALSE, x.lim=newx, ...)
 ##
 ## propagate clade labels offset, font and colors
 if (length(ct.off) == 1) ct.off <- rep(ct.off, length(cladelist))
 if (length(ct.fnt) == 1) ct.fnt <- rep(ct.fnt, length(cladelist))
 if (length(col.ct) == 1) col.ct <- rep(col.ct, length(cladelist))
 if (length(col.pfl) == 1) col.pfl <- rep(col.pfl, length(cladelist))
 if (length(col.pbr) == 1) col.pbr <- rep(col.pbr, length(cladelist))
 if (length(lty.p) == 1) lty.p <- rep(lty.p, length(cladelist))
 if (length(lwd.p) == 1) lwd.p <- rep(lwd.p, length(cladelist))
 ##
 ## MAIN CYCLE
 for (n in seq_along(cladelist)) {
 ##
 clade <- cladelist[[n]]
 tipsn <- match(clade, tree$tip.label)
 mm <- ape::getMRCA(tree, clade)
 ## if strict=FALSE, get all descendants as tips numbers
 if (!strict & (length(clade) > 1)) {
  tipsn <- which(sapply(ape::nodepath(tree, tipsn), function(.x) mm %in% .x))
  clade <- tree$tip.label[tipsn]
 }
 ##
 x0 <- ape::node.depth.edgelength(tree)[mm] # x of MRCA node
 y0 <- ape::node.height(tree)[mm] # y of MRCA node
 y1 <- min(ape::node.height(tree)[tipsn]) # y of top tip
 y2 <- max(ape::node.height(tree)[tipsn]) # y of bottom tip
 xx <- max(ape::node.depth.edgelength(tree)[tipsn]) # x of tips
 y3 <- (y1 + y2) / 2 # middle y, for label
 ##
 ## add polygons
 if(what == "rectangles") polygon(c(x0, x0, xx, xx), c(y1, y2, y2, y1), col=col.pfl, border=col.pbr, lty=lty.p, lwd=lwd.p)
 if(what == "triangles") polygon(c(x0, xx, xx), c(y0, y1, y2), col=col.pfl, border=col.pbr, lty=lty.p, lwd=lwd.p)
 ## add clade text labels
 text(xx, y3, names(cladelist)[n], pos=4, font=ct.fnt[n], offset=ct.off[n], col=col.ct[n], cex=cex)
 ## make transparent part
 if (length(clade) > 1) colo[ape::which.edge(tree, clade)] <- col.etr
 tcolo[tree$tip.label %in% clade] <- col.ttr
 }
 ## plot partially transparent tree
 oldpar <- par(new=TRUE)
  ape::plot.phylo(tree, edge.color=colo, tip.color=tcolo, x.lim=newx, cex=cex, ...)
 par(oldpar)
invisible(names(cladelist))
}
