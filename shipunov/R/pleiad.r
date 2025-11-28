Pleiad <- function (tbl, abs=FALSE, corr=FALSE, dist=FALSE, treshold=FALSE,
circ=list(1, 1, 1), breaks=5, auto=TRUE, gr=6, lwd=NULL,
lty=NULL, lcol=NULL, abbr=-1, lbltext="internal",
lblcex=1, off=1.09, hofft=0.07, hoff=1.02, legend=TRUE,
legtext=1, legpos="topright", leghoriz=FALSE, show.int=FALSE,
dig.lab=1, neg.col=NULL, ...) {
if (breaks[1] == "cramer") breaks <- c(0, 0.1, 0.3, 0.5, 1)
 lwds <- list(b1=1, b2=c(1, 4), b3=c(1, 1.2, 3), b4=c(1, 1, 2, 4), b5=c(1, 1, 1, 2.5, 4))
 ltys <- list(b1=1, b2=c(3, 1), b3=c(3, 2, 1), b4=c(3, 2, 1, 1), b5=c(3, 2, 1, 1, 1))
 lcols <- list(b1=1, b2=c(1, 1), b3=c(1, 1, 1), b4=c(grey(0.5), 1, 1, 1), b5=c(grey(c(0.6, 0.5)), 1, 1, 1))
 if (dist) tbl <- as.matrix(max(tbl) - tbl) # convert distances (dissimilarities) into similarities
 tbl <- data.frame(tbl)
 ddu <- unique(unlist(dimnames(tbl)))
 ddc <- t(combn(ddu, 2))
 ddn <- apply(ddc, 1, function(.x) { # could be fragile
  .y <- tbl[.x[1], .x[2]]
  ifelse(is.null(.y), NA, .y)
 })
 ddn[is.na(ddn)] <- 0
 if (corr || abs) {
  neg <- which(ddn < 0) # keep which are negative
  ddn <- abs(ddn)
 }
 x <- sin(seq(0, 2 * pi, length.out=length(ddu) + 1))
 y <- cos(seq(0, 2 * pi, length.out=length(ddu) + 1))
 fromx <- as.numeric(Recode(ddc[ddn != 0, 1], ddu, x))
 fromy <- as.numeric(Recode(ddc[ddn != 0, 1], ddu, y))
 tox <- as.numeric(Recode(ddc[ddn != 0, 2], ddu, x))
 toy <- as.numeric(Recode(ddc[ddn != 0, 2], ddu, y))
 segcut <- cut(ddn[ddn != 0], breaks, dig.lab=dig.lab)
 if ((length(breaks) == 1) & corr) {
  segcut <- cut(ddn[ddn != 0], seq(0, 1, length.out=(breaks + 1)))
 }
 br <- nlevels(segcut)
 if (auto) {
  if ((br < gr) & (br < 6)) {
   lwd <- lwds[[br]]
   lty <- ltys[[br]]
   if (length(lcol) != 1)
    lcol <- lcols[[br]]
  } else {
   lwd <- seq(1, 4, length.out=br)
   lty <- 1
   lcol <- grey(seq(1, 0, length.out=(br + 1)))[-1]
 }} else {
  if (any(is.null(lwd), is.null(lty), is.null(lcol)))
    stop("auto=FALSE therefore lwd, lty and lcol must be all non-null")
  if (!all(br == length(lwd), br == length(lty), br == length(lcol)))
    stop("Lengths of breaks, lwd, lty and lcol must be the same")
 }
 seglwd <- cbind(1:br, lwd)
 seglty <- cbind(1:br, lty)
 segcol <- cbind(1:br, lcol)
 segp <- as.numeric(segcut)
 segpt <- Recode(segp, seglty[, 1], seglty[, 2])
 segpt[is.na(segpt)] <- 0
 segpw <- Recode(segp, seglwd[, 1], seglwd[, 2])
 segpw[is.na(segpw)] <- 1
 segcl <- Recode(segp, segcol[, 1], segcol[, 2])
 segcl[is.na(segcl)] <- 0
 if (!is.null(neg.col) & (corr || abs)) segcl[neg] <- neg.col # colorize negative
 if (treshold) segcol <- segpw <- segpt <- (ddn > treshold) * 1
 if (abbr > -1) dda <- abbreviate(ddu, abbr) else dda <- ddu
 seglev <- sub(",", " - ", levels(segcut), fixed=TRUE)
 if (!show.int) {
  seglev <- sub("(", "", seglev, fixed=TRUE)
  seglev <- sub("]", "", seglev, fixed=TRUE)
 }
 legtxtable <- cbind(c("weaker", rep("", br - 2), "stronger"), seglev, 1:br)
 legtxt <- legtxtable[, legtext]
 if (treshold) {
  lcol <- lty <- lwd <- 1
  legtxt <- paste(">", treshold)
 }
 oldpar <- par(mar=c(0, 0, 0, 0))
 plot(x, y, xlim=c(-1, 1) * 1.15, ylim=c(-1, 1) * 1.15, axes=FALSE, type="n")
 polygon(sin(seq(0, 2 * pi, length.out=100)), cos(seq(0, 2 * pi, length.out=100)),
  lty=circ[[1]], lwd=circ[[2]], border=circ[[3]])
 segments(fromx, fromy, tox, toy, lwd=segpw, lty=segpt, col=segcl)
 points(x, y, ...)
 if (lbltext[1] == "internal") lbltxt <- dda else lbltxt <- lbltext
 posd <- x[-length(x)]
 posx <- posd * off
 ifelse(abs(posx - posd) > hofft, posx <- posx * hoff, posx)
 text(posx, y[-length(y)] * off, labels=lbltxt, cex=lblcex)
 if (legend) legend(legpos, horiz=leghoriz, lty=lty, lwd=lwd, col=lcol,
  legend=legtxt, bty="n", seg.len=1.2)
 par(oldpar)
 invisible(data.frame(x=x[-length(x)], y=y[-length(y)])) # useful if you need to enhance plot
}
