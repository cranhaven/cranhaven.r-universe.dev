
# A wrapper for 'text' which keeps text away from the edges of the plot.

safeText <- function(x, y, labels, adj = c(0.5,0), cex=1, ...) {

  strw <- strwidth(labels, units = "user", cex = cex)
  safePos <- pmax(pmin(x, par("usr")[2]-strw*0.7), par("usr")[1]+strw*0.7)
  text(x=safePos, y=y, labels=labels, adj=adj, cex=cex, ...)
}
