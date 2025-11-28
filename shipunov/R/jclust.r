Jclust <- function (data, n.cl, iter=1000, method.d="euclidean", method.c="ward.D", bootstrap=TRUE, monitor=TRUE) {
j.res <- matrix(rep(0, nrow(data)^2), ncol=nrow(data))
if (method.c == "ward.D" & sum(grep("ward.D", body(hclust))) ==  0) method.c <- "ward"
if (bootstrap) {
 for (i in 1:iter) {
  j.sample <- sample(seq_len(ncol(data)), replace=TRUE)
  j.data <- data[, j.sample]
  j.dist <- dist(j.data, method=method.d)
  j.clust <- cutree(hclust(j.dist, method=method.c), k=n.cl)
  j.mat <- outer(j.clust, j.clust, "==")
  j.res <- j.res + j.mat
  if (monitor) cat(".")
  }
} else {
 iter <- ncol(data)
 for (i in seq_len(ncol(data))) {
  j.data <- data[, -i]
  j.dist <- dist(j.data, method=method.c)
  j.clust <- cutree(hclust(j.dist, method=method.d), k=n.cl)
  j.mat <- outer(j.clust, j.clust, "==")
  j.res <- j.res + j.mat
  if (monitor) cat(".")
  }
}
if (monitor) cat("\n")
j.supp <- c(rep(0, n.cl))
j.hcl <- hclust(dist(j.res, method=method.d), method=method.c)
j.group <- cutree(j.hcl, k=n.cl)
for (j in 1:n.cl) {
 j.which <- which(j.group == j)
 j.subset <- j.res[j.which, j.which]
 j.supp[j] <- median(as.vector(j.subset), na.rm=TRUE)/iter
}
j.meth <- ifelse(bootstrap, "Bootstrap", "Jackknife")
j.clust <- list(meth=j.meth, mat=j.res, hclust=j.hcl, gr=j.group, supp=j.supp, iter=iter, n.cl=n.cl)
class(j.clust) <- "Jclust"
j.clust
}

## ===

print.Jclust <- function(x, ...)
{
 cat("\n", x$meth, "support for", x$n.cl, "clusters,", x$iter, "iterations: \n")
 cat("\n")
 if (is.null(names(x$gr))) names(x$gr) <- as.character(x$gr)
 clus <- aggregate(names(x$gr), list(x$gr), toString)
 clus <- cbind(x$supp*100, clus)
 colnames(clus) <- c("support", "cluster", "members")
 print(clus[rev(order(clus$support)),], row.names=FALSE, ...)
}


## ===

plot.Jclust <- function (x, main="", xlab=NULL, rect.lty=3, rect.col=1,
rect.xpd=TRUE, top=FALSE,  lab.pos=3, lab.offset=0.5,
lab.col=par("col"), lab.font=par("font"), ...) {
if (is.null(xlab)) xlab <- paste(x$meth, ", ", x$iter, " replicates", sep = "")
plot(x$hclust, main=main, xlab=xlab, ...)
tree <- x$hclust
k <- x$n.cl
cluster <- x$gr
clusorder <- unique(cluster[tree$order])
clustab <- table(cluster)[clusorder]
m <- c(0, cumsum(clustab))
which <- 1L:k
for (n in seq_along(which)) {
 xleft <- m[which[n]] + 0.7
 ybottom <- par("usr")[3L]
 xright <- m[which[n] + 1] + 0.32
 xmid <- (xleft + xright)/2
 ytop <- mean(rev(tree$height)[(k - 1):k])
 rect(xleft, ybottom, xright, ytop, lty=rect.lty, border=rect.col, xpd=rect.xpd)
 text(xmid, if(top) ytop else ybottom,
  labels=paste0(round(x$sup[clusorder[n]] * 100, 1), "%"),
  pos=lab.pos, offset=lab.offset, col=lab.col, font=lab.font)
 }
}
