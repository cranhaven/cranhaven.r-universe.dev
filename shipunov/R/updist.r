Updist <- function(dst, link=NULL, unlink=NULL, dmax=max(dst), dmin=min(dst)) {
a <- as.matrix(dst)
dd <- diag(a)
if(!is.null(link)) for (i in link) a[i, i] <- dmin
if(!is.null(unlink)) for (i in unlink) a[i, i] <- dmax
diag(a) <- dd
as.dist(a)
}
