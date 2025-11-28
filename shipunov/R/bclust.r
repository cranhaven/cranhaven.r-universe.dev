Bclust <- function(data, method.d="euclidean", method.c="ward.D",
 FUN=function(.x) hclust(dist(.x, method=method.d), method=method.c),
 iter=1000, mc.cores=1, monitor=TRUE, bootstrap=TRUE, relative=FALSE,
 hclist=NULL) {
 .calc.matches <- function(origin, current, nc=ncol(origin)) {
  one <- tcrossprod(origin, current) # both 1
  zero <- tcrossprod(1 - origin, 1 - current) # both 0
  res <- rowSums(one + zero == nc) # absolute matches (cluster present or absent)
  zeros <- sum(res == 0) # how many non-matches
  if (zeros > 0 && relative) { # select non-matches and calculate _relative matches_
  dsts <- as.matrix(dist(rbind(origin[res == 0, ], current), # find distances
   method="binary"))[seq_len(zeros), -seq_len(zeros), drop=FALSE] # asymmetric binary because "0" and "1" are principally different
  res[res == 0] <- apply(dsts, 1, min) # replace zeroes with closest distances
  }
  res
 }
 fun <- match.fun(FUN)
 if (is.null(hclist)) hcl <- fun(data) else hcl <- hclist[[1]]
 origin <- Hcl2mat(hcl)
 if (!bootstrap) iter <- ncol(data) # jacknife
 if (!is.null(hclist)) iter <- length(hclist) - 1 # pre-built list
 v <- parallel::mclapply(seq_len(iter), function(.y, origin, size, fun, nco) {
  if (!is.null(hclist)) {
   current <- Hcl2mat(hclist[[.y + 1]]) # pre-built list
  } else {
  if(!bootstrap) {
   current <- Hcl2mat(fun(data[, -.y])) # jacknife
  } else {
   current <- Hcl2mat(fun(data[, sample.int(ncol(data), replace=TRUE)]))
  }}
  if (monitor) cat(".")
  return(list(matches=.calc.matches(origin, current, nco), current=current))
  },
 mc.cores=mc.cores, origin=origin, size=ncol(data), fun=fun, nco=ncol(origin))
 if (monitor) cat("\n")
 values <- colSums(do.call(rbind, lapply(v, `[[`, "matches")))/iter
 con <- t(Reduce("+", lapply(v, `[[`, "current")))/iter
 if (is.null(hclist)) row.names(con) <- row.names(data) else row.names(con) <- hcl$labels
 b.meth <- ifelse(bootstrap, "Bootstrap", "Jackknife")
 b.clust <- list(values=values, hclust=hcl, consensus=con, meth=b.meth, iter=iter)
 class(b.clust) <- "Bclust"
 b.clust
}

## ===

Hcl2mat <- function(hcl) {
 nr <- as.integer(nrow(hcl$merge))
 m <- matrix(0L, nrow=nr, ncol=nr+1L)
 for (i in seq_len(nr)) {
  left <- hcl$merge[i, 1L]
  if (left < 0L) {
  m[i, -left] <- 1L # negative values are observations
  } else {
  m[i, ] <- m[left, ] # positive values are child clusters
  }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  m[i, -right] <- 1L
  } else {
  m[i, ] <- m[i,] | m[right, ]
  }
 }
return(m)
}

## ===

Hcoords <- function(hcl) {
 nr <- as.integer(nrow(hcl$merge))
 p <- matrix(c(rep(0L, nr), hcl$height), nrow=nr, ncol=2, byrow=FALSE,
  dimnames=list(c(), c("x", "y")))
 o <- order(hcl$order)
 tmp <- double(2)
 for (i in seq_len(nr)) {
  left <- hcl$merge[i, 1L]
  if (left < 0L) {
   tmp[1L] <- o[-left] # negative values are observations
  } else {
  tmp[1L] <- p[left, 1L] # positive values are child clusters
 }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  tmp[2L] <- o[-right]
  } else {
  tmp[2L] <- p[right, 1L]
  }
 p[i, 1L] <- mean(tmp)
 }
 return(p)
}


## ===

Bclabels <- function(hcl, values, coords=NULL, horiz=FALSE, method="text",
 threshold=NULL, top=NULL, percent=FALSE, ...) {
if (is.null(coords)) coords <- Hcoords(hcl)
if (horiz) coords[, 2:1] <- coords
if (method == "text") {
 if (percent) values <- round(values*100)
 if (!is.null(threshold)) values[values < threshold] <- NA
 if (percent) values[!is.na(values)] <- paste0(values[!is.na(values)], "%")
 if (!is.null(top)) values[1:(length(values) - top)] <- NA
 text(coords, labels=values, ...)
 }
if (method == "points") {
 if (!is.null(threshold)) coords <- coords[values >= threshold, ]
 if (!is.null(top)) coords <- coords[1:(length(values) - top), ]
 points(coords, ...)
 }
invisible(list(coords=coords, labels=values))
}

# ===

plot.Bclust <- function (x, main="", xlab=NULL, ...) {
 if (is.null(xlab)) xlab <- paste(x$meth, ", ", x$iter, " replicates", sep = "")
 plot(x$hclust, main=main, xlab=xlab, ...)
 Bclabels(x$hclust, x$values, percent=TRUE, pos=3, offset=0.1)
}
