# ========
# Heeringa-style coloring
# mapping dimensions of MDS onto RG
# ========

heeringa <- function(dist, power = 0.5, mapping = c(1,2,3), method = "eigs", center = NULL) {

  # check mapping
  if (length(mapping) != 3
      | sum(mapping > 3) > 0
      | sum(mapping < -3) > 0) {
    stop("mapping is not correct")
  }

  # check format of dist
  if (class(dist)[1] == "dist") {
    dist <- as.matrix(dist)
  }

  if (method == "mds") {
    mds <- cmdscale(dist, k = 3)
  } else if (method == "eigs") {
    e <- RSpectra::eigs(max(dist)-dist, 4)
    d <- diag(e$values[2:4])^0.5
    mds <- e$vectors[,2:4] %*% d
  }

  if (is.character(center)) {
    center <- which(rownames(dist) ==  center)[1]
  }

  norm <- function(x){
    if (!is.null(center)) {
      x <- x - x[center]
    }
    x <- abs(x)^power * sign(x)
    x[x>0] <- x[x>0]/max(x, na.rm = TRUE)
    x[x<0] <- x[x<0]/(-min(x, na.rm = TRUE))
    (x+1)/2
  }
  mds <- apply(mds, 2, norm)

  # user-defined mapping
  mds <- mds[,abs(mapping)]
  mds <- t(t(mds) * sign(mapping))
  mds[mds<0] <- 1+mds[mds<0]

  return(rgb(mds))
}
