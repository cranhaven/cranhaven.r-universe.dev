as.matrix.mrts <- function(x, ...) {
    attr(x, "S") <- NULL
    attr(x, "UZ") <- NULL
    attr(x, "Xu") <- NULL
    attr(x, "nconst") <- NULL
    attr(x, "BBBH") <- NULL
    attr(x, "class") <- NULL
    class(x) <- "matrix"
    return(x)
}

getEigen <- function(A) {
    obj <- getASCeigens(A)
    obj$value <- rev(obj$value)
    obj$vector <- obj$vector[, NCOL(A):1]
    obj
}

getHalf <- function(Fk, iDFk) {
    dec <- getEigen(t(Fk) %*% iDFk)
    dec$vector <- dec$vector
    sroot <- sqrt(pmax(dec$value, 0))
    sroot[sroot == 0] <- Inf
    sroot <- 1 / sroot
    dec$vector %*% (sroot * t(dec$vector))
}

getLikelihood <- function(Data, Fk, M, s, Depsilon) {
    logdet <- function(R, L, K) {
      spam::determinant(diag(1, K) + t(L) %*% solve(R) %*%
                          L, logarithm = TRUE)$modulus + spam::determinant(R,
                                                                           logarithm = TRUE
                          )$modulus
    }
    Data <- as.matrix(Data)
    O <- as.matrix(!is.na(Data))
    TT <- NCOL(Data)
    n2loglik <- sum(O) * log(2 * pi)
    R <- toSpMat(s * Depsilon)
    eg <- eigen(M)
    L <- Fk %*% eg$vector %*% diag(sqrt(pmax(eg$value, 0))) %*%
      t(eg$vector)
    K <- NCOL(Fk)
    for (tt in 1:TT) {
      zt <- Data[O[, tt], tt]
      Rt <- R[O[, tt], O[, tt]]
      Lt <- L[O[, tt], ]
      if (NCOL(Lt) == 1) {
        Lt <- t(Lt)
        n2loglik <- n2loglik + log(Rt + Lt %*% t(Lt))
      }
      else {
        n2loglik <- n2loglik + logdet(Rt, Lt, K) + sum(zt *
                                                         invCz(Rt, Lt, zt))
      }
    }
    return(n2loglik)
  }

ifElse <- function(cond, yes_out, no_out) {
    if (cond) {
      return(yes_out)
    } else {
      return(no_out)
    }
}

checkDiag <- function(X) {
  if (is(X, "numeric") & (length(X) == 1)) {
    return(TRUE)
  }
  if (is(X, "matrix")) {
    if (sum(abs(diag(diag(X)) - X)) < .Machine$double.eps) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  else {
    x <- diag.spam(diag.of.spam(X), NROW(X))
    return(identical(x, X))
  }
}

setNC <- function(z, loc, nlevel) {
    Dimension <- NCOL(loc)
    N <- nrow(z)
    a <- sum(2^(Dimension * (0:(nlevel - 1))))
    NCtest <- (N / a)^(1 / Dimension)
    return(round(max(4, NCtest)))
}

subKnot <- function(x, nknot, xrng = NULL, nsamp = 1) {
    x <- as.matrix(x)
    xdim <- dim(x)
    if (xdim[2] > 1) {
      for (kk in xdim[2]:1) x <- x[order(x[, kk]), ]
    }
    else {
      x <- as.matrix(sort(x))
    }
    if (is.null(xrng)) {
      if (xdim[2] > 1) {
        xrng <- apply(x, 2, range)
      }
      else {
        xrng <- matrix(range(x), 2, 1)
      }
    }
    mysamp <- function(zANDid) {
      z <- as.double(names(zANDid))
      if (length(z) == 1L) {
        z
      }
      else {
        set.seed(mean(zANDid))
        sample(z, size = min(nsamp, length(z)))
      }
    }
    rng <- sqrt(xrng[2, ] - xrng[1, ])
    rng[rng == 0] <- min(rng[rng > 0]) / 5
    rng <- rng * 10 / min(rng)
    nmbin <- round(exp(log(rng) * log(nknot) / sum(log(rng))))
    nmbin <- pmax(2, nmbin)
    while (prod(nmbin) < nknot) {
      nmbin[which.max(rng)] <- nmbin[which.max(rng)] +
        1
    }
    gvec <- matrix(1, xdim[1], 1)
    cnt <- 0
    while (length(unique(gvec)) < nknot) {
      nmbin <- nmbin + cnt
      gvec <- matrix(1, xdim[1], 1)
      kconst <- 1
      for (kk in 1:xdim[2]) {
        grp <- pmin(round((nmbin[kk] - 1) * ((x[, kk] - xrng[
          1,
          kk
          ]) / (xrng[2, kk] - xrng[1, kk]))), nmbin[kk] -
            1L)
        if (length(unique(grp)) < nmbin[kk]) {
          brk <- quantile(x[, kk], seq(0, 1, l = nmbin[kk] +
                                         1))
          brk[1] <- brk[1] - 0.1^8
          grp <- as.double(cut(x[, kk], brk))
        }
        gvec <- gvec + kconst * grp
        kconst <- kconst * nmbin[kk]
      }
      cnt <- cnt + 1
    }
    gvec <- as.factor(gvec)
    gid <- as.double(as.character(gvec))
    names(gid) <- 1:xdim[1]
    index <- unlist(tapply(gid, gvec, mysamp))
    if (xdim[2] > 1) {
      x[index, ]
    } else {
      x[index]
    }
}

systemRam <- function(os) {
    remove_white <- function(x) {
      gsub(
        "(^[[:space:]]+|[[:space:]]+$)",
        "", x
      )
    }
    toBytes <- function(value) {
      num <- as.numeric(value[1])
      units <- value[2]
      power <- match(units, c("kB", "MB", "GB", "TB"))
      if (!is.na(power)) {
        return(num * 1024^power)
      }
      power <- match(units, c(
        "Kilobytes", "Megabytes", "Gigabytes",
        "Terabytes"
      ))
      if (!is.na(power)) {
        return(num * 1024^power)
      }
      num
    }
    if (length(grep("^linux", os))) {
      cmd <- "awk '/MemTotal/ {print $2}' /proc/meminfo"
      ram <- system(cmd, intern = TRUE)
      ram <- as.numeric(ram) * 1024
    }
    else if (length(grep("^darwin", os))) {
      ram <- system("system_profiler -detailLevel mini | grep \"  Memory:\"",
                    intern = TRUE
      )[1]
      ram <- remove_white(ram)
      ram <- toBytes(unlist(strsplit(ram, " "))[2:3])
    }
    else if (length(grep("^solaris", os))) {
      cmd <- "prtconf | grep Memory"
      ram <- system(cmd, intern = TRUE)
      ram <- remove_white(ram)
      ram <- toBytes(unlist(strsplit(ram, " "))[3:4])
    }
    else {
      ram <- system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
      ram <- remove_white(ram)
      ram <- ram[nchar(ram) > 0]
      sum(as.numeric(ram))
    }
    as.double(ram)
}

toSpMat <- function(mat) {
    if (is(mat, "data.frame")) {
      mat <- as.matrix(mat)
    }
    if (is(mat, "matrix")) {
      if (length(mat) > 10^8) {
        warnings("Use sparse matrix as input instead; otherwise it could take a very long time!")
        db <- tempfile()
        NR <- NROW(mat)
        NC <- NCOL(mat)
        f <- fm.create(db, NR, NC)
        f[, 1:NCOL(mat)] <- mat
        j <- sapply(1:NC, function(j) which(f[, j] != 0))
        ridx <- unlist(j)
        k <- sapply(1:NR, function(k) rbind(k, which(f[k, ] != 0)))
        kk <- matrix(unlist(k), ncol = 2, byrow = T)
        cidx <- sort(kk[, 2])
        where <- (cidx - 1) * NR + ridx
        closeAndDeleteFiles(f)
      }
      else {
        where <- which(mat != 0)
        ridx <- row(mat)[where]
        cidx <- col(mat)[where]
      }
      nonzero <- mat[where]
      mat <- spam(0, nrow = NROW(mat), ncol = NCOL(mat))
      mat[ridx, cidx] <- nonzero
    }
    return(mat)
}

uniquecombs <- function(x) {
    unique(x)
}

ZinvC <- function(R, L, z) {
    K <- NCOL(L)
    iR <- solve(R)
    ZiR <- z %*% iR
    left <- ZiR %*% L %*% solve(diag(1, K) + t(L) %*% iR %*% L) %*%
      t(L)
    ZiR - left %*% iR
}

print.FRK <- function(x, ...) {
    attr(x, "pinfo") <- NULL
    if (!is.null(x$LKobj)) {
      x$LKobj <- x$LKobj$summary
    }
    out <- paste("a ", NROW(x$G), " by ", NCOL(x$G), " mrts matrix",
      sep = ""
    )
    print(out)
}

print.mrts <- function(x, ...) {
    if (NCOL(x) == 1) {
      out <- c(x)
    } else {
      out <- x[, 1:NCOL(x)]
    }
    print(out)
}

mkpd <- function(M) {
  v <- try(min(eigen(M, only.values = T)$value), silent = TRUE)
  if (is(v, "try-error")) {
    M <- (M + t(M)) / 2
    v <- min(eigen(M, only.values = T)$value)
  }
  if (v <= 0) {
    M <- M + diag(max(0, -v) + 0.1^7.5, NROW(M))
  }
  return(M)
}


extractLK <- function(obj, loc = NULL, w = NULL, pick = NULL) {
  out <- list()
  if (is.null(loc)) {
    loc <- ifElse(
      is.null(obj$LKinfo.MLE$x), obj$LKinfo.MLE$call["x"][[1]],
      obj$LKinfo.MLE$x
    )
  }
  phi <- LKrig.basis(loc, obj$LKinfo)
  Q <- LKrig.precision(obj$LKinfo)
  out$Q <- Q
  if (!is.null(w)) {
    out$weights <- w
  } else {
    out$weights <- obj$LKinfo.MLE$weights
  }
  w <- diag.spam(sqrt(out$weights))
  wX <- w %*% phi
  out$wX <- wX
  out$G <- t(wX) %*% wX + obj$lambda.MLE * Q
  out$lambda <- obj$lambda.MLE
  if (is.null(pick)) {
    pick <- 1:NROW(loc)
  }
  out$pick <- pick

  return(out)
}
