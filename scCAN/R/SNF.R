SNF <- function(Wall,K=20,t=20) {
    check_wall_names <- function(Wall) {
        name_match <- function(names_A, names_B) {
            return(identical(dimnames(names_A), dimnames(names_B)))
        }
        return(all(unlist(lapply(Wall, FUN = name_match, Wall[[1]]))))
    }
    wall.name.check <- check_wall_names(Wall)
    wall.names <- dimnames(Wall[[1]])
    if (!wall.name.check) {
        warning("Dim names not consistent across all matrices in Wall.\n            Returned matrix will have no dim names.")
    }
    LW <- length(Wall)
    normalize <- function(X) {
        row.sum.mdiag <- rowSums(X) - diag(X)
        row.sum.mdiag[row.sum.mdiag == 0] <- 1
        X <- X/(2 * (row.sum.mdiag))
        diag(X) <- 0.5
        return(X)
    }
    newW <- vector("list", LW)
    nextW <- vector("list", LW)
    for (i in 1:LW) {
        Wall[[i]] <- normalize(Wall[[i]])
        Wall[[i]] <- (Wall[[i]] + t(Wall[[i]]))/2
    }
    for (i in 1:LW) {
        newW[[i]] <- (.dominateset(Wall[[i]], K))
    }
    for (i in 1:t) {
        for (j in 1:LW) {
            sumWJ <- matrix(0, dim(Wall[[j]])[1], dim(Wall[[j]])[2])
            for (k in 1:LW) {
                if (k != j) {
                  sumWJ <- sumWJ + Wall[[k]]
                }
            }
            nextW[[j]] <- newW[[j]] %*% (sumWJ/(LW - 1)) %*% 
                t(newW[[j]])
        }
        for (j in 1:LW) {
            Wall[[j]] <- normalize(nextW[[j]])
            Wall[[j]] <- (Wall[[j]] + t(Wall[[j]]))/2
        }
    }
    W <- matrix(0, nrow(Wall[[1]]), ncol(Wall[[1]]))
    for (i in 1:LW) {
        W <- W + Wall[[i]]
    }
    W <- W/LW
    W <- normalize(W)
    W <- (W + t(W))/2
    if (wall.name.check) {
        dimnames(W) <- wall.names
    }
    return(W)
  }
