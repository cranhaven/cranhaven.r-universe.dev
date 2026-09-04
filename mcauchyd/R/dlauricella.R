#' @importFrom data.table rbindlist

dlauricella <- function(nu1, nu2, lambda, eps = 1e-6) {
  
  p <- length(lambda)
  
  buildMlist <- function(j, isupp, k, k1, p = p) {
    jsupp <- isupp[, j]
    Ml <- rep(list(0:k), p)
    for (j in jsupp)
      Ml[[j]] <- k1
    return(expand.grid(Ml))
  }
  
  buildxlist <- function(j, isupp, x, xsupp, p = p) {
    jsupp <- isupp[, j]
    xl <- rep(list(x), p)
    xl[jsupp] <- xsupp[jsupp]
    xl[-jsupp] <- x[-jsupp]
    return(expand.grid(xl))
  }
  
  prodlambda <- prod(lambda)
  
  lambdanu <- lambda*nu1/nu2
  prodlambdanu <- prod(lambdanu)
  
  k <- 5
  
  # M: data.frame of the indices for the nested sums
  # (i.e. all arrangements of n elements from {0:k})
  M <- expand.grid(rep(list(0:k), p))
  M <- M[-1, , drop = FALSE]
  
  Munique <- 0:k
  
  # Sum of the indices
  Msum <- rowSums(M)
  # Msumunique <- 0:max(Msum)
  
  kstep <- 5
  
  if (lambdanu[p] <= 1) {  # lambda[1] < ... < lambda[p] < 1
    
    if (p > 1 & lambda[p] == 1) {
      lambda <- lambda[1:(p-1)]
      p1 <- p-1
      M <- expand.grid(rep(list(Munique), p1))
      M <- M[-1, , drop = FALSE]
      # Sum of the indices
      Msum <- rowSums(M)
    } else {
      p1 <- p
    }
    
    Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
    matpoch <- matrix(rep(Mpoch, p1), ncol = p1, byrow = FALSE)
    matM <- matrix(rep(Munique, p1), ncol = p1, byrow = FALSE)
    matlabmda <- matrix(rep(log(1 - lambdanu[1:p1]), length(Munique)), ncol = p1, byrow = TRUE)
    lambdaM <- matlabmda*matM
    matfact <- matrix(rep(lfactorial(Munique), p1), ncol = p1, byrow = FALSE)
    
    # matcommun  <- exp(matpoch + lambdaM - matfact)
    matcommun  <- as.data.frame(matpoch + lambdaM - matfact)
    # matcommun <- as.data.frame(apply(matcommun, 2, Re))
    matcommunsupp <- as.data.frame(matrix(nrow = 0, ncol = ncol(matcommun)))
    colnames(matcommunsupp) <- colnames(matcommun)
    
    gridcommun <- expand.grid(matcommun)
    gridcommun <- gridcommun[-1, , drop = FALSE]
    # commun <- apply(gridcommun, 1, prod)
    commun <- rowSums(gridcommun)
    
    # d <- sum(
    #   commun * sapply(Msum, function(i) {
    #     pochhammer(1, i) / ( pochhammer((nu1 + p)/2, i) * i )
    #   })
    # )
    d <- Re(sum(exp(
      commun + sapply(Msum, function(i) {
        lnpochhammer(1, i) - ( lnpochhammer((nu1 + p)/2, i) + log(i) )
      })
    )))
    
    # Next elements of the sum, until the expected precision
    k1 <- 1:k
    derive <- 0
    while (abs(d) > eps/10 & !is.nan(d)) {
      epsret <- signif(abs(d), 1)*10
      k <- k1[length(k1)]
      k1 <- k + (1:kstep)
      derive <- derive + d
      
      # M: data.frame of the indices for the nested sums
      # M <- expand.grid(rep(list(k1), p1))
      # if (p1 > 1) {
      #   for (i in 1:(p1-1)) {
      #     indsupp <- combn(p1, i)
      #     for (j in 1:ncol(indsupp)) {
      #       jsupp <- indsupp[, j]
      #       Mlist <- vector("list", p1)
      #       for (l in jsupp) Mlist[[l]] <- k1
      #       for (l in (1:p1)[-jsupp]) Mlist[[l]] <- 0:k
      #       M <- rbind(M, expand.grid(Mlist))
      #     }
      #   }
      # }
      Mlist <- list(expand.grid(rep(list(k1), p1)))
      if (p1 == 1) {
        M <- Mlist[[1]]
      }
      if (p1 > 1) {
        for (i in 1:(p1-1)) {
          indsupp <- combn(p1, i)
          Mlist <- c(Mlist,
                     lapply(1:ncol(indsupp), buildMlist, isupp = indsupp, k = k, k1 = k1, p = p1))
        }
        M <- data.frame(rbindlist(Mlist))
      }
      
      # Sum of the indices
      Msum <- rowSums(M)
      
      Munique <- (max(Munique)+1):max(M)
      # Msumunique <- (max(Msumunique)+1):max(Msum)

      Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
      matpoch <- matrix(rep(Mpoch, p1), ncol = p1, byrow = FALSE)
      matM <- matrix(rep(Munique, p1), ncol = p1, byrow = FALSE)
      matlabmda <- matrix(rep(log(1 - lambdanu[1:p1]), length(Munique)), ncol = p1, byrow = TRUE)
      lambdaM <- matlabmda*matM
      matfact <- matrix(rep(lfactorial(Munique), p1), ncol = p1, byrow = FALSE)
      
      matcommun <- rbind(matcommun, matcommunsupp)
      # matcommunsupp  <- exp(matpoch + lambdaM - matfact)
      matcommunsupp  <- as.data.frame(matpoch + lambdaM - matfact)
      # matcommunsupp <- as.data.frame(apply(matcommunsupp, 2, Re))
      colnames(matcommunsupp) <- colnames(matcommun)
      
      communlist <- list(expand.grid(matcommunsupp))
      if (p1 == 1) {
        gridcommun <- communlist[[1]]
      }
      if (p1 > 1) {
        for (i in 1:(p1-1)) {
          indsupp <- combn(p1, i)
          communlist <- c(communlist,
                          lapply(1:ncol(indsupp), buildxlist, isupp = indsupp, x = matcommun, xsupp = matcommunsupp, p = p1))
        }
        names(communlist[[1]]) <- names(communlist[[2]])
        gridcommun <- data.frame(rbindlist(communlist))
      }
      # commun <- apply(gridcommun, 1, prod)
      commun <- rowSums(gridcommun)
      
      # d <- sum(
      #   commun * sapply(Msum, function(i) {
      #     pochhammer(1, i) / ( pochhammer((nu1 + p)/2, i) * i )
      #   })
      # )
      d <- Re(sum(exp(
        commun + sapply(Msum, function(i) {
          lnpochhammer(1, i) - ( lnpochhammer((nu1 + p)/2, i) + log(i) )
        })
      )))
      
    }
    
    derive <- as.numeric(-0.5 * log(prodlambda) - (nu2 + p)/2 * derive)
    
  } else if (lambdanu[1] > 1) { # 1 < lambda[1] < ... < lambda[p]
    
    Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
    matpoch <- matrix(rep(Mpoch, p), ncol = p, byrow = FALSE)
    matM <- matrix(rep(Munique, p), ncol = p, byrow = FALSE)
    matlabmda <- matrix(rep(log(1 - 1/lambdanu), length(Munique)), ncol = p, byrow = TRUE)
    lambdaM <- matlabmda*matM
    matfact <- matrix(rep(lfactorial(Munique), p), ncol = p, byrow = FALSE)
    
    # matcommun  <- exp(matpoch + lambdaM - matfact)
    matcommun  <- as.data.frame(matpoch + lambdaM - matfact)
    # matcommun <- as.data.frame(apply(matcommun, 2, Re))
    matcommunsupp <- as.data.frame(matrix(nrow = 0, ncol = ncol(matcommun)))
    colnames(matcommunsupp) <- colnames(matcommun)
    
    gridcommun <- expand.grid(matcommun)
    gridcommun <- gridcommun[-1, , drop = FALSE]
    # commun <- apply(gridcommun, 1, prod)
    commun <- rowSums(gridcommun)
    
    d <- 0
    for (i in 1:length(Msum)) {
      A <- sum(1/(0:(Msum[i]-1) + (nu1+p)/2))
      d <- d - exp(commun[i]) * A
    }
    d <- Re(d)
    
    # Next elements of the sum, until the expected precision
    k1 <- 1:k
    derive <- 0
    # vd <- vderive <- numeric()
    while (abs(d) > eps/10 & !is.nan(d)) {
      epsret <- signif(abs(d), 1)*10
      k <- k1[length(k1)]
      k1 <- k + (1:kstep)
      derive <- derive + d
      # vd <- c(vd, d); vderive <- c(vderive, derive)
      
      # # M: data.frame of the indices for the nested sums
      # M <- as.data.frame(matrix(nrow = 0, ncol = p))
      # if (p > 1) {
      #   for (i in 1:(p-1)) {
      #     Mlist <- c( rep(list(0:k), p-i), rep(list(k1), i) )
      #     M <- rbind( M, expand.grid(Mlist) )
      #     for (j in 1:(p-1)) {
      #       Mlist <- Mlist[c(p, 1:(p-1))]
      #       M <- rbind(M, expand.grid(Mlist))
      #     }
      #   }
      # }
      # M <- rbind( M, expand.grid(rep(list(k1), p)) )
      
      # M: data.frame of the indices for the nested sums
      # M <- expand.grid(rep(list(k1), p))
      # if (p > 1) {
      #   for (i in 1:(p-1)) {
      #     indsupp <- combn(p, i)
      #     for (j in 1:ncol(indsupp)) {
      #       jsupp <- indsupp[, j]
      #       Mlist <- vector("list", p)
      #       for (l in jsupp) Mlist[[l]] <- k1
      #       for (l in (1:p)[-jsupp]) Mlist[[l]] <- 0:k
      #       M <- rbind(M, expand.grid(Mlist))
      #     }
      #   }
      # }
      Mlist <- list(expand.grid(rep(list(k1), p)))
      if (p == 1) {
        M <- Mlist[[1]]
      }
      if (p > 1) {
        for (i in 1:(p-1)) {
          indsupp <- combn(p, i)
          Mlist <- c(Mlist,
                     lapply(1:ncol(indsupp), buildMlist, isupp = indsupp, k = k, k1 = k1, p = p))
        }
        M <- data.frame(rbindlist(Mlist))
      }
      
      Msum <- rowSums(M)
      
      Munique <- (max(Munique) + 1):max(M)
      # Msumunique <- (max(Msumunique) + 1):max(Msum)
      
      # d <- 0
      # for (i in 1:length(Msum)) {
      #   commun <- prod(
      #     sapply(1:p, function(j) {
      #       pochhammer(0.5, M[i, j])*(1 - 1/lambda[j])^M[i, j]/factorial(M[i, j])
      #     })
      #   )
      #   A <- sum(1/(0:(Msum[i]-1) + (1+p)/2))
      #   d <- d - commun * A # / pochhammer((1 + p)/2, Msum[i])
      # }
      
      Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
      matpoch <- matrix(rep(Mpoch, p), ncol = p, byrow = FALSE)
      matM <- matrix(rep(Munique, p), ncol = p, byrow = FALSE)
      matlabmda <- matrix(rep(log(1 - 1/lambdanu), length(Munique)), ncol = p, byrow = TRUE)
      lambdaM <- matlabmda*matM
      matfact <- matrix(rep(lfactorial(Munique), p), ncol = p, byrow = FALSE)
      
      matcommun <- rbind(matcommun, matcommunsupp)
      # matcommunsupp  <- exp(matpoch + lambdaM - matfact)
      matcommunsupp  <- as.data.frame(matpoch + lambdaM - matfact)
      # matcommunsupp <- as.data.frame(apply(matcommunsupp, 2, Re))
      colnames(matcommunsupp) <- colnames(matcommun)
      
      communlist <- list(expand.grid(matcommunsupp))
      if (p == 1) {
        gridcommun <- communlist[[1]]
      }
      if (p > 1) {
        for (i in 1:(p-1)) {
          indsupp <- combn(p, i)
          communlist <- c(communlist,
                          lapply(1:ncol(indsupp), buildxlist, isupp = indsupp, x = matcommun, xsupp = matcommunsupp, p = p))
        }
        names(communlist[[1]]) <- names(communlist[[2]])
        gridcommun <- data.frame(rbindlist(communlist))
      }
      # commun <- apply(gridcommun, 1, prod)
      commun <- rowSums(gridcommun)
      
      # d <- 0
      # for (i in 1:length(Msum)) {
      #   A <- sum(1/(0:(Msum[i]-1) + (1+p)/2))
      #   d <- d - commun[i] * A
      # }
      A <- sapply(Msum, function (i) sum(1/(0:(i-1) + (nu1+p)/2)))
      
      d <- Re(-sum(exp(commun)*A))
      
    }
    
    derive <- as.numeric(-0.5 * log(prodlambda) - (nu2 + p)/2 * prod(1/sqrt(lambdanu)) * derive)
    
  } else { # lambda[1] < ... < 1 < ... < lambda[p]
    
    Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
    matpoch <- cbind(matrix(rep(Mpoch, p-1), ncol = p-1, byrow = FALSE), 0)
    matM <- matrix(rep(Munique, p), ncol = p, byrow = FALSE)
    matlabmda <- matrix(rep(log(1 - c(lambdanu[-p], 1)/lambdanu[p]), length(Munique)), ncol = p, byrow = TRUE)
    lambdaM <- matlabmda*matM
    matfact <- matrix(rep(lfactorial(Munique), p), ncol = p, byrow = FALSE)
    
    # matcommun  <- exp(matpoch + lambdaM - matfact)
    matcommun  <- as.data.frame(matpoch + lambdaM - matfact)
    # matcommun <- as.data.frame(apply(matcommun, 2, Re))
    matcommunsupp <- as.data.frame(matrix(nrow = 0, ncol = ncol(matcommun)))
    colnames(matcommunsupp) <- colnames(matcommun)
    
    gridcommun <- expand.grid(matcommun)
    gridcommun <- gridcommun[-1, , drop = FALSE]
    # commun <- apply(gridcommun, 1, prod)
    commun <- rowSums(gridcommun)
    
    # d <- sum(
    #   commun * sapply(1:length(Msum), function(i) {
    #     pochhammer(0.5, M[i, p]) * pochhammer(1, Msum[i]) / ( pochhammer((nu1 + p)/2, Msum[i]) * Msum[i] )
    #   })
    # )
    d <- Re(sum(exp(
      commun + sapply(1:length(Msum), function(i) {
        lnpochhammer(0.5, M[i, p]) + lnpochhammer(1, Msum[i]) -
          ( lnpochhammer((nu1 + p)/2, Msum[i]) + log(Msum[i]) )
      })
    )))
    
    # Next elements of the sum, until the expected precision
    k1 <- 1:k
    derive <- 0
    while (abs(d) > eps/10 & !is.nan(d)) {
      epsret <- signif(abs(d), 1)*10
      k <- k1[length(k1)]
      k1 <- k + (1:kstep)
      derive <- derive + d
      
      # # M: data.frame of the indices for the nested sums
      # M <- as.data.frame(matrix(nrow = 0, ncol = p))
      # if (p > 1) {
      #   for (i in 1:(p-1)) {
      #     Mlist <- c( rep(list(0:k), p-i), rep(list(k1), i) )
      #     M <- rbind( M, expand.grid(Mlist) )
      #     for (j in 1:(p-1)) {
      #       Mlist <- Mlist[c(p, 1:(p-1))]
      #       M <- rbind(M, expand.grid(Mlist))
      #     }
      #   }
      # }
      # M <- rbind( M, expand.grid(rep(list(k1), p)) )
      
      # M <- expand.grid(rep(list(k1), p))
      # if (p > 1) {
      #   for (i in 1:(p-1)) {
      #     indsupp <- combn(p, i)
      #     for (j in 1:ncol(indsupp)) {
      #       jsupp <- indsupp[, j]
      #       Mlist <- vector("list", p)
      #       for (l in jsupp) Mlist[[l]] <- k1
      #       for (l in (1:p)[-jsupp]) Mlist[[l]] <- 0:k
      #       M <- rbind(M, expand.grid(Mlist))
      #     }
      #   }
      # }
      
      # M: data.frame of the indices for the nested sums
      Mlist <- list(expand.grid(rep(list(k1), p)))
      if (p == 1) {
        M <- Mlist[[1]]
      }
      if (p > 1) {
        for (i in 1:(p-1)) {
          indsupp <- combn(p, i)
          Mlist <- c(Mlist,
                     lapply(1:ncol(indsupp), buildMlist, isupp = indsupp, k = k, k1 = k1, p = p))
        }
        M <- data.frame(rbindlist(Mlist))
      }
      
      Msum <- rowSums(M)
      
      Munique <- (max(Munique)+1):max(M)
      # Msumunique <- (max(Msumunique)+1):max(Msum)
      
      # d <- 0
      # for (i in 1:length(Msum)) {
      #   commun <- prod(
      #     sapply(1:(p-1), function(j) {
      #       pochhammer(0.5, M[i, j])*(1 - lambda[j]/lambda[p])^M[i, j]/factorial(M[i, j])
      #     })
      #   )
      #   commun <- commun*(1 - 1/lambda[p])^M[i, p]/factorial(M[i, p])
      #   d <- d + commun * pochhammer(0.5, M[i, p])*pochhammer(1, Msum[i]) / ( pochhammer((1 + p)/2, Msum[i]) * Msum[i] )
      # }
      
      Mpoch <- sapply(Munique, function(j) lnpochhammer(0.5, j))
      matpoch <- cbind(matrix(rep(Mpoch, p-1), ncol = p-1, byrow = FALSE), 0)
      matM <- matrix(rep(Munique, p), ncol = p, byrow = FALSE)
      matlabmda <- matrix(rep(log(1 - c(lambdanu[-p], 1)/lambdanu[p]), length(Munique)), ncol = p, byrow = TRUE)
      lambdaM <- matlabmda*matM
      matfact <- matrix(rep(lfactorial(Munique), p), ncol = p, byrow = FALSE)
      
      matcommun <- rbind(matcommun, matcommunsupp)
      # matcommunsupp  <- exp(matpoch + lambdaM - matfact)
      matcommunsupp  <- as.data.frame(matpoch + lambdaM - matfact)
      # matcommunsupp <- as.data.frame(apply(matcommunsupp, 2, Re))
      colnames(matcommunsupp) <- colnames(matcommun)
      
      communlist <- list(expand.grid(matcommunsupp))
      if (p == 1) {
        gridcommun <- communlist[[1]]
      }
      if (p > 1) {
        for (i in 1:(p-1)) {
          indsupp <- combn(p, i)
          communlist <- c(communlist,
                          lapply(1:ncol(indsupp), buildxlist, isupp = indsupp, x = matcommun, xsupp = matcommunsupp, p = p))
        }
        names(communlist[[1]]) <- names(communlist[[2]])
        gridcommun <- data.frame(rbindlist(communlist))
      }
      commun <- apply(gridcommun, 1, prod)
      commun <- rowSums(gridcommun)
      
      # d <- sum(
      #   commun * sapply(1:length(Msum), function(i) {
      #     pochhammer(0.5, M[i, p]) * pochhammer(1, Msum[i]) / ( pochhammer((nu1 + p)/2, Msum[i]) * Msum[i] )
      #   })
      # )
      d <- Re(sum(exp(
        commun + sapply(1:length(Msum), function(i) {
          lnpochhammer(0.5, M[i, p]) + lnpochhammer(1, Msum[i]) -
            ( lnpochhammer((nu1 + p)/2, Msum[i]) + log(Msum[i]) )
        })
      )))
      
    }
    
    derive <- as.numeric(-0.5 * log(prodlambda) + (nu2 + p)/2 * (log(lambdanu[p]) - derive))
    
  }
  
  attr(derive, "epsilon") <- eps
  attr(derive, "k") <- k
  
  return(derive)
}
