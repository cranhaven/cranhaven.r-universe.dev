## This is an internal function which compute Kronecker product for PIC method
kpr <- function(bl, tr) {
  wa <- combn(bl, 2)
  wb <- combn(tr, 2)
  cb <- matrix(0, nrow = choose(bl, 2), ncol = bl)
  ct <- matrix(0, nrow = choose(tr, 2), ncol = tr)
  for (i in 1:choose(bl, 2)) {
    cb[i, wa[1, i]] <- 1
    cb[i, wa[2, i]] <- -1
  }
  for (i in 1:choose(tr, 2)) {
    ct[i, wb[1, i]] <- 1
    ct[i, wb[2, i]] <- -1
  }
  c <- kronecker(cb, ct)
  return(c)
}

#' @importFrom mvtnorm pmvnorm
comb <- function(pvalues) {
  P <- pvalues
  P[P == 0] <- 10^(-6)
  P[P == 1] <- 1 - 10^(-6)
  k <- length(P)
  Te <- qnorm(P)
  r <- 1 - var(Te)
  rohat <- max(-1 / (k - 1), r)
  j <- matrix(1, nrow = k, ncol = k)
  i <- diag(k)
  S <- (1 - rohat) * i + rohat * j
  minp <- min(P)
  m <- qnorm(minp)
  GC <- 1 - pmvnorm(lower = rep(m, k), upper = Inf, sigma = S)
  q0 <- max(1 / minp - 1, 1)
  q <- min(q0, (k - 1))
  Bon <- min(minp * k, 1)
  jacobi <- 1 - (1 - minp)^q # minpv~Betha(1,q)
  Sidak <- 1 - (1 - minp)^k # minpv~Betha(1,k)
  list(Bon = Bon, Sidak = Sidak, jacobi = jacobi, GC = GC) # GC=Goussian Copula=MVNormal
}

#' @importFrom utils combn
Result_KKSA <- function(x, nsim, alpha, simu) {
  bl <- nrow(x)
  tr <- ncol(x)
  if (bl < 4) {
    str <- paste0("This test is not applicable when the row number is less than four. You may use the transpose of the data matrix if the number of column is greater than three.", "\n")
    index <- 1:bl
  } else {
    qKKSA <- quantile(simu, prob = alpha, names = FALSE)
    Nrow <- 2:(as.integer(bl / 2))
    R <- x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x)
    sse <- sum(R^2)
    count <- 0
    Minpvalue <- 2
    f2 <- rep(0, 0)
    for (i in Nrow) {
      ind <- combn(bl, i)
      Nsplit <- ncol(ind)
      if ((bl / 2) == i) Nsplit <- Nsplit / 2
      for (j in 1:Nsplit) {
        count <- count + 1
        x1 <- x[ind[, j], ]
        x2 <- x[-ind[, j], ]
        rss1 <- sum((x1 - matrix(rowMeans(x1), nrow(x1), ncol(x1)) - matrix(colMeans(x1), nrow(x1), ncol(x1), byrow = TRUE) + mean(x1))^2)
        rss2 <- sum((x2 - matrix(rowMeans(x2), nrow(x2), ncol(x2)) - matrix(colMeans(x2), nrow(x2), ncol(x2), byrow = TRUE) + mean(x2))^2)
        f2[count] <- (rss1 * (bl - i - 1)) / (rss2 * (i - 1))
        if (f2[count] < 1) f2[count] <- 1 / f2[count]
        ex1 <- 1 - pf(f2[count], (tr - 1) * (i - 1), (bl - i - 1) * (tr - 1)) + pf(1 / f2[count], (tr - 1) * (i - 1), (bl - i - 1) * (tr - 1))
        if (ex1 < Minpvalue) {
          Minpvalue <- ex1
          index <- ind[, j]
          RSS1 <- rss1
          RSS2 <- rss2
          df1 <- (tr - 1) * (i - 1)
          df2 <- (tr - 1) * (bl - i - 1)
          fvalue <- f2[count]
        }
      }
    }
    str1 <- paste0("There exists a significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The magnitude of interaction effects is heteroscedastic across the sub-tables of observations.")
    expre1 <- paste0(index, collapse = ", ")
    expre2 <- paste0((1:bl)[-index], collapse = ", ")
    str2 <- paste0("The first sub-table consists of rows ", expre1, " with RSS=", round(RSS1, 4), " on ", df1, " degrees of freedoms.")
    str3 <- paste0("The second sub-table consists of rows ", expre2, " with RSS=", round(RSS2, 4), " on ", df2, " degrees of freedoms.")
    str4 <- paste0("The estimated critical value of the KKSA_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qKKSA, 4), ".")
    str <- paste(str1, str2, str3, str4)
  }
  list(string = str, index = index)
}



Result_Piepho <- function(x, nsim, alpha, simu) {
  bl <- nrow(x)
  tr <- ncol(x)
  if (bl < 3) {
    str <- paste0("This test is not applicable when the row number is less than three. You may use the transpose of the data matrix if the number of column is greater than two.", "\n")
  }
  if (any(is.nan(simu))) {
    str <- paste("This test is not applicable", "\n")
  }
  if (bl >= 3 & !any(is.nan(simu))) {
    qPiepho <- quantile(simu, prob = 1 - alpha, names = FALSE)
    R <- x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x)
    W <- rowSums(R^2)
    sigmahat <- (bl * (bl - 1) * W - sum(W)) / ((bl - 1) * (bl - 2) * (tr - 1))
    str1 <- paste0("There exists a significant interaction at the ", paste0(100 * (alpha), "%"), " level.")
    str2 <- paste0("The Grubbs' estimtors of the row variances are heterogeneous")
    Grubbs <- paste(round(sigmahat, 4), collapse = ", ")
    str3 <- paste0("and they are: ", Grubbs, ".")
    str4 <- paste0("The estimated critical value of the Piepho_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qPiepho, 4), ".")
    str <- paste(str1, str2, str3, str4)
  }
  str
}



Result_Malik <- function(x, simu, alpha, nsim) {
  bl <- nrow(x)
  tr <- ncol(x)
  qMalik <- quantile(simu, prob = 1 - alpha, names = FALSE)
  R <- x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x)
  R <- round(R, 5)
  Min <- min(R)
  Max <- max(R)
  Index_Min <- which(R == Min)
  Index_Max <- which(R == Max)
  pmax <- Index_Max %/% bl
  rmax <- Index_Max %% bl
  pmin <- Index_Min %/% bl
  rmin <- Index_Min %% bl
  cellmax <- matrix(0, nrow = length(pmax), ncol = 2)
  cellmin <- matrix(0, nrow = length(pmin), ncol = 2)
  for (i in 1:length(pmax)) {
    if (rmax[i] == 0) {
      cellmax[i, ] <- c(bl, pmax[i])
    } else {
      cellmax[i, ] <- c(rmax[i], pmax[i] + 1)
    }
  }
  for (i in 1:length(pmin)) {
    if (rmin[i] == 0) {
      cellmin[i, ] <- c(bl, pmin[i])
    } else {
      cellmin[i, ] <- c(rmin[i], pmin[i] + 1)
    }
  }
  str1 <- paste0("There exists a significant interaction at the ", paste0(100 * (alpha), "%"), " level.", "The significant interaction might due to the some outliers in residuals; some cells produce large negative or positive residuals:", "\n")
  str2 <- paste0("The cell with row=", cellmin[1, 1], " and column=", cellmin[1, 2], " produces a large negative residual", "\n")
  if (length(pmin) > 2) {
    for (i in 2:length(pmin)) {
      str2 <- paste0(str2, "The cell with row=", cellmin[i, 1], " and column=", cellmin[i, 2], " produces a large negative residual", "\n")
    }
  }
  str3 <- paste0("The cell with row=", cellmax[1, 1], " and column=", cellmax[1, 2], " produces a large positive residual", "\n")
  if (length(pmax) > 2) {
    for (i in 2:length(pmax)) {
      str3 <- paste0(str3, "The cell with row=", cellmax[i, 1], " and column=", cellmax[i, 2], " produces a large positive residual", "\n")
    }
  }
  str4 <- paste0("The estimated critical value of the Malik_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qMalik, 4), ".")
  str <- paste(str1, str2, str3, str4)
  return(str)
}

Result_KKM <- function(x, simu, nsim, alpha, nc0) {
  if (requireNamespace("MASS", quietly = TRUE)) {
    if (requireNamespace("Matrix", quietly = TRUE)) {
      qKKM <- quantile(simu, prob = 1 - alpha, names = FALSE)
      bl <- nrow(x)
      tr <- ncol(x)
      n <- bl * tr
      kp <- kpr(bl, tr)
      c0 <- C0(kp, n, nc0)
      y <- c(t(x))
      Z <- abs(kp %*% y)
      S0 <- median(Z) / c0
      PSE <- median(Z[Z <= 5 * S0])
      SZ <- Z[Z > qKKM * PSE]
      Index <- (1:nrow(kp))[Z > (qKKM * PSE)]
      if (length(Index) != 0) {
        M <- matrix(0, length(Index), 4)
        count <- 0
        for (k in Index) {
          count <- count + 1
          count2 <- 0
          for (i in 1:tr) {
            for (j in 1:bl) {
              jj <- (i - 1) * bl + j
              if (kp[k, jj] != 0) {
                count2 <- count2 + 1
                M[count, count2] <- paste0(j, i)
              }
            }
          }
        }
        C1 <- kp[Index, ]
        sigma2hat <- t(y) %*% (MASS::ginv(C1) %*% C1) %*% y / Matrix::rankMatrix(C1)[1]
        str1 <- paste0("There exists a significant interaction at the ", paste0(100 * (alpha), "%"), " level and it might be caused by some cells.", " The absolute estimates of the significant pairwise interaction contrasts (PIC) and the corresponding involved cell means are:", "\n")
        ex1 <- paste(paste0("|mu_{", M[1, 1], "}-mu_{", M[1, 2], "}-mu_{", M[1, 3], "}+mu_{", M[1, 4], "}|="), round(SZ[1], 4), "\n")
        for (i in 2:length(Index)) {
          ex1 <- paste(ex1, paste0("|mu_{", M[i, 1], "}-mu_{", M[i, 2], "}-mu_{", M[i, 3], "}+mu_{", M[i, 4], "}|="), round(SZ[i], 4), "\n")
        }
        str2 <- ex1
        str3 <- paste0("The variance estimate under the non-additivity assumption is ", round(sigma2hat, 4), " on ", Matrix::rankMatrix(C1)[1], " degrees of freedom.", " The estimated critical value of the KKM_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qKKM, 4), ".")
        str <- paste0(str1, str2, str3)
      } else {
        str <- paste0("The KKM_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the KKM_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qKKM, 4), ".")
      }
      return(str)
    }
  }
}





#' @importFrom stats qbeta
Result_Boik <- function(x, nsim, alpha, simu) {
  bl <- nrow(x)
  tr <- ncol(x)
  n <- tr * bl
  p <- min(tr - 1, bl - 1)
  q <- max(tr - 1, bl - 1)
  df <- (p + 2) * (p - 1) / 2
  if (p == 1) {
    boik_p <- 1
    qBoik <- 1
    str3 <- paste0("The exact critical value of the Boik_test is: ", 1, ".")
  }
  if (p > 2) {
    qBoik <- quantile(simu, prob = alpha, names = FALSE)
    str3 <- paste0("The estimated critical value of the Boik_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qBoik, 4), ".")
  }
  if (p == 2) {
    qBoik <- qbeta(1 - alpha, 1, (q - 1) / 2)
    qBoik <- 1 / (qBoik + 1)
    str3 <- paste0("The exact critical value of the Boik_test at the ", paste0(100 * (alpha), "%"), " is ", round(qBoik, 4), ".")
  }
  R <- x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x)
  EV <- round(eigen(R %*% t(R))$values, 4)
  str1 <- paste0("There exists a significant multiplicative form of interaction at the ", paste0(100 * (alpha), "%"), " level.")
  expre <- paste(as.character(EV), collapse = ", ")
  str2 <- paste0("The eigen values of the RR' matrix are: ", expre, ".")
  str3 <- str3
  str <- paste(str1, str2, str3)
  return(str)
}




#' @importFrom utils combn
Result_Franck <- function(x, nsim, alpha, simu) {
  tr <- ncol(x)
  bl <- nrow(x)
  if (bl < 3) {
    str <- paste("This test is not applicable when the row number is less than three. You may use the transpose of the data matrix if the number of column is greater than two.", "\n")
    sb1 <- 1:bl
  } else {
    qFranck <- quantile(simu, prob = 1 - alpha, names = FALSE)
    cc <- 2^(bl - 1) - 1
    Nrow <- 2:(as.integer(bl / 2))
    sse <- sum((x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x))^2)
    fvalues <- rep(0, cc)
    count <- 0
    maxfv <- 0
    for (i in Nrow) {
      ind <- combn(bl, i)
      Nsplit <- ncol(ind)
      if (bl / 2 == i) Nsplit <- Nsplit / 2
      for (j in 1:Nsplit) {
        count <- count + 1
        x1 <- x[ind[, j], ]
        if (length(ind[, j]) == 1) {
          x1 <- matrix(x1, 1, ncol(x))
        }
        x2 <- x[(1:bl)[-ind[, j]], ]
        if (length((1:bl)[-ind[, j]]) == 1) {
          x2 <- matrix(x2, 1, ncol(x))
        }
        rss1 <- sum((x1 - matrix(rowMeans(x1), nrow(x1), ncol(x1)) - matrix(colMeans(x1), nrow(x1), ncol(x1), byrow = TRUE) + mean(x1))^2)
        rss2 <- sum((x2 - matrix(rowMeans(x2), nrow(x2), ncol(x2)) - matrix(colMeans(x2), nrow(x2), ncol(x2), byrow = TRUE) + mean(x2))^2)
        sse7 <- rss1 + rss2
        fvalues[count] <- (sse - sse7) * (bl - 2) / sse7
        if (fvalues[count] > maxfv) {
          maxfv <- fvalues[count]
          sb1 <- ind[, j]
        }
      }
    }
    for (d in 1:bl) {
      count <- count + 1
      x3 <- x[-d, ]
      sse7 <- sum((x3 - matrix(rowMeans(x3), nrow(x3), ncol(x3)) - matrix(colMeans(x3), nrow(x3), ncol(x3), byrow = TRUE) + mean(x3))^2)
      fvalues[count] <- (sse - sse7) * (bl - 2) / sse7
      if (fvalues[count] > maxfv) {
        maxfv <- fvalues[count]
        sb1 <- c(1:bl)[-d]
      }
    }
    sb2 <- c(1:bl)[-sb1]
    str1 <- paste0("A significant hidden structure exists at the ", paste0(100 * (alpha), "%"), " level.")
    expre1 <- paste0((sb1), collapse = ", ")
    expre2 <- paste0((sb2), collapse = ", ")
    str2 <- paste0("The first group includes rows: ", expre1, ".")
    str3 <- paste0("The second group includes rows: ", expre2, ".")
    str4 <- paste0("The estimated critical value of the Franck_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qFranck, 4), ".")
    str <- paste(str1, str2, str3, str4)
  }
  return(list(string = str, index = sb1))
}
