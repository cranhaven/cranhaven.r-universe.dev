#' Generalized fiducial inference for logistic regression
#'
#' @description Simulates the fiducial distribution of a logistic regression
#'   model.
#'
#' @param formula formula describing the model
#' @param data dataframe containing the variables in the model
#' @param N number of fiducial simulations
#' @param thresh threshold criterion for the alteration; expert usage only
#' @param progress whether to print messages showing the progress of the
#'   algorithm
#' @param gmp whether to use exact arithmetic in the algorithm (experimental)
#' @param ufactr,vfactr these are control parameters of an optimization
#'   performed in the algorithm; these parameters should not be changed except
#'   if you encounter some messages about convergence issues
#'
#' @return A list with two fields: \code{Beta}, the fiducial simulations of
#'   the parameters, and \code{Weights}, their weight.
#' @export
#'
#' @importFrom lazyeval f_eval_lhs
#' @importFrom rcdd scdd q2d d2q makeH addHin makeV
#' @importFrom EigenR Eigen_rank Eigen_range
#' @importFrom stats model.matrix rlogis rmultinom
#'
#' @examples y <- c(0, 0, 1, 1, 1)
#' x <- c(-2, -1, 0, 1, 2)
#' gf <- gfilogisreg(y ~ x, N = 400) # (N=400 is not serious)
#' gfiSummary(gf)
#' glm(y ~ x, family = binomial())
gfilogisreg <- function(
  formula, data = NULL, N, thresh = N/2, progress = TRUE,
  gmp = FALSE,
  ufactr = .Machine$double.eps^(-0.5), vfactr = .Machine$double.eps^(-0.38)
){
  if(gmp){
    gfilogisreg_gmp(
      formula, data, N, thresh, progress, ufactr, vfactr
    )
  }else{
    gfilogisreg_nogmp(
      formula, data, N, thresh, progress, ufactr, vfactr
    )
  }
}

gfilogisreg_nogmp <- function(
  formula, data = NULL, N, thresh = N/2, progress = TRUE,
  ufactr = .Machine$double.eps^(-0.5), vfactr = .Machine$double.eps^(-0.38)
){
  y <- f_eval_lhs(formula, data = data)
  stopifnot(all(y %in% c(0,1)))
  #signs <- -2*y + 1
  X <- model.matrix(formula, data = data)
  n <- length(y)
  p <- ncol(X)
  stopifnot(p >= 2)
  idx <- 3L:(2L+p)
  Beta <- matrix(NA_real_, nrow = N, ncol = p)
  colnames(Beta) <- colnames(X)
  H <- vector("list", N)
  weight <- matrix(1, nrow = n, ncol = N)
  ESS <- rep(N, n)
  # Kstart ####
  Kstart <- i <- 1L
  rk <- 1L
  while(rk < p){
    i <- i + 1L
    if(Eigen_rank(X[c(Kstart, i), ]) == rk + 1L){
      Kstart <- c(Kstart, i)
      rk <- rk + 1L
    }
  }
  Xstart <- X[Kstart, , drop = FALSE]
  ystart <- y[Kstart]
  K <- (1L:n)[-Kstart]
  XK <- X[K, , drop = FALSE]
  yK <- y[K]
  # t = 1 to p ####
  At <- matrix(NA_real_, nrow = p, ncol = N)
  for(i in 1L:N){
    a <- rlogis(p)
    At[, i] <- a
    C <- matrix(NA_real_, nrow = p, ncol = p)
    b <- numeric(p)
    for(j in 1L:p){
      if(ystart[j] == 0L){
        C[j, ] <- Xstart[j, ]
        b[j] <- a[j]
      }else{
        C[j, ] <- -Xstart[j, ]
        b[j] <- -a[j]
      }
    }
    H[[i]] <- makeH(C, b)
  }
  # t from p+1 to n ####
  for(t in 1L:(n-p)){
    if(progress){
      message(sprintf("%d/%d", p+t, n))
    }
    At <- rbind(At, NA_real_)
    Xt <- XK[t, ]

    # Points <- lapply(H, function(Hi){
    #   V <- q2d(scdd(Hi)[["output"]])
    #   V[isone(V[, 2L]), idx, drop = FALSE]
    # })
    # L <- loop1(H, Points, yK[t], Xt)
    # H <- L[["H"]]
    # At[p+t, ] <- L[["At"]]
    # weight[t, ] <- L[["weight"]]

    for(i in 1L:N){
      V <- scdd(H[[i]])[["output"]]
      points <- V[isone(V[, 2L]), idx, drop = FALSE]
      if(yK[t] == 0L){
        MIN <- min(points %*% Xt)
        atilde <- rtlogis2(MIN)
        weight[t, i] <- 1 - plogis(MIN)
        H[[i]] <- addHin(Xt, atilde, H[[i]])
      }else{ # yK[t] = 1L
        MAX <- max(points %*% Xt)
        atilde <- rtlogis1(MAX)
        weight[t, i] <- plogis(MAX)
        H[[i]] <- addHin(-Xt, -atilde, H[[i]])
      }
      At[p+t, i] <- atilde
      # V <- scdd(Hi)[["output"]]
      # if(nrow(V) == 0) stop("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO")
      # Vertices[[i]] <- V
    }

    WT <- apply(weight, 2L, prod)
    WTnorm <- WT / sum(WT)
    ESS[p+t] <- 1 / sum(WTnorm*WTnorm)
    if(ESS[p+t] < thresh || t == n-p){ # ALTERATION ####
      if(progress) message("alteration in progress...")
      Nsons <- rmultinom(1L, N, WT)[, 1L]
      counter <- 1L
      At_new <- matrix(NA_real_, nrow = p+t, ncol = 0L)
      D <- rbind(Xstart, XK[1L:t, ])
      P <- Eigen_range(D)
      Pt <- t(P)
      QQt <- diag(p+t) - tcrossprod(P)
      M <- solve(crossprod(D)) %*% t(D) %*% P
      HH <- vector("list", N)
      for(i in 1L:N){
        ncopies <- Nsons[i]
        if(ncopies >= 1L){
          HH[[counter]] <- H[[i]]
          At_new <- cbind(At_new, At[, i])
          if(ncopies > 1L){
            VT <- scdd(H[[i]])[["output"]]
            b <- QQt %*% At[, i]
            B <- Pt %*% At[, i]
            # #
            # assign("P", P, envir = .GlobalEnv)
            # assign("b", b, envir = .GlobalEnv)
            # assign("B", B, envir = .GlobalEnv)
            # #
            # stop()
            BTILDES <- rcd(ncopies-1L, P, b, rep(0.5,p),
                           ufactr = ufactr, vfactr = vfactr)
            points <- VT[isone(VT[, 2L]), idx, drop = FALSE]
            rays <- VT[!isone(VT[, 2L]), idx, drop = FALSE]
            for(j in 2L:ncopies){
              VT_new <- matrix(NA_real_, nrow = nrow(points), ncol = p)
              Btilde <- BTILDES[j-1L, ]
              At_tilde <- P %*% Btilde + b
              At_new <- cbind(At_new, At_tilde)
              for(k in 1L:nrow(points)){
                VT_new[k, ] <- points[k, ] - M %*% (B - Btilde)
              }
              V <- if(nrow(rays)){
                makeV(points = VT_new, rays = rays)
              }else{
                makeV(points = VT_new)
              }
              HH[[counter + j - 1L]] <- scdd(V)[["output"]]
            }
          }
          counter <- counter + ncopies
        }
      }
      H <- HH
      At <- At_new
      if(t < n-p) weight <- matrix(1, nrow = n, ncol = N)
      if(progress) message("done")
    }
  }
  for(i in 1L:N){
    RAYS <- 0L
    VT <- scdd(H[[i]])[["output"]]
    if(any(!isone(VT[, 2L]))) RAYS <- RAYS + 1L
    points <- VT[isone(VT[, 2L]), idx, drop = FALSE]
    for(j in 1L:p){
      if(sample.int(2L, 1L) == 1L)
        Beta[i, j] <- min(points[, j])
      else
        Beta[i, j] <- max(points[, j])
    }
    # v <- sample.int(1:nrow(points), 1)
    # for(j in 1L:p){
    #   Beta[i, j] <- points[v, j]
    # }
    # for(j in 1L:p){
    #   Beta[i, j] <- sample(points[, j], 1L)
    # }
  }
  out <- list(Beta = as.data.frame(Beta), Weights = WTnorm)
  attr(out, "rays") <- RAYS
  attr(out, "ESS") <- ESS
  out
}

gfilogisreg_gmp <- function(
  formula, data = NULL, N, thresh = N/2, progress = TRUE,
  ufactr = .Machine$double.eps^(-0.5), vfactr = .Machine$double.eps^(-0.38)
){
  y <- f_eval_lhs(formula, data = data)
  stopifnot(all(y %in% c(0,1)))
  #signs <- -2*y + 1
  X <- model.matrix(formula, data = data)
  n <- length(y)
  p <- ncol(X)
  stopifnot(p >= 2)
  idx <- 3L:(2L+p)
  Beta <- matrix(NA_real_, nrow = N, ncol = p)
  colnames(Beta) <- colnames(X)
  H <- vector("list", N)
  weight <- matrix(1, nrow = n, ncol = N)
  ESS <- rep(N, n)
  # Kstart ####
  Kstart <- i <- 1L
  rk <- 1L
  while(rk < p){
    i <- i + 1L
    if(Eigen_rank(X[c(Kstart, i), ]) == rk + 1L){
      Kstart <- c(Kstart, i)
      rk <- rk + 1L
    }
  }
  Xstart <- X[Kstart, , drop = FALSE]
  ystart <- y[Kstart]
  K <- (1L:n)[-Kstart]
  XK <- X[K, , drop = FALSE]
  yK <- y[K]
  # t = 1 to p ####
  At <- matrix(NA_real_, nrow = p, ncol = N)
  for(i in 1L:N){
    a <- rlogis(p)
    At[, i] <- a
    C <- matrix(NA_real_, nrow = p, ncol = p)
    b <- numeric(p)
    for(j in 1L:p){
      if(ystart[j] == 0L){
        C[j, ] <- Xstart[j, ]
        b[j] <- a[j]
      }else{
        C[j, ] <- -Xstart[j, ]
        b[j] <- -a[j]
      }
    }
    H[[i]] <- makeH(d2q(C), d2q(b))
  }
  # t from p+1 to n ####
  for(t in 1L:(n-p)){
    if(progress){
      message(sprintf("%d/%d", p+t, n))
    }
    At <- rbind(At, NA_real_)
    Xt <- XK[t, ]

    Points <- lapply(H, function(Hi){
      V <- q2d(scdd(Hi)[["output"]])
      V[isone(V[, 2L]), idx, drop = FALSE]
    })
    # pbreaks <- cumsum(c(1L, vapply(Points, nrow, integer(1L)))) - 1L
    # #Points <- do.call(rbind, Points)
    # hbreaks <- cumsum(c(1L, vapply(H, nrow, integer(1L)))) - 1L
    # L <- loop1(do.call(rbind, H), hbreaks, Points, pbreaks, yK[t], Xt)

    L <- loop1(H, Points, yK[t], Xt)
    H <- L[["H"]]
    At[p+t, ] <- L[["At"]]
    weight[t, ] <- L[["weight"]]

    # for(i in 1L:N){
    #   V <- q2d(scdd(H[[i]])[["output"]])
    #   points <- V[isone(V[, 2L]), idx, drop = FALSE]
    #   if(yK[t] == 0L){
    #     MIN <- min(points %*% Xt)
    #     atilde <- rtlogis2(MIN)
    #     weight[t, i] <- 1 - plogis(MIN)
    #     H[[i]] <- addHin(d2q(Xt), d2q(atilde), H[[i]])
    #   }else{ # yK[t] = 1L
    #     MAX <- max(points %*% Xt)
    #     atilde <- rtlogis1(MAX)
    #     weight[t, i] <- plogis(MAX)
    #     H[[i]] <- addHin(d2q(-Xt), d2q(-atilde), H[[i]])
    #   }
    #   At[p+t, i] <- atilde
    #   # V <- scdd(Hi)[["output"]]
    #   # if(nrow(V) == 0) stop("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO")
    #   # Vertices[[i]] <- V
    # }

    WT <- apply(weight, 2L, prod)
    WTnorm <- WT / sum(WT)
    ESS[p+t] <- 1 / sum(WTnorm*WTnorm)
    if(ESS[p+t] < thresh || t == n-p){ # ALTERATION ####
      if(progress) message("alteration in progress...")
      Nsons <- rmultinom(1L, N, WT)[, 1L]
      counter <- 1L
      At_new <- matrix(NA_real_, nrow = p+t, ncol = 0L)
      D <- rbind(Xstart, XK[1L:t, ])
      P <- Eigen_range(D)
      Pt <- t(P)
      QQt <- diag(p+t) - tcrossprod(P)
      M <- solve(crossprod(D)) %*% t(D) %*% P
      HH <- vector("list", N)
      for(i in 1L:N){
        ncopies <- Nsons[i]
        if(ncopies >= 1L){
          HH[[counter]] <- H[[i]]
          At_new <- cbind(At_new, At[, i])
          if(ncopies > 1L){
            VT <- q2d(scdd(H[[i]])[["output"]])
            b <- QQt %*% At[, i]
            B <- Pt %*% At[, i]
            # #
            # assign("P", P, envir = .GlobalEnv)
            # assign("b", b, envir = .GlobalEnv)
            # assign("B", B, envir = .GlobalEnv)
            # #
            # stop()
            BTILDES <- rcd(ncopies-1L, P, b, rep(0.5,p),
                           ufactr = ufactr, vfactr = vfactr)
            points <- VT[isone(VT[, 2L]), idx, drop = FALSE]
            rays <- VT[!isone(VT[, 2L]), idx, drop = FALSE]
            for(j in 2L:ncopies){
              VT_new <- matrix(NA_real_, nrow = nrow(points), ncol = p)
              Btilde <- BTILDES[j-1L, ]
              At_tilde <- P %*% Btilde + b
              At_new <- cbind(At_new, At_tilde)
              for(k in 1L:nrow(points)){
                VT_new[k, ] <- points[k, ] - M %*% (B - Btilde)
              }
              V <- if(nrow(rays)){
                makeV(points = d2q(VT_new), rays = d2q(rays))
              }else{
                makeV(points = d2q(VT_new))
              }
              HH[[counter + j - 1L]] <- scdd(V)[["output"]]
            }
          }
          counter <- counter + ncopies
        }
      }
      H <- HH
      At <- At_new
      if(t < n-p) weight <- matrix(1, nrow = n, ncol = N)
      if(progress) message("done")
    }
  }
  for(i in 1L:N){
    RAYS <- 0L
    VT <- q2d(scdd(H[[i]])[["output"]])
    if(any(!isone(VT[, 2L]))) RAYS <- RAYS + 1L
    points <- VT[isone(VT[, 2L]), idx, drop = FALSE]
    for(j in 1L:p){
      if(sample.int(2L, 1L) == 1L)
        Beta[i, j] <- min(points[, j])
      else
        Beta[i, j] <- max(points[, j])
    }
    # v <- sample.int(1:nrow(points), 1)
    # for(j in 1L:p){
    #   Beta[i, j] <- points[v, j]
    # }
    # for(j in 1L:p){
    #   Beta[i, j] <- sample(points[, j], 1L)
    # }
  }
  out <- list(Beta = as.data.frame(Beta), Weights = WTnorm)
  attr(out, "rays") <- RAYS
  attr(out, "ESS") <- ESS
  out
}

