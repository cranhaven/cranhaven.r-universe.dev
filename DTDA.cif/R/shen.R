



shen <- function(X, U = NA, V = NA, wt = NA, error = NA, nmaxit = NA) {

  D <- cbind(X, U, V)

  ord <- order(D[, 1])
  C <- matrix(0, nrow = nrow(D), ncol = ncol(D))
  EE <- matrix(0, nrow = nrow(D), ncol = ncol(D))
  C[, 1] <- sort(D[, 1])
  C[, 2:ncol(D)] <- D[ord, 2:ncol(D)]

  if(is.na(error) == TRUE){
    error <- 1e-6
  }

  au <- outer(C[, 1], C[, 2], ">=")
  av <- outer(C[, 1], C[, 3], "<=")
  auu <- outer(C[, 2], C[, 2], "<=") * 1L

  J <- matrix(data = 0,
              ncol = nrow(C),
              nrow = nrow(C))
  J <- au * av

  JI <- t(J)
  f0 <- matrix(data = 1 / nrow(C),
               ncol = 1,
               nrow = nrow(C))
  f <- f0
  k <- rep(1, times = length(f))
  S0 <- 1
  S1 <- 1

  if(is.na(nmaxit) == TRUE){
    nmaxit <- 100000000000000000
  }

  iter <- 0

  while((S0 > error | S1 > error ) | iter > nmaxit){
    iter <- iter + 1

    if (iter > nmaxit) {
      stop("Default number of iterations not enough for convergence")
    }

    F0 <- JI %*% f
    k0 <- ((sum(1 / F0)) ^ (-1)) * (1 / F0)

    if (sum(k0) != 1) {
      k0 <- k0 / sum(k0)
    }

    K0 <- J %*% k0
    f <- ((sum(1 / K0)) ^ (-1)) * (1 / K0)

    if (sum(f) != 1){
      f <- f / sum(f)
    }

    S0 <- max(abs(f - f0))
    f0 <- f
    S1 <- max(abs(k - k0))
    k <- k0
  }

  F0 <- JI %*% f
  K0 <- J %*% k0
  k <- k0
  mult <- tabulate(match(C[, 1], unique(C[, 1])))

  if (sum(mult) == length(unique(C[, 1]))) {
    Fval <- (f * mult)
  }

  if (sum(mult) > length(unique(C[, 1]))) {
    weigth <- f[!duplicated(C[, 1])]
    Fval <- (weigth * mult)
  }

  x <- unique(C[, 1])
  events <- sum(mult)
  n.event <- mult


  FF <- cumsum(Fval)
  FFF <- cumsum(f)
  Sob <- 1 - FF + Fval
  Sob0 <- 1 - FFF
  Sob[Sob < 1e-12] <- 0
  Sob0[Sob0 < 1e-12] <- 0

  multk <- tabulate(match(C[, 2], unique(C[, 2])))

  if (sum(multk) == length(unique(C[, 2]))) {
    Fvalk <- (k * multk)
  }

  if (sum(multk) > length(unique(C[, 2]))) {
    weigthk <- k[!duplicated(C[, 2])]
    Fvalk <- (weigthk * multk)
  }

  uu <- unique(C[, 2])
  KK <- apply(auu * as.vector(k), 2, "sum")
  kMUV <- cbind(C[, 2], C[, 3], k)

  kuv <- numeric(nrow(C))

  for(i in 1:nrow(kMUV)) {

    indbbb <- ((kMUV[, 1] == kMUV[i, 1]) & (kMUV[, 2] == kMUV[i, 2]))
    pos1 <- min(which(indbbb == TRUE))

    if(pos1 == 1) {
      kuv[indbbb] <- sum(k[indbbb])
    }

    if (pos1 > 1) {
      kuv[indbbb] <- sum(k[indbbb])
    }
  }

  Gf <- matrix(data = k, ncol = ncol(J), nrow = nrow(J), byrow = TRUE)
  Gff <- J * Gf
  biasf <- apply(Gff, 1, "sum")

  summary <- cbind("time" = x, "n.event" = mult,
                   "density" = round(as.vector(Fval), 5),
                   "cumulative.df" = round(FF, 5),
                   "survival" = round(Sob, 5))

  colnames(summary) <- c("time", "n.event", "density",
                         "cumulative.df", "survival")
  rownames(summary) <- rep("", times = length(x))

  return(invisible(list(time = x, biasf = biasf)))
}
