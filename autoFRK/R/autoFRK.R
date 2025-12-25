#' Automatic Fixed Rank Kriging
#'
#' This function performs resolution adaptive fixed rank kriging based on
#' spatial data observed at one or multiple time points via the following
#' spatial random-effects model:
#' \deqn{z[t]=\mu + G \cdot w[t]+\eta[t]+e[t], w[t] \sim N(0,M), e[t] \sim N(0, s \cdot D); t=1,...,T,}{
#' z[t]=\mu +G*w[t]+\eta[t]+e[t], w[t]~N(0,M); e[t] ~ N(0, s*D); t=1,...,T, }
#' where \eqn{z[t]} is an \emph{n}-vector of (partially) observed data at \emph{n} locations,
#' \eqn{\mu} is an \emph{n}-vector of deterministic mean values,
#' \eqn{D} is a given n by n matrix,
#' \eqn{G} is a given \emph{n} by \emph{K} matrix,
#' \eqn{\eta[t]} is an n-vector of random variables corresponding to a spatial stationary process,
#' and \eqn{w[t]} is a K-vector of unobservable random weights.
#' Parameters are estimated by maximum likelihood in a closed-form expression. The matrix \eqn{G} corresponding to basis functions is
#' given by an ordered class of thin-plate spline functions, with the number of basis functions
#' selected by  Akaike's information criterion.
#' @param Data  \emph{n} by \emph{T} data matrix (NA allowed) with
#' \eqn{z[t]} as the \emph{t}-th column.
#' @param loc \emph{n} by \emph{d} matrix of coordinates corresponding to \emph{n} locations.
#' @param mu \emph{n}-vector or scalar for \eqn{\mu}; Default is 0.
#' @param D \emph{n} by \emph{n} matrix (preferably sparse) for the covariance matrix of the measurement errors up to a constant scale.
#' Default is an identity matrix.
#' @param G \emph{n} by \emph{K} matrix of basis function values with each column being a basis function taken values at \code{loc}.
#' Default is NULL, which is automatic determined.
#' @param finescale logical; if \code{TRUE} then a (approximate) stationary finer scale process \eqn{\eta[t]} will be included
#'  based on \code{LatticeKrig} pacakge.
#'  In such a case, only the diagonals of \eqn{D} would be taken into account. Default is \code{FALSE}.
#' @param maxit maximum number of iterations. Default is 50.
#' @param tolerance precision tolerance for convergence check. Default is 0.1^6.
#' @param maxK maximum number of basis functions considered. Default is
#' \eqn{10 \cdot \sqrt{n}} for \emph{n>100} or \emph{n} for \emph{n<=100}.
#' @param Kseq user-specified vector of numbers of basis functions considered. Default is
#' \code{NULL}, which is determined from \code{maxK}.
#' @param method "fast" or " EM"; if "fast" then the missing data are filled in using k-nearest-neighbor imputation;
#' if "EM" then the missing data are taken care by the EM algorithm. Default is "fast".
#' @param n.neighbor number of neighbors to be used in the "fast" imputation method. Default is 3.
#' @param maxknot maximum number of knots to be used in
#' generating basis functions. Default is 5000.
#'
#' @return an object of class \code{FRK} is returned, which is a list of the following components:
#' \item{M}{ML estimate of \eqn{M}.}
#' \item{s}{estimate for the scale parameter of measurement errors.}
#' \item{negloglik}{ negative  log-likelihood.}
#' \item{w}{\emph{K} by \emph{T} matrix with \eqn{w[t]} as the \emph{t}-th column.}
#' \item{V}{\emph{K} by \emph{K} matrix of the prediction error covariance matrix of \eqn{w[t]}.}
#' \item{G}{user specified basis function matrix or an automatically generated \code{mrts} object.}
#' \item{LKobj}{a list from calling \code{LKrig.MLE} in \code{LatticeKrig} package if \code{useLK=TRUE};
#'  otherwise \code{NULL}. See that package for details.}
#' @details The function computes the ML estimate of M using the closed-form expression in Tzeng and Huang (2018).
#' If the user would like to specify
#' a \code{D} other than an identity matrix for a large \emph{n}, it is better to provided via \code{spam} function
#' in \code{spam} package.
#' @export
#' @seealso \code{\link{predict.FRK}}
#' @references
#' Tzeng, S., & Huang, H.-C. (2018). Resolution Adaptive Fixed Rank Kriging, Technometrics, https://doi.org/10.1080/00401706.2017.1345701.
#' @examples
#' #### generating data from two eigenfunctions
#' originalPar <- par(no.readonly = TRUE)
#' set.seed(0)
#' n <- 150
#' s <- 5
#' grid1 <- grid2 <- seq(0, 1, l = 30)
#' grids <- expand.grid(grid1, grid2)
#' fn <- matrix(0, 900, 2)
#' fn[, 1] <- cos(sqrt((grids[, 1] - 0)^2 + (grids[, 2] - 1)^2) * pi)
#' fn[, 2] <- cos(sqrt((grids[, 1] - 0.75)^2 + (grids[, 2] - 0.25)^2) * 2 * pi)
#'
#' #### single realization simulation example
#' w <- c(rnorm(1, sd = 5), rnorm(1, sd = 3))
#' y <- fn %*% w
#' obs <- sample(900, n)
#' z <- y[obs] + rnorm(n) * sqrt(s)
#' X <- grids[obs, ]
#'
#' #### method1: automatic selection and prediction
#' one.imat <- autoFRK(Data = z, loc = X, maxK = 15)
#' yhat <- predict(one.imat, newloc = grids)
#'
#' #### method2: user-specified basis functions
#' G <- mrts(X, 15)
#' Gpred <- predict(G, newx = grids)
#' one.usr <- autoFRK(Data = z, loc = X, G = G)
#' yhat2 <- predict(one.usr, newloc = grids, basis = Gpred)
#'
#' require(fields)
#' par(mfrow = c(2, 2))
#' image.plot(matrix(y, 30, 30), main = "True")
#' points(X, cex = 0.5, col = "grey")
#' image.plot(matrix(yhat$pred.value, 30, 30), main = "Predicted")
#' points(X, cex = 0.5, col = "grey")
#' image.plot(matrix(yhat2$pred.value, 30, 30), main = "Predicted (method 2)")
#' points(X, cex = 0.5, col = "grey")
#' plot(yhat$pred.value, yhat2$pred.value, mgp = c(2, 0.5, 0))
#' par(originalPar)
#' #### end of single realization simulation example
#'
#' #### independent multi-realization simulation example
#' set.seed(0)
#' wt <- matrix(0, 2, 20)
#' for (tt in 1:20) wt[, tt] <- c(rnorm(1, sd = 5), rnorm(1, sd = 3))
#' yt <- fn %*% wt
#' obs <- sample(900, n)
#' zt <- yt[obs, ] + matrix(rnorm(n * 20), n, 20) * sqrt(s)
#' X <- grids[obs, ]
#' multi.imat <- autoFRK(Data = zt, loc = X, maxK = 15)
#' Gpred <- predict(multi.imat$G, newx = grids)
#'
#' G <- multi.imat$G
#' Mhat <- multi.imat$M
#' dec <- eigen(G %*% Mhat %*% t(G))
#' fhat <- Gpred %*% Mhat %*% t(G) %*% dec$vector[, 1:2]
#' par(mfrow = c(2, 2))
#' image.plot(matrix(fn[, 1], 30, 30), main = "True Eigenfn 1")
#' image.plot(matrix(fn[, 2], 30, 30), main = "True Eigenfn 2")
#' image.plot(matrix(fhat[, 1], 30, 30), main = "Estimated Eigenfn 1")
#' image.plot(matrix(fhat[, 2], 30, 30), main = "Estimated Eigenfn 2")
#' par(originalPar)
#' #### end of independent multi-realization simulation example
#' @author ShengLi Tzeng and Hsin-Cheng Huang.
autoFRK <- function(Data, loc, mu = 0, D = diag.spam(NROW(Data)), G = NULL,
                    finescale = FALSE, maxit = 50, tolerance = 0.1^6, maxK = NULL,
                    Kseq = NULL, method = c("fast", "EM"), n.neighbor = 3, maxknot = 5000) {
  method <- match.arg(method)
  Data <- Data - mu
  if (!is.null(G)) {
    Fk <- G
  } else {
    Fk <- selectBasis(
      Data, loc, D, maxit, tolerance, maxK,
      Kseq, method, n.neighbor, maxknot, NULL
    )
  }
  K <- NCOL(Fk)
  if (method == "fast") {
    Data <- as.matrix(Data)
    for (tt in 1:NCOL(Data)) {
      where <- is.na(Data[, tt])
      if (sum(where) == 0) {
        next
      }
      cidx <- which(!where)
      nnidx <- FNN::get.knnx(loc[cidx, ], as.matrix(loc[where, ]), k = n.neighbor)
      nnidx <- array(cidx[nnidx$nn.index], dim(nnidx$nn.index))
      nnval <- array((Data[, tt])[nnidx], dim(nnidx))
      Data[where, tt] <- rowMeans(nnval)
    }
  }
  {
    if (!finescale) {
      obj <- indeMLE(Data, Fk[, 1:K], D, maxit,
        avgtol = tolerance,
        wSave = TRUE, DfromLK = NULL
      )
    } else {
      nu <- .Options$LKinfoSetup$nu
      nlevel <- .Options$LKinfoSetup$nlevel
      a.wght <- .Options$LKinfoSetup$a.wght
      NC <- .Options$LKinfoSetup$NC
      if (is.null(nu)) {
        nu <- 1
      }
      if (is.null(nlevel)) {
        nlevel <- 3
      }
      iniobj <- initializeLKnFRK(
        loc = loc,
        nlevel = nlevel,
        weights = 1 / diag(D),
        n.neighbor = n.neighbor,
        nu = nu
      )
      DnLK <- setLKnFRKOption(iniobj, Fk[, 1:K], nc = NC, a.wght = a.wght)
      DfromLK <- DnLK$DfromLK
      LKobj <- DnLK$LKobj
      Depsilon <- diag.spam(
        iniobj$weight[iniobj$pick],
        length(iniobj$weight[iniobj$pick])
      )
      obj <- indeMLE(Data, Fk[, 1:K], D, maxit,
        avgtol = tolerance,
        wSave = TRUE, DfromLK = DfromLK, vfixed = DnLK$s
      )
    }
    obj$G <- Fk
    if (finescale) {
      obj$LKobj <- LKobj
      attr(obj, "pinfo")$loc <- loc
      attr(obj, "pinfo")$weights <- 1 / diag(D)
    }
    else {
      obj$LKobj <- NULL
    }
    class(obj) <- "FRK"
    return(obj)
  }
}

selectBasis <- function(Data, loc, D = diag.spam(NROW(Data)), maxit = 50, avgtol = 0.1^6,
                        maxK = NULL, Kseq = NULL, method = c("fast", "EM"), n.neighbor = 3,
                        maxknot = 5000, DfromLK = NULL, Fk = NULL) {
  Data <- as.matrix(Data)
  empty <- apply(!is.na(Data), 2, sum) == 0
  if (sum(empty) > 0) {
    Data <- Data[, which(!empty)]
  }
  if (is(Data, "vector")) {
    Data <- as.matrix(Data)
  }
  loc <- as.matrix(loc)
  d <- NCOL(loc)
  withNA <- sum(is.na(Data)) > 0
  del <- which(rowSums(as.matrix(!is.na(Data))) == 0)
  pick <- 1:NROW(Data)
  if (length(del) > 0) {
    Data <- Data[-del, ]
    D <- D[-del, -del]
    pick <- pick[-del]
    withNA <- sum(is.na(Data)) > 0
  }
  N <- length(pick)
  klim <- min(N, round(10 * sqrt(N)))
  if (N < maxknot) {
    knot <- loc[pick, ]
  } else {
    knot <- subKnot(loc[pick, ], min(maxknot, klim))
  }
  if (!is.null(maxK)) {
    maxK <- round(maxK)
  } else {
    if (!is.null(Kseq)) {
      maxK <- round(max(Kseq))
    } else {
      maxK <- klim
    }
  }
  if (!is.null(Kseq)) {
    K <- unique(round(Kseq))
    if (max(K) > maxK) {
      stop("maximum of Kseq is larger than maxK!")
    }
    if (any(K < (d + 1))) {
      warning(
        "The minimum of Kseq can not less than ",
        d + 1, ". Too small values will be ignored."
      )
    }
    K <- K[K > d]
    if (length(K) == 0) {
      stop("Not valid Kseq!")
    }
  }
  else {
    K <- unique(round(seq(d + 1, maxK, by = maxK^(1 / 3) * d)))
    if (length(K) > 30) {
      K <- unique(round(seq(d + 1, maxK, l = 30)))
    }
  }
  if (is.null(Fk)) {
    Fk <- mrts(knot, max(K), loc, maxknot)
  }
  AIClist <- rep(Inf, length(K))
  method <- match.arg(method)
  if ((method == "EM") & (is.null(DfromLK))) {
    for (k in 1:length(K)) {
      AIClist[k] <- indeMLE(Data, Fk[
        pick,
        1:K[k]
      ], D, maxit, avgtol, wSave = FALSE, num.report = FALSE)$negloglik
    }
  }
  else {
    if (withNA) {
      Data <- as.matrix(Data)
      for (tt in 1:NCOL(Data)) {
        where <- is.na(Data[, tt])
        if (sum(where) == 0) {
          next
        }
        cidx <- which(!where)
        nnidx <- FNN::get.knnx(loc[cidx, ], as.matrix(loc[where, ]), k = n.neighbor)
        nnidx <- array(cidx[nnidx$nn.index], dim(nnidx$nn.index))
        nnval <- array((Data[, tt])[nnidx], dim(nnidx))
        Data[where, tt] <- rowMeans(nnval)
      }
    }
    TT <- NCOL(Data)
    if (is.null(DfromLK)) {
      iD <- solve(D)
      iDFk <- iD %*% Fk[pick, ]
      iDZ <- iD %*% Data
    }
    else {
      wX <- DfromLK$wX[pick, ]
      G <- t(DfromLK$wX) %*% DfromLK$wX + DfromLK$lambda *
        DfromLK$Q
      weight <- DfromLK$weights[pick]
      wwX <- diag.spam(sqrt(weight)) %*% wX
      wXiG <- (wwX) %*% solve(G)
      iDFk <- weight * Fk[pick, ] - wXiG %*% (t(wwX) %*%
        as.matrix(Fk[pick, ]))
      iDZ <- weight * Data - wXiG %*% (t(wwX) %*% as.matrix(Data))
    }
    trS <- sum(rowSums(as.matrix(iDZ) * Data)) / TT
    for (k in 1:length(K)) {
      half <- getHalf(Fk[pick, 1:K[k]], iDFk[, 1:K[k]])
      ihFiD <- half %*% t(iDFk[, 1:K[k]])
      JSJ <- tcrossprod(ihFiD %*% Data) / TT
      JSJ <- (JSJ + t(JSJ)) / 2
      AIClist[k] <- cMLE(
        Fk = Fk[pick, 1:K[k]],
        TT = TT,
        trS = trS,
        half = half,
        JSJ = JSJ
      )$negloglik
    }
  }

  TT <- NCOL(Data)
  df <- (K * (K + 1) / 2 + 1) * (K <= TT) + (K * TT + 1 - TT * (TT - 1) / 2) * (K > TT)
  AIClist <- AIClist + 2 * df
  Kopt <- K[which.min(AIClist)]
  out <- Fk[, 1:Kopt]

  dimnames(Fk) <- NULL
  aname <- names(attributes(Fk))
  attributes(out) <- c(attributes(out), attributes(Fk)[setdiff(aname, "dim")])

  return(out)
}

cMLE <- function(Fk, TT, trS, half, JSJ = NULL, s = 0, ldet = NULL,
                 wSave = FALSE, onlylogLike = !wSave, vfixed = NULL) {
  sol.v <- function(d, s, trS, n) {
    k <- length(d)
    if (max(d) < max(trS / n, s)) {
      return(max(trS / n - s, 0))
    }
    cumd <- cumsum(d)
    ks <- 1:k
    if (k == n) {
      ks[n] <- n - 1
    }
    pick <- d > ((trS - cumd) / (n - ks))
    L <- max(which(pick))
    if (L == n) {
      L <- n - 1
    }
    return(max((trS - cumd[L]) / (n - L) - s, 0))
  }
  sol.eta <- function(d, s, v) pmax(d - s - v, 0)
  neg2llik <- function(d, s, v, trS, n) {
    k <- length(d)
    eta <- pmax(d - s - v, 0)
    if (max(eta / (s + v)) > 10^20) {
      return(Inf)
    }
    n * log(2 * pi) + sum(log(eta + s + v)) + log(s + v) *
      (n - k) + 1 / (s + v) * trS - 1 / (s + v) * sum(d * eta / (eta +
      s + v))
  }
  if (is.null(ldet)) {
    ldet <- 0
  }
  n <- nrow(Fk)
  k <- ncol(Fk)
  eg <- eigen(JSJ)
  d <- eg$value[1:k]
  P <- eg$vector[, 1:k]
  if (is.null(vfixed)) {
    v <- sol.v(d, s, trS, n)
  } else {
    v <- vfixed
  }
  dii <- pmax(d, 0)
  dhat <- sol.eta(dii, s, v)
  if (onlylogLike) {
    return(list(negloglik = neg2llik(dii, s, v, trS, n) *
      TT + ldet * TT))
  }
  M <- half %*% P %*% (dhat * t(P)) %*% half
  dimnames(M) <- NULL
  if (!wSave) {
    L <- NULL
  } else {
    L <- Fk %*% t((sqrt(dhat) * t(P)) %*% half)
    if (all(dhat == 0)) {
      dhat[1] <- 0.1^10
    }
    L <- as.matrix(L[, dhat > 0])
  }
  return(list(v = v, M = M, s = s, negloglik = neg2llik(
    dii,
    s, v, trS, n
  ) * TT + ldet * TT, L = L))
}

cMLEimat <- function(Fk, Data, s, wSave = FALSE, S = NULL, onlylogLike = !wSave) {
  sol.v <- function(d, s, trS, n) {
    k <- length(d)
    if (max(d) < max(trS / n, s)) {
      return(max(trS / n - s, 0))
    }
    cumd <- cumsum(d)
    ks <- 1:k
    if (k == n) {
      ks[n] <- n - 1
    }
    pick <- d > ((trS - cumd) / (n - ks))
    L <- max(which(pick))
    if (L == n) {
      L <- n - 1
    }
    return(max((trS - cumd[L]) / (n - L) - s, 0))
  }
  sol.eta <- function(d, s, v) pmax(d - s - v, 0)
  neg2llik <- function(d, s, v, trS, n) {
    k <- length(d)
    eta <- pmax(d - s - v, 0)
    if (max(eta / (s + v)) > 10^20) {
      return(Inf)
    }
    n * log(2 * pi) + sum(log(eta + s + v)) + log(s + v) *
      (n - k) + 1 / (s + v) * trS - 1 / (s + v) * sum(d * eta / (eta +
      s + v))
  }
  n <- nrow(Fk)
  k <- ncol(Fk)
  TT <- NCOL(Data)
  trS <- sum(rowSums(as.matrix(Data)^2)) / TT
  half <- getHalf(Fk, Fk)
  ihF <- half %*% t(Fk)
  if (is.null(S)) {
    JSJ <- tcrossprod(ihF %*% Data) / TT
  }
  else {
    JSJ <- (ihF %*% S) %*% t(ihF)
  }
  JSJ <- (JSJ + t(JSJ)) / 2
  eg <- eigen(JSJ)
  d <- eg$value[1:k]
  P <- eg$vector[, 1:k]
  v <- sol.v(d, s, trS, n)
  dii <- pmax(d, 0)
  if (onlylogLike) {
    return(list(negloglik = neg2llik(dii, s, v, trS, n) *
      TT))
  }
  dhat <- sol.eta(dii, s, v)
  M <- half %*% P %*% (dhat * t(P)) %*% half
  dimnames(M) <- NULL
  if (!wSave) {
    return(list(v = v, M = M, s = s, negloglik = neg2llik(
      dii,
      s, v, trS, n
    ) * TT))
  } else {
    L <- Fk %*% t((sqrt(dhat) * t(P)) %*% half)
    if (all(dhat == 0)) {
      dhat[1] <- 0.1^10
    }
    L <- L[, dhat > 0]
    invD <- rep(1, n) / (s + v)
    iDZ <- invD * Data
    right <- L %*% (solve(diag(1, NCOL(L)) + t(L) %*% (invD *
      L)) %*% (t(L) %*% iDZ))
    INVtZ <- iDZ - invD * right
    etatt <- as.matrix(M %*% t(Fk) %*% INVtZ)
    GM <- Fk %*% M
    V <- as.matrix(M - t(GM) %*% invCz(
      (s + v) * diag.spam(n),
      L, GM
    ))
    return(list(v = v, M = M, s = s, negloglik = neg2llik(
      dii,
      s, v, trS, n
    ) * TT, w = etatt, V = V))
  }
}

cMLElk <- function(Fk, Data, Depsilon, wSave = FALSE, DfromLK, vfixed = NULL) {
  TT <- NCOL(Data)
  N <- NROW(Data)
  lambda <- DfromLK$lambda
  pick <- DfromLK$pick
  wX <- DfromLK$wX[pick, ]
  G <- t(wX) %*% wX + lambda * DfromLK$Q
  weight <- DfromLK$weights[pick]
  wwX <- diag.spam(sqrt(weight)) %*% wX
  wXiG <- (wwX) %*% solve(G)
  iDFk <- weight * Fk - wXiG %*% (t(wwX) %*% as.matrix(Fk))
  iDZ <- weight * Data - wXiG %*% (t(wwX) %*% as.matrix(Data))
  half <- getHalf(Fk, iDFk)
  ihFiD <- half %*% t(iDFk)
  JSJ <- tcrossprod(ihFiD %*% Data) / TT
  JSJ <- (JSJ + t(JSJ)) / 2
  ldet <- function(m) spam::determinant(m, logarithm = TRUE)$modulus
  ldetD <- -nrow(DfromLK$Q) * log(lambda) + ldet(G) - ldet(DfromLK$Q) -
    sum(log(weight))
  trS <- sum(rowSums(as.matrix(iDZ) * Data)) / TT
  out <- cMLE(Fk, TT, trS, half, JSJ,
    s = 0, ldet = as.vector(ldetD),
    wSave = TRUE, onlylogLike = FALSE, vfixed = vfixed
  )
  L <- out$L
  out$s <- out$v
  out <- out[-which(names(out) == "v")]
  out <- out[-which(names(out) == "L")]
  if (!wSave) {
    return(out)
  } else {
    iDL <- weight * L - wXiG %*% (t(wwX) %*% L)
    itmp <- solve(diag(1, NCOL(L)) + t(L) %*% iDL / out$s)
    iiLiD <- itmp %*% t(iDL / out$s)
    MFiS11 <- out$M %*% t(iDFk) / out$s - ((out$M %*% t(iDFk / out$s)) %*%
      L) %*% iiLiD
    out$w <- MFiS11 %*% Data
    out$V <- MFiS11 %*% (Fk %*% out$M)
    wlk <- t(wXiG) %*% Data - t(wXiG) %*% L %*% (iiLiD %*%
      Data)
    ihL <- chol(itmp) %*% t(L)
    attr(out, "pinfo") <- list(wlk = wlk, pick = pick)
    return(out)
  }
}

cMLEsp <- function(Fk, Data, Depsilon, wSave = FALSE) {
  De <- toSpMat(Depsilon)
  iD <- solve(De)
  ldetD <- spam::determinant(De, logarithm = TRUE)$modulus
  iDFk <- iD %*% Fk
  half <- getHalf(Fk, iDFk)
  ihFiD <- half %*% t(iDFk)
  TT <- NCOL(Data)
  JSJ <- tcrossprod(ihFiD %*% Data) / TT
  JSJ <- (JSJ + t(JSJ)) / 2
  trS <- sum(rowSums(as.matrix(iD %*% Data) * Data)) / TT
  out <- cMLE(Fk, TT, trS, half, JSJ, s = 0, ldet = ldetD, wSave)
  if (wSave) {
    L <- as.matrix(out$L)
    invD <- iD / (out$s + out$v)
    iDZ <- invD %*% Data
    right0 <- L %*% solve(diag(1, NCOL(L)) + t(L) %*% (invD %*%
      L))
    INVtZ <- iDZ - invD %*% right0 %*% (t(L) %*% iDZ)
    etatt <- out$M %*% t(Fk) %*% INVtZ
    out$w <- as.matrix(etatt)
    GM <- Fk %*% out$M
    iDGM <- invD %*% GM
    out$V <- as.matrix(out$M - t(GM) %*% (iDGM - invD %*%
      right0 %*% (t(L) %*% iDGM)))
  }
  out$s <- out$v
  out <- out[-which(names(out) == "v")]
  out <- out[-which(names(out) == "L")]
  out
}

EM0miss <- function(Fk, Data, Depsilon, maxit, avgtol, wSave = FALSE, external = FALSE,
                    DfromLK = NULL, num.report = TRUE, vfixed = NULL) {
  saveOLD <- function(external) if (external) save(old, Ptt1, file = oldfile)

  O <- !is.na(Data)
  TT <- NCOL(Data)
  K <- NCOL(Fk)
  tmpdir <- tempfile()
  dir.create(tmpdir)
  ftmp <- paste(tmpdir, 1:TT, sep = "/")
  oldfile <- paste(tmpdir, "old_par.Rdata", sep = "/")
  ziDz <- rep(NA, TT)
  ziDB <- matrix(NA, TT, K)
  db <- list()
  D <- toSpMat(Depsilon)
  iD <- solve(D)
  diagD <- checkDiag(D)

  if (!is.null(DfromLK)) {
    pick <- DfromLK$pick
    if (is.null(pick)) pick <- 1:length(DfromLK$weights)
    weight <- DfromLK$weights[pick]
    DfromLK$wX <- DfromLK$wX[pick, ]
    wwX <- diag.spam(sqrt(weight)) %*% DfromLK$wX
    lQ <- DfromLK$lambda * DfromLK$Q
  }

  for (tt in 1:TT) {
    if (!is.null(DfromLK)) {
      iDt <- NULL
      if (sum(O[, tt]) == NROW(O)) {
        wXiG <- wwX %*% solve(DfromLK$G)
      } else {
        G <- t(DfromLK$wX[O[, tt], ]) %*% DfromLK$wX[O[, tt], ] + lQ
        wXiG <- wwX[O[, tt], ] %*% solve(G)
      }
      Bt <- as.matrix(Fk[O[, tt], ])
      if (NCOL(Bt) == 1) Bt <- t(Bt)
      iDBt <- as.matrix(weight[O[, tt]] * Bt - wXiG %*% (t(wwX[O[, tt], ]) %*% Bt))
      zt <- Data[O[, tt], tt]
      ziDz[tt] <- sum(zt * as.vector(weight[O[, tt]] * zt - wXiG %*% (t(wwX[O[, tt], ]) %*% zt)))
      ziDB[tt, ] <- t(zt) %*% iDBt
      BiDBt <- t(Bt) %*% iDBt
    } else {
      if (!diagD) {
        iDt <- solve(D[O[, tt], O[, tt]])
      } else {
        iDt <- iD[O[, tt], O[, tt]]
      }
      Bt <- Fk[O[, tt], ]
      if (NCOL(Bt) == 1) Bt <- t(Bt)
      iDBt <- as.matrix(iDt %*% Bt)
      zt <- Data[O[, tt], tt]
      ziDz[tt] <- sum(zt * as.vector(iDt %*% zt))
      ziDB[tt, ] <- t(zt) %*% iDBt
      BiDBt <- t(Bt) %*% iDBt
    }

    if (external) {
      db[[tt]] <- dumpObjects(
        iDBt,
        zt,
        BiDBt,
        external,
        oldfile,
        dbName = ftmp[tt]
      )
    } else {
      db[[tt]] <- list(
        iDBt = iDBt,
        zt = zt,
        BiDBt = BiDBt,
        external = external,
        oldfile = oldfile
      )
    }
  }
  # gc
  rm(iDt, Bt, iDBt, zt, BiDBt)
  gc()

  dif <- Inf
  cnt <- 0
  Z0 <- Data
  Z0[is.na(Z0)] <- 0
  old <- cMLEimat(Fk, Z0, s = 0, wSave = T)
  if (is.null(vfixed)) old$s <- old$v else old$s <- vfixed
  old$M <- mkpd(old$M)
  Ptt1 <- old$M
  saveOLD(external)
  inv <- MASS::ginv

  while ((dif > (avgtol * (100 * K^2))) && (cnt < maxit)) {
    etatt <- matrix(0, K, TT)
    sumPtt <- 0
    s1 <- rep(0, TT)
    if (external) load(oldfile)
    for (tt in 1:TT) {
      s1.eta.P <- with(db[[tt]], {
        iP <- mkpd(MASS::ginv(mkpd(Ptt1)) + BiDBt / old$s)
        Ptt <- solve(iP)
        Gt <- as.matrix(Ptt %*% t(iDBt) / old$s)
        eta <- c(0 + Gt %*% zt)
        s1kk <- diag(BiDBt %*% (eta %*% t(eta) + Ptt))
        rbind(s1kk, eta, Ptt)
      })
      sumPtt <- sumPtt + s1.eta.P[-c(1:2), ]
      etatt[, tt] <- s1.eta.P[2, ]
      s1[tt] <- sum(s1.eta.P[1, ])
    }
    if (is.null(vfixed)) {
      new <- list(
        M = (etatt %*% t(etatt) + sumPtt) / TT,
        s = max((sum(ziDz) - 2 * sum(ziDB * t(etatt)) + sum(s1)) / sum(O), 0.1^8)
      )
    } else {
      new <- list(
        M = (etatt %*% t(etatt) + sumPtt) / TT,
        s = vfixed
      )
    }
    new$M <- (new$M + t(new$M)) / 2
    dif <- sum(abs(new$M - old$M)) + abs(new$s - old$s)
    cnt <- cnt + 1
    old <- new
    Ptt1 <- old$M
    saveOLD(external)
  }
  if (num.report) cat("Number of iteration: ", cnt, "\n")
  unlink(tmpdir, recursive = TRUE)
  n2loglik <- getLikelihood(Data, Fk, new$M, new$s, Depsilon)

  if (!wSave) {
    return(list(M = new$M, s = new$s, negloglik = n2loglik))
  } else if (!is.null(DfromLK)) {
    out <- list(
      M = new$M, s = new$s, negloglik = n2loglik,
      w = etatt, V = new$M - etatt %*% t(etatt) / TT
    )
    dec <- eigen(new$M)
    L <- Fk %*% dec$vector %*% diag(sqrt(pmax(
      dec$value,
      0
    )))
    weight <- DfromLK$weights[pick]
    wlk <- matrix(NA, NROW(lQ), TT)
    for (tt in 1:TT) {
      if (sum(O[, tt]) == NROW(O)) {
        wXiG <- wwX %*% solve(DfromLK$G)
      } else {
        G <- t(DfromLK$wX[O[, tt], ]) %*% DfromLK$wX[O[, tt], ] + lQ
        wXiG <- wwX[O[, tt], ] %*% solve(G)
      }
      dat <- Data[O[, tt], tt]
      Lt <- L[O[, tt], ]
      iDL <- weight[O[, tt]] * Lt - wXiG %*% (t(wwX[O[, tt], ]) %*% Lt)
      itmp <- solve(diag(1, NCOL(L)) + t(Lt) %*% iDL / out$s)
      iiLiD <- itmp %*% t(iDL / out$s)
      wlk[, tt] <- t(wXiG) %*% dat - t(wXiG) %*% Lt %*% (iiLiD %*% dat)
    }
    attr(out, "pinfo") <- list(wlk = wlk, pick = pick)
    attr(out, "missing") <- list(
      miss = toSpMat(1 - O),
      maxit = maxit, avgtol = avgtol
    )
    return(out)
  } else {
    out <- list(
      M = as.matrix(new$M), s = new$s, negloglik = n2loglik,
      w = etatt, V = new$M - etatt %*% t(etatt) / TT
    )
    attr(out, "missing") <- list(
      miss = toSpMat(1 - O),
      maxit = maxit, avgtol = avgtol
    )
    return(out)
  }
}

indeMLE <- function(Data, Fk, D = diag.spam(NROW(Data)), maxit = 50, avgtol = 0.1^6,
                    wSave = FALSE, DfromLK = NULL, vfixed = NULL, num.report = TRUE) {
  withNA <- sum(is.na(Data)) > 0
  if (is(Data, "vector")) {
    Data <- as.matrix(Data)
  }
  TT <- NCOL(Data)
  empty <- apply(!is.na(Data), 2, sum) == 0
  notempty <- which(!empty)
  if (sum(empty) > 0) {
    Data <- as.matrix(Data[, notempty])
  }
  if (is(Data, "vector")) {
    Data <- as.matrix(Data)
  }
  del <- which(rowSums(as.matrix(!is.na(Data))) == 0)
  pick <- 1:NROW(Data)
  if (!checkDiag(D)) {
    D0 <- toSpMat(D)
  } else {
    D0 <- diag.spam(diag(D), NROW(Data))
  }
  if (withNA && (length(del) > 0)) {
    pick <- pick[-del]
    Data <- Data[-del, ]
    Fk <- Fk[-del, ]
    if (!checkDiag(D)) {
      D <- D[-del, -del]
    } else {
      D <- diag.spam(diag(D)[-del], NROW(Data))
    }
    withNA <- sum(is.na(Data)) > 0
  }
  N <- NROW(Data)
  K <- NCOL(Fk)
  Depsilon <- toSpMat(D)
  isimat <- checkDiag(D) * (sum(abs(rep(mean(diag(D)), N) -
    diag(Depsilon))) < .Machine$double.eps)
  if (!withNA) {
    if (isimat & is.null(DfromLK)) {
      if (!is.null(.Options$sigma_FRK)) {
        sigma <- .Options$sigma_FRK
      } else {
        sigma <- 0
      }
      if (NCOL(Data) == 1) {
        Data <- as.matrix(Data)
      }
      out <- cMLEimat(Fk, Data, s = sigma, wSave)
      if (!is.null(out$v)) {
        if (sigma == 0) {
          out$s <- out$v
        } else {
          out$s <- sigma
        }
        out <- out[-which(names(out) == "v")]
      }
      if (wSave) {
        w <- matrix(0, K, TT)
        w[, notempty] <- out$w
        out$w <- w
        attr(out, "pinfo") <- list(D = D0, pick = pick)
      }
      return(out)
    }
    else {
      if (is.null(DfromLK)) {
        out <- cMLEsp(Fk, Data, Depsilon, wSave)
        if (wSave) {
          w <- matrix(0, K, TT)
          w[, notempty] <- out$w
          out$w <- w
          attr(out, "pinfo") <- list(D = D0, pick = pick)
        }
        return(out)
      }
      else {
        out <- cMLElk(
          Fk, Data, Depsilon, wSave, DfromLK,
          vfixed
        )
        if (wSave) {
          w <- matrix(0, K, TT)
          w[, notempty] <- out$w
          out$w <- w
        }
        return(out)
      }
    }
  }
  else {
    out <- EM0miss(Fk, Data, Depsilon, maxit, avgtol, wSave,
      external = FALSE, DfromLK, num.report, vfixed
    )
    if (wSave) {
      w <- matrix(0, K, TT)
      w[, notempty] <- out$w
      out$w <- w
      if (is.null(DfromLK)) {
        attr(out, "pinfo") <- list(D = D0, pick = pick)
      }
    }
    return(out)
  }
}

invCz <- function(R, L, z) {
  K <- NCOL(L)
  iR <- solve(R)
  iRZ <- iR %*% z
  right <- L %*% solve(diag(1, K) + as.matrix(t(L) %*% iR %*% L)) %*% (t(L) %*% iRZ)

  return(iRZ - iR %*% right)
}

initializeLKnFRK <- function(Data, loc, nlevel = 3, weights = NULL, n.neighbor = 3, nu = 1) {
  if ("numeric" %in% class(Data)) Data <- as.matrix(Data)
  empty <- apply(!is.na(Data), 2, sum) == 0
  if (sum(empty) > 0) Data <- Data[, which(!empty)]
  if ("numeric" %in% class(Data)) Data <- as.matrix(Data)

  loc <- as.matrix(loc)
  N <- NROW(Data)
  d <- NCOL(loc)
  nas <- sum(is.na(Data))
  del <- which(rowSums(as.matrix(!is.na(Data))) == 0)
  pick <- 1:N

  if (length(del) > 0) {
    Data <- Data[-del, ]
    loc <- loc[-del, ]
    pick <- (1:N)[-del]
  }

  nas <- sum(is.na(Data))

  if (nas > 0) {
    for (tt in 1:NCOL(Data)) {
      where <- is.na(Data[, tt])
      if (sum(where) == 0) {
        next
      }
      cidx <- which(!where)
      nnidx <- FNN::get.knnx(loc[cidx, ], as.matrix(loc[where, ]), k = n.neighbor)
      nnidx <- array(cidx[nnidx$nn.index], dim(nnidx$nn.index))
      nnval <- array((Data[, tt])[nnidx], dim(nnidx))
      Data[where, tt] <- rowMeans(nnval)
    }
  }
  x <- as.matrix(loc[pick, ])
  z <- as.matrix(Data)
  d <- NCOL(x)

  gtype <- ifelse(d == 1, "LKInterval", ifelse(d == 2, "LKRectangle", "LKBox"))

  thetaL <- 2^(-1 * (1:nlevel))
  alpha <- thetaL^(2 * nu)
  alpha <- alpha / sum(alpha)
  n <- NROW(x)

  if (is.null(weights)) weights <- rep(1, NROW(z))

  return(list(
    x = x,
    z = z,
    n = n,
    alpha = alpha,
    gtype = gtype,
    weights = weights,
    nlevel = nlevel,
    loc = loc,
    pick = pick
  ))
}

setLKnFRKOption <- function(iniobj, Fk, nc = NULL, Ks = NCOL(Fk), a.wght = NULL) {
  x <- iniobj$x
  z <- iniobj$z
  alpha <- iniobj$alpha
  alpha <- alpha / sum(alpha)
  gtype <- iniobj$gtype
  weights <- iniobj$weights[iniobj$pick]
  nlevel <- iniobj$nlevel
  TT <- NCOL(z)
  Fk <- Fk[iniobj$pick, ]

  if (is.null(nc)) nc <- setNC(z, x, nlevel)
  if (is.null(a.wght)) a.wght <- 2 * NCOL(x) + 0.01

  info <- LKrigSetup(
    x = x,
    a.wght = a.wght,
    nlevel = nlevel,
    NC = nc,
    alpha = alpha,
    LKGeometry = gtype,
    lambda = 1
  )

  loc <- x
  phi <- LKrig.basis(loc, info)
  w <- diag.spam(sqrt(weights))
  wX <- w %*% phi
  wwX <- w %*% wX
  XwX <- t(wX) %*% wX

  ldet <- function(m) spam::determinant(m, logarithm = TRUE)$modulus
  iniLike <- function(par, Data = z, full = FALSE) {
    lambda <- exp(par)
    G <- XwX + lambda * Qini
    wXiG <- (wwX) %*% solve(G)
    iDFk <- weights * Fk - wXiG %*% (t(wwX) %*% as.matrix(Fk))
    iDZ <- weights * Data - wXiG %*% (t(wwX) %*% as.matrix(Data))
    ldetD <- -nrow(Qini) * log(lambda) + ldet(G)
    ldetD <- as.vector(ldetD)
    trS <- sum(rowSums(as.matrix(iDZ) * Data)) / TT
    half <- getHalf(Fk, iDFk)
    ihFiD <- half %*% t(iDFk)
    LSL <- tcrossprod(ihFiD %*% Data) / TT
    if (!full) {
      cMLE(Fk, TT, trS, half, LSL,
        s = 0, ldet = ldetD,
        wSave = FALSE
      )$negloglik
    } else {
      llike <- ldetD - ldet(Qini) - sum(log(weights))
      cMLE(Fk, TT, trS, half, LSL,
        s = 0, ldet = llike,
        wSave = TRUE, onlylogLike = FALSE, vfixed = NULL
      )
    }
  }

  Qini <- LKrig.precision(info)
  sol <- optimize(iniLike, c(-16, 16), tol = .Machine$double.eps^0.025)
  lambda.MLE <- sol$minimum
  out <- iniLike(sol$minimum, z, full = TRUE)
  llike <- out$negloglik
  info.MLE <- LKrigSetup(
    x = x,
    a.wght = a.wght,
    nlevel = nlevel,
    NC = nc,
    alpha = alpha,
    LKGeometry = gtype,
    lambda = lambda.MLE
  )
  info.MLE$llike <- llike
  info.MLE$time <- NA
  Q <- LKrig.precision(info.MLE)
  G <- t(wX) %*% wX + info.MLE$lambda * Q

  return(list(
    DfromLK = list(
      Q = Q,
      weights = weights,
      wX = wX,
      G = G,
      lambda = info.MLE$lambda,
      pick = iniobj$pick
    ),
    s = out$v,
    LKobj = list(
      summary = NULL,
      par.grid = NULL,
      LKinfo.MLE = info.MLE,
      lnLike.eval = NULL,
      lambda.MLE = info.MLE$lambda,
      call = NA,
      taskID = NULL
    )
  ))
}

#' Multi-Resolution Thin-plate Spline Basis Functions
#'
#' This function generates multi-resolution thin-plate spline basis functions.
#' The basis functions are (descendingly) ordered
#' in terms of their degrees of smoothness with a higher-order function corresponding
#' to larger-scale features and a lower-order one corresponding to smaller-scale details.
#' They are useful in the spatio-temporal random effects model.
#'
#' @param knot \emph{m} by \emph{d} matrix (\emph{d<=3}) for \emph{m} locations of \emph{d}-dimensional knots as in ordinary splines.
#'          Missing values are not allowed.
#' @param k the number (\emph{<=m}) of basis functions.
#' @param x  \emph{n} by \emph{d} matrix of coordinates corresponding to \emph{n} locations where the values of basis functions to be evaluated.
#' Default is \code{NULL}, which uses the \emph{m} by \emph{d} matrix in \code{knot}.
#' @param maxknot maximum number of knots to be used in generating basis functions. If  \code{maxknot} < \emph{m}, a deterministic subset selection of knots will be used.  For using all knots, set \code{maxknot}>=\emph{m}.
#' @return  An \code{mrts} object is generated. If \code{x=NULL} (default) it returns
#' an \emph{m} by \emph{k} matrix of \emph{k} basis function taken values at knots.
#' With \code{x} given, it returns \emph{n} by \emph{k} matrix for basis functions taken values at \code{x}.
#' @export
#' @seealso \code{\link{predict.mrts}}
#' @examples
#' originalPar <- par(no.readonly = TRUE)
#' knot <- seq(0, 1, l = 30)
#' b <- mrts(knot, 30)
#' x0 <- seq(0, 1, l = 200)
#' bx <- predict(b, x0)
#' par(mfrow = c(5, 6), mar = c(0, 0, 0, 0))
#' for (i in 1:30) {
#'   plot(bx[, i], type = "l", axes = FALSE)
#'   box()
#' }
#' par(originalPar)
#' @references
#' Tzeng, S., & Huang, H. C. (2018). Resolution Adaptive Fixed Rank Kriging. Technometrics, https://doi.org/10.1080/00401706.2017.1345701.
#' Tzeng, S., & Huang, H. C. (2015). Multi-Resolution Spatial Random-Effects Models
#' for Irregularly Spaced Data. arXiv preprint arXiv:1504.05659.
#' @author ShengLi Tzeng and Hsin-Cheng Huang.
#
mrts <- function(knot, k, x = NULL, maxknot = 5000) {
  is64bit <- length(grep("64-bit", sessionInfo()$platform)) > 0
  if ((!is64bit) & (max(NROW(x), NROW(knot)) > 20000)) {
    stop("Use 64-bit version of R for such a volume of data!")
  }
  if (NCOL(knot) == 1) {
    xobs <- as.matrix(as.double(as.matrix(knot)))
  } else {
    xobs <- apply(knot, 2, as.double)
  }
  Xu <- uniquecombs(cbind(xobs))
  if (is.null(x) & length(Xu) != length(xobs)) {
    x <- xobs
  }
  colnames(Xu) <- NULL
  n <- n.Xu <- NROW(Xu)
  ndims <- NCOL(Xu)
  if (k < (ndims + 1)) {
    stop("k-1 can not be smaller than the number of dimensions!")
  }
  if (maxknot < n) {
    bmax <- maxknot
    Xu <- subKnot(Xu, bmax)
    Xu <- as.matrix(Xu)
    if (is.null(x)) {
      x <- knot
    }
    n <- NROW(Xu)
    n.Xu <- n
  }
  xobs_diag <- diag(sqrt(n / (n - 1)) / apply(xobs, 2, sd), ndims)
  if (!is.null(x)) {
    if (NCOL(x) == 1) {
      x <- as.matrix(as.double(as.matrix(x)))
    } else {
      x <- as.matrix(array(as.double(as.matrix(x)), dim(x)))
    }
    if (k - ndims - 1 > 0) {
      result <- mrtsrcpp_predict0(Xu, xobs_diag, x, k -
        ndims - 1)
    } else {
      X2 <- scale(Xu, scale = FALSE)
      shift <- colMeans(Xu)
      nconst <- sqrt(diag(t(X2) %*% X2))
      X2 <- cbind(1, t((t(x) - shift) / nconst) * sqrt(n))
      result <- list(X = X2[, 1:k])
      x <- NULL
    }
  }
  else {
    if (k - ndims - 1 > 0) {
      result <- mrtsrcpp(Xu, xobs_diag, k - ndims - 1)
    } else {
      X2 <- scale(Xu, scale = FALSE)
      shift <- colMeans(Xu)
      nconst <- sqrt(diag(t(X2) %*% X2))
      X2 <- cbind(1, t((t(Xu) - shift) / nconst) * sqrt(n))
      result <- list(X = X2[, 1:k])
    }
  }
  obj <- result$X
  if (is.null(result$nconst)) {
    X2 <- scale(Xu, scale = FALSE)
    result$nconst <- sqrt(diag(t(X2) %*% X2))
  }
  attr(obj, "UZ") <- result$UZ
  attr(obj, "Xu") <- Xu
  attr(obj, "nconst") <- result$nconst
  attr(obj, "BBBH") <- result$BBBH
  attr(obj, "class") <- c("matrix", "mrts")
  class(obj) <- "mrts"
  if (is.null(x)) {
    return(obj)
  } else {
    shift <- colMeans(attr(obj, "Xu"))
    X2 <- sweep(cbind(x), 2, shift, "-")
    X2 <- cbind(1, sweep(X2, 2, attr(obj, "nconst"), "/"))
    if (k - ndims - 1 > 0) {
      obj0 <- as.matrix(cbind(X2, result$X1))
    } else {
      obj0 <- as.matrix(X2)
    }
    dimnames(obj) <- NULL
    aname <- names(attributes(obj))
    attributes(obj0) <- c(attributes(obj0), attributes(obj)[setdiff(
      aname,
      c("dim", "dimnames")
    )])
    return(obj0)
  }
}

#' Predict method for Fixed Rank Kriging
#'
#' Predicted values and estimate of standard errors based on an "\code{autoFRK}" model object.
#' @param object  a model object obtained from "\code{autoFRK}".
#' @param obsData a vector with observed data used for prediction.
#' Default is \code{NULL}, which uses the \code{Data} input from \code{object}.
#' @param obsloc a matrix with rows being coordinates of observation locations for \code{obsData}.
#' Only \code{object} using \code{mrts} basis functions can have
#' \code{obsloc} different from the \code{loc} input of \code{object};
#' not applicable for user-specified basis functions.
#' Default is \code{NULL}, which uses the \code{loc} input of \code{object}.
#' @param mu.obs  a vector or scalar for the deterministic mean values at \code{obsloc}. Default is 0.
#' @param newloc a matrix with rows being coordinates of new locations for prediction.
#'  Default is \code{NULL}, which gives prediction at the locations of the observed data.
#' @param basis a matrix with each column being a basis function taken values at \code{newloc}.
#'  It can be omitted if \code{object} was fitted using  default \code{mrts} basis functions.
#' @param mu.new  a vector or scalar for the deterministic mean values at \code{newloc}. Default is 0.
#' @param se.report logical; if \code{TRUE} then the standard error of prediction is reported.
#' @param ... not used but needed for the S3 generic/method compatibility.
#' @export
#' @seealso \code{\link{autoFRK}}
#' @author ShengLi Tzeng and Hsin-Cheng Huang.
#' @return A list with the components described below.
#' \item{pred.value}{a matrix with the \emph{(i,t)} element being the predicted value at \emph{i}-th location and time \emph{t}.}
#' \item{se}{a vector with the \emph{i}-th element being the standard error of the predicted value at the \emph{i}-th location.}
#
predict.FRK <- function(object, obsData = NULL, obsloc = NULL, mu.obs = 0,
                        newloc = obsloc, basis = NULL, mu.new = 0, se.report = FALSE,
                        ...) {
  if (!"w" %in% names(object)) {
    stop("input model (object) should use the option \"wsave=TRUE\"!")
  }
  if (is.null(basis)) {
    if (is.null(newloc) & is.null(obsloc)) {
      basis <- object$G
    } else {
      if (!is(object$G, "mrts")) {
        stop("Basis matrix of new locations should be given (unless the model was fitted with mrts bases)!")
      } else {
        if (is.null(newloc)) {
          basis <- object$G
        } else {
          basis <- predict.mrts(object$G, newx = newloc)
        }
      }
    }
  }
  if (NROW(basis) == 1) {
    basis <- as.matrix(t(basis))
  }
  if (is.null(obsloc)) {
    nobs <- NROW(object$G)
  } else {
    nobs <- NROW(obsloc)
  }
  if (!is.null(obsData)) {
    obsData <- as.matrix(obsData - mu.obs)
    if (length(obsData) != nobs) {
      stop("Dimensions of obsloc and obsData are not compatible!")
    }
  }
  if (!is.null(newloc)) {
    if (NROW(basis) != NROW(newloc)) {
      stop("Dimensions of newloc and basis are not compatible!")
    }
  }
  else {
    if (NROW(basis) != NROW(object$G)) {
      stop("Dimensions of obsloc and basis are not compatible!")
    }
  }
  if (is.null(object$LKobj)) {
    if (is.null(obsloc) & is.null(obsData)) {
      miss <- attr(object, "missing")
      yhat <- basis %*% object$w
      if (se.report) {
        TT <- NCOL(object$w)
        if (is.null(miss)) {
          se <- sqrt(pmax(0, rowSums((basis %*% object$V) *
            basis)))
          se <- matrix(se, length(se), TT)
        }
        else {
          se <- matrix(NA, NROW(basis), TT)
          pick <- attr(object, "pinfo")$pick
          D0 <- attr(object, "pinfo")$D[pick, pick]
          miss <- (as.matrix(miss$miss) == 1)
          Fk <- object$G[pick, ]
          M <- object$M
          dec <- eigen(M)
          for (tt in 1:TT) {
            G <- Fk[!miss[, tt], ]
            GM <- G %*% M
            De <- D0[!miss[, tt], !miss[, tt]]
            L <- G %*% dec$vector %*% diag.spam(sqrt(pmax(
              dec$value,
              0
            )), NROW(M))
            V <- as.matrix(M - t(GM) %*% invCz(object$s *
              De, L, GM))
            se[, tt] <- sqrt(pmax(0, rowSums((basis %*%
              V) * basis)))
          }
        }
      }
    }
    if (!is.null(obsData)) {
      pick <- which(!is.na(obsData))
      if (is.null(obsloc)) {
        De <- attr(object, "pinfo")$D[pick, pick]
        G <- object$G[pick, ]
      }
      else {
        De <- diag.spam(length(pick))
        G <- predict.mrts(object$G, newx = as.matrix(obsloc)[pick, ])
      }
      M <- object$M
      GM <- G %*% M
      dec <- eigen(M)
      L <- G %*% dec$vector %*% diag.spam(sqrt(pmax(
        dec$value,
        0
      )), NROW(M))
      yhat <- basis %*% t(GM) %*% invCz(
        object$s * De, L,
        obsData[pick]
      )
      if (se.report) {
        V <- as.matrix(M - t(GM) %*% invCz(object$s *
          De, L, GM))
        se <- sqrt(pmax(0, rowSums((basis %*% V) * basis)))
      }
    }
  }
  else {
    LKpeon <- function(M, s, Fk, basis, weight, phi1, phi0,
                       Q, lambda, phi0P, L = NULL, Data = NULL, only.wlk = FALSE,
                       only.se = FALSE) {
      wwX <- diag.spam(weight) %*% phi1
      wXiG <- (wwX) %*% solve(t(wwX) %*% phi1 + lambda *
        Q)
      fM <- Fk %*% M
      if (is.null(L)) {
        dec <- eigen(M)
        L <- Fk %*% dec$vector %*% diag.spam(sqrt(pmax(
          dec$value,
          0
        )), NROW(M))
        L <- as.matrix(L)
      }
      iDL <- weight * L - wXiG %*% (t(wwX) %*% L)
      iDFk <- weight * Fk - wXiG %*% (t(wwX) %*% as.matrix(Fk))
      itmp <- solve(diag(1, NCOL(L)) + t(L) %*% iDL / s)
      ihL <- chol(itmp) %*% t(L)
      iiLiD <- itmp %*% t(iDL / s)
      if (only.wlk) {
        LiiLiDZ <- L %*% (iiLiD %*% Data)
        w <- M %*% t(iDFk) %*% Data / s - (M %*% t(iDFk / s)) %*%
          (LiiLiDZ)
        wlk <- t(wXiG) %*% Data - t(wXiG) %*% (LiiLiDZ)
        return(list(w = w, wlk = wlk))
      }
      MFiS11 <- M %*% t(iDFk) / s - ((M %*% t(iDFk / s)) %*%
        L) %*% iiLiD
      FMfi <- basis %*% MFiS11
      p0Pp1 <- as.matrix(phi0P %*% t(phi1))
      se0 <- rowSums((basis %*% M) * basis) + rowSums(as.matrix(phi0P *
        phi0)) / lambda * s
      se11 <- rowSums((FMfi %*% fM) * basis)
      se12 <- rowSums(p0Pp1 * (FMfi)) * s / lambda
      se13 <- se12
      se14 <- rowSums(as.matrix(phi0 %*% t(wXiG)) * p0Pp1) *
        s / lambda - colSums((ihL %*% wXiG %*% t(phi0))^2)
      se <- sqrt(pmax(
        se0 - (se11 + se12 + se13 + se14),
        0
      ))
      if (only.se) {
        return(se)
      } else {
        return(list(se = se, w = MFiS11 %*% Data, wlk = t(wXiG) %*%
          Data - t(wXiG) %*% L %*% (iiLiD %*% Data)))
      }
    }
    if (is.null(obsloc) & is.null(obsData)) {
      if (is.null(newloc)) {
        newloc <- attr(object, "pinfo")$loc
      }
      miss <- attr(object, "missing")
      info <- object$LKobj$LKinfo.MLE
      phi0 <- LKrig.basis(newloc, info)
      pinfo <- attr(object, "pinfo")
      yhat <- basis %*% object$w + phi0 %*% pinfo$wlk
      if (se.report) {
        TT <- NCOL(object$w)
        lambda <- object$LKobj$lambda.MLE
        loc <- attr(object, "pinfo")$loc
        pick <- pinfo$pick
        G <- object$G[pick, ]
        M <- object$M
        dec <- eigen(M)
        L <- G %*% dec$vector %*% diag.spam(sqrt(pmax(
          dec$value,
          0
        )), NROW(M))
        L <- as.matrix(L)
        phi1 <- LKrig.basis(as.matrix(loc)[pick, ], info)
        Q <- LKrig.precision(info)
        weight <- pinfo$weights[pick]
        s <- object$s
        phi0P <- phi0 %*% solve(Q)
        if (is.null(miss)) {
          se <- LKpeon(M, s, G, basis, weight, phi1, phi0,
            Q, lambda, phi0P, L,
            only.se = TRUE
          )
          se <- matrix(se, length(se), TT)
        }
        else {
          se <- matrix(NA, NROW(basis), TT)
          miss <- (as.matrix(miss$miss) == 1)
          for (tt in 1:TT) {
            se[, tt] <- LKpeon(M, s, G[!miss[
              ,
              tt
            ], ], basis, weight[!miss[, tt]], phi1[!miss[
              ,
              tt
            ], ], phi0, Q, lambda, phi0P, L[!miss[
              ,
              tt
            ], ], only.se = TRUE)
          }
        }
      }
    }
    if (!is.null(obsData)) {
      loc <- attr(object, "pinfo")$loc
      if (is.null(newloc)) {
        newloc <- loc
      }
      pick <- which(!is.na(obsData))
      if (is.null(obsloc)) {
        obsloc <- loc
        De <- attr(object, "pinfo")$D[pick, pick]
        G <- object$G[pick, ]
      }
      else {
        G <- predict.mrts(object$G, newx = as.matrix(obsloc)[pick, ])
      }
      M <- object$M
      dec <- eigen(M)
      L <- G %*% dec$vector %*% diag.spam(sqrt(pmax(
        dec$value,
        0
      )), NROW(M))
      L <- as.matrix(L)
      info <- object$LKobj$LKinfo.MLE
      phi1 <- LKrig.basis(as.matrix(obsloc)[pick, ], info)
      Q <- LKrig.precision(info)
      weight <- rep(1, length(pick))
      s <- object$s
      phi0 <- LKrig.basis(newloc, info)
      phi0P <- phi0 %*% solve(Q)
      lambda <- object$LKobj$lambda.MLE
      pred <- LKpeon(M, s, G, basis, weight, phi1, phi0,
        Q, lambda, phi0P, L,
        Data = obsData[pick, ],
        only.wlk = !se.report
      )
      yhat <- basis %*% pred$w + phi0 %*% pred$wlk
      if (se.report) {
        se <- pred$se
      }
    }
  }
  if (!se.report) {
    return(list(pred.value = yhat + mu.new, se = NULL))
  } else {
    return(list(pred.value = yhat + mu.new, se = as.matrix(se)))
  }
}

#' Multi-Resolution Thin-plate Spline Basis Functions
#'
#' Evaluate multi-resolution thin-plate spline basis  functions at given locations.
#' This function provides a generic prediction method for \code{mrts} objects,
#' in a similar way as \code{predict.ns} and \code{predict.bs} in \code{splines} package.
#'
#' @param object object produced from calling mrts.
#' @param newx  an \emph{n} by \emph{d} matrix of coordinates corresponding to \emph{n} locations.
#' @param ... not used but needed for the S3 generic/method compatibility.
#' @return an \emph{n} by \emph{k} matrix of the \emph{k} basis function in \code{object} taken values at \code{newx}.
#' @export
#' @seealso \code{\link{mrts}}
#' @author ShengLi Tzeng and Hsin-Cheng Huang.
#
predict.mrts <- function(object, newx, ...) {
  if (missing(newx)) {
    return(object)
  }
  Xu <- attr(object, "Xu")
  n <- NROW(Xu)
  xobs_diag <- diag(sqrt(n / (n - 1)) / apply(Xu, 2, sd), ncol(Xu))
  ndims <- NCOL(Xu)
  k <- NCOL(object)
  x0 <- matrix(as.matrix(newx), ncol = ndims)
  kstar <- (k - ndims - 1)
  if (kstar <= 0) {
    X1 <- NULL
  } else {
    X1 <- mrtsrcpp_predict(
      Xu, xobs_diag, x0, attr(
        object,
        "BBBH"
      ), attr(object, "UZ"), attr(object, "nconst"),
      k
    )$X1
    X1 <- X1[, 1:kstar]
  }
  shift <- colMeans(attr(object, "Xu"))
  X2 <- sweep(cbind(x0), 2, shift, "-")
  X2 <- cbind(1, sweep(X2, 2, attr(object, "nconst"), "/"))
  if (kstar > 0) {
    return(as.matrix(cbind(X2, X1)))
  } else {
    return(as.matrix(X2))
  }
}
