rotmat <- function(gamma) {
  matrice <- array(0, c(2, 2), dimnames = list(c("X", "Y"), c("X", "Y")))
  matrice[1, 1] <- cos(gamma)
  matrice[1, 2] <- -sin(gamma)
  matrice[2, 1] <- sin(gamma)
  matrice[2, 2] <- cos(gamma)

  return(matrice)
}

comb <- function(x, ...) {
  lapply(
    seq_along(x),
    function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))
  )
}

rmnorm <- function (n = 1, mean = rep(0, d), varcov, sqrt = NULL) {
  sqrt.varcov <- if (is.null(sqrt))
    chol(varcov)
  else sqrt
  d <- if (is.matrix(sqrt.varcov))
    ncol(sqrt.varcov)
  else 1
  mean <- outer(rep(1, n), as.vector(matrix(mean, d)))
  drop(mean + t(matrix(stats::rnorm(n * d), d, n)) %*% sqrt.varcov)
}

Mix_Sen <- function(X, k, init.par = NULL, model = NULL, theta.model = NULL, tol = 0.001, tol2 = 0.001, maxit = 500, verbose = TRUE) {

  # X is a matrix with num obs (row) and d variables (columns)

  tr <- function(x) {
    return(sum(diag(x)))
  }
  dmsen <- function(x, mu = rep(0, d), Sigma, theta = Inf, formula = "direct") {
    if (missing(Sigma)) {
      stop("Sigma is missing")
    }
    if (theta < 0) {
      stop("theta must be greater than, or equal to, 0")
    }
    if (is.matrix(Sigma)) {
      d <- ncol(Sigma)
    }
    if (!is.matrix(Sigma)) {
      d <- 1
    }
    if (is.vector(x)) {
      x <- matrix(x, length(x), 1)
      Sigma <- matrix(Sigma, nrow = d, ncol = d)
    }
    if (formula == "direct") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                          pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                           1)))
      pdfgamma <- expint::gammainc(a = (d / 2 + 1), x = 1 / 2 *
                                     delta + theta) * (1 / 2 * delta + theta)^(-(d / 2 +
                                                                                   1))
      pdfconst <- (2 * pi)^(-d / 2) * theta * exp(theta) * det(Sigma)^(-1 / 2)
      PDF <- pdfconst * pdfgamma
    }
    if (formula == "indirect") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      intf <- function(w, gamm) {
        w^(d / 2) * exp(-w * gamm)
      }
      pdfinteg <- sapply(1:nrow(x), function(i) {
        stats::integrate(intf,
                         lower = 1, upper = Inf, gamm = delta[i] / 2 + theta
        )$value
      })
      pdfconst <- (2 * pi)^(-d / 2) * theta * exp(theta) * det(Sigma)^(-1 / 2)
      PDF <- pdfconst * pdfinteg
    }
    if (formula == "series") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                          pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                           1)))
      n <- d / 2
      term <- sapply(1:length(delta), function(j) {
        exp(-delta[j] / 2 -
              theta) * (delta[j] / 2 + theta)^(-1) * (1 + sum(sapply(
                1:floor(n),
                function(i) {
                  prod(seq(from = n, to = n - i + 1, by = -1)) *
                    (delta[j] / 2 + theta)^(-i)
                }
              )))
      })
      if (d %% 2 == 1) {
        term <- term + sapply(1:length(delta), function(j) {
          prod(seq(
            from = n,
            to = 0.5, by = -1
          )) * sqrt(pi) * 2 * 1 / (delta[j] / 2 +
                                     theta)^(floor(n) + 1 + 1 / 2) * (1 - stats::pnorm(sqrt(2) *
                                                                                         sqrt(delta[j] / 2 + theta)))
        })
      }
      PDF <- (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2) * theta *
        exp(theta) * term
    }
    return(PDF)
  }

  name <- "sen"
  num <- nrow(X)
  d <- ncol(X)

  # contenitori #

  prior <- numeric(k)
  mu <- matrix(0, nrow = d, ncol = k)
  Sigma <- array(0, dim = c(d, d, k))
  theta <- numeric(k)

  w <- matrix(0, nrow = num, ncol = k)
  dens <- array(0, c(num, k), dimnames = list(1:num, paste("comp.", 1:k, sep = "")))
  z <- matrix(0, nrow = num, ncol = k)

  W <- array(0, dim = c(d, d, k))
  tempW <- array(0, dim = c(d, d, num))

  phi <- numeric(1)
  phi.k <- numeric(k)
  temp.phi <- vector("list", k) # for VEI, VEE, VEV
  temp.numdelta <- array(0, dim = c(d, d, k)) # for VEI, EVI, VEE, VEV
  ftemp.r <- array(0, dim = c(d, d, k)) # MM object
  numphi <- numeric(k) # for EVE, EVV
  gammaU.k <- array(0, dim = c(d, d, k))
  tempW_EEV <- vector("list", k) # for EEV, VEV
  tempomega <- array(0, dim = c(d, d, k)) # for EEV, VEV
  V_EVV.U.K <- array(0, dim = c(d, d, k)) # for EVV
  deltaU <- matrix(0, d, d)

  # Preliminary definition of convergence criterions

  check <- 0
  check2 <- 0
  loglik.old <- -Inf
  loglik.new <- NULL
  ll <- NULL
  mark <- 1
  MM.r.old <- -Inf
  m.iter <- 0

  ### Algorithm ###

  if (verbose == TRUE) {
    print(paste(paste(paste("Fitting Matrix Sen Mixtures with k =", k), paste("and", model)), paste("-", theta.model), paste("parsimonious structure")))
  }

  oper0 <- init.par
  prior <- unlist(oper0[[1]])
  mu <- matrix(unlist(oper0[[2]]), d, k)
  Sigma <- array(unlist(oper0[[3]]), dim = c(d, d, k))
  theta <- unlist(oper0[[4]])
  classy <- unlist(oper0[[6]])

  if (model == "VEI" | model == "VEE" | model == "VEV") {
    for (j in 1:k) {
      temp.phi[[j]] <- base::eigen(Sigma[, , j])$values

      phi.k[j] <- (prod(temp.phi[[j]]))^(1 / d)
    }
  }
  if (model == "EVE" | model == "VVE") {
    deltaU.k <- array(0, dim = c(d, d, k))
    TempW2 <- array(0, dim = c(d, d, k))
    Tempz <- mclust::unmap(classy)

    for (j in 1:k) {
      deltaU.k[, , j] <- diag(base::eigen(Sigma[, , j])$values, d, d)
      TempW2[, , j] <- Sigma[, , j] * sum(Tempz[, j])
    }

    gammaU <- base::eigen(rowSums(TempW2, dims = 2) / ((det(rowSums(TempW2, dims = 2)))^(1 / d)))$vectors
  } else {
    deltaU.k <- array(0, dim = c(d, d, k))
    gammaU <- matrix(0, d, d)
  }
  if (model == "EII" | model == "VII") {
    for (j in 1:k) {
      Sigma[, , j] <- diag(1, d, d)
    }
  }

  try(while (check < 1) {
    m.iter <- m.iter + 1

    # E - STEP

    for (j in 1:k) {
      dens[, j] <- dmsen(x = X, mu = mu[, j], Sigma = Sigma[, , j], theta = theta[j])
    }

    numerator <- matrix(rep(prior, num), num, k, byrow = TRUE) * dens
    mixt.dens <- rowSums(numerator)
    z <- numerator / mixt.dens

    for (j in 1:k) {
      delta <- stats::mahalanobis(X, center = mu[, j], cov = Sigma[, , j])

      numer <- expint::gammainc(a = (d / 2 + 2), x = (delta / 2 + theta[j]))
      den <- (delta / 2 + theta[j]) * expint::gammainc(a = (d / 2 + 1), x = (delta / 2 + theta[j]))

      numer[numer < .Machine$double.xmin] <- .Machine$double.xmin
      den[den < .Machine$double.xmin] <- .Machine$double.xmin

      w[, j] <- numer / den
    }

    # M - STEP

    for (j in 1:k) {
      mu[, j] <- colSums(w[, j] * z[, j] / sum(w[, j] * z[, j]) * X)

      if (theta.model == "V") {
        theta[j] <- sum(z[, j]) / (sum(z[, j] * (w[, j] - 1)))
      }
    }

    if (theta.model == "E") {
      product <- matrix(0, nrow = num, ncol = k)

      for (j in 1:k) {
        product[, j] <- z[, j] * (w[, j] - 1)
      }

      product[product < .Machine$double.xmin] <- .Machine$double.xmin
      product[is.nan(product)] <- .Machine$double.xmin


      theta <- rep(num / (sum(rowSums(product))), k)
    }

    if (k == 1) {
      prior <- 1
    } else {
      prior <- colMeans(z)
    }

    for (j in 1:k) {
      for (i in 1:num) {
        tempW[, , i] <- (z[i, j] * w[i, j]) * (X[i, ] - mu[, j]) %*% t(X[i, ] - mu[, j])
      }

      W[, , j] <- rowSums(tempW, dims = 2)
    }

    for (j in 1:k) {
      if (model == "EII") {
        phi <- tr(rowSums(W, dims = 2)) / (num * d)

        for (j in 1:k) {
          Sigma[, , j] <- phi * diag(1, d, d)
        }
      }

      if (model == "VII") {
        for (j in 1:k) {
          phi.k[j] <- tr(W[, , j]) / (d * sum(z[, j]))
          Sigma[, , j] <- phi.k[j] * diag(1, d, d)
        }
      }

      if (model == "EEI") {
        deltaU <- diag(diag(rowSums(W, dims = 2)), d, d) / (det(diag(diag(rowSums(W, dims = 2)), d, d)))^(1 / d)

        phi <- (det(diag(diag(rowSums(W, dims = 2)), d, d)))^(1 / d) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * deltaU
        }
      }

      if (model == "VEI") {
        for (j in 1:k) {
          temp.numdelta[, , j] <- (1 / phi.k[j]) * W[, , j]
        }

        deltaU <- diag(diag(rowSums(temp.numdelta, dims = 2)), d, d) / (det(diag(diag(rowSums(temp.numdelta, dims = 2)), d, d)))^(1 / d)

        for (j in 1:k) {
          phi.k[j] <- (tr(solve(deltaU) %*% W[, , j])) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU
        }
      }

      if (model == "EVI") {
        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(W[, , j]), d, d) / (det(diag(diag(W[, , j]), d, d)))^(1 / d)

          temp.numdelta[, , j] <- det(diag(diag(W[, , j]), d, d))^(1 / d)
        }

        phi <- rowSums(temp.numdelta, dims = 2) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * deltaU.k[, , j]
        }
      }

      if (model == "VVI") {
        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(W[, , j]), d, d) / (det(diag(diag(W[, , j]), d, d)))^(1 / d)

          phi.k[j] <- det(diag(diag(W[, , j]), d, d))^(1 / d) / (sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU.k[, , j]
        }
      }

      if (model == "EEE") {
        for (j in 1:k) {
          Sigma[, , j] <- rowSums(W, dims = 2) / (num)
        }
      }

      if (model == "VEE") {
        for (j in 1:k) {
          temp.numdelta[, , j] <- (1 / phi.k[j]) * W[, , j]
        }

        deltaU <- rowSums(temp.numdelta, dims = 2) / ((det(rowSums(temp.numdelta, dims = 2)))^(1 / d))

        for (j in 1:k) {
          phi.k[j] <- tr(solve(deltaU) %*% W[, , j]) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU
        }
      }

      if (model == "EVE") {
        while (check2 < 1) {
          for (j in 1:k) {
            ftemp.r[, , j] <- solve(deltaU.k[, , j]) %*% (t(gammaU) %*% W[, , j]) - max(base::eigen(W[, , j])$values) * (solve(deltaU.k[, , j]) %*% t(gammaU))
          }

          f <- rowSums(ftemp.r, dims = 2)

          MM.r.new <- tr(f %*% gammaU)

          if ((abs(MM.r.new - MM.r.old)) < tol2) {
            check2 <- 1
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          } else {
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          }

          MM.r.old <- MM.r.new
        }

        check2 <- 0
        MM.r.old <- -Inf

        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d) / (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d)))^(1 / d)

          numphi[j] <- tr(gammaU %*% solve(deltaU.k[, , j]) %*% t(gammaU) %*% W[, , j])
        }

        phi <- sum(numphi) / (num * d)

        for (j in 1:k) {
          Sigma[, , j] <- phi * gammaU %*% deltaU.k[, , j] %*% t(gammaU)
        }
      }

      if (model == "VVE") {
        while (check2 < 1) {
          for (j in 1:k) {
            ftemp.r[, , j] <- solve(deltaU.k[, , j]) %*% (t(gammaU) %*% W[, , j]) - max(base::eigen(W[, , j])$values) * (solve(deltaU.k[, , j]) %*% t(gammaU))
          }

          f <- rowSums(ftemp.r, dims = 2)

          MM.r.new <- tr(f %*% gammaU)

          if ((abs(MM.r.new - MM.r.old)) < tol2) {
            check2 <- 1
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          } else {
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          }

          MM.r.old <- MM.r.new
        }

        check2 <- 0
        MM.r.old <- -Inf

        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d) / (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d)))^(1 / d)
          phi.k[j] <- (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d))^(1 / d)) / (sum(z[, j]))
          Sigma[, , j] <- phi.k[j] * gammaU %*% deltaU.k[, , j] %*% t(gammaU)
        }
      }

      if (model == "EEV") {
        for (j in 1:k) {
          tempW_EEV[[j]] <- base::eigen(W[, , j])

          gammaU.k[, , j] <- tempW_EEV[[j]][["vectors"]]

          tempomega[, , j] <- diag(tempW_EEV[[j]][["values"]], d, d)
        }

        deltaU <- rowSums(tempomega, dims = 2) / ((det(rowSums(tempomega, dims = 2)))^(1 / d))

        phi <- ((det(rowSums(tempomega, dims = 2)))^(1 / d)) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * gammaU.k[, , j] %*% deltaU %*% t(gammaU.k[, , j])
        }
      }

      if (model == "VEV") {
        for (j in 1:k) {
          tempW_EEV[[j]] <- base::eigen(W[, , j])

          gammaU.k[, , j] <- tempW_EEV[[j]][["vectors"]]

          tempomega[, , j] <- diag(tempW_EEV[[j]][["values"]], d, d)

          temp.numdelta[, , j] <- (1 / phi.k[j]) * tempomega[, , j]
        }

        deltaU <- rowSums(temp.numdelta, dims = 2) / ((det(rowSums(temp.numdelta, dims = 2)))^(1 / d))

        for (j in 1:k) {
          phi.k[j] <- tr(tempomega[, , j] %*% solve(deltaU)) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * gammaU.k[, , j] %*% deltaU %*% t(gammaU.k[, , j])
        }
      }

      if (model == "EVV") {
        for (j in 1:k) {
          V_EVV.U.K[, , j] <- W[, , j] / ((det(W[, , j]))^(1 / d))

          numphi[j] <- det(W[, , j])^(1 / d)
        }

        phi <- sum(numphi) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * V_EVV.U.K[, , j]
        }
      }

      if (model == "VVV") {
        for (j in 1:k) {
          Sigma[, , j] <- W[, , j] / (sum(z[, j]))
        }
      }
    }

    # component density

    for (j in 1:k) {
      dens[, j] <- dmsen(x = X, mu = mu[, j], Sigma = Sigma[, , j], theta = theta[j])
    }

    dens[dens < .Machine$double.xmin] <- .Machine$double.xmin

    # mixture density

    numerator <- matrix(rep(prior, num), num, k, byrow = TRUE) * dens
    mixt.dens <- rowSums(numerator)
    loglik.new <- sum(log(mixt.dens))
    ll <- c(ll, loglik.new)

    # stopping rule

    if ((loglik.new - loglik.old) < tol) {
      check <- 1
    }

    if (m.iter >= maxit) {
      check <- 1
    }

    if (loglik.new < loglik.old) {
      mark <- 1
    } else {
      mark <- 0
    }

    loglik.old <- loglik.new
  })

  if (verbose == TRUE) {
    plot(ll, type = "l", xlab = "Iterations", ylab = "Log-Likelihoods")
  }

  if (k == 1) {
    classification <- rep(1, num)
  } else {
    colnames(z) <- c(1:k)
    classification <- as.numeric(colnames(z)[max.col(z, ties.method = "first")])
  }

  # Number of parameters

  npar.Mean <- d * k

  if (model == "EII") {
    npar.Cov <- 1
  }
  if (model == "VII") {
    npar.Cov <- k
  }
  if (model == "EEI") {
    npar.Cov <- d
  }
  if (model == "VEI") {
    npar.Cov <- k + (d - 1)
  }
  if (model == "EVI") {
    npar.Cov <- 1 + k * (d - 1)
  }
  if (model == "VVI") {
    npar.Cov <- k * d
  }
  if (model == "EEE") {
    npar.Cov <- d * (d + 1) / 2
  }
  if (model == "VEE") {
    npar.Cov <- k + d - 1 + d * (d - 1) / 2
  }
  if (model == "EVE") {
    npar.Cov <- 1 + k * (d - 1) + d * (d - 1) / 2
  }
  if (model == "VVE") {
    npar.Cov <- k * d + d * (d - 1) / 2
  }
  if (model == "EEV") {
    npar.Cov <- d + k * d * (d - 1) / 2
  }
  if (model == "VEV") {
    npar.Cov <- k + (d - 1) + k * d * (d - 1) / 2
  }
  if (model == "EVV") {
    npar.Cov <- 1 + k * (d - 1) + k * d * (d - 1) / 2
  }
  if (model == "VVV") {
    npar.Cov <- k * d * (d + 1) / 2
  }

  if (theta.model == "V") {
    npar <- npar.Mean + npar.Cov + k + (k - 1)
  } else {
    npar <- npar.Mean + npar.Cov + 1 + (k - 1)
  }

  # Information Criteria

  BIC <- -2 * loglik.new + log(num) * npar # to be minimized

  hard <- mclust::unmap(classification = classification)
  softhard <- matrix(0, num, k)
  for (i in 1:num) {
    for (j in 1:k) {
      if (z[i, j] == 1) {
        z[i, j] <- 0.99999
      }
      if (z[i, j] == 0) {
        z[i, j] <- 0.00001
      }
      softhard[i, j] <- hard[i, j] * log(z[i, j])
    }
  }
  som <- rowSums(softhard)
  A <- sum(som)
  ICL <- BIC - 2 * A

  return(list(name = name, model = model, theta = theta.model, prior = prior, mu = mu, Sigma = Sigma, theta = theta, classification = classification, loglik = loglik.new, ll = ll, mark = mark, check = check, npar = npar, BIC = BIC, ICL = ICL))
}

Mix_Tin <- function(X, k, init.par = NULL, model = NULL, theta.model = NULL, tol = 0.001, tol2 = 0.001, formula = "direct", maxit = 500, verbose = TRUE) {

  # X is a matrix with n obs (row) and d variables (columns)

  tr <- function(x) {
    return(sum(diag(x)))
  }
  dmtin <- function(x, mu = rep(0, d), Sigma, theta = 0.01, formula = "direct") {
    if (missing(Sigma)) {
      stop("Sigma is missing")
    }
    if (theta <= 0 | theta > 1) {
      stop("theta must be in the interval (0,1)")
    }
    if (is.matrix(Sigma)) {
      d <- ncol(Sigma)
    }
    if (!is.matrix(Sigma)) {
      d <- 1
    }
    if (is.vector(x)) {
      x <- matrix(x, 1, d)
      Sigma <- matrix(Sigma, nrow = d, ncol = d)
    }
    if (formula == "direct") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                          pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                           1)))
      pdfgamma <- (2 / delta)^(d / 2 + 1) * (zipfR::Igamma(a = (d / 2 +
                                                                  1), x = delta / 2 * (1 - theta), lower = FALSE) -
                                               zipfR::Igamma(a = (d / 2 + 1), x = delta / 2, lower = FALSE))
      pdfconst <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2)
      PDF <- pdfconst * pdfgamma
    }
    if (formula == "indirect") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      intf <- function(w, del) {
        w^(d / 2) * exp(-w / 2 * del)
      }
      pdfinteg <- sapply(1:nrow(x), function(i) {
        stats::integrate(intf,
                         lower = (1 - theta), upper = 1, del = delta[i]
        )$value
      })
      pdfconst <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2)
      PDF <- pdfconst * pdfinteg
    }
    if (formula == "series") {
      delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
      delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                          pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                           1)))
      n <- d / 2
      term <- sapply(1:length(delta), function(j) {
        -exp(-delta[j] / 2) *
          (delta[j] / 2)^n + exp(-(1 - theta) * delta[j] / 2) *
          ((1 - theta) * delta[j] / 2)^n + sum(sapply(
            1:floor(n),
            function(i) {
              prod(seq(from = n, to = n - i + 1, by = -1)) *
                (delta[j] / 2)^(n - i) * (exp(-(1 - theta) * delta[j] / 2) *
                                            (1 - theta)^(n - i) - exp(-delta[j] / 2))
            }
          ))
      })
      if (d %% 2 == 1) {
        term <- term + sapply(1:length(delta), function(j) {
          prod(seq(
            from = n,
            to = 1.5, by = -1
          )) * sqrt(pi) * (stats::pnorm(sqrt(2 *
                                               delta[j] / 2)) - stats::pnorm(sqrt(2 * (1 - theta) *
                                                                                    delta[j] / 2)))
        })
      }
      PDF <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2) *
        (2 / delta)^(d / 2 + 1) * term
    }
    return(PDF)
  }
  Mstep_AECM <- function(X, k, weights = NULL, mu, Sigma, theta.model, formula = formula) {
    n <- nrow(X)

    if (is.null(weights)) {
      weights <- rep(1, n)
    }

    if (theta.model == "V") {
      f1 <- function(par, weights, X, mu, Sigma, formula) {
        theta <- par
        pll <- sum(weights * log(dmtin(x = X, mu = mu, Sigma = Sigma, theta = theta, formula = formula)))
        return(pll)
      }

      res <- stats::optimize(f = f1, interval = c(0, 1), weights = weights, X = X, mu = mu, Sigma = Sigma, formula = formula, maximum = TRUE)

      theta <- res$maximum
    }

    if (theta.model == "E") {
      f2 <- function(par, weights, X, mu, k, Sigma, formula) {
        theta <- par
        pll <- 0

        for (j in 1:k) {
          pll <- pll + sum(weights[, j] * log(dmtin(x = X, mu = mu[, j], Sigma = Sigma[, , j], theta = theta, formula = formula)))
        }

        return(pll)
      }

      res <- stats::optimize(f = f2, interval = c(0, 1), weights = weights, X = X, k = k, mu = mu, Sigma = Sigma, formula = formula, maximum = TRUE)

      theta <- res$maximum
    }

    return(theta)
  }

  name <- "tin"
  num <- nrow(X)
  d <- ncol(X)

  # contenitori #

  prior <- numeric(k)
  mu <- matrix(0, nrow = d, ncol = k)
  Sigma <- array(0, dim = c(d, d, k))
  theta <- numeric(k)

  w <- matrix(0, nrow = num, ncol = k)
  dens <- array(0, c(num, k), dimnames = list(1:num, paste("comp.", 1:k, sep = "")))
  z <- matrix(0, nrow = num, ncol = k)

  W <- array(0, dim = c(d, d, k))
  tempW <- array(0, dim = c(d, d, num))

  phi <- numeric(1)
  phi.k <- numeric(k)
  temp.phi <- vector("list", k) # for VEI, VEE, VEV
  temp.numdelta <- array(0, dim = c(d, d, k)) # for VEI, EVI, VEE, VEV
  ftemp.r <- array(0, dim = c(d, d, k)) # MM object
  numphi <- numeric(k) # for EVE, EVV
  gammaU.k <- array(0, dim = c(d, d, k))
  tempW_EEV <- vector("list", k) # for EEV, VEV
  tempomega <- array(0, dim = c(d, d, k)) # for EEV, VEV
  V_EVV.U.K <- array(0, dim = c(d, d, k)) # for EVV
  deltaU <- matrix(0, d, d)

  # Preliminary definition of convergence criterions

  check <- 0
  check2 <- 0
  loglik.old <- -Inf
  loglik.new <- NULL
  ll <- NULL
  mark <- 1
  MM.r.old <- -Inf
  m.iter <- 0

  ### Algorithm ###

  if (verbose == TRUE) {
    print(paste(paste(paste("Fitting Matrix Tin Mixtures with k =", k), paste("and", model)), paste("-", theta.model), paste("parsimonious structure")))
  }

  oper0 <- init.par
  prior <- unlist(oper0[[1]])
  mu <- matrix(unlist(oper0[[2]]), d, k)
  Sigma <- array(unlist(oper0[[3]]), dim = c(d, d, k))
  theta <- unlist(oper0[[4]])
  classy <- unlist(oper0[[6]])

  if (model == "VEI" | model == "VEE" | model == "VEV") {
    for (j in 1:k) {
      temp.phi[[j]] <- base::eigen(Sigma[, , j])$values

      phi.k[j] <- (prod(temp.phi[[j]]))^(1 / d)
    }
  }
  if (model == "EVE" | model == "VVE") {
    deltaU.k <- array(0, dim = c(d, d, k))
    TempW2 <- array(0, dim = c(d, d, k))
    Tempz <- mclust::unmap(classy)

    for (j in 1:k) {
      deltaU.k[, , j] <- diag(base::eigen(Sigma[, , j])$values, d, d)
      TempW2[, , j] <- Sigma[, , j] * sum(Tempz[, j])
    }

    gammaU <- base::eigen(rowSums(TempW2, dims = 2) / ((det(rowSums(TempW2, dims = 2)))^(1 / d)))$vectors
  } else {
    deltaU.k <- array(0, dim = c(d, d, k))
    gammaU <- matrix(0, d, d)
  }
  if (model == "EII" | model == "VII") {
    for (j in 1:k) {
      Sigma[, , j] <- diag(1, d, d)
    }
  }

  try(while (check < 1) {
    m.iter <- m.iter + 1

    # E - STEP

    for (j in 1:k) {
      dens[, j] <- dmtin(x = X, mu = mu[, j], Sigma = Sigma[, , j], theta = theta[j], formula = formula)
    }

    numerator <- matrix(rep(prior, num), num, k, byrow = TRUE) * dens
    mixt.dens <- rowSums(numerator)
    z <- numerator / mixt.dens

    for (j in 1:k) {
      delta <- stats::mahalanobis(X, center = mu[, j], cov = Sigma[, , j])

      numer <- 2 * (zipfR::Igamma(a = (d / 2 + 2), x = (1 - theta[j]) * delta / 2, lower = FALSE) - zipfR::Igamma(a = (d / 2 + 2), x = delta / 2, lower = FALSE))
      den <- delta * (zipfR::Igamma(a = (d / 2 + 1), x = (1 - theta[j]) * delta / 2, lower = FALSE) - zipfR::Igamma(a = (d / 2 + 1), x = delta / 2, lower = FALSE))

      numer[numer < .Machine$double.xmin] <- .Machine$double.xmin
      den[den < .Machine$double.xmin] <- .Machine$double.xmin

      w[, j] <- numer / den
    }

    # M - STEP

    for (j in 1:k) {
      mu[, j] <- colSums(w[, j] * z[, j] / sum(w[, j] * z[, j]) * X)
    }

    if (k == 1) {
      prior <- 1
    } else {
      prior <- colMeans(z)
    }

    for (j in 1:k) {
      for (i in 1:num) {
        tempW[, , i] <- (z[i, j] * w[i, j]) * (X[i, ] - mu[, j]) %*% t(X[i, ] - mu[, j])
      }

      W[, , j] <- rowSums(tempW, dims = 2)
    }

    for (j in 1:k) {
      if (model == "EII") {
        phi <- tr(rowSums(W, dims = 2)) / (num * d)

        for (j in 1:k) {
          Sigma[, , j] <- phi * diag(1, d, d)
        }
      }

      if (model == "VII") {
        for (j in 1:k) {
          phi.k[j] <- tr(W[, , j]) / (d * sum(z[, j]))
          Sigma[, , j] <- phi.k[j] * diag(1, d, d)
        }
      }

      if (model == "EEI") {
        deltaU <- diag(diag(rowSums(W, dims = 2)), d, d) / (det(diag(diag(rowSums(W, dims = 2)), d, d)))^(1 / d)

        phi <- (det(diag(diag(rowSums(W, dims = 2)), d, d)))^(1 / d) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * deltaU
        }
      }

      if (model == "VEI") {
        for (j in 1:k) {
          temp.numdelta[, , j] <- (1 / phi.k[j]) * W[, , j]
        }

        deltaU <- diag(diag(rowSums(temp.numdelta, dims = 2)), d, d) / (det(diag(diag(rowSums(temp.numdelta, dims = 2)), d, d)))^(1 / d)

        for (j in 1:k) {
          phi.k[j] <- (tr(solve(deltaU) %*% W[, , j])) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU
        }
      }

      if (model == "EVI") {
        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(W[, , j]), d, d) / (det(diag(diag(W[, , j]), d, d)))^(1 / d)

          temp.numdelta[, , j] <- det(diag(diag(W[, , j]), d, d))^(1 / d)
        }

        phi <- rowSums(temp.numdelta, dims = 2) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * deltaU.k[, , j]
        }
      }

      if (model == "VVI") {
        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(W[, , j]), d, d) / (det(diag(diag(W[, , j]), d, d)))^(1 / d)

          phi.k[j] <- det(diag(diag(W[, , j]), d, d))^(1 / d) / (sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU.k[, , j]
        }
      }

      if (model == "EEE") {
        for (j in 1:k) {
          Sigma[, , j] <- rowSums(W, dims = 2) / (num)
        }
      }

      if (model == "VEE") {
        for (j in 1:k) {
          temp.numdelta[, , j] <- (1 / phi.k[j]) * W[, , j]
        }

        deltaU <- rowSums(temp.numdelta, dims = 2) / ((det(rowSums(temp.numdelta, dims = 2)))^(1 / d))

        for (j in 1:k) {
          phi.k[j] <- tr(solve(deltaU) %*% W[, , j]) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * deltaU
        }
      }

      if (model == "EVE") {
        while (check2 < 1) {
          for (j in 1:k) {
            ftemp.r[, , j] <- solve(deltaU.k[, , j]) %*% (t(gammaU) %*% W[, , j]) - max(base::eigen(W[, , j])$values) * (solve(deltaU.k[, , j]) %*% t(gammaU))
          }

          f <- rowSums(ftemp.r, dims = 2)

          MM.r.new <- tr(f %*% gammaU)

          if ((abs(MM.r.new - MM.r.old)) < tol2) {
            check2 <- 1
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          } else {
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          }

          MM.r.old <- MM.r.new
        }

        check2 <- 0
        MM.r.old <- -Inf

        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d) / (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d)))^(1 / d)

          numphi[j] <- tr(gammaU %*% solve(deltaU.k[, , j]) %*% t(gammaU) %*% W[, , j])
        }

        phi <- sum(numphi) / (num * d)

        for (j in 1:k) {
          Sigma[, , j] <- phi * gammaU %*% deltaU.k[, , j] %*% t(gammaU)
        }
      }

      if (model == "VVE") {
        while (check2 < 1) {
          for (j in 1:k) {
            ftemp.r[, , j] <- solve(deltaU.k[, , j]) %*% (t(gammaU) %*% W[, , j]) - max(base::eigen(W[, , j])$values) * (solve(deltaU.k[, , j]) %*% t(gammaU))
          }

          f <- rowSums(ftemp.r, dims = 2)

          MM.r.new <- tr(f %*% gammaU)

          if ((abs(MM.r.new - MM.r.old)) < tol2) {
            check2 <- 1
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          } else {
            res.svd <- base::svd(f)
            gammaU <- (res.svd$v) %*% t(res.svd$u)
          }

          MM.r.old <- MM.r.new
        }

        check2 <- 0
        MM.r.old <- -Inf

        for (j in 1:k) {
          deltaU.k[, , j] <- diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d) / (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d)))^(1 / d)
          phi.k[j] <- (det(diag(diag(t(gammaU) %*% W[, , j] %*% gammaU), d, d))^(1 / d)) / (sum(z[, j]))
          Sigma[, , j] <- phi.k[j] * gammaU %*% deltaU.k[, , j] %*% t(gammaU)
        }
      }

      if (model == "EEV") {
        for (j in 1:k) {
          tempW_EEV[[j]] <- base::eigen(W[, , j])

          gammaU.k[, , j] <- tempW_EEV[[j]][["vectors"]]

          tempomega[, , j] <- diag(tempW_EEV[[j]][["values"]], d, d)
        }

        deltaU <- rowSums(tempomega, dims = 2) / ((det(rowSums(tempomega, dims = 2)))^(1 / d))

        phi <- ((det(rowSums(tempomega, dims = 2)))^(1 / d)) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * gammaU.k[, , j] %*% deltaU %*% t(gammaU.k[, , j])
        }
      }

      if (model == "VEV") {
        for (j in 1:k) {
          tempW_EEV[[j]] <- base::eigen(W[, , j])

          gammaU.k[, , j] <- tempW_EEV[[j]][["vectors"]]

          tempomega[, , j] <- diag(tempW_EEV[[j]][["values"]], d, d)

          temp.numdelta[, , j] <- (1 / phi.k[j]) * tempomega[, , j]
        }

        deltaU <- rowSums(temp.numdelta, dims = 2) / ((det(rowSums(temp.numdelta, dims = 2)))^(1 / d))

        for (j in 1:k) {
          phi.k[j] <- tr(tempomega[, , j] %*% solve(deltaU)) / (d * sum(z[, j]))

          Sigma[, , j] <- phi.k[j] * gammaU.k[, , j] %*% deltaU %*% t(gammaU.k[, , j])
        }
      }

      if (model == "EVV") {
        for (j in 1:k) {
          V_EVV.U.K[, , j] <- W[, , j] / ((det(W[, , j]))^(1 / d))

          numphi[j] <- det(W[, , j])^(1 / d)
        }

        phi <- sum(numphi) / (num)

        for (j in 1:k) {
          Sigma[, , j] <- phi * V_EVV.U.K[, , j]
        }
      }

      if (model == "VVV") {
        for (j in 1:k) {
          Sigma[, , j] <- W[, , j] / (sum(z[, j]))
        }
      }
    }

    if (theta.model == "V") {
      for (j in 1:k) {
        theta[j] <- Mstep_AECM(X = X, weights = z[, j], mu = mu[, j], Sigma = Sigma[, , j], theta.model = theta.model, formula = formula)
      }
    }

    if (theta.model == "E") {
      theta <- rep(Mstep_AECM(X = X, k = k, weights = z, mu = mu, Sigma = Sigma, formula = formula, theta.model = theta.model), k)
    }

    # component density

    for (j in 1:k) {
      dens[, j] <- dmtin(x = X, mu = mu[, j], Sigma = Sigma[, , j], theta = theta[j], formula = formula)
    }

    dens[dens < .Machine$double.xmin] <- .Machine$double.xmin

    # mixture density

    numerator <- matrix(rep(prior, num), num, k, byrow = TRUE) * dens
    mixt.dens <- rowSums(numerator)
    loglik.new <- sum(log(mixt.dens))
    ll <- c(ll, loglik.new)

    # stopping rule

    if ((loglik.new - loglik.old) < tol) {
      check <- 1
    }

    if (m.iter >= maxit) {
      check <- 1
    }

    if (loglik.new < loglik.old) {
      mark <- 1
    } else {
      mark <- 0
    }

    loglik.old <- loglik.new
  })

  if (verbose == TRUE) {
    plot(ll, type = "l", xlab = "Iterations", ylab = "Log-Likelihoods")
  }

  if (k == 1) {
    classification <- rep(1, num)
  } else {
    colnames(z) <- c(1:k)
    classification <- as.numeric(colnames(z)[max.col(z, ties.method = "first")])
  }

  # Number of parameters

  npar.Mean <- d * k

  if (model == "EII") {
    npar.Cov <- 1
  }
  if (model == "VII") {
    npar.Cov <- k
  }
  if (model == "EEI") {
    npar.Cov <- d
  }
  if (model == "VEI") {
    npar.Cov <- k + (d - 1)
  }
  if (model == "EVI") {
    npar.Cov <- 1 + k * (d - 1)
  }
  if (model == "VVI") {
    npar.Cov <- k * d
  }
  if (model == "EEE") {
    npar.Cov <- d * (d + 1) / 2
  }
  if (model == "VEE") {
    npar.Cov <- k + d - 1 + d * (d - 1) / 2
  }
  if (model == "EVE") {
    npar.Cov <- 1 + k * (d - 1) + d * (d - 1) / 2
  }
  if (model == "VVE") {
    npar.Cov <- k * d + d * (d - 1) / 2
  }
  if (model == "EEV") {
    npar.Cov <- d + k * d * (d - 1) / 2
  }
  if (model == "VEV") {
    npar.Cov <- k + (d - 1) + k * d * (d - 1) / 2
  }
  if (model == "EVV") {
    npar.Cov <- 1 + k * (d - 1) + k * d * (d - 1) / 2
  }
  if (model == "VVV") {
    npar.Cov <- k * d * (d + 1) / 2
  }

  if (theta.model == "V") {
    npar <- npar.Mean + npar.Cov + k + (k - 1)
  } else {
    npar <- npar.Mean + npar.Cov + 1 + (k - 1)
  }

  # Information Criteria

  BIC <- -2 * loglik.new + log(num) * npar # to be minimized

  hard <- mclust::unmap(classification = classification)
  softhard <- matrix(0, num, k)
  for (i in 1:num) {
    for (j in 1:k) {
      if (z[i, j] == 1) {
        z[i, j] <- 0.99999
      }
      if (z[i, j] == 0) {
        z[i, j] <- 0.00001
      }
      softhard[i, j] <- hard[i, j] * log(z[i, j])
    }
  }
  som <- rowSums(softhard)
  A <- sum(som)
  ICL <- BIC - 2 * A

  return(list(name = name, model = model, theta = theta.model, prior = prior, mu = mu, Sigma = Sigma, theta = theta, classification = classification, loglik = loglik.new, ll = ll, mark = mark, check = check, npar = npar, BIC = BIC, ICL = ICL))
}
