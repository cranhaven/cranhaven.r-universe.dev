#' Initialization for the EM-based algorithms
#'
#' Runs the initialization of the EM-based algorithms used for fitting parsimonious mixtures of MSEN or MTIN distributions.
#' Parallel computing is implemented and highly recommended for a faster calculation.
#'
#' @param X A data matrix with \code{n} rows and \code{d} columns, being \code{n} the number of data points and \code{d} the data the dimensionality.
#' @param k An integer or a vector indicating the number of groups of the models.
#' @param density A character indicating the density of the mixture components. Possible values are: "MSEN" or "MTIN".
#' @param nstartR An integer specifying the number of random starts to be considered.
#' @param ncores A positive integer indicating the number of cores used for running in parallel.
#' @param verbose A logical indicating whether the running output should be displayed.
#'
#' @return
#' \item{init}{A list of objects to be used by the \code{Mixt.fit()} function.}
#' @export
#' @importFrom foreach %dopar%
#' @examples
#' set.seed(1234)
#' n <- 50
#' k <- 2
#' Pi <- c(0.5, 0.5)
#' mu <- matrix(c(0, 0, 4, 5), 2, 2)
#' cov.model <- "EEE"
#' lambda <- c(0.5, 0.5)
#' delta <- c(0.7, 0.7)
#' gamma <- c(2.62, 2.62)
#' theta <- c(0.1, 0.1)
#' density <- "MSEN"
#' data <- rMixt(n, k, Pi, mu, cov.model, lambda, delta, gamma, theta, density)
#'
#' X <- data$X
#' nstartR <- 1
#' init.par <- Mixt.fit.init(X, k, density, nstartR)
Mixt.fit.init <- function(X, k = 1:3, density, nstartR = 100, ncores = 1, verbose = FALSE) {
  if (!is.matrix(X)) {
    base::stop("Provided data are not in a matrix format.")
  }

  num <- nrow(X)
  d <- ncol(X)
  l <- (num * d)^2
  if (l > .Machine$integer.max) {
    l <- .Machine$integer.max
  }
  oper0 <- vector(mode = "list", length = length(k))
  init <- vector(mode = "list", length = length(k))

  for (t in 1:length(k)) {
    init[[t]] <- vector(mode = "list", length = 6)
  }

  ## Initialization

  for (t in 1:length(k)) {
    if (verbose == TRUE) {
      if (length(k) == 1) {
        print(paste("Initializing", paste(density, paste("mixture models with k =", k))))
      } else {
        print(paste("Initializing", paste(density, paste("mixture models with k =", k[t]))))
      }
    }

    # Create some objects

    prior <- numeric(k[t])
    mu <- matrix(0, nrow = d, ncol = k[t])
    Sigma <- array(0, dim = c(d, d, k[t]))
    theta <- numeric(k[t])

    ## Random initialization ##

    eu <- matrix(0, nrow = num, ncol = k[t])
    classy <- numeric(num)
    ptllk <- rep(NA, k[t])
    wptllk <- rep(NA, k[t])
    set.seed(l)
    rand.start <- matrix(0, nstartR, k[t])
    rand.theta <- matrix(0, nstartR, k[t])

    withr::with_seed(l, {
      for (i in 1:nstartR) {
        rand.start[i, ] <- sample(c(1:num), k[t])

        if (density == "MSEN") {
          rand.theta[i, ] <- stats::runif(k[t], 0.05, 1)
        }
        if (density == "MTIN") {
          rand.theta[i, ] <- stats::runif(k[t], 0.5, 0.95)
        }
      }
    })

    cluster <- snow::makeCluster(ncores, type = "SOCK")
    doSNOW::registerDoSNOW(cluster)

    oper0[[t]] <- foreach::foreach(l = 1:nstartR, .export = c("dmsen", "dmtin"), .combine = "comb", .multicombine = TRUE, .init = list(list(), list(), list(), list(), list(), list())) %dopar% {
      sec <- rand.start[l, ]

      for (j in 1:k[t]) {
        mu[, j] <- X[sec[j], ]
      }

      for (j in 1:k[t]) {
        for (i in 1:num) {
          eu[i, j] <- TSdist::EuclideanDistance(X[i, ], mu[, j])
        }
      }

      for (i in 1:num) {
        classy[i] <- which.min(eu[i, ])
      }

      tempX <- vector(mode = "list", length = k[t])

      try(for (j in 1:k[t]) {
        temp.num <- dim(X[which(classy == j), ])[1]
        tempX[[j]] <- vector(mode = "list", length = temp.num)

        for (i in 1:temp.num) {
          tempX[[j]][[i]] <- X[which(classy == j)[i], ]
        }

        mu[, j] <- Reduce("+", tempX[[j]]) / temp.num

        Sigma[, , j] <- Reduce("+", lapply(1:length(tempX[[j]]), function(i) (X[which(classy == j)[i], ] - mu[, j]) %*% t(X[which(classy == j)[i], ] - mu[, j]))) / (d * temp.num)

        prior[j] <- temp.num / num

        if (density == "MSEN") {
          theta[j] <- rand.theta[l, j]

          ptllk[j] <- sum(log(dmsen(X[which(classy == j), ], mu[, j], Sigma[, , j], theta[j])))
          wptllk[j] <- sum(rep(log(prior[j]), temp.num))
        }
        if (density == "MTIN") {
          theta[j] <- rand.theta[l, j]

          ptllk[j] <- sum(log(dmtin(X[which(classy == j), ], mu[, j], Sigma[, , j], theta[j], formula = "direct")))
          wptllk[j] <- sum(rep(log(prior[j]), temp.num))
        }
      })

      pll <- try(sum(wptllk) + sum(ptllk))

      list(prior, mu, Sigma, theta, pll, classy)
    }

    snow::stopCluster(cluster)
    foreach::registerDoSEQ()
  }

  ## Preparing the results

  for (t in 1:length(k)) {
    spurious <- numeric(nstartR)
    for (g in 1:nstartR) {
      if (any(oper0[[t]][[1]][[g]] < 0.05) == TRUE) {
        spurious[g] <- NA
      }
      if (sum(oper0[[t]][[1]][[g]]) != 1) {
        spurious[g] <- NA
      }

      vr <- numeric(t)

      for (o in 1:t) {
        vr[o] <- tryCatch(sum(solve(oper0[[t]][[3]][[g]][, , o])), error = function(e) {
          NA
        })
      }

      if (any(is.na(vr))) {
        spurious[g] <- NA
      }
    }

    df <- data.frame(llk = unlist(oper0[[t]][[5]]), pos = c(1:nstartR), sp = spurious)
    df <- tidyr::drop_na(df)

    bestR <- utils::head(data.table::setorderv(df, cols = "llk", order = -1), n = 1)$pos

    for (i in 1:6) {
      init[[t]][[i]] <- oper0[[t]][[i]][bestR]
    }
  }

  return(init)
}
