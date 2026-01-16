#' Fitting for parsimonious mixtures of MSEN or MTIN distributions
#'
#' Fits, by using EM-based algorithms, parsimonious mixtures of MSEN or MTIN distributions to the given data.
#' Parallel computing is implemented and highly recommended for a faster model fitting. The Bayesian
#' information criterion (BIC) and the integrated completed likelihood (ICL) are used to select the best
#' fitting models according to each information criterion.
#'
#' @param X A data matrix with \code{n} rows and \code{d} columns, being \code{n} the number of data points and \code{d} the data the dimensionality.
#' @param k An integer or a vector indicating the number of groups of the models to be estimated.
#' @param init.par The initial values for starting the algorithms, as produced by the \code{Mixt.fit.init()} function.
#' @param cov.model A character vector indicating the parsimonious structure of the scale matrices. Possible values are:
#'     "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VEE", "EVE", "EEV", "VVE", "VEV", "EVV", "VVV" or "all".
#'     When "all" is used, all of the 14 parsimonious structures are considered.
#' @param theta.model A character vector indicating the parsimonious structure of the tailedness parameters. Possible values are:
#'     "E", "V" or "all". When "all" is used, both parsimonious structures are considered.
#' @param density A character indicating the density of the mixture components. Possible values are: "MSEN" or "MTIN".
#' @param ncores A positive integer indicating the number of cores used for running in parallel.
#' @param verbose A logical indicating whether the running output should be displayed.
#' @param ret.all A logical indicating whether to report the results of all the models or only those of the best models according to BIC and ICL.
#'
#' @return A list with the following elements:
#' \item{all.models}{The results related to the all the fitted models (only when \code{ret.all=TRUE}).}
#' \item{BicWin}{The best fitting model according to the BIC.}
#' \item{IclWin}{The best fitting model according to the ICL.}
#' \item{Summary}{A quick table showing summary results for the best fitting models according to BIC and ICL.}
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
#'
#' theta.model <- "E"
#' res <- Mixt.fit(X, k, init.par, cov.model, theta.model, density)
Mixt.fit <- function(X, k = 1:3, init.par = NULL, cov.model = "all", theta.model = "all", density, ncores = 1, verbose = FALSE, ret.all = FALSE) {
  if (!is.matrix(X)) {
    base::stop("Provided data are not in a matrix format.")
  }

  if (any(cov.model == "all")) {
    model.c <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VEE", "EVE", "EEV", "VVE", "VEV", "EVV", "VVV")
  } else {
    model.c <- cov.model
  }

  if (any(theta.model == "all")) {
    model.t <- c("E", "V")
  } else {
    model.t <- theta.model
  }

  model <- expand.grid(model.c, model.t)
  tot <- as.numeric(nrow(model))
  l <- NULL

  oper <- vector(mode = "list", length = length(k))

  if (density == "MSEN") {
    for (t in 1:length(k)) {
      if (verbose == TRUE) {
        if (length(k) == 1) {
          print(paste("Fitting Parsimonious Multivariate Sen Mixtures with k =", k))
        } else {
          print(paste("Fitting Parsimonious Multivariate Sen Mixtures with k =", k[t]))
        }
      }

      cluster <- snow::makeCluster(ncores, type = "SOCK")
      doSNOW::registerDoSNOW(cluster)

      pb <- utils::txtProgressBar(max = tot, style = 3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      oper[[t]] <- foreach::foreach(l = 1:tot, .export = c("Mix_Sen"), .combine = "comb", .multicombine = TRUE, .init = list(list()), .options.snow = opts) %dopar% {
        res <- tryCatch(Mix_Sen(X = X, k = k[t], init.par = init.par[[t]], model = as.character(model[l, 1]), theta.model = as.character(model[l, 2]), verbose = FALSE), error = function(e) {
          NA
        })

        list(res)
      }

      snow::stopCluster(cluster)
      foreach::registerDoSEQ()
      close(pb)
    }
  }

  if (density == "MTIN") {
    for (t in 1:length(k)) {
      if (verbose == TRUE) {
        if (length(k) == 1) {
          print(paste("Fitting Parsimonious Multivariate Tin Mixtures with k =", k))
        } else {
          print(paste("Fitting Parsimonious Multivariate Tin Mixtures with k =", k[t]))
        }
      }

      cluster <- snow::makeCluster(ncores, type = "SOCK")
      doSNOW::registerDoSNOW(cluster)

      pb <- utils::txtProgressBar(max = tot, style = 3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      oper[[t]] <- foreach::foreach(l = 1:tot, .export = c("Mix_Tin"), .combine = "comb", .multicombine = TRUE, .init = list(list()), .options.snow = opts) %dopar% {
        res <- tryCatch(Mix_Tin(X = X, k = k[t], init.par = init.par[[t]], model = as.character(model[l, 1]), theta.model = as.character(model[l, 2]), verbose = FALSE), error = function(e) {
          NA
        })

        list(res)
      }

      snow::stopCluster(cluster)
      foreach::registerDoSEQ()
      close(pb)
    }
  }

  BICres <- array(NA, dim = c(tot, 1, length(k)))
  ICLres <- array(NA, dim = c(tot, 1, length(k)))

  for (t in 1:length(k)) {
    for (m in 1:tot) {
      if (length(oper[[t]][[1]][[m]]) > 1) {
        if (oper[[t]][[1]][[m]][["mark"]] == 0 & oper[[t]][[1]][[m]][["check"]] == 1) {
          BICres[m, 1, t] <- oper[[t]][[1]][[m]][["BIC"]]
          ICLres[m, 1, t] <- oper[[t]][[1]][[m]][["ICL"]]
        } else {
          oper[[t]][[1]][[m]] <- NA
        }
      } else {
        BICres[m, 1, t] <- NA
        ICLres[m, 1, t] <- NA
      }
    }
  }

  df <- data.frame(matrix(NA, nrow = 2, ncol = 3), row.names = c("BIC", "ICL"))
  colnames(df) <- c("Model", "Value", "G")

  complete.model <- cbind(rep(as.character(model$Var1), length(k)), rep(as.character(model$Var2), length(k)))

  df[1, 1] <- paste(complete.model[which.min(BICres), 1], complete.model[which.min(BICres), 2], sep = "-")
  df[1, 2] <- min(BICres, na.rm = TRUE)

  if (length(k) == 1) {
    df[1, 3] <- k
  } else {
    df[1, 3] <- k[which(BICres == min(BICres, na.rm = TRUE), arr.ind = TRUE)[1, 3]]
  }

  df[2, 1] <- paste(complete.model[which.min(ICLres), 1], complete.model[which.min(ICLres), 2], sep = "-")
  df[2, 2] <- min(ICLres, na.rm = TRUE)

  if (length(k) == 1) {
    df[2, 3] <- k
  } else {
    df[2, 3] <- k[which(ICLres == min(ICLres, na.rm = TRUE), arr.ind = TRUE)[1, 3]]
  }

  b.w1 <- substr(df[1, 1], 1, 3)
  b.w2 <- substr(df[1, 1], 5, 5)
  i.w1 <- substr(df[2, 1], 1, 3)
  i.w2 <- substr(df[2, 1], 5, 5)

  if (length(k) == 1) {
    for (b in 1:length(oper[[1]][[1]])) {
      tryCatch(if (b.w1 == oper[[1]][[1]][[b]][[2]] & b.w2 == oper[[1]][[1]][[b]][[3]]) {
        BicWin <- oper[[1]][[1]][[b]]
      }, error = function(e) {})
    }

    for (i in 1:length(oper[[1]][[1]])) {
      tryCatch(if (i.w1 == oper[[1]][[1]][[i]][[2]] & i.w2 == oper[[1]][[1]][[i]][[3]]) {
        IclWin <- oper[[1]][[1]][[i]]
      }, error = function(e) {})
    }
  } else {
    slb <- which(BICres == min(BICres, na.rm = TRUE), arr.ind = TRUE)[1, 3]
    sli <- which(ICLres == min(ICLres, na.rm = TRUE), arr.ind = TRUE)[1, 3]

    for (b in 1:length(oper[[slb]][[1]])) {
      tryCatch(if (b.w1 == oper[[slb]][[1]][[b]][[2]] & b.w2 == oper[[slb]][[1]][[b]][[3]]) {
        BicWin <- oper[[slb]][[1]][[b]]
      }, error = function(e) {})
    }

    for (i in 1:length(oper[[sli]][[1]])) {
      tryCatch(if (i.w1 == oper[[sli]][[1]][[i]][[2]] & i.w2 == oper[[sli]][[1]][[i]][[3]]) {
        IclWin <- oper[[sli]][[1]][[i]]
      }, error = function(e) {})
    }
  }

  if (ret.all == FALSE) {
    return(list(BicWin = BicWin, IclWin = IclWin, Summary = df))
  } else {
    return(list(all.models = oper, BicWin = BicWin, IclWin = IclWin, Summary = df))
  }
}
