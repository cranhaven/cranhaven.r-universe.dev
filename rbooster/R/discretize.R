#' @title  Discretize
#'
#' @description Discretizes numeric variables
#'
#' @param xx matrix or data.frame whose variables needs to be discretized.
#' @param breaks number of categories for each variable. Ignored if \code{boundaries} != \code{NULL}.
#' @param boundaries user-defined upper and lower limit matrix of discretization
#' for each variable. Default is \code{NULL}.
#' @param categories user-defined category names for each variable. Default is \code{NULL}.
#' @param w sample weights for quantile calculation.
#' @details
#' Uses quantiles for discretization. However, quantiles may be equal in some cases.
#' Then equal interval discretization used instead.
#'
#' @return a list consists of:
#'  \item{x_discrete}{data.frame of discretized variables. Each variable is a factor.}
#'  \item{boundaries}{upper and lower limit matrix of discretization
#' for each variable.}
#'  \item{categories}{category names for each variable.}
#'
#' @author Fatih Saglam, fatih.saglam@omu.edu.tr
#' @export

discretize <- function(xx, breaks = 3, boundaries = NULL, categories = NULL, w = NULL) {
  n <- nrow(xx)
  p <- ncol(xx)

  x_discrete <- data.frame(matrix(NA, nrow = n, ncol = p))

  if (is.null(w)) {
    w <- rep(1/n, n)
  }

  w <- w/sum(w)*n
  discretization <- FALSE

  if (is.null(boundaries)) {
    boundaries <- matrix(NA, nrow = breaks + 1, ncol = p)
    bb = TRUE
  } else {
    bb = FALSE
  }
  if (is.null(categories)) {
    categories <- list()
    cc = TRUE
  } else {
    cc = FALSE
  }

  for (i in 1:p) {
    if (bb) {
      boundaries[,i] <- Hmisc::wtd.quantile(x = xx[,i], probs = seq(from = 0, to = 1, length.out = breaks + 1), weights = w)
      if (length(unique(boundaries[,i])) < (breaks + 1)) {
        boundaries[,i] <- seq(min(xx[,i]), max(xx[,i]), length.out = breaks + 1)
      }
    }
    if (cc) {
      categories[[i]] <- sapply(1:(breaks), function(m){
        dat <- boundaries[,i]
        paste(ifelse(m == 1, "(", "["),
              dat[m],
              ",",
              dat[m],
              ")", sep = "", collapse = "")
      })
    }

    for (j in 1:breaks) {
      if (j == 1) {
        x_discrete[xx[,i] < boundaries[j + 1,i],i] <- categories[[i]][j]
      } else {
        if (j != breaks) {
          x_discrete[xx[,i] >= boundaries[j,i] & xx[,i] < boundaries[j + 1,i],i] <- categories[[i]][j]
        } else {
          x_discrete[xx[,i] >= boundaries[j,i],i] <- categories[[i]][j]
        }
      }
    }
  }

  for (i in 1:p) {
    x_discrete[,i] <- factor(x_discrete[,i], levels = categories[[i]], labels = categories[[i]])
  }

  return(list(x_discrete = x_discrete,
              boundaries = boundaries,
              categories = categories))
}
