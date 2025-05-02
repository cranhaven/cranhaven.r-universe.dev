#' @title ADSIHT in multi-task learning framework
#'
#' @description An implementation of the sparse group selection in linear regression model via ADSIHT.
#'
#' @param x_list The list of input matrix.
#' @param y_list The list of response variable.
#' @param group_list A vector indicating which group each variable belongs to
#' For variables in the same group, they should be located in adjacent columns of \code{x}
#' and their corresponding index in \code{group} should be the same.
#' Denote the first group as \code{1}, the second \code{2}, etc.
#' @param s0 A vector that controls the degrees with group.
#' Default is \eqn{d^((l-1)/(L-1))} : \eqn{1 \leq l \leq L}, where d is the maximum group size.
#' @param kappa A parameter that controls the rapid of the decrease of threshold. Default is 0.9.
#' @param ic.type The type of criterion for choosing the support size.
#' Available options are \code{"dsic"}, \code{"loss"}.
#' Default is \code{"dsic"}.
#' @param ic.scale A non-negative value used for multiplying the penalty term
#' in information criterion. Default: \code{ic.scale = 3}.
#' @param ic.coef A non-negative value used for multiplying the penalty term
#' for choosing the optimal stopping time. Default: \code{ic.coef = 3}.
#' @param L The length of the sequence of s0. Default: \code{L = 5}.
#' @param weight The weight of the samples, with the default value set to 1 for each sample.
#' @param coef1 A positive value to control the sub-optimal stopping time.
#' @param coef2 A positive value to control the overall stopping time. A small value leads to larger search range.
#' @param eta A parameter controls the step size in the gradient descent step.
#' Default: \code{eta = 0.8}.
#' @param max_iter A parameter that controls the maximum number of line search, ignored if \code{OLS} is employed.
#' @param method Whether \code{ols} (default) or \code{linesearch} method should be employed.
#' @param center A boolean value indicating whether centralization is required. Default: \code{center = TRUE}.
#' @param scale A positive value to control the column-wise L2 norm of each observation matrix. Default: \code{scale=1}.
#'
#' @return A \code{list} object comprising:
#' \item{beta}{A \eqn{p}-by-\code{length(s0)} matrix of coefficients, stored in column format.}
#' \item{intercept}{A \code{length(s0)} vector of intercepts.}
#' \item{lambda}{A \code{length(s0)} vector of threshold values}
#' \item{A_out}{The selected variables given threshold value in \code{lambda}.}
#' \item{ic}{The values of the specified criterion for each fitted model given threshold \code{lamdba}.}
#'
#' @author Yanhang Zhang, Zhifan Li, Shixiang Liu, Jianxin Yin.
#'
#' @export
#'
#' @examples
#'set.seed(1)
#' n <- 200
#' p <- 100
#' K <- 4
#' s <- 5
#' s0 <- 2
#' x_list <- lapply(1:K, function(x) matrix(rnorm(n*p, 0, 1), nrow = n))
#'vec <- rep(0, K * p)
#'non_sparse_groups <- sample(1:p, size = s, replace = FALSE)
#'for (group in non_sparse_groups) {
#'  group_indices <- seq(group, K * p, by = p)
#'  non_zero_indices <- sample(group_indices, size = s0, replace = FALSE)
#'  vec[non_zero_indices] <- rep(2, s0)
#'}
#' y_list <- lapply(1:K, function(i) return(
#'   y = x_list[[i]] %*% vec[((i-1)*p+1):(i*p)]+rnorm(n, 0, 0.5))
#' )
#' fit <- ADSIHT.ML(x_list, y_list)
#' fit$A_out[, which.min(fit$ic)]

ADSIHT.ML <- function(x_list, y_list, group_list,
                      s0,
                      kappa = 0.9,
                      ic.type = c("dsic", "loss"),
                      ic.scale = 3.0,
                      ic.coef = 3.0,
                      L = 5,
                      weight,
                      coef1 = 1,
                      coef2 = 1,
                      eta = 0.8,
                      max_iter = 20,
                      method = "ols",
                      center = TRUE,
                      scale = 1)
{
  if (length(x_list) != length(y_list)) stop("The length of x_list should be the same with y_list")
  K <- length(x_list)
  p <- ncol(x_list[[1]])
  n <- sapply(x_list, function(x) nrow(x))
  if (missing(weight)) weight <- rep(1, sum(n))
  if (missing(group_list)) group <- rep(1:p, K)

  #
  # if need centralization
  #

  if(center == TRUE){
    y_mean <- lapply(y_list, mean)
    y_list2 <- lapply(1:K, function(i) y_list[[i]]-y_mean[[i]])
    y_norm <- lapply(y_list2, function(x) { sqrt(sum(x^2)) })
    x_mean <- lapply(x_list, function(x) { apply(x, 2, mean) })
    x_list2 <- lapply(1:K, function(i) x_list[[i]]-x_mean[[i]])
    x_norm <- lapply(x_list2, function(x) {
      apply(x, 2, function(col) sqrt(sum(col^2)))
    })

    if(scale==1){
      y_new <- unlist(lapply(1:K, function(i) y_list2[[i]]/y_norm[[i]]))
      x_new <- as.matrix(  Matrix::bdiag(  lapply(1:K, function(i)
        t({apply(x_list2[[i]], 1, function(x) {x/x_norm[[i]]} )} )  )  )  )
    }else{
      y_new = unlist( y_list2 )
      x_new <- as.matrix(  Matrix::bdiag(  lapply(1:K, function(i)
        t({apply(x_list2[[i]], 1, function(x) {x/x_norm[[i]]})})  )  )  )* scale
    }
    orderGi <- order(group)
    x_new <- x_new[, orderGi]
    index <- seq(1, K*p, by = K)-1
    if (missing(s0)) {
      s0 <- max(table(group))^(seq(1, L-1, length.out = L)/(L-1))
    }
    inverse_order <- match(1:length(group), orderGi)
    ic.type <- match.arg(ic.type)
    ic_type <- switch(ic.type,
                      "loss" = 0,
                      "dsic" = 1
    )
    if (method == "ols") {
      method = TRUE
    } else {
      method = FALSE
    }
    res <- DSIHT_ML_Cpp(x_new, y_new, weight = weight, sequence = s0, ic_type = ic_type,
                        ic_scale = ic.scale, kappa = kappa, g_index = index, ic_coef = ic.coef,
                        coef1 = coef1, coef2 = coef2, eta = eta, max_iter = max_iter,
                        method = method, nor = FALSE)
    if(scale==1){
      beta <- apply(res$beta[inverse_order, ], 2, function(x) {
        temp <- rep(unlist(y_norm), each = p)
        x/unlist(x_norm)*temp
      })
    }else{
      beta <- apply(res$beta[inverse_order, ], 2, function(x) {
        x/unlist(x_norm)*scale })
    }
    intercept <- unlist(y_mean) - sapply(1:K, function(i) {
      ind <- (1+(i-1)*p):(i*p)
      unlist(x_mean)[ind] %*% beta[ind, ]
    })
    res$beta <- split(beta, col(beta))
    res$intercept <- split(intercept, row(intercept))
    res$A_out <- apply(beta, 2, function(x) {which(x!=0)})

  }else{

    #
    # if DO NOT NEED centralization
    #

    y_norm <- lapply(y_list, function(x) { sqrt(sum(x^2)) })
    x_norm <- lapply(x_list, function(x) {
      apply(x, 2, function(col) sqrt(sum(col^2)))
    })

    if(scale==1){
      y_new <- unlist(lapply(1:K, function(i) y_list[[i]]/y_norm[[i]]))
      x_new <- as.matrix(  Matrix::bdiag(  lapply(1:K, function(i)
        t({apply(x_list[[i]], 1, function(x) {x/x_norm[[i]]} )} )  )  )  )
    }else{
      y_new = unlist( y_list )
      x_new <- as.matrix(  Matrix::bdiag(  lapply(1:K, function(i)
        t({apply(x_list[[i]], 1, function(x) {x/x_norm[[i]]})})  )  )  )* scale
    }
    orderGi <- order(group)
    x_new <- x_new[, orderGi]
    index <- seq(1, K*p, by = K)-1
    if (missing(s0)) {
      s0 <- max(table(group))^(seq(1, L-1, length.out = L)/(L-1))
    }
    inverse_order <- match(1:length(group), orderGi)
    ic.type <- match.arg(ic.type)
    ic_type <- switch(ic.type,
                      "loss" = 0,
                      "dsic" = 1
    )
    if (method == "ols") {
      method = TRUE
    } else {
      method = FALSE
    }
    res <- DSIHT_ML_Cpp(x_new, y_new, weight = weight, sequence = s0, ic_type = ic_type,
                        ic_scale = ic.scale, kappa = kappa, g_index = index, ic_coef = ic.coef,
                        coef1 = coef1, coef2 = coef2, eta = eta, max_iter = max_iter,
                        method = method, nor = FALSE)
    if(scale==1){
      beta <- apply(res$beta[inverse_order, ], 2, function(x) {
        temp <- rep(unlist(y_norm), each = p)
        x/unlist(x_norm)*temp
      })
    }else{
      beta <- apply(res$beta[inverse_order, ], 2, function(x) {
        x/unlist(x_norm)*scale })
    }
    res$beta <- split(beta, col(beta))
    res$A_out <- apply(beta, 2, function(x) {which(x!=0)})
  }
  return(res)
}
