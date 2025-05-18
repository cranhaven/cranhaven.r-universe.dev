#' Construct PGx PRS using penalized regression
#'
#' Shrink prognostic and predictive effect sizes simultaneously via the penalized term. With different assumptions on the relationship between the two effects, can be PRS-PGx-L (Lasso), PRS-PGx-GL (Group Lasso), and PRS-PGx-SGL (Sparse Group Lasso)
#' @param Y a numeric vector containing the quantitative trait
#' @param Tr a numeric vector containing the treatment assignment
#' @param G a numeric matrix containing genotype information
#' @param intercept a logical flag indicating should intercept be fitted (default=TRUE) or set to be FALSE
#' @param lambda a numeric value indicating the penalty
#' @param method a logical flag for different penalized regression methods: 1 = PRS-PGx-L, 2 = PRS-PGx-GL, 3 = PRS-PGx-SGL
#' @param alpha a numeric value indicating the mixing parameter (only used when method = 3). alpha = 1 is the lasso penalty. alpha = 0 is the group lasso penalty
#' @details PRS-PGx-Lasso requires individudal-level data
#' @return A numeric list, the first sublist contains estimated prognostic effect sizes, the second sublist contains estimated predictive effect sizes
#' @references Yang, Y. & Zou, H. A fast unified algorithm for solving group-lasso penalize learning problems. Statistics and Computing 25, 1129-1141 (2015).
#' @references Simon, N., Friedman, J., Hastie, T. & Tibshirani, R. Fit a GLM (or cox model) with a combination of lasso and group lasso regularization. R package version, 1 (2015).
#' @references Zhai, S., Zhang, H., Mehrotra, D.V. & Shen, J. Paradigm Shift from Disease PRS to PGx PRS for Drug Response Prediction using PRS-PGx Methods (submitted).
#' @author Song Zhai
#' @export
#' @examples
#' \donttest{
#' data(PRSPGx.example); attach(PRSPGx.example)
#' coef_est <- PRS_PGx_Lasso(Y, Tr, G, lambda = 1, method = 1)
#' summary(coef_est$coef.G)
#' summary(coef_est$coef.TG)
#' }
#'
PRS_PGx_Lasso <- function(Y, Tr, G, intercept = TRUE, lambda, method, alpha=0.5){
  G <- as.matrix(G)

  X <- cbind(G, Tr*G)
  group <- c(c(1:(ncol(G))), c(1:(ncol(G))))

  # perform Lasso ------------------------------------------------------------
  if(method == 1){
    fit <- glmnet(x=X, y=Y, family="gaussian", intercept = intercept, lambda = lambda, maxit = 100000, thresh = 0.00001)
    b.hat <- as.vector(coef(fit)); b.hat <- b.hat[-1]
  }

  # perform group Lasso
  if(method == 2){
    fit <- gglasso(x=X, y=Y, group=group, loss="ls", intercept = intercept, lambda = lambda, nlambda = 1, eps = 0.0001, maxit = 100000)
    b.hat <- as.vector(coef(fit)); b.hat <- b.hat[-1]
  }

  # perform sparse group Lasso
  if(method == 3){
    data <- list(x=X, y=Y)
    fit <- SGL(data, index = group, type = "linear", alpha = alpha, nlam = 1, lambdas = lambda, maxit = 100000, thresh = 0.01, standardize = TRUE)
    b.hat <- fit$beta
  }

  coef.G <- b.hat[1:(length(b.hat)/2)]
  coef.TG <- b.hat[(length(b.hat)/2+1):length(b.hat)]

  names(coef.G) <- names(coef.TG) <- colnames(G)

  re <- list(coef.G = coef.G, coef.TG = coef.TG)
  return(re)
}
