utils::globalVariables(c("Var1","Var2","X1","X2","value")) #, package="valse")

#' Plot
#'
#' A function which plots relevant parameters.
#'
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param model the model constructed by valse procedure
#' @param comp TRUE to enable pairwise clusters comparison
#' @param k1 index of the first cluster to be compared
#' @param k2 index of the second cluster to be compared
#'
#' @importFrom ggplot2 ggplot aes ggtitle geom_tile geom_line scale_fill_gradient2 geom_boxplot theme
#' @importFrom cowplot background_grid
#' @importFrom reshape2 melt
#'
#' @return No return value (only plotting).
#'
#' @export
plot_valse <- function(X, Y, model, comp = FALSE, k1 = NA, k2 = NA)
{
  n <- nrow(X)
  K <- length(model$pi)
  ## regression matrices
  gReg <- list()
  for (r in 1:K) {
    Melt <- reshape2::melt(t((model$phi[, , r])))
    gReg[[r]] <- ggplot2::ggplot(data = Melt, ggplot2::aes(x = Var1, y = Var2, fill = value))  +
      ggplot2::geom_tile() + ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
      midpoint = 0, space = "Lab") + ggplot2::ggtitle(paste("Regression matrices in cluster", r))
  }
  print(gReg)

  ## Differences between two clusters
  if (comp) {
    if (is.na(k1) || is.na(k2))
      print("k1 and k2 must be integers, representing the clusters you want to compare")
    Melt <- reshape2::melt(t(model$phi[, , k1] - model$phi[, , k2]))
    gDiff <- ggplot2::ggplot(data = Melt, ggplot2::aes(x = Var1, y = Var2, fill = value)) + 
      ggplot2::geom_tile() + ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
        space = "Lab") + ggplot2::ggtitle(paste("Difference between regression matrices in cluster",
        k1, "and", k2))
    print(gDiff)
  }

  ### Covariance matrices
  matCov <- matrix(NA, nrow = dim(model$rho[, , 1])[1], ncol = K)
  for (r in 1:K)
    matCov[, r] <- diag(model$rho[, , r])
  MeltCov <- reshape2::melt(matCov)
  gCov <- ggplot2::ggplot(data = MeltCov, ggplot2::aes(x = Var1, y = Var2, fill = value)) + ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
      space = "Lab") + ggplot2::ggtitle("Covariance matrices (diag., one row per cluster)")
  print(gCov)

  ### Proportions
  gam2 <- matrix(NA, ncol = 2, nrow = n)
  for (i in 1:n)
    gam2[i, ] <- c(model$proba[i, model$affec[i]], model$affec[i])

  bp <- ggplot2::ggplot(data.frame(gam2), ggplot2::aes(x = X2, y = X1, color = X2, group = X2)) + ggplot2::geom_boxplot() +
     ggplot2::theme(legend.position = "none") + cowplot::background_grid(major = "xy", minor = "none")  + 
    ggplot2::ggtitle("Assignment boxplot per cluster")
  print(bp)
}
