#' constructionModelesLassoRank
#'
#' Construct a collection of models with the Lasso-Rank procedure.
#'
#' @param S output of selectVariables.R
#' @param k number of components
#' @param mini integer, minimum number of iterations in the EM algorithm, by default = 10
#' @param maxi integer, maximum number of iterations in the EM algorithm, by default = 100
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param eps real, threshold to say the EM algorithm converges, by default = 1e-4
#' @param rank.min integer, minimum rank in the low rank procedure, by default = 1
#' @param rank.max integer, maximum rank in the low rank procedure, by default = 5
#' @param ncores Number of cores, by default = 3
#' @param fast TRUE to use compiled C code, FALSE for R code only
#' @param verbose TRUE to show some execution traces
#'
#' @return a list with several models, defined by phi (the regression parameter reparametrized),
#' rho (the covariance parameter reparametrized), pi (the proportion parameter is the mixture model), llh
#' (the value of the loglikelihood function for this estimator on the training dataset). The list is given
#' for several levels of sparsity, given by several regularization parameters computed automatically,
#' and several ranks (between rank.min and rank.max).
#'
#' @export
constructionModelesLassoRank <- function(S, k, mini, maxi, X, Y, eps, rank.min, rank.max,
  ncores, fast, verbose)
{
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)
  L <- length(S)

  # Possible interesting ranks
  deltaRank <- rank.max - rank.min + 1
  Size <- deltaRank^k
  RankLambda <- matrix(0, nrow = Size * L, ncol = k + 1)
  for (r in 1:k)
  {
    # On veut le tableau de toutes les combinaisons de rangs possibles, et des
    # lambdas Dans la premiere colonne : on repete (rank.max-rank.min)^(k-1) chaque
    # chiffre : ca remplit la colonne Dans la deuxieme : on repete
    # (rank.max-rank.min)^(k-2) chaque chiffre, et on fait ca (rank.max-rank.min)^2
    # fois ...  Dans la derniere, on repete chaque chiffre une fois, et on fait ca
    # (rank.min-rank.max)^(k-1) fois.
    RankLambda[, r] <- rep(rank.min + rep(0:(deltaRank - 1), deltaRank^(r - 1),
      each = deltaRank^(k - r)), each = L)
  }
  RankLambda[, k + 1] <- rep(1:L, times = Size)

  if (ncores > 1)
  {
    cl <- parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl, envir = environment(), varlist = c("A1", "Size",
      "Pi", "Rho", "mini", "maxi", "X", "Y", "eps", "Rank", "m", "phi", "ncores",
      "verbose"))
  }

  computeAtLambda <- function(index)
  {
    lambdaIndex <- RankLambda[index, k + 1]
    rankIndex <- RankLambda[index, 1:k]
    if (ncores > 1)
      require("valse")  #workers start with an empty environment

    # 'relevant' will be the set of relevant columns
    selected <- S[[lambdaIndex]]$selected
    relevant <- c()
    for (j in 1:p)
    {
      if (length(selected[[j]]) > 0)
        relevant <- c(relevant, j)
    }
    if (max(rankIndex) < length(relevant))
    {
      phi <- array(0, dim = c(p, m, k))
      if (length(relevant) > 0)
      {
        res <- EMGrank(S[[lambdaIndex]]$Pi, S[[lambdaIndex]]$Rho, mini, maxi,
          X[, relevant], Y, eps, rankIndex, fast)
        llh <- c(res$LLF, sum(rankIndex * (length(relevant) - rankIndex + m)))
        phi[relevant, , ] <- res$phi
      }
      list(llh = llh, phi = phi, pi = S[[lambdaIndex]]$Pi, rho = S[[lambdaIndex]]$Rho)
    }
  }

  # For each lambda in the grid we compute the estimators
  out <-
    if (ncores > 1) {
      parallel::parLapply(cl, seq_len(length(S) * Size), computeAtLambda)
    } else {
      lapply(seq_len(length(S) * Size), computeAtLambda)
    }

  if (ncores > 1)
    parallel::stopCluster(cl)

  out
}
