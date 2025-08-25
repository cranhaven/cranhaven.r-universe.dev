utils::globalVariables("K_i")
utils::globalVariables("K_pop")

#' Title Pearson-Aitken algorithm to calculate mean values in truncated multivariate normal distributions
#'
#' @param mu vector of means
#' @param lower vector of lower thresholds
#' @param upper vector of upper thresholds
#' @param covmat covariance matrix, contaning kinship coefficient and heritability on each entry (except diagnoal, which is 1 for full liabilities and h2 for genetic liabilities)
#' @param target_id ID of target individual (or genetic liability), i.e. rowname in covmat to return expected genetic liability for
#' @param K_i vector of stratified CIPs for each individual. Only used for estimating genetic liability under the mixture model.
#' @param K_pop vector of population CIPs. Only used for estimating genetic liability under the mixture model.
#'
#' @returns A list with two elements: est (expected genetic liability, given input data) and var (variance of genetic liability, given input data).
#'
#' @importFrom stats pnorm
#' @export
#'


PA_algorithm = function(mu, covmat, target_id, lower, upper, K_i = NA, K_pop = NA) {
  target_indx = which(target_id == rownames(covmat))
  if (length(target_indx) == 0) {
    stop("PA_algorithm: Target ID not found in covariance matrix")
  }

  # if target is not the first element, we will move it to the first index
  if (target_indx != 1) {
    # keeping order as-is, except target entry is move to first entry
    ordr = c(target_indx, 1:nrow(covmat)[-target_indx])
    covmat = covmat[ordr, ordr]
    mu = mu[ordr]
    lower = lower[ordr]
    upper = upper[ordr]
    K_i = K_i[ordr]
    K_pop = K_pop[ordr]
  }
  for (i in length(mu):2) { # length(m) - 1 iterations, since we will return last entry
    # we will always take the last entry
    # mean and variance of last entry individual is calculated given information on them:
    update = tnorm_mixture_conditional(mu = mu[i],
                                       var = covmat[i, i],
                                       lower = lower[i],
                                       upper = upper[i],
                                       K_i = K_i[i],
                                       K_pop = K_pop[i])

    # updating mean and variances of all other individuals conditional on last entry values
    mu = mu[-i] + covmat[-i, i] %*% solve(covmat[i, i]) * (update$mean - mu[i])
    covmat = covmat[-i, -i] - covmat[-i, i] %*% (solve(covmat[i, i]) - solve(covmat[i, i]) %*% update$var %*% solve(covmat[i, i])) %*% covmat[i, -i]
    # note: one dimension is "lost" after each iteration, meaning final result is one-dimentional
  }
  return(tibble(est = as.vector(mu), var = as.vector(covmat)))
}




# PA_updates = function(mu, covmat, target_id, lower, upper, mixture_prob) {
#
#   if (!any(target_id == rownames(covmat))) {
#     stop("Target ID not found in covariance matrix")
#   }
#   # ensures order is the same across input
#   target_indx = which(target_id == rownames(covmat))
#   ord <- order(-mixture_prob[-target_indx], -covmat[-target_indx, target_indx], -rowSums(covmat[-target_indx, , drop = F])) # will probably leave this as-is.
#
#   # keeping order as-is, except target entry is move to first entry
#   ordr = c(target_indx, 1:nrow(covmat)[-target_indx])
#   covmat = covmat[ordr, ordr]
#   mu = mu[ordr]
#   lower = lower[ordr]
#   upper = upper[ordr]
#   mixture_prob = mixture_prob[ordr]
#
#   print(mu)
#   print(covmat)
#   print(lower)
#   print(upper)
#   print(mixture_prob)
#   cat("----- starting PA algorithm ----- \n")
#   for (i in length(mu):2) { # length(m) - 1 iterations, since we will return last entry
#     cat("----- Conditioning on entry: ", i, " ----- \n")
#     # we will always take the last entry
#     # mean and variance of last entry individual is calculated given information on them:
#     update = tnorm_mixture_conditional(mu = mu[i],
#                                        var = covmat[i, i],
#                                        lower = lower[i],
#                                        upper = upper[i],
#                                        Kp = mixture_prob[i] * (1 - pnorm(upper[i])))
#
#     # updating mean and variances of all other individuals conditional on last entry values
#     mu = mu[-i] + covmat[-i, i] %*% solve(covmat[i, i]) * (update$mean - mu[i])
#     covmat = covmat[-i, -i] - covmat[-i, i] %*% (solve(covmat[i, i]) - solve(covmat[i, i]) %*% update$var %*% solve(covmat[i, i])) %*% covmat[i, -i]
#     # note: one dimention is "lost" after each iteration, meaning final result is one-dimentional
#     print(mu)
#     print(covmat)
#   }
#   return(c(postM = as.vector(mu), postVar = as.vector(covmat)))
# }
