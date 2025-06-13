convertmuVecToMat <- function(muvec, dim){
  mumat <- matrix(0, dim, dim)
  mumat[lower.tri(mumat, diag=TRUE)] <- muvec
  mumat <- rbind(mumat, rep(0, dim))
  mumat
}

phiinvall <- function(logu, mu){
  Minv <- ginv(mu[-nrow(mu),])
  sweep(logu %*% t(Minv), 2, 0.5 * (Minv %*% rowSums(mu**2)[-nrow(mu)]), "+")
}
# 
# probatz <- function(y, mu, prop)
#   rowSums(sapply(as.list(1:ncol(y)),
#                  function(h, y, mu) dnorm(y[,h], mu[h], log=TRUE),
#                  y=y,
#                  mu=mu)) + log(prop)
# 
# 
# 
# gradphiinvhh <- function(yi, mumatupref, h)
#   c((mumatupref[h,1:h] - yi[1:h])/mumatupref[h,h], rep(0, length(yi) - h))
# 
# computegradPhiInv <- function(yi, mumatupref){
#   listgradphiinvhkl <- list()
#   dim <- ncol(mumatupref)
#   for (h in 1:dim){
#     listgradphiinvhkl[[h]] <- matrix(0, dim+1, dim)
#     listgradphiinvhkl[[h]][h,] <- gradphiinvhh(yi, mumatupref, h)
#     if (h>1)
#       listgradphiinvhkl[[h]] <- listgradphiinvhkl[[h]] -  matrix(rowSums(sapply(1:(h-1), function(hp) as.numeric(listgradphiinvhkl[[hp]] * mumatupref[h, hp]) )), dim+1, dim)/mumatupref[h,h]
# 
#   }
#   listgradphiinvhkl
# }
# 
# gradl1ind <- function(w, yi, mumatupref){
#   K <- ncol(mumatupref) + 1
#   gradphiinv <- computegradPhiInv(yi, mumatupref)
#   tmp <- sweep(mumatupref, 2, yi, "-")
#   delta <- sweep(tmp, 1, w, "*")
#   sumdelta <- colSums(delta)
#   delta[upper.tri(delta)] <- 0
#   out <- matrix(rowSums(sapply(1:(K-1), function(h) as.numeric(gradphiinv[[h]] * sumdelta[h]))), K, K-1) - delta
#   out[1:length(yi), ,drop=FALSE]
# }
# 
# 
# computeGradient <- function(muvec, prop, logu, tik){
#   n <- nrow(logu)
#   mumat <- convertmuVecToMat(muvec, ncol(logu))
#   y <- phiinvall(logu, mumat)
#   grr <-  - diag( n * sign(diag(mumat)) /diag(mumat))
#   for (i in 1:n)  grr <- grr + gradl1ind(tik[i,], y[i,], mumat)
#   grr[lower.tri(grr, diag=TRUE)]
# }
# 
# completelikelihood <- function(muvec, prop, logu, tik){
#   mu <- convertmuVecToMat(muvec, ncol(logu))
#   y <- phiinvall(logu, mu)
#   loglikecomplete <- rowSums(sapply(as.list(1:ncol(tik)),
#                                     function(k, y, mu, prop) probatz(y, mu[k,], prop[k]),
#                                     y=y,
#                                     mu=mu,
#                                     prop=prop) * tik)
#   logjacobian <- -  sum(log(abs(diag(mu)))) - rowSums(logu)
#   sum(loglikecomplete + logjacobian)
# }
# 
# 
# completelikelihoodCPP <- function(Rparam, Rprop, Rlogu, Rtik)
#   completelikelihood(Rparam, Rprop, Rlogu, Rtik)

dmixtmvnorm <- function(x, y, mu, prop)
  dnorm(x, mu[1]) * dnorm(y, mu[2]) * prop

