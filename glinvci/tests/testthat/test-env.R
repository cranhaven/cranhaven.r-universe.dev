test_this("Dynamic scoping", {
  set.seed(1)
  ntips = 200
  k     = 2                                # No. of trait dimensions
  tr    = ape::rtree(ntips)
  X     = matrix(rnorm(k*ntips), k, ntips) # Trait matrix
  x0    = rnorm(k)                         # Root value
  repar = get_restricted_ou(H='diag', theta=NULL, Sig=NULL, lossmiss='halt')
  my_par = function (par, ...) {
    phiwV = repar$par(par[1:7], ...)
    if (INFO__$node_id > 200)  ## If not tip just return the original
        return(phiwV)
    Sig_e = diag(par[8:9])     ## Our measurement error matrix
    phiwV[7:9] = phiwV[7:9] + Sig_e[lower.tri(Sig_e, diag=T)]
    phiwV
  }
  my_jac = function (par, ...) {
    new_jac = matrix(0.0, 9, 9)
    new_jac[,1:7] = repar$jac(par[1:7], ...)
    if (INFO__$node_id <= 200)
        new_jac[7,8] = new_jac[9,9] = 1.0
    new_jac
  }
  my_hess = function (par, ...)
    lapply(repar$hess(par[1:7], ...), function (H) {
        newH = array(0.0, dim=c(dim(H)[1], 9, 9))
        newH[,1:7,1:7] = H[,,]   ## Copy the original part
        newH                     ## Other entries are just zero
    })
  mod = glinv(tr, x0, X,
            pardims = 9,
            parfns  = my_par,
            parjacs = my_jac,
            parhess = my_hess)
  expect_type(lik(mod)(c(1,1,0,0,0,0,0,0.5,0.5)), 'double')
  expect_type(grad(mod)(c(1,1,0,0,0,0,0,0.5,0.5)), 'double')
  expect_type(hess(mod)(c(1,1,0,0,0,0,0,0.5,0.5)), 'double')
})

