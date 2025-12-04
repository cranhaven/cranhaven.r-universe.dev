Initial = function(t.data, t.lambda, A.data, A.lambda,
                   A.orac = NULL, method = "sepa", method.aux = "sepa",
                   TT=2, normalize = TRUE, mode.set = NULL){
  # Initial: the function calculating initial precision matrices of the target domain
  #          and covariance matrices of the auxiliary domain,
  #          via two alternative methods:
  #          "Tlasso" (PAMI, 2020) & "sepa" (JCGS, 2022)

  p.vec = dim(t.data)
  M = length(p.vec) - 1
  n.da = p.vec[M+1]
  p.vec = p.vec[-(M+1)]
  K = length(A.data)
  nA.vec = rep(0, K)
  for (k in 1:K) {
    p.vec.A = dim(A.data[[k]])
    nA.vec[k] = p.vec.A[length(p.vec.A)]
  }
  if(is.null(mode.set)){
    mode.set = 1:M
  } else {
    mode.set = mode.set
  }

  t0 <- proc.time()
  if(method == "sepa"){
    # Initialization in target domain
    t.Omega.hat.list = Separate.fit(t.data, lambda.vec=t.lambda, normalize = normalize)$Omegahat
  }

  if(method == "Tlasso"){
    # Initialization in target domain
    t.Omega.hat.list = Tlasso.fit(t.data, T=TT, lambda.vec = t.lambda, norm.type = 1+as.numeric(normalize))
  }

  if(method.aux == "sepa"){
    # Initialization in auxiliary domains
    A.Omega.hat.list = list()
    for (k in 1:K) {
      A.Omega.hat.list[[k]] = Separate.fit(A.data[[k]], lambda.vec=A.lambda[[k]], normalize = normalize)$Omegahat
    }
  }
  if(method.aux == "Tlasso"){
    # Initialization in auxiliary domains
    A.Omega.hat.list = list()
    for (k in 1:K) {
      A.Omega.hat.list[[k]] = Tlasso.fit(A.data[[k]], lambda.vec = A.lambda[[k]], norm.type = 1+as.numeric(normalize))
    }
  }

  A.S.hat.list0 = list()
  A.S.hat.list1 = list()
  for (k in 1:K) {
    A.S.hat.list = S.est(A.data[[k]], A.Omega.hat.list[[k]])
    A.S.hat.list0[[k]] = A.S.hat.list$sig0
    A.S.hat.list1[[k]] = A.S.hat.list$sig1
  }

  S.hat.A.M = list()
  for (m in mode.set) {
    mi = match(m, mode.set)
    S.hat.A.M[[mi]] = diag(p.vec[m]) - diag(p.vec[m])
  }
  # weight determined by the differences
  S.hat.A.M.diff0 = S.hat.A.M
  S.hat.A.M.diff1 = S.hat.A.M
  weight.KM0 = matrix(0, ncol = K, nrow = length(mode.set))
  weight.KM1 = matrix(0, ncol = K, nrow = length(mode.set))
  for (k in 1:K) {
    for (m in mode.set) {
      mi = match(m, mode.set)
      weight.KM0[mi,k] = 1/sum((A.S.hat.list0[[k]][[m]] %*% t.Omega.hat.list[[m]] - diag(p.vec[m]))^2)
      weight.KM1[mi,k] = 1/sum((A.S.hat.list1[[k]][[m]] %*% t.Omega.hat.list[[m]] - diag(p.vec[m]))^2)
    }
  }

  wed0 = t(t(weight.KM0)*nA.vec)
  alpha.k.diff0 = wed0 / apply(wed0, 1, sum)
  wed1 = t(t(weight.KM1)*nA.vec)
  alpha.k.diff1 = wed1 / apply(wed1, 1, sum)
  for (k in 1:K) {
    for (m in mode.set) {
      mi = match(m, mode.set)
      S.hat.A.M.diff0[[mi]] = S.hat.A.M.diff0[[mi]] + A.S.hat.list0[[k]][[m]] * alpha.k.diff0[mi,k]
      S.hat.A.M.diff1[[mi]] = S.hat.A.M.diff1[[mi]] + A.S.hat.list1[[k]][[m]] * alpha.k.diff1[mi,k]
    }
  }


  # weight determined by the sample sizes
  S.hat.A.M0 = S.hat.A.M
  S.hat.A.M1 = S.hat.A.M
  alpha.k = nA.vec/sum(nA.vec)
  for (k in 1:K) {
    for (m in mode.set) {
      mi = match(m, mode.set)
      S.hat.A.M0[[mi]] = S.hat.A.M0[[mi]] + A.S.hat.list0[[k]][[m]] * alpha.k[k]
      S.hat.A.M1[[mi]] = S.hat.A.M1[[mi]] + A.S.hat.list1[[k]][[m]] * alpha.k[k]
    }
  }
  t00 = proc.time() - t0


  if(sum(A.orac) > 0){
    S.hat.A.M0.o = S.hat.A.M
    S.hat.A.M1.o = S.hat.A.M
    alpha.k.o = nA.vec[A.orac]/sum(nA.vec[A.orac])
    A.S.hat.list0.o = list()
    A.S.hat.list1.o = list()
    for (k in A.orac) {
      ki = match(k, A.orac)
      A.S.hat.list.o = S.est(A.data[[k]], A.Omega.hat.list[[k]])
      A.S.hat.list0.o[[ki]] = A.S.hat.list.o$sig0
      A.S.hat.list1.o[[ki]] = A.S.hat.list.o$sig1
      for (m in mode.set) {
        mi = match(m, mode.set)
        S.hat.A.M0.o[[mi]] = S.hat.A.M0.o[[mi]] + A.S.hat.list0.o[[ki]][[m]] * alpha.k.o[ki]
        S.hat.A.M1.o[[mi]] = S.hat.A.M1.o[[mi]] + A.S.hat.list1.o[[ki]][[m]] * alpha.k.o[ki]
      }
    }
    Init.res = list(t.Omega.hat.list=t.Omega.hat.list, A.Omega.hat.list=A.Omega.hat.list,
                    S.hat.A.M0=S.hat.A.M0, S.hat.A.M1=S.hat.A.M1,
                    S.hat.A.M.diff0=S.hat.A.M.diff0, S.hat.A.M.diff1=S.hat.A.M.diff1,
                    A.S.hat.list0=A.S.hat.list0, A.S.hat.list1=A.S.hat.list1,
                    S.hat.A.M0.o=S.hat.A.M0.o, S.hat.A.M1.o=S.hat.A.M1.o,
                    A.S.hat.list0.o=A.S.hat.list0.o, A.S.hat.list1.o=A.S.hat.list1.o,
                    time = t00)
    return(Init.res)
  } else {
    Init.res = list(t.Omega.hat.list=t.Omega.hat.list, A.Omega.hat.list=A.Omega.hat.list,
                    S.hat.A.M0=S.hat.A.M0, S.hat.A.M1=S.hat.A.M1,
                    S.hat.A.M.diff0=S.hat.A.M.diff0, S.hat.A.M.diff1=S.hat.A.M.diff1,
                    A.S.hat.list0=A.S.hat.list0, A.S.hat.list1=A.S.hat.list1,
                    time = t00)
    return(Init.res)
  }

}
