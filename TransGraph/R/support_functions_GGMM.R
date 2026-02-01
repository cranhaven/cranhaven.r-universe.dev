Init_trans_GGMM = function(t.data, lambda.t, M, A.data, lambda.A.list, M.A.vec,
                           initial.selection="K-means", trace=F ){
  p = dim(t.data)[2]
  res.target = GGMPF(lambda.t, t.data, M, initial.selection=initial.selection, trace = trace)
  t.Theta_hat.array0 = res.target$opt_Theta_hat
  M0.hat = res.target$K_hat
  t.member = res.target$opt_member
  MM.K = sort(unique(t.member))
  t.n.vec = apply(res.target$opt_L.mat, 2, sum)
  t.mean = res.target$opt_Mu_hat

  K = length(A.data)
  res.aux.list = list()
  for (k in 1:K) {
    res.k = GGMPF(lambda.A.list[[k]], A.data[[k]], M.A.vec[k], initial.selection=initial.selection, trace = trace)
    res.aux.list[[k]] = res.k
  }

  #### auxiliary covarianve matrices
  # auxiliary pseudo.cov: Bayesian posterior
  A.cov.soft = list()
  nA.vec.soft = c()
  v = 1
  for (k in 1:K) {
    A.data.k = A.data[[k]]
    res.k = res.aux.list[[k]]
    member.k = res.k$opt_member
    MM.K = sort(unique(member.k))
    L.mat.k = res.k$opt_L.mat
    mu.k = res.k$opt_Mu_hat
    for (m in MM.K) {
      nm = sum(member.k == m)
      nA.vec.soft[v] = nm
      L_ikx = sqrt(L.mat.k[,match(m,MM.K)])*t(t(A.data.k) - mu.k[match(m,MM.K),])
      A.cov.soft[[v]] = t(L_ikx) %*% L_ikx / nm
      v = v+1
    }
  }

  # auxiliary pseudo.cov: subgrouping refitting
  A.cov.hard = list()
  nA.vec.hard = c()
  v = 1
  for (k in 1:K) {
    A.data.k = A.data[[k]]
    res.k = res.aux.list[[k]]
    member.k = res.k$opt_member
    MM.K = sort(unique(member.k))
    for (m in MM.K) {
      nm = sum(member.k == m)
      nA.vec.hard[v] = nm
      A.cov.hard[[v]] = cov(A.data.k[member.k == m,])
      v = v+1
    }
  }

  v0 = v-1
  #### auxiliary mean vectors
  A.mean = matrix(0, ncol = p, nrow = v0)
  v = 1
  for (k in 1:K) {
    res.k = res.aux.list[[k]]
    member.k = res.k$opt_member
    MM.K = sort(unique(member.k))
    for (m in MM.K) {
      A.mean[v,] = res.k$opt_Mu_hat[m,]
      v = v+1
    }
  }


  res = list(t.Theta_hat.array0=t.Theta_hat.array0, M0.hat=M0.hat, t.n.vec=t.n.vec, t.mean=t.mean,
             A.cov.soft=A.cov.soft, A.cov.hard=A.cov.hard, A.mean=A.mean,
             nA.vec.soft=nA.vec.soft, nA.vec.hard=nA.vec.hard,
             res.target=res.target, res.aux.list=res.aux.list)
  return(res)

}

f.den.vec = function(data, mu, Theta){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: f.den.vec
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            calculate the density function values at each sample point.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ data: n * p matrix, the design matrix.
  ## @ mu1: p * 1 vector, the mean vector.
  ## @ Omega1: p * p matrix, the precision matrix.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ fdensity: The density function values at each sample point.
  ## -----------------------------------------------------------------------------------------------------------------

  p = length(mu)
  fden = as.numeric( (2*pi)^(-p/2) * (det(Theta))^(1/2) * exp(-1/2*diag(t(t(data) - as.numeric(mu)) %*% Theta %*% (t(data) - as.numeric(mu)))) )
  return(fden)
}


