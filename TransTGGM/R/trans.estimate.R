trans.estimate = function(t.data.tran, A.data, A.lambda, A.orac = NULL,
                          t.lambda.int=NULL, adjust.BIC=FALSE, mode.set = NULL,
                          init.method="sepa", init.method.aux="sepa",
                          init.iter=3, normalize = TRUE,
                          theta.algm="cd", cov.select="tensor.prod",
                          c.lam.sepa=20, c.lam.Tlasso=20, cn.lam2=1,
                          inti.the=TRUE){

  p.vec = dim(t.data.tran)
  M = length(p.vec) - 1
  n.da = p.vec[M+1]
  p.vec = p.vec[-(M+1)]
  if(is.null(mode.set)){
    mode.set = 1:M
  } else {
    mode.set = mode.set
  }

  K = length(A.data)
  nA.vec = rep(0, K)
  for (k in 1:K) {
    p.vec.A = dim(A.data[[k]])
    nA.vec[k] = p.vec.A[length(p.vec.A)]
  }

  # Initialization
  if(is.null(t.lambda.int)){
    if(init.method == "sepa"){
      t.lambda.tran = c.lam.sepa*sqrt( p.vec*log(p.vec) / ( n.da * prod(p.vec) ))
    }
    if(init.method == "Tlasso"){
      t.lambda.tran = c.lam.Tlasso*sqrt( log(p.vec) / ( n.da * prod(p.vec) ))
    }
    if(M==2){
      t.lambda.tran = c.lam.sepa*sqrt( p.vec*log(p.vec) / ( n.da * prod(p.vec) ))
    }
  }else{
    t.lambda.tran = t.lambda.int
  }

  init.res = Initial(t.data.tran, t.lambda.tran, A.data, A.lambda,
                     A.orac = A.orac, method = init.method, method.aux = init.method.aux,
                     TT=init.iter, normalize = normalize, mode.set = mode.set)
  init.time = init.res$time
  t.Omega.hat.list = init.res$t.Omega.hat.list


  if(cov.select=="inverse"){
    # 0 Covariance matrix: directly inverting precision matrix
    S.hat.A.list = init.res$S.hat.A.M0
    S.hat.A.diff.list = init.res$S.hat.A.M.diff0
  }
  if(cov.select=="tensor.prod"){
    # 1 Covariance matrix: multiplication by tensors and precision matrices
    S.hat.A.list = init.res$S.hat.A.M1
    S.hat.A.diff.list = init.res$S.hat.A.M.diff1
  }


  t1 = proc.time()
  # using the weight (in Sigma.A.m) determined by the differences
  Theta.hat.diff.list = list()
  for (m in mode.set) {
    mi = match(m, mode.set)
    S.hat.A = S.hat.A.diff.list[[mi]]
    Omega.hat0 = t.Omega.hat.list[[m]]
    lam1 = 2*max(apply(abs(Omega.hat0), 2, sum))*sqrt(log(p.vec[m]) / n.da)
    delta.hat = delta.est(S.hat.A, Omega.hat0, lam1=lam1)

    h.hat = max(apply(delta.hat, 2, function(x) sum(abs(x))))
    lam2.del = min(h.hat*sqrt(p.vec[m]*log(p.vec[m]) / n.da / prod(p.vec)), h.hat^2)
    lam2.N = sqrt(p.vec[m]*log(p.vec[m]) / sum(nA.vec) / prod(p.vec))
    lambda2 = cn.lam2*min(max(lam2.del, lam2.N), n.da*lam2.N)

    Omega.hat00 = Omega.hat0 * inti.the + 0 * (!inti.the)
    Theta.tuning.res = Theta.tuning(lambda2, S.hat.A, delta.hat, Omega.hat00,
                                    n.A=sum(nA.vec), theta.algm=theta.algm, adjust.BIC=adjust.BIC)
    Theta.hat.m = Theta.tuning.res$Theta.hat.m
    Theta.hat.diff.list[[mi]] = Theta.hat.m
  }

  ## using the weight (in Sigma.A.m) determined by the sample sizes
  Theta.hat.list = list()
  for (m in mode.set) {
    mi = match(m, mode.set)
    S.hat.A = S.hat.A.list[[mi]]
    Omega.hat0 = t.Omega.hat.list[[m]]
    lam1 = 2*max(apply(abs(Omega.hat0), 2, sum))*sqrt(log(p.vec[m]) / n.da)
    delta.hat = delta.est(S.hat.A, Omega.hat0, lam1=lam1)

    h.hat = max(apply(delta.hat, 2, function(x) sum(abs(x))))
    lam2.del = min(h.hat*sqrt(p.vec[m]*log(p.vec[m]) / n.da / prod(p.vec)), h.hat^2)
    lam2.N = sqrt(p.vec[m]*log(p.vec[m]) / sum(nA.vec) / prod(p.vec))
    lambda2 = cn.lam2*min(max(lam2.del, lam2.N), n.da*lam2.N)

    Omega.hat00 = Omega.hat0 * inti.the + 0 * (!inti.the)
    Theta.tuning.res = Theta.tuning(lambda2, S.hat.A, delta.hat, Omega.hat00,
                                    n.A=sum(nA.vec), theta.algm=theta.algm, adjust.BIC=adjust.BIC)
    Theta.hat.m = Theta.tuning.res$Theta.hat.m
    Theta.hat.list[[mi]] = Theta.hat.m
  }
  t.theta = proc.time() - t1

  if(sum(A.orac) > 0){
    ########### oracle auxiliary domains
    if(cov.select=="inverse"){
      S.hat.A.list.o = init.res$S.hat.A.M0.o   # 0 Covariance matrix: directly inverting precision matrix
    }
    if(cov.select=="tensor.prod"){
      S.hat.A.list.o = init.res$S.hat.A.M1.o   # 1 Covariance matrix: multiplication by tensors and precision matrices
    }

    Theta.hat.list.o = list()
    for (m in mode.set) {
      mi = match(m, mode.set)
      S.hat.A = S.hat.A.list.o[[mi]]
      Omega.hat0 = t.Omega.hat.list[[m]]
      lam1 = 2*max(apply(abs(Omega.hat0), 2, sum))*sqrt(log(p.vec[m]) / n.da)
      delta.hat = delta.est(S.hat.A, Omega.hat0, lam1=lam1)

      lambda2 = cn.lam2*sqrt(p.vec[m]*log(p.vec[m]) / sum(nA.vec[A.orac]) / prod(p.vec))
      Theta.tuning.res = Theta.tuning(lambda2, S.hat.A, delta.hat, Omega.hat0,
                                      n.A=sum(nA.vec[A.orac]), theta.algm=theta.algm, adjust.BIC=adjust.BIC)
      Theta.hat.m = Theta.tuning.res$Theta.hat.m
      Theta.hat.list.o[[mi]] = Theta.hat.m
    }
    res.trans = list(Theta.hat.list=Theta.hat.list, Theta.hat.diff.list=Theta.hat.diff.list,
                     S.hat.A.list=S.hat.A.list, S.hat.A.diff.list=S.hat.A.diff.list,
                     t.Omega.hat.list=t.Omega.hat.list, init.res=init.res,
                     init.time=init.time, theta.time=t.theta,
                     Theta.hat.list.o=Theta.hat.list.o, S.hat.A.list.o=S.hat.A.list.o)
  } else {
    res.trans = list(Theta.hat.list=Theta.hat.list, Theta.hat.diff.list=Theta.hat.diff.list,
                     S.hat.A.list=S.hat.A.list, S.hat.A.diff.list=S.hat.A.diff.list,
                     t.Omega.hat.list=t.Omega.hat.list, init.res=init.res,
                     init.time=init.time, theta.time=t.theta)
  }

  return(res.trans)

}


