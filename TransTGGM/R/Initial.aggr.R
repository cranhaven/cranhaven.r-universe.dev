Initial.aggr = function(t.data, t.lambda.int=NULL, method = "sepa",
                        cov.select= "tensor.prod", TT=2,
                        c.lam.sepa=20, c.lam.Tlasso=20, normalize = TRUE){
  # Initial.aggr: the function calculating initial covariance matrices of
  #               the target domain for the aggregation step,
  #               via two alternative methods:
  #               "Tlasso" (PAMI, 2020) & "sepa" (JCGS, 2022)

  p.vec = dim(t.data)
  M = length(p.vec) - 1
  n.da = p.vec[M+1]
  p.vec = p.vec[-(M+1)]

  if(is.null(t.lambda.int)){
    if(method == "sepa"){
      t.lambda = c.lam.sepa*sqrt( p.vec*log(p.vec) / ( n.da * prod(p.vec) ))
    }
    if(method == "Tlasso"){
      t.lambda = c.lam.Tlasso*sqrt( log(p.vec) / ( n.da * prod(p.vec) ))
    }
    if(M==2){
      t.lambda = c.lam.sepa*sqrt( p.vec*log(p.vec) / ( n.da * prod(p.vec) ))
    }
  }else{
    t.lambda = t.lambda.int
  }


  if(method == "sepa"){
    # Initialization in target domain
    t.Omega.hat.list = Separate.fit(t.data, lambda.vec=t.lambda, normalize=normalize)$Omegahat
  }

  if(method == "Tlasso"){
    # Initialization in target domain
    t.Omega.hat.list = Tlasso.fit(t.data, T=TT, t.lambda, norm.type = 1+as.numeric(normalize))
  }

  t.S.hat.list = S.est(t.data, t.Omega.hat.list)

  t.S.hat.list0 = t.S.hat.list$sig0  # 0 Covariance matrix: directly inverting precision matrix
  t.S.hat.list1 = t.S.hat.list$sig1  # 1 Covariance matrix: multiplication by tensors and precision matrices

  if(cov.select=="inverse"){
    t.S.hat.list = t.S.hat.list0
  }
  if(cov.select=="tensor.prod"){
    t.S.hat.list = t.S.hat.list1
  }

  Init.res = list(t.Omega.hat.list=t.Omega.hat.list,
                  t.S.hat.list=t.S.hat.list)



}
