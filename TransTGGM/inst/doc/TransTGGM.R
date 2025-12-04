## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # example.data
#  
#  library(TransTGGM)
#  library(Tlasso)
#  data(example.data)
#  t.data = example.data$t.data
#  A.data = example.data$A.data
#  t.Omega.true.list = example.data$t.Omega.true.list
#  normalize = T
#  
#  K = length(A.data)
#  p.vec = dim(t.data)
#  M = length(p.vec) - 1
#  n = p.vec[M+1]
#  p.vec = p.vec[1:M]
#  tla.lambda = 20*sqrt( p.vec*log(p.vec) / ( n * prod(p.vec) ))
#  A.lambda = list()
#  for (k in 1:K) {
#    A.lambda[[k]] = 20*sqrt( log(p.vec) / ( dim(A.data[[k]])[M+1] * prod(p.vec) ))
#  }
#  
#  # the proposed method
#  res.final = tensor.GGM.trans(t.data, A.data, A.lambda, normalize = normalize)
#  # Tlasso
#  Tlasso.Omega.list = Tlasso.fit(t.data, lambda.vec = tla.lambda, norm.type = 1+as.numeric(normalize))
#  
#  # summary
#  i.Omega = as.data.frame(t(unlist(est.analysis(res.final$Omega.list, t.Omega.true.list))))
#  i.Omega.diff = as.data.frame(t(unlist(est.analysis(res.final$Omega.list.diff, t.Omega.true.list))))
#  i.Tlasso = as.data.frame(t(unlist(est.analysis(Tlasso.Omega.list, t.Omega.true.list))))
#  i.Omega.diff     # proposed.v
#  i.Omega          # proposed
#  i.Tlasso         # Tlasso
#  

