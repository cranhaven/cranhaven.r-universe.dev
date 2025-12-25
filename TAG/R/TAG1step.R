TAG1step <- function(y, X,
                     set.initial = list(Candi.lambda= seq(from=-2, to=2,by=0.5)),
                     set.TAG = list(delta.threshold = -6),
                     set.TAAG = list(adj.nu = FALSE)){
  ini.TAG <- initial.TAG(y, X, unlist(set.initial))
  par.TAG <- TAG(ini.TAG, unlist(set.TAG))
  temp.m <-  km(formula=~1, design=X, response=par.TAG$ty,
                covtype="gauss",nugget = (10^-15), multistart = 4,
                control = list(trace = FALSE, verbose = FALSE))
  nu.est <- sqrt(2*(coef(temp.m)$range^2))
  obj <- TAAG(par.TAG, nu.est, unlist(set.TAAG))
  return(obj)
}
