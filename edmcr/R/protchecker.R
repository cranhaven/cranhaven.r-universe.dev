protchecker <- function(X, Comp, eq_cons, lo_bounds, up_bounds, print_flag=0){
  
  report <- list(eq_err = c(),
                 lo_err = c(),
                 up_err = c(),
                 chiral = c(),
                 phi = c(),
                 psi = c(),
                 hbond = c(),
                 dihed = c())
  
  report$eq_err <- bondcheck(X, eq_cons)
  report$lo_err <- lobound(X, lo_bounds)
  report$up_err <- upbound(X, up_bounds)
  report$chiral <- chirality_check(X, Comp, 0)
  
  out <- ang_checker(X, Comp, 0)
  report$phi <- out$phi
  report$psi <- out$psi
  
  report$hbond <- report$up_err[which(up_bounds[,4] == -1)]
  report$dihed <- report$up_err[which(up_bounds[,4] == -2)]
  
  return(report)
}

