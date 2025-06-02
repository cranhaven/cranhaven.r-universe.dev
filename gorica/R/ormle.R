ormle <- function(est,covmtrx,constr,rhs,nec, num_par = length(est)){
  covmtrx = as.matrix(covmtrx)
  ginvcovmat <- ginv(covmtrx)
  Dmat = 2*ginvcovmat
  dvec = 2*(est %*% ginvcovmat)
  solveQP = solve.QP(Dmat, dvec = dvec, t(constr), rhs, meq = nec, factorized = FALSE)
  restrictedest = solveQP$solution
  names(restrictedest)=names(est)
  loglik = as.numeric( ( -num_par/2*log(2*pi) )-( 0.5*log(det(covmtrx) ) )-( 0.5* t(est- restrictedest)%*%ginvcovmat%*% (est-restrictedest)) )
  #(-num_pars/2*log(2*pi) ) -( 0.5*log(det(BKcov) ) )-( 0.5* t(eta- tildeeta[j,])%*%ginv(BKcov)%*%(eta-tildeeta[j,]))
  out <- list(est=est, covmtrx=covmtrx, constr=constr, rhs=rhs, nec=nec, logLik=loglik,restrictedest=restrictedest)
  class(out) <- "ormle"
  out

   }


