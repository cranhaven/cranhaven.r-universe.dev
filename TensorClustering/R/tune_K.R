#' @export

tune_K = function(X, seqK, seqlamb, initial=TRUE, vec_x=NULL){
  minclust = bic = opt_lambs = rep(0,length(seqK))
  a = length(X)*0.05

  for (iK in 1:length(seqK)) {
    res_lamb = tune_lamb(X, seqK[iK], seqlamb=seqlamb, initial=initial, vec_x=vec_x)
    bic[iK] = res_lamb$opt_bic
    opt_lambs[iK] = res_lamb$opt_lamb

    minclust[iK] = min(table(res_lamb$opt_y))
  }

  bic2 = bic[minclust>a]
  seqK2 = seqK[minclust>a]
  opt_lambs2 = opt_lambs[minclust>a]

  opt_K = seqK2[which.min(bic2)]
  opt_lamb = opt_lambs2[which.min(bic2)]

  Krank = rbind(seqK,minclust)[,order(bic)]
  rownames(Krank) = c("K","N_min")


  return(list(opt_K=opt_K,opt_lamb=opt_lamb,Krank=Krank))
}



