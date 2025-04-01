pconmul<-function(t,lam,thres,sige){

  nume <- sum(lam * t)-thres
  term <- nume/sige
  n <- exp(1.702*term)
  p <- n / (1+n)
}
