defaultTimeseries <-
function(N,AC,Years,PD){
  Psi=rnorm(Years)
  PDcond1=pnorm((qnorm(PD)-sqrt(AC)*Psi)/sqrt(1-AC))
  size_p=rep(N,Years)
  D1=rbinom(Years,size_p,PDcond1)
  return(D1)
}
