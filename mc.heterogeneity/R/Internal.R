# MC simulation function for Standardized Mean Differences (d)
simulate.d<-function(nrep, d_overall, vi, n1, n2, mods){
  set.seed(nrep)
  d.s<-stats::rnorm(length(n1),mean=d_overall, sd=sqrt(vi))
  vi.s<-(n1+n2)/n1/n2+d.s^2/(2*(n1+n2))
  model.f1.s<-try(metafor::rma(d.s, vi.s, mods = mods, tau2=0, method="ML"), silent = TRUE)
  model.f2.s<-try(metafor::rma(d.s, vi.s, mods = mods, tau2=0,method="REML"), silent = TRUE)
  model.r1.s<-try(metafor::rma(d.s, vi.s, mods = mods, method="ML"), silent = TRUE)
  model.r2.s<-try(metafor::rma(d.s, vi.s, mods = mods, method="REML"), silent = TRUE)

  #if (class(model.r1.s)!="try-error" & class(model.f1.s)!="try-error"){
  if (sum(!class(model.r1.s)!="try-error" , !class(model.f1.s)!="try-error")==0){
    lllr1.s<-(metafor::fitstats(model.r1.s)-metafor::fitstats( model.f1.s))[1]*2} else {
      lllr1.s<-NA}
  #if (class(model.r2.s)!="try-error" & class(model.f2.s)!="try-error"){
  if (sum(!class(model.r2.s)!="try-error" , !class(model.f2.s)!="try-error")==0){
    lllr2.s<-(metafor::fitstats(model.r2.s)-metafor::fitstats( model.f2.s))[1]*2} else {
      lllr2.s<-NA}

  return(c(lllr1.s,lllr2.s))
}

#  mc.d.random: find.c<- mclapply(1:10^4, simulate.d, mc.cores=ncores)

# MC simulation function for Fisher's Transformed z Scores (r, z)

simulate.z<-function(nrep, z_overall, vi, n, mods){
  set.seed(nrep)
  z.s<-stats::rnorm(length(n), mean = z_overall, sd = sqrt(vi))
  vi.s<- vi
  model.f1.s<-try(metafor::rma(z.s, vi.s, mods = mods, tau2=0, method="ML"), silent = TRUE)
  model.f2.s<-try(metafor::rma(z.s, vi.s, mods = mods, tau2=0,method="REML"), silent = TRUE)
  model.r1.s<-try(metafor::rma(z.s, vi.s, mods = mods, method="ML"), silent = TRUE)
  model.r2.s<-try(metafor::rma(z.s, vi.s, mods = mods, method="REML"), silent = TRUE)

  #if (class(model.r1.s)!="try-error" & class(model.f1.s)!="try-error"){
  if (sum(!class(model.r1.s)!="try-error" , !class(model.f1.s)!="try-error")==0){
    lllr1.s<-(metafor::fitstats(model.r1.s)-metafor::fitstats( model.f1.s))[1]*2} else {
      lllr1.s<-NA}
  #if (class(model.r2.s)!="try-error" & class(model.f2.s)!="try-error"){
  if (sum(!class(model.r2.s)!="try-error" , !class(model.f2.s)!="try-error")==0){
    lllr2.s<-(metafor::fitstats(model.r2.s)-metafor::fitstats( model.f2.s))[1]*2} else {
      lllr2.s<-NA}

  return(c(lllr1.s,lllr2.s))
}

# MC simulation function for log odds ratio

simulate.OR<-function(nrep, lnOR_overall, vi, n, n_00, n_01, mods){
  set.seed(nrep)
  lnOR.s <- stats::rnorm(length(n),mean=lnOR_overall, sd=sqrt(vi))
  n_00_s <- n_00
  n_01_s <- n_01
  n_10_s <- n_00_s*(n-n_00_s-n_01_s)/(n_00_s + n_01_s*exp(lnOR.s))
  n_11_s <- n - n_00_s - n_01_s - n_10_s
  #########################################################################
  # zero count correction
  df <- cbind(n_00_s, n_01_s, n_10_s, n_11_s)
  if(any(df == 0)){
    df <- df + 0.5*(df==0)
    n_00_s <- df$n_00_s
    n_01_s <- df$n_01_s
    n_10_s <- df$n_10_s
    n_11_s <- df$n_11_s
  }
  #########################################################################
  vi.s <- 1/n_00_s+1/n_01_s+1/n_10_s+1/n_11_s

  model.f1.s<-try(metafor::rma(lnOR.s, vi.s, mods = mods, tau2=0, method="ML"), silent = TRUE)
  model.f2.s<-try(metafor::rma(lnOR.s, vi.s, mods = mods, tau2=0,method="REML"), silent = TRUE)
  model.r1.s<-try(metafor::rma(lnOR.s, vi.s, mods = mods, method="ML"), silent = TRUE)
  model.r2.s<-try(metafor::rma(lnOR.s, vi.s, mods = mods, method="REML"), silent = TRUE)

  #if (class(model.r1.s)!="try-error" & class(model.f1.s)!="try-error"){
  if (sum(!class(model.r1.s)!="try-error" , !class(model.f1.s)!="try-error")==0){
    lllr1.s<-(metafor::fitstats(model.r1.s)-metafor::fitstats( model.f1.s))[1]*2} else {
      lllr1.s<-NA}
  #if (class(model.r2.s)!="try-error" & class(model.f2.s)!="try-error"){
  if (sum(!class(model.r2.s)!="try-error" , !class(model.f2.s)!="try-error")==0){
    lllr2.s<-(metafor::fitstats(model.r2.s)-metafor::fitstats( model.f2.s))[1]*2} else {
      lllr2.s<-NA}

  return(c(lllr1.s,lllr2.s))
}
