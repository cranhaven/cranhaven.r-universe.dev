#  Utils -------------------------------------------------------------------
sum_fdata<-function(x){
  out<-x
  out$data<-matrix(colSums(x$data),1)
  return(out)
}
diff_fdata<-function(x1,x2){
  out<-x1
  out$data<-x1$data-x2$data
  return(out)
}


diff_fdata_sur<-function(x1,x2){
  n<-dim(x1$data)[1]
  out<-x1
  for(ii in 1:n)out$data[ii,,]=x1$data[ii,,]-x2$data[ii,,]
  return(out)
}
sum_fdata_sur<-function(x1,x2){
  n<-dim(x1$data)[1]
  out<-x1
  for(ii in 1:n)out$data[ii,,]=x1$data[ii,,]+x2$data[ii,,]
  return(out)
}

func.mean_sur<-function (fdataobj)
{
  n<-dim(fdataobj$data)[1]
  pro<-array(NA,dim = c(1,dim(fdataobj$data)[2],dim(fdataobj$data)[3]))
  pro[1,,] <-apply(fdataobj$data , c(2,3), base::mean)
  fdataobj$data<-pro
  fdataobj$names$main <- "mean"
  fdataobj
}
func.var_sur<-function (fdataobj)
{

  n<-dim(fdataobj$data)[1]
  pro<-array(NA,dim = c(1,dim(fdataobj$data)[2],dim(fdataobj$data)[3]))
  pro[1,,] <-apply(fdataobj$data , c(2,3), stats::var)
  fdataobj$data<-pro
  fdataobj$names$main <- "var"
  fdataobj
}
frac_fdata_sur<-function(x1,x2){
  n<-dim(x1$data)[1]
  out<-x1
  for(ii in 1:n)out$data[ii,,]=x1$data[ii,,]/(x2$data[ii,,]+1e-15)
  return(out)
}
standardize_sur<-function(x,mu0,sig0=NA){
  if(is.na(sig0))sig0<-fdata(array(1,dim = c(1,dim(x$data)[2],dim(x$data)[3])),argvals = x$argvals)
  n<-dim(x$data)[1]
  mu0_rep<-x
  sig0_rep<-x

  for(ii in 1:n)mu0_rep$data[ii,,]=mu0$data[1,,]
  for(ii in 1:n)sig0_rep$data[ii,,]=sig0$data[1,,]
  frac_fdata_sur(diff_fdata_sur(x,mu0_rep),sig0_rep)

}
ex_fdata<-function(x,index){
  if(is.logical(index))index=which(index==TRUE)
  out<-x
  out$data<-x$data[index,,,drop=FALSE]
  return(out)

}
