#' US States Production
#'
#' \itemize{
#' \item{state}{the state}
#' \item{year}{the year}
#' \item{pcap}{private capital stock}
#' \item{hwy}{highway and streets}
#' \item{water}{water and sewer facilities}
#' \item{util}{other public buildings and structures}
#' \item{pc}{public capital}
#' \item{gsp}{gross state products}
#' \item{emp}{labor input measured by the employement in non--agricultural payrolls}
#' \item{unemp}{state unemployment rate}
#' }
#'
#' @docType data
#' @name Produc
#' @usage data(Produc)
#' @format A data frame with 816 rows and 10 variables
NULL

ginv <- function(x, tol = sqrt(.Machine$double.eps))
{
  ## Generalized Inverse of a Matrix
  dnx <- dimnames(x)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(x)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else x,
    dimnames = dnx[2:1])
}
#' @importFrom gtools smartbind
pandyn<-function(z,p,meth=c(0,1,2,3,4)){
  NT=nrow(z)
  k=ncol(z)
  Tmin=min(z[,2])
  Tmax=max(z[,2])
  T=Tmax-Tmin+1
  T0=T-p-1
  N=NT/T
  range0=Tmax
  range1=Tmin
  zy<-0
  zx<-0
  zwz<-0
  yy<-0
  xy<-0
  xx<-0
  sx<-0
  sy<-0
  sxx<-0
  sxy<-0
  ssy<-0
  ssx<-0
  syy<-0
  sT<-0
  NTges<-0
  if(meth==0){
    ivn1=T0*(T0+1)/2 + (k-3) + N/10
    ivn2=T0 + T0*(k-3) + N/10
    ivn3=T0+k-3+N/10
    meth=4
    if(ivn1>N){
      meth=3
      if(ivn2>N){
        meth=2
        if(ivn3>N){
          meth=1

        }
      }
    }
  }

  w=matrix(0,T0,T0)
  w[1, 1:2]=cbind(2,-1)
  w[T0,T0-1]=-1
  w[T0,T0]=2
  i=2
  while(i<=T0-1){

    w[i,i-1] = -1
    w[i,i] = 2
    w[i,i+1] = -1
    i=i+1
  }
  #------------------------------------------------
  pos=1
  N=0
  while(pos< NT-2) {
    yi=matrix(0,1,1)
    xi=matrix(0,1,k-3)
    dyi=matrix(0,2,1)
    dxi=matrix(0,2,k+p-3)
    ctl=dyi
    dvec=matrix(0,T,1)
    Ti=0
    year=Tmin
    while(year<=Tmax){

      if(z[pos,2]==year){
        dvec[year-Tmin+1]=1
        yi=rbind(yi,z[pos,3]);
        xi=smartbind(xi,z[pos,4:k])
        pos=pos+1
      }

      else{
        yi=rbind(yi,(z[pos,3]))

        xi=rbind(xi,(z[pos,4:k]))
      }
      if (year>=Tmin+p+1){
        d=0
        m=nrow(yi)
        #s<-0-p-2
        s<-as.numeric(m-p-2)
        rr<-as.numeric(m-1)
        if (sum(dvec[s:rr])==p+2)  {
          d=1
        }

        #d=1
        ctl=rbind(ctl,d)
        dyi=rbind(dyi,(d*(yi[m]-yi[m-1])))
        ss<-as.numeric(p-1)
        i<- as.numeric(m-p)
        j<-as.numeric(m-1)
        #dfy=yi[j:i]
        ii<-as.numeric(m-2)
        jj<-as.numeric(m-ss)
        #dffy=yi[ii]
        LagDep=t(yi[j:i]-yi[ii])
        dxi=smartbind(dxi,d*(cbind((xi[m,]-xi[m-1,]),LagDep )))

        Ti=Ti+d
      }
      year=year+1
    }
    yi=yi[1:T+1]
    xi=xi[1:T+1,]
    dyi=dyi[3:nrow(dyi)]
    dxi=dxi[3:nrow(dxi),]
    ctl=ctl[3:nrow(ctl)]

    ff=as.numeric(T-2)
    if (meth==1){
      zi=yi[p:ff]
      j=2
      while (j<=p){
        zi=cbind(zi,yi[p-j+1:T-j-1])
        j=j+1
      }
      kk=as.numeric(k-3)
      zi=cbind(zi,cbind((xi[p+3:T-2,]-xi[p:ff,]),dxi[,1:kk]) )
      zi=t(zi)

    }

    if (meth==2) {

      zi=(matrix(1,T0,1)*yi[p])
      j=p+1
      while (j<=T-2){
        z00=matrix(0,j-p,1)
        z11=matrix(1,T-j-1,1)*yi[j]
        z0=rbind(z00,z11)
        zi=cbind(zi,z0)
        j=j+1
      }
      q<-as.numeric(k-3)
      dx=dxi[,1:q]
      zi=rbind(zi,t(dx))
    }

    if (meth==3)  {
      zi=(matrix(1,T0,1)*yi[p])
      j=p+1
      while (j<=T-2){
        z00=matrix(0,j-p,1)
        z11=matrix(1,T-j-1,1)*yi[j]
        z0=rbind(z00,z11)
        zi=cbind(zi,z0)
        j=j+1
      }

      ein=diag(T0)
      z0=matrix(1,(k-3)*T0,T0)
      j=1
      while (j<=T0){
        q<-as.numeric(k-3)
        z0[,j]=kronecker(ein[,j],t(dxi[j,1:q]))
        j=j+1
      }
      zi=rbind(zi,z0)
    }

    if (meth==4){
      z0=cbind(yi[1:p],matrix(0,p,T-p-2))
      zi=z0
      i=p+1
      while (i<=T-3) {
        z0=cbind(yi[1:i],matrix(0,i,T-i-2))
        z0=cbind(matrix(0,i,i-p),z0)
        zi=rbind(zi,z0)
        i=i+1
      }
      qq<-as.numeric(T-2)
      z0=cbind(matrix(0,T-2,T-2-p),yi[1:qq])
      zi=rbind(zi,z0)
      q<-as.numeric(k-3)
      zi=rbind(zi,t(dxi[,1:q]))
    }
    Ti=Ti-1
    range0=min(rbind(range0,Ti))
    range1=max(rbind(range1,Ti))
    y=yi[p+2:T-1]
    x=xi[p+2:T-1,]
    zi=t(t(zi)*ctl)
    dxi<-as.matrix(dxi)
    sxx=sxx+t(dxi)%*%dxi
    sxy=sxy+t(dxi)%*%dyi
    syy=syy+t(dyi)%*%dyi
    sy=sy+colSums(as.matrix(dyi))
    sx=sx+t(colSums(dxi))
    zy=zy+zi%*%dyi
    zx=zx+zi%*%dxi
    zwz=zwz+zi%*%w%*%t(zi)
    yy=yy+t(dyi)%*%dyi
    xy=xy+t(dxi)%*%dyi
    xx=xx+t(dxi)%*%dxi
    sT=sT+sum(ctl)
    N=N+1
    NTges=NTges+sum(ctl)
  }
  k=k-3
  zwz=ginv(zwz)
  eig=eigen(t(zx)%*%zwz%*%zx)
  r=eig$values

  bgmm=ginv(t(zx)%*%zwz%*%zx)%*%t(zx)%*%zwz%*%zy
  sige=syy-2*t(bgmm)%*%sxy+t(bgmm)%*%sxx%*%bgmm
  sige=as.numeric(sige/2/sT )
  SEbgmm=sige*ginv(t(zx)%*%zwz%*%zx)

  SEbgmm=sqrt(diag(SEbgmm))
  tval=bgmm/SEbgmm

  m=zy-zx%*%bgmm
  J=t(m)%*%zwz%*%m/sige

  cov=t(bgmm)%*%(as.vector(sxy)-as.vector(sx)*as.numeric(sy/sT))/sT
  gg<-tcrossprod(as.vector(sx))/sT
  var1=t(bgmm)%*%(sxx-gg)%*%bgmm/sT
  var2=(syy-sy^2/sT)/sT
  R2=cov^2/var1/var2

  df=nrow(zx)-k-p
  pval = 2*pt(-abs(tval), df=df)
  Jpval=1-pchisq(J,df=df)
  if(meth==4){iv<-"Arellano Bond (1991)"}
  if(meth==0){iv<-"Automatic selection of appropriate IV matrix"}
  if(meth==1){iv<-" GMM estimator with the smallest set of instruments"}
  if(meth==2){iv<-"A reduced form of IV from method 3"}
  if(meth==3){iv<-"IV matrix where the number of moments grows with k.T"}
  list(coefficients=bgmm,std=SEbgmm,tval=tval,pval=pval, Jstat=J, Jpval=Jpval, R2=R2,NTges=NTges,N=N,iv=iv )
}
#' method
#'
#' @author Zaghdoudi Taha
#' @param x a numeric design matrix for the model.
#' @param ... not used
#' @export
dpd<- function(x,...){UseMethod("dpd") }



dpd.default <- function(z,p,meth=c(0,1,2,3,4),...)
{
  if(meth==0){ est <- pandyn(z,p,0)}
  if(meth==1){ est <- pandyn(z,p,1)}
  if(meth==2){ est <-pandyn(z,p,2)}
  if(meth==3){ est <- pandyn(z,p,3)}
  if(meth==4){ est <- pandyn(z,p,4)}
  est$call <- match.call()
  class(est) <- "dpd"
  est

}
print.dpd <- function(x,...)
{
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

#' Summary
#'
#' @param object is the object of the function
#' @param ... not used
#' @importFrom stats pchisq printCoefmat pt
#' @export
summary.dpd<-function(object,...)
{
  res<-cbind(object$coefficients,object$std, object$tval,object$pval)
  colnames(res)<-c("Estimate","SE ","t-value","Pvalue")
  cat("\nGMM-Estimation of the Dynamic Panel Data Models:\n")
  cat("Instruments according to method: \n", object$iv,"\n")
  printCoefmat(res,has.Pvalue = TRUE)
  cat("\nNumber of groups :",object$N,"Number of obs :",object$NTges,"\n")
  cat("R-square :",object$R2,"\n")
  cat("Hansen's J-statistic: ",object$Jstat,"Pvalue: ",object$Jpval,"\n")



}
#' formula
#'
#' @param formula PIB~INF+TIR
#' @param data the dataframe
#' @param index : id is the name of the identity groups and time is the time per group
#' @param p scalar, autoregressive order for dependent variable
#' @param meth scalar, indicator for the Instruments to use
#' @param ... not used
#' @importFrom stats model.frame model.matrix model.response  update
#' @export
dpd.formula <-function(formula,data=list(),index=c("id","time"),p,meth=c(0,1,2,3,4),...)
{
  mff <- update(formula, ~ . -1)
  mf <- model.frame(mff, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  idx=data[,index]
  z=cbind(idx,y,x)
  if(meth==0){ est <- dpd.default(z,p,0,...)}
  if(meth==1){ est <- dpd.default(z,p,1,...)}
  if(meth==2){ est <- dpd.default(z,p,2,...)}
  if(meth==3){ est <- dpd.default(z,p,3,...)}
  if(meth==4){ est <- dpd.default(z,p,4,...)}
  est$call <- match.call()
  est$equa <- formula
  est
}
