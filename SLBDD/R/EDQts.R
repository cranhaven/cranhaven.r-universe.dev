#'  Empirical Dynamic Quantile for Visualization of High-Dimensional Time Series
#'
#'  Compute empirical dynamic quantile (EDQ) for a given probability "p" based on the weighted algorithm
#'  proposed in the article by Peña, Tsay and Zamar (2019).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param p Probability, the quantile series of which is to be computed. Default value is 0.5.
#' @param h Number of time series observations used in the algorithm. The larger h is the longer to compute.
#' Default value is 30.
#'
#' @return The column of the matrix x which stores the "p" EDQ of interest.
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' edqts(TaiwanAirBox032017[,1:25])
#'
#' @references Peña, D. Tsay, R. and Zamar, R. (2019). Empirical Dynamic Quantiles for
#' Visualization of High-Dimensional Time Series, \emph{Technometrics}, 61:4, 429-444.
#'
"edqts" <- function(x, p = 0.5, h = 30){
  if(!is.matrix(x))x <- as.matrix(x)
  if(p <= 0) p <- 0.5
  if(h < 1)h <- 30
  if(ncol(x) < h)h <- ncol(x)

  loc <- WDQTS(t(x),p=p,h=h)
  return(loc)
}


"WDQTS" <- function(x, p, h) {

  dd=dim(x)
  N=dd[1]
  T=dd[2]
  q=seq(1,T,1)
  for (tt in 1:T) {
    q[tt]=quantile(x[,tt],p)     }

  MA=x-x
  Mq=matrix(rep(q,N),N,T,byrow=TRUE)
  MA=abs(x-Mq)
  vdq=apply(MA,1,sum)
  i1=which.min(vdq)
  orden=sort(vdq,index.return=TRUE)$ix[1:h]

  Msol=matrix(0,h,2)

  for (k in 1:h)  {

    ic=orden[k]
    yy=x[ic,]
    pesos=RO(x,yy,q, p)
    fin= WOPT(x,ic,pesos,q,p)

    Msol[k,]=c(fin[[1]],fin[[2]])
  }
  ifinal2=which.min(Msol[,2])[1]
  ifinal=Msol[ifinal2,1]

  out=ifinal
  return(out)
}


"RO" <- function(x, yy, q, p){

  dd=dim(x)
  N=dd[1]
  T=dd[2]
  Mb=matrix(0,N,T)
  for (i in 1:N){
    ss1=sign(x[i,]-yy)
    ss2=sign(x[i,]-q)
    a1=abs(x[i,]-yy)
    a2=abs(x[i,]-q)
    vd1=ifelse (ss1>0,p*a1,(1-p)*a1)
    vd2=ifelse (ss2>0,p*a2,(1-p)*a2)
    vd=vd1-vd2

    Mb[i,]=ifelse(abs(yy-q)>0, vd/abs(yy-q),0)
  }
  pesos=apply(Mb,2,mean)
  out=pesos
  return(out)
}

"WOPT" <- function(x, i1, pesos, q, p) {
  dd=dim(x)
  N=dd[1]
  T=dd[2]
  pesos1=pesos
  dpes1=sum(abs(x[i1,]-q)*pesos1)

  dpes=rep(0,N)
  for (i in 1:N) {
    dpes[i]=sum(abs(x[i,]-q)*pesos1)
  }
  i2=which.min(dpes)
  pesos2=RO(x,x[i2,],q, p)
  dpes2=sum(abs(x[i2,]-q)*pesos2)

  while (dpes2<dpes1) {

    i1=i2
    dpes1=dpes2
    pesos=RO(x,x[i1,],q, p)
    for (i in 1:N) {
      dpes[i]=sum(abs(x[i,]-q)*pesos)
    }
    i2=which.min(dpes)
    pesos2=RO(x,x[i2,],q, p)
    dpes2=sum(abs(x[i2,]-q)*pesos2)
  }
  VV=rep(0,N)
  for (j in 1:N) {
    ss1=sign(x[j,]-q)
    a1=abs(x[j,]-q)
    vd1=ifelse (ss1>0,p*a1,(1-p)*a1)

    VV[j]=sum(vd1)

  }
  VVT=sum(VV)
  FOb=dpes1*N+VVT

  out=list(i1=i1,FOb=FOb)
  return(out)
}
