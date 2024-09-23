rankit<-function(x){
  x=as.matrix(x)
  if(ncol(x)==1) x<-t(x)
  n=nrow(x)
  apply(x,2,rank)/(n+1)
}

pecdf<-function(X)
{
d = ncol(X)
n = nrow(X)

s = matrix(rep(0,(n-1)*d),n-1,d)
m = rep(0,d)
for(j in 1:d)
for(k in 1:(n-1))
{
s[k,j]<-max(sqrt(n)*k/n*(1-k/n)*abs(ecdf(X[1:k,j])(X[,j]) - ecdf(X[(k+1):n,j])(X[,j])))
}
for(j in 1:d)
m[j]<-which.max(s[,j])
min(m)
}



kn<-function(X,b)
{
  d <- ncol(X)
  n <- nrow(X)
  m <- floor(n*b)

  pseudo = NULL
  pseudo = as.list(pseudo)
  for(k in 1:(n-1))
    pseudo[[k]] = matrix(rep(0,n*d),n,d)
  m = c(0,m)
  if(max(m)!=n) {m = c(m,n)}
  for(k in 1:(n-1))
  {
    for(q in 1:(length(m)-1))
    {
      if(k<=m[q+1] & k>m[q])
      {
        pseudo[[k]][(m[q]+1):k,] = rankit(X[(m[q]+1):k,])
        pseudo[[k]][(k+1):m[q+1],] = rankit(X[(k+1):m[q+1],])
      }
      if(k>m[q+1]  | k<=m[q]) {pseudo[[k]][(m[q]+1):m[q+1],] = rankit(X[(m[q]+1):m[q+1],])}
    }
  }
  
  psbk<-as.vector(pseudo[[1]])
  for(k in 2 :(n-1)){psbk = c(psbk,as.vector(pseudo[[k]]))}
  
  ps<-NULL
  for(q in 1:(length(m)-1))
    ps<-rbind(ps,rankit(X[(m[q]+1):m[q+1],]))
  if(max(m)!=n){ps<-rbind(ps,rankit(X[(max(m)+1):n,]))}
  ps<-as.vector(ps)
  m = c(m,n+1)
  
 out <- .C("argk",
                as.double(X),
                as.integer(n),
                as.integer(d),
                as.integer(m),
                kstar=integer(1),
		            as.double(ps),
		            as.double(psbk),
	             	double(d),
                PACKAGE="npcopTest")

list(k.star= out$kstar)

}

argb <- function(X)
{
  d <- ncol(X)
  n <- nrow(X)
bstar=rep(0,n-1)
for(b in 1:(n-1))
{
bstar[b]=kn(X,b)$k.star
}
which.max(bstar)
}

CopTestdm <- function(X,b=1,M=1000)
{
  stopifnot(is.matrix(X))
  d <- ncol(X)
  stopifnot(d > 1)
  n <- nrow(X)
  b = sort(b);
  stopifnot(0< b[1] && b[length(b)]<=1)
  m = floor( n *b)
  mm<-m
  sq = length(m)
if(is.matrix(M) == FALSE)
  multipliers<-matrix(rnorm(M*n),n,M)
if(is.matrix(M) == TRUE)
{
  multipliers<-M
  M<-ncol(multipliers)
}

  pseudo = NULL
  pseudo = as.list(pseudo)
  for(k in 1:(n-1))
    pseudo[[k]] = matrix(rep(0,n*d),n,d)
  m = c(0,m)
  if(max(m)!=n) {m = c(m,n)}
  for(k in 1:(n-1))
  {
    for(q in 1:(length(m)-1))
    {
    if(k<=m[q+1] & k>m[q])
      {
      pseudo[[k]][(m[q]+1):k,] = rankit(X[(m[q]+1):k,])
      pseudo[[k]][(k+1):m[q+1],] = rankit(X[(k+1):m[q+1],])
      }
     if(k>m[q+1]  | k<=m[q]) {pseudo[[k]][(m[q]+1):m[q+1],] = rankit(X[(m[q]+1):m[q+1],])}
    }
  }
  
  psbk<-as.vector(pseudo[[1]])
  for(k in 2 :(n-1)){psbk = c(psbk,as.vector(pseudo[[k]]))}
  
  ps<-NULL
  for(q in 1:(length(m)-1))
  ps<-rbind(ps,rankit(X[(m[q]+1):m[q+1],]))
  if(max(m)!=n){ps<-rbind(ps,rankit(X[(max(m)+1):n,]))}
  ps<-as.vector(ps)
  m = c(m,n+1)
  
  out <- .C("Snbmult",
            as.double(X),
            as.integer(n),
            as.integer(d),
            as.integer(m),
            Snm=double(1),
            as.double(multipliers),
            as.double(ps),
            as.double(psbk),
            as.integer(M),
            double(M),
            checkDnm=double(M),
            double(d),
            double(d),
            PACKAGE="npcopTest")
  
  pval1<-sum(out$checkDnm>=out$Snm)/M
  stat = out$Snm
  
  
  structure(class = "htest",
            list(method = sprintf("Test for change-point detection based on the multivariate empirical c.d.f. with change in the m.c.d.f. at time(s) m=%s\n",paste(mm, collapse=", ")),
                 statistic = c(Snm = stat),
                 p.value =  pval1,
                 m = c(m=mm),
                 data.name = deparse(substitute(X))))
  
}



