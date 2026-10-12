distancem<-function(a,b){
n<-length(a)
distan1<-0
if(length(a)!=length(b)) distance = -1;
    for (i in 1:length(a)){
        distan1 <- distan1 + (a[i]!= b[i])
     }
return(distan1)
}


Mode <- function(x) {
x<-sort(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


Modv<-function(x){
Len<-dim(x)
re<-NULL
for (j in 1:Len[2]){
re[j]<-Mode(x[,j])
}
re}


hammingD<-DistM<-function(dat){
  Len<-dim(dat)
  ss<-Len[1]
  dismat<- matrix(1,ncol=ss,ss)
  for(i in 1:ss){
    if (i==ss) break
    for(j in (i:ss)){
      dismat[i,j]<-mean(dat[i,]==dat[j,],na.rm=T)
    }
  }
  return(1-t(dismat))
}


HCALB<-function (x,kn0){
  #x<-x[,-(dim(x)[2])]
  REDIST<-as.dist(DistM((x)))
  hc <- hclust(REDIST,method = "average")
  cutree(hc,k=kn0)
}
