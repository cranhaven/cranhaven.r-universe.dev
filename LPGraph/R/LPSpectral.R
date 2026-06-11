LPSpectral <-
function(W,k,m=8,sparse=TRUE){
    if(k>m){
        k=m;
        warning("k is greater than m, returning first m approximations.")
    }
    P=W/sum(W)
px <- as.vector(apply(P,2,"sum"))
S <- as.matrix(LP.basis(px,m+1))[,1:m]
L <- t(S)%*%P%*%S
if(sparse==FALSE){
        L.svd <-svd(L)
        T <- S%*%L.svd$u[,1:k]
sval <-L.svd$d[1:k]
}else{
v<-svd(L)$u[,1]
    cv.out<-PMA::SPC.cv(L,sumabsvs=seq(1.1, sqrt(m),len=10),niter=50,
trace=FALSE,center=FALSE)
Lspc=PMA::SPC(L,K=k,sumabsv=cv.out$bestsumabsv,v=v,
niter=100,trace=FALSE,center=FALSE)
T<-S%*%Lspc$v
sval<-Lspc$d
}
R<-list()
R$LP   <- L
R$Phi  <- T
    R$sval <-sval 
return(R)
}
