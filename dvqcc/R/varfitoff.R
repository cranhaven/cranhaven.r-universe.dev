# Function to estimate the VAR matrix coefficients
# return VAR coefficients matrix and  theoretical covariance matrix of VAR coefficients



varfitoff=function(data=NULL,size=NULL,newdata=NULL){

  I=dim(data)[1]/size
  n=dim(data)[2]

  if(!is.null(newdata)){

    data1=data-apply(data,1,mean) #removing the intercept

    data2=matrix(0,ncol=n, nrow=size*I)
    for(j in 1:size){
      data2[seq(j,size*I,size),]= data1[seq(j,size*I,size),]-t(matrix(rep(apply(data1[seq(j,size*I,size),],2,mean),I), ncol =I))#detrend
    }


    vec.phis<-matrix(ncol=(size*size), nrow=I)
    cov.B1<-list()
    if(I!=round(I)){stop("Error...number of lines must be multiple of size")}

    for(i in 1:I){

      v1=data2[((i-1)*size +1):(i*size),]
      Y=v1[,2:n]
      Z=v1[,1:(n-1)]
      zz=solve(Z%*%t(Z))
      hatB=Y%*%t(Z)%*%zz    # Pg 72 Helmut or Prop 3.1 Helmut
      vec.phis[i,]=as.vector(hatB)

      S.tilda.res=(1/(n-1))*Y%*%(diag(1,(n-1))-t(Z)%*%zz%*%Z)%*%t(Y)#(3.2.18) Helmut
      S.hat.res=S.tilda.res*(n/(n-1*size))#(3.2.19) Helmut (Covariancia dos residuos via MQO)
      cov.B1[[i]]=kronecker(S.hat.res,zz) # Matriz de variancia e covariancia dos coeficientes
    }

    vec.cov.theoretical= Reduce('+',cov.B1)/I
    vec.cov.empirical=cov(vec.phis)


    #newdata
    Inew=dim(newdata)[1]/size
    vec.phis.new<-matrix(ncol=(size*size), nrow=Inew)
    cov.B1new=list()

    if(Inew!=round(Inew)){stop("Error...number of lines must be multiple of size")}

    if(size!=dim(newdata)[1]/Inew){stop("Error...number of variables in newdata must be the same as in data")}

    if(dim(newdata)[2]!=dim(data)[2]){stop("Error...number of time-instants in newdata must be the same as in data")}

    newdata1=newdata-apply(newdata,1,mean) #removing the intercept

    newdata2=matrix(0,ncol=n, nrow=size*Inew)
 ################################Cpmo desestacionalizar quando Inew=1???
if (Inew>1) {
    for(j in 1:size){
      newdata2[seq(j,size*Inew,size),]= newdata1[seq(j,size*Inew,size),]-t(matrix(rep(apply(newdata1[seq(j,size*Inew,size),],2,mean),Inew), ncol =Inew))#d
      #detrend
      }
} else {

  for(j in 1:size){
  newdata2[seq(j,size*Inew,size),]= newdata1[seq(j,size*Inew,size),]-t(matrix(rep(apply(data1[seq(j,size*I,size),],2,mean),Inew), ncol =Inew))}

}
    for(i in 1:Inew){
      v1=newdata2[((i-1)*size +1):(i*size),]
      Y=v1[,2:n]
      Z=v1[,1:(n-1)]
      zz=solve(Z%*%t(Z))
      hatB=Y%*%t(Z)%*%zz    # Pg 72 Helmut ou Prop 3.1 Helmut
      vec.phis.new[i,]=as.vector(hatB)

      S.tilda.res=(1/(n-1))*Y%*%(diag(1,(n-1))-t(Z)%*%zz%*%Z)%*%t(Y)#(3.2.18) Helmut
      S.hat.res=S.tilda.res*(n/(n-1*size))#(3.2.19) Helmut (Covariancia dos residuos via MQO)
      #S.hat.res[[i]]=S.tilda.res*(1/(p*n-def*p^2-p))#(3.2.21) Helmut (Covariancia dos residuos via MQO)
      cov.B1new[[i]]=kronecker(S.hat.res,zz) # Matriz de variancia e covariancia dos coeficientes
    }

    rout=list( vec.phis=vec.phis, vec.cov.theoretical=vec.cov.theoretical, vec.cov.empirical=vec.cov.empirical, vec.phis.new=vec.phis.new,cov.B1=cov.B1,cov.B1new=cov.B1new,n=n,I=I,Inew=Inew)
  }

  else{

    I=dim(data)[1]/size
    n=dim(data)[2]
    vec.phis<-matrix(ncol=(size*size), nrow=I)
    cov.B1<-list()
    if(I!=round(I)){stop("Error...number of lines must be multiple of size")}

    data=data-apply(data,1,mean) #removing the intercept

    for(j in 1:size){
      data[seq(j,size*I,size),]= data[seq(j,size*I,size),]-t(matrix(rep(apply(data[seq(j,size*I,size),],2,mean),I), ncol =I))#detrend
    }

    for(i in 1:I){

      v1=data[((i-1)*size +1):(i*size),]
      v1=v1-rowMeans(v1)
      Y=v1[,2:n]
      Z=v1[,1:(n-1)]
      zz=solve(Z%*%t(Z))
      hatB=Y%*%t(Z)%*%zz    # Pg 72 Helmut ou Prop 3.1 Helmut
      vec.phis[i,]=as.vector(hatB)

      S.tilda.res=(1/(n-1))*Y%*%(diag(1,(n-1))-t(Z)%*%zz%*%Z)%*%t(Y)#(3.2.18) Helmut
      S.hat.res=S.tilda.res*(n/(n-1*size))#(3.2.19) Helmut (Covariancia dos residuos via MQO)
      #S.hat.res[[i]]=S.tilda.res*(1/(p*n-def*p^2-p))#(3.2.21) Helmut (Covariancia dos residuos via MQO)
      cov.B1[[i]]=kronecker(S.hat.res,zz) # Matriz de variancia e covariancia dos coeficientes
    }
    vec.cov.theoretical= Reduce('+',cov.B1)/I
    vec.cov.empirical=cov(vec.phis)
    #rout=list(vec.phis=vec.phis, vec.cov.theoretical=vec.cov.theoretical, vec.cov.empirical=vec.cov.empirical,cov.B1=cov.B1,cov.B1new=cov.B1new,n=n,I=I)
    rout=list(vec.phis=vec.phis, vec.cov.theoretical=vec.cov.theoretical, vec.cov.empirical=vec.cov.empirical,cov.B1=cov.B1,n=n,I=I)
  }
  return(rout)
}
