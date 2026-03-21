# Function to estimate the VAR matrix coefficients
# return VAR coefficients matrix and  theoretical covariance matrix of VAR coefficients



varfiton=function(data=NULL,size=NULL,newdata=NULL,Lc,Lr,confidence.level,covvar){

  I=dim(data)[1]/size
  n=dim(data)[2]
  C=size*size

  if(!is.null(newdata)){

    data1=data-apply(data,1,mean) #removing the intercept

    data2=matrix(0,ncol=n, nrow=size*I)
    for(j in 1:size){
      data2[seq(j,size*I,size),]= data1[seq(j,size*I,size),]-t(matrix(rep(apply(data1[seq(j,size*I,size),],2,mean),I), ncol =I))#detrend
    }

    if(I!=round(I)){stop("Error...number of lines must be multiple of size")}


    # Fase I2
    #Lc > Lr
    I2=round(I/2)

    def=1 # Var lag
    b_j=list()
    b_js=list()
    c_j=list()
    c_js=list()

    for(i2 in 1:I2){
      vnI2 <-data2[((i2-1)*size +1):(i2*size),]
      #for(t in 1:(n-1)){
      for(t in 1:(n)){
        si=sample(c((I2+1):I),1)
        viI2=data2[((si-1)*size +1):(si*size),1:Lc]
        if (t<=Lr){
          v1cI2=cbind(viI2[,t:Lc],vnI2[,1:t])
        }else{
          v1cI2=cbind(viI2[,(Lr):Lc],vnI2[,(t-Lr+1):t])
        }


        YnI2=v1cI2[,(def+1):Lc]
        ZnI2=v1cI2[,1:(Lc-def)]

        zznI2=solve(ZnI2%*%t(ZnI2))
        hatBnI2=YnI2%*%t(ZnI2)%*%zznI2
        S.tilda.resnI2=(1/(Lc-def))*YnI2%*%(diag(1,(Lc-def))-t(ZnI2)%*%zznI2%*%ZnI2)%*%t(YnI2)#(3.2.18) Helmut
        S.hat.resnI2=S.tilda.resnI2*(Lc/(Lc-def*size))#(3.2.19) Helmut (Covariancia dos residuos via MQO)
        #S.hat.res[[i]]=S.tilda.res*(1/(p*n-def*p^2-p))#(3.2.21) Helmut (Covariancia dos residuos via MQO)
        cov.B1nI2=kronecker(S.hat.resnI2,zznI2) # Matriz de variancia e covariancia dos coeficientes

        b_j[[t]]=as.vector(t(hatBnI2))
        c_j[[t]]=cov.B1nI2

      } #t
      b_js[[i2]]=b_j
      c_js[[i2]]=c_j
    } #i2


    covMeanRef=list()      # Theoretical covariance
    covMeanRef_Emp=list()  # Empirical covariance
    #for(t in 1:(n-1)){
    for(t in 1:(n)){
      covlist=list()#
      matrix_phis=matrix(ncol=size*size,nrow=I2)
      for(i in 1:I2){
        covlist[[i]]=c_js[[i]][[t]]#
        matrix_phis[i,]=b_js[[i]][[t]]

      }
      covMeanRef[[t]]=(Reduce('+',covlist)/I2)#
      covMeanRef_Emp[[t]]=cov(matrix_phis)#
    }

    if(covvar=="empirical"){covMeanRef=covMeanRef_Emp}



    covT=list()
    meanT=list()
    qt=c()
    qwt=c()
   # for(j in 1:(n-1)){
    for(j in 1:(n)){
      #ht=c()
      wt=c()
      Cb_js=matrix(ncol=size*size,nrow=I2)
      for(i2 in 1:I2){
        Cb_js[i2,]=b_js[[i2]][[j]]
      }
      for(i2 in 1:I2){
        covT[[j]]=cov(Cb_js)
        meanT[[j]]=colMeans(Cb_js)
        U=c_js[[i2]][[j]]
        #ht[i2]=mahalanobis(Cb_js[i2,],center=meanT[[j]],cov= covT[[j]],inverted=FALSE)
        wt[i2]<- -C*Lc+C*Lc*log(Lc)-Lc*log(det((Lc-1)*U)/det(covMeanRef[[j]]))+sum(diag(solve(covMeanRef[[j]])%*%((Lc-1)*U)))

      }

      #alfa=1-((1-confidence.level)^(1/(n-1)))
      #qt[j]=(C*(I*(n-1)-1)/(I*(n-1)-C))*qf(alfa,C,(I*(n-1)-C))
      #qt[j]=(C*(I*(n-1)-1)/(I*(n-1)-C))*qf(confidence.level,C,(I*(n-1)-C))



      qt[j]=(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))
      #qt[j]=quantile(ht,confidence.level)
      qwt[j]=quantile(wt,confidence.level)

    }








         #newdata
         Inew=dim(newdata)[1]/size


         if(Inew!=round(Inew)){stop("Error...number of lines must be multiple of size")}

         if(size!=dim(newdata)[1]/Inew){stop("Error...number of variables in newdata must be the same as in data")}

         if(dim(newdata)[2]!=dim(data)[2]){stop("Error...number of time-instants in newdata must be the same as in data")}

         newdata1=newdata-apply(newdata,1,mean) #removing the intercept

         newdata2=matrix(0,ncol=n, nrow=size*Inew)


         if (Inew>1) {
           for(j in 1:size){
             newdata2[seq(j,size*Inew,size),]= newdata1[seq(j,size*Inew,size),]-t(matrix(rep(apply(newdata1[seq(j,size*Inew,size),],2,mean),Inew), ncol =Inew))#d
             #detrend
           }
         } else {

           for(j in 1:size){
             newdata2[seq(j,size*Inew,size),]= newdata1[seq(j,size*Inew,size),]-t(matrix(rep(apply(data1[seq(j,size*I,size),],2,mean),Inew), ncol =Inew))}

         }


       #     for(j in 1:size){
       #    newdata2[seq(j,size*Inew,size),]= newdata1[seq(j,size*Inew,size),]-t(matrix(rep(apply(newdata1[seq(j,size*Inew,size),],2,mean),Inew), ncol =Inew))#d
          #detrend
      #     }

        # tnew_l=matrix(nrow=Inew,ncol=(n-1))
         #wnew_l=matrix(nrow=Inew,ncol=(n-1))

         tnew_l=matrix(nrow=Inew,ncol=(n))
         wnew_l=matrix(nrow=Inew,ncol=(n))

         for(i in 1:Inew){
           vnI2new <-newdata2[((i-1)*size +1):(i*size),]
           #tnew=numeric(length=(n-1))
          # wnew=numeric(length=(n-1))
           tnew=numeric(length=(n))
           wnew=numeric(length=(n))

           #for(t in 1:(n-1))
           for(t in 1:(n)){
             si=sample(c((I2+1):I),1)
             viI2=data2[((si-1)*size +1):(si*size),1:Lc]
             if (t<=Lr){
               v1cI2new=cbind(viI2[,t:Lc],vnI2new[,1:t])
             }else{
               v1cI2new=cbind(viI2[,(Lr):Lc],vnI2new[,(t-Lr+1):t])
             }

             # readline("Press <return to continue")

             YnI2=v1cI2new[,(def+1):Lc]
             ZnI2=v1cI2new[,1:(Lc-def)]

             zznI2=solve(ZnI2%*%t(ZnI2))
             hatBnI2=YnI2%*%t(ZnI2)%*%zznI2
             S.tilda.resnI2=(1/(Lc-def))*YnI2%*%(diag(1,(Lc-def))-t(ZnI2)%*%zznI2%*%ZnI2)%*%t(YnI2)#(3.2.18) Helmut
             S.hat.resnI2=S.tilda.resnI2*(Lc/(Lc-def*size))#(3.2.19) Helmut (Covariancia dos residuos via MQO)
             #S.hat.res[[i]]=S.tilda.res*(1/(p*n-def*p^2-p))#(3.2.21) Helmut (Covariancia dos residuos via MQO)
             cov.B1nI2=kronecker(S.hat.resnI2,zznI2) # Matriz de variancia e covariancia dos coeficientes



             bnew_j=as.vector(t(hatBnI2)) #c(coef11nI2,coef12nI2,coef21nI2,coef22nI2)
             cnew_j=cov.B1nI2
             tnew[t]= mahalanobis(bnew_j,center=meanT[[t]],cov= covT[[t]],inverted=FALSE)
             wnew[t]= -C*Lc+C*Lc*log(Lc)-Lc*log(det((Lc-1)*cnew_j)/det(covMeanRef[[t]]))+sum(diag(solve(covMeanRef[[t]])%*%((Lc-1)*cnew_j)))

           }
           tnew_l[i,]=tnew
           wnew_l[i,]=wnew
           #resnwI2[[i2]]= resnw
         }




    return(list(Lim_T2=qt,Lim_W=qwt,tnew=tnew_l,wnew=wnew_l,I=I,C=C,Inew=Inew,n=n,cov=covMeanRef))#completar a saida
  }
}

