mypca.fd <- function(fdobj,center=TRUE){
  call = match.call()
  if (inherits(fdobj,"list")){
    #save object before center data
    mean_fd<-list()
    for (i in 1:length(fdobj)){
      mean_fd[[i]]<-fdobj[[i]]
    }
    
    #center each functional dataset
    if (isTRUE(center)){
      for (i in 1:length(fdobj)){
        coefmean <- apply(fdobj[[i]]$coefs, 1, mean)
        fdobj[[i]]$coefs <- sweep(fdobj[[i]]$coefs, 1, coefmean)
        mean_fd[[i]]$coefs = as.matrix(data.frame(mean=coefmean))
      }
    }
    
    #Calculate W for each dataset, matrix of basis functions inner product
    for (i in 1:length(fdobj)){
      name<-paste('W_var',i,sep='')
      W_fdobj<-inprod(fdobj[[i]]$basis,fdobj[[i]]$basis)
      assign(name,W_fdobj)
    }
    
    #Addition of 0 before merging all matrices into \phi (cf. Publication)
    prow<-dim(W_fdobj)[[1]]
    pcol<-length(fdobj)*prow
    W1<-cbind(W_fdobj,matrix(0,nrow=prow,ncol=(pcol-ncol(W_fdobj))))
    W_list<-list()
    for (i in 2:(length(fdobj))){
      W2<-cbind(matrix(0,nrow=prow,ncol=(i-1)*ncol(W_fdobj)),get(paste('W_var',i,sep='')),matrix(0,nrow=prow,ncol=(pcol-i*ncol(W_fdobj))))
      W_list[[i-1]]<-W2
    }
    
    #Creation of \phi
    W_tot<-rbind(W1,W_list[[1]])
    if (length(fdobj)>2){
      for(i in 2:(length(fdobj)-1)){
        W_tot<-rbind(W_tot,W_list[[i]])
      }
    }
    #To avoid numerical issues, the smallest inner product are set to 0
    W_tot[W_tot<1e-15]=0
    
    #Creation of C, the coefficients matrix
    coef<-t(fdobj[[1]]$coefs)
    for (i in 2:length(fdobj)){
      coef<-cbind(coef,t(fdobj[[i]]$coefs))
    }
    
    mat_interm<-1/sqrt(ncol(fdobj[[1]]$coefs)-1)*coef%*%chol(W_tot,pivot=TRUE)
    cov<-t(mat_interm)%*%mat_interm
    valeurs<-Eigen(cov)
    valeurs_propres<-valeurs$values
    vecteurs_propres<-valeurs$vectors
    #Calculation on eigenfunctions coefficients
    bj<-solve(chol(W_tot))%*%vecteurs_propres
    fonctionspropres<-fdobj[[1]]
    fonctionspropres$coefs<-bj
    scores<-coef%*%W_tot%*%bj
    
    varprop<-valeurs_propres/sum(valeurs_propres)
    
    pcafd<-list(call=call,values=valeurs_propres,harmonics=fonctionspropres,scores=scores,U=bj,varprop=varprop,meanfd=mean_fd,Wmat=W_tot)
    
  } else {
    #save object before center data
    mean_fd<-fdobj
    if (isTRUE(center)){
      #center each functional dataset
      coefmean <- apply(fdobj$coefs, 1, mean)
      fdobj$coefs <- sweep(fdobj$coefs, 1, coefmean)
      mean_fd$coefs = as.matrix(data.frame(mean=coefmean))
    }
    #Calculate W, matrix of basis functions inner product
    W<-inprod(fdobj$basis,fdobj$basis)
    #To avoid numerical issues, the smallest inner product are set to 0
    W[W<1e-15]=0
    
    coef<-t(fdobj$coefs)
    mat_interm<-1/sqrt(ncol(fdobj$coefs)-1)*coef%*%chol(W)
    cov<-t(mat_interm)%*%mat_interm
    valeurs<-Eigen(cov)
    valeurs_propres<-valeurs$values
    vecteurs_propres<-valeurs$vectors
    #Calculation on eigenfunctions coefficients
    fonctionspropres<-fdobj
    bj<-solve(chol(W))%*%vecteurs_propres
    fonctionspropres$coefs<-bj
    #Scores calculation according to pca.fd formula
    scores<-inprod(fdobj,fonctionspropres)
    
    varprop<-valeurs_propres/sum(valeurs_propres)
    
    pcafd <-list(call=call,values=valeurs_propres,harmonics=fonctionspropres,scores=scores,U=bj, varprop=varprop,meanfd=mean_fd,Wmat=W)
    
  }
  class(pcafd) <- "mfpca"
  return(pcafd)
}
