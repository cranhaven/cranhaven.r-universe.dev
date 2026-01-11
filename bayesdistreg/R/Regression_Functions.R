#==============================================================================================#
#' Bayesian distribution regression
#'
#' \code{distreg} draws randomly from the density of F(yo) at a threshold value yo
#'
#' @param thresh threshold value that is used to binarise the continuous outcome variable
#' @param data0 original data set with the first column being the continuous outcome variable
#' @param MH metropolis-hastings algorithm to use; default:"IndepMH", alternative "RWMH"
#' @param ... any additional inputs to pass to the MH algorithm
#' @return fitob a vector of fitted values corresponding to the distribution at threshold thresh
#'
#' @examples
#' data0=faithful[,c(2,1)]; qnt<-quantile(data0[,1],0.25)
#' distob<- distreg(qnt,data0,iter = 102, burn = 2); 
#' plot(density(distob,.1),main="Kernel density plot")
#'
#' @export
distreg<- function(thresh,data0,MH="IndepMH",...){
  y = indicat(data0[,1],thresh) #create binary y
  data=data.frame(y,data0[,-1])#create new data set
  if(identical(MH,"IndepMH")){
    MHob=IndepMH(data = data,...)
  }else{
    MHob=RWMH(data = data,...)
  }
  fitob<-fitdist(MHob$Matpram,data)
  return(fitob)
}

#==============================================================================================#
#' Counterfactual bayesian distribution regression
#'
#' \code{distreg} draws randomly from the density of counterfactual of F(yo) at a threshold 
#' value yo
#'
#' @param thresh threshold value that is used to binarise the continuous outcome variable
#' @param data0 original data set with the first column being the continuous outcome variable
#' @param MH metropolis-hastings algorithm to use; default:"IndepMH", alternative "RWMH"
#' @param cft column vector of counterfactual treatment
#' @param cfIND the column index(indices) of treatment variable(s) to replace with \code{cft} 
#' in \code{data0}
#' @param ... any additional inputs to pass to the MH algorithm
#' @return robj a list of a vector of fitted values corresponding to random draws from
#' F(yo), counterfactual F(yo), and the parameters
#'
#' @examples
#' data0=faithful[,c(2,1)]; qnt<-quantile(data0[,1],0.25)
#' cfIND=2 #Note: the first column is the outcome variable. 
#' cft=0.95*data0[,cfIND] # a decrease by 5%
#' dist_cfa<- distreg_cfa(qnt,data0,cft,cfIND,MH="IndepMH",iter = 102, burn = 2)
#' par(mfrow=c(1,2)); plot(density(dist_cfa$counterfactual,.1),main="Original")
#' plot(density(dist_cfa$counterfactual,.1),main="Counterfactual"); par(mfrow=c(1,1))
#' 
#' @export
distreg_cfa<- function(thresh,data0,MH="IndepMH",cft,cfIND,...){
  y = indicat(data0[,1],thresh) #create binary y
  data=data.frame(y,data0[,-1])#create new data set
  if(identical(MH,"IndepMH")){
    MHob=IndepMH(data = data,...)
  }else{
    MHob=RWMH(data = data,...)
  }
  data.cf<- data
  data.cf[,cfIND]<- cft
  fitob.o<-fitdist(MHob$Matpram,data)
  fitob.cf<-fitdist(MHob$Matpram,data.cf)
  robj<- list(original=fitob.o,counterfactual=fitob.cf,Matpram=MHob$Matpram)
  return(robj)
}
#==============================================================================================#
#' Parallel compute bayesian distribution regression
#'
#' \code{par_distreg} uses parallel computation to compute bayesian distribution regression for a given
#' vector of threshold values and a data (with first column being the continuous outcome variable)
#'
#' @param thresh vector of threshold values.
#' @param data0 the original data set with a continous dependent variable in the first column
#' @param fn bayesian distribution regression function. the default is distreg provided in the package
#' @param no_cores number of cores for parallel computation
#' @param type \code{type} passed to \code{makeCluster()} in the package \code{parallel}
#' @param ... any additional input parameters to pass to fn
#' @return mat a G x M matrix of output (G is the length of thresh, M is the number of draws)
#'
#' @examples
#' data0=faithful[,c(2,1)]; qnts<-quantile(data0[,1],c(0.05,0.25,0.5,0.75,0.95))
#' out<- par_distreg(qnts,data0,no_cores=1,iter = 102, burn = 2)
#' par(mfrow=c(3,2));invisible(apply(out,1,function(x)plot(density(x,30))));par(mfrow=c(1,1))
#'
#' @export
par_distreg<-function(thresh,data0,fn=distreg,no_cores=1,type = "PSOCK",...){ #takes a vector of threshold values
c1<-parallel::makeCluster(no_cores, type = type)
mat<- parallel::parSapply(c1,thresh,fn,data0=data0,...)
parallel::stopCluster(c1)
mat<- t(mat); mat<- t(apply(mat,1,sort))
return(mat)
}
#==============================================================================================#
#' Binary glm object at several threshold values
#' 
#' \code{dr_asympar} computes a normal approximation of the likelihood at a vector of threshold
#' values
#' 
#' @param y outcome variable
#' @param x matrix of covariates
#' @param thresh vector of threshold values on the support of outcome y
#' @param ... additional arguments to pass to \code{lapl_aprx2}
#' @return a list of glm objects corresponding to \code{thresh}
#' 
#' @examples 
#' y = faithful$waiting
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' qtaus = quantile(y,c(0.05,0.25,0.5,0.75,0.95))
#' drabj<- dr_asympar(y=y,x=x,thresh = qtaus)
#' lapply(drabj,coef); lapply(drabj,vcov) 
#' # mean and covariance at respective threshold values
#' 
#' @export
#' 
dr_asympar<- function(y,x,thresh,...){
  lfun= function(y0) lapl_aprx2(indicat(y,y0),x,...)
  lapply(thresh,lfun)
}

#==============================================================================================#
#' Semi-asymptotic bayesian distribution
#' 
#' \code{distreg.sas} takes input object from dr_asympar() for semi asymptotic bayesian 
#' distribution. This involves taking random draws from the normal approximation of the 
#' posterior at each threshold value.
#' 
#' @param ind index of object in list \code{drabj} (i.e. a threshold value) from which to take draws
#' @param drabj object from dr_asympar()
#' @param data dataframe, first column is the outcome
#' @param vcovfn a string denoting the function to extract the variance-covariance. Defaults at
#' "vcov". Other variance-covariance estimators in the sandwich package are usable.
#' @param iter number of draws to simulate
#' @return fitob vector of random draws from density of F(yo) using semi-asymptotic BDR
#' 
#' @examples 
#' y = faithful$waiting
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' qtaus = quantile(y,c(0.05,0.25,0.5,0.75,0.95))
#' drabj<- dr_asympar(y=y,x=x,thresh = qtaus); data = data.frame(y,x)
#' drsas1 = lapply(1:5,distreg.sas,drabj=drabj,data=data,iter=100)
#' drsas2 = lapply(1:5,distreg.sas,drabj=drabj,data=data,vcovfn="vcovHC",iter=100)
#' par(mfrow=c(3,2));invisible(lapply(1:5,function(i)plot(density(drsas1[[i]],.1))));par(mfrow=c(1,1))
#' par(mfrow=c(3,2));invisible(lapply(1:5,function(i)plot(density(drsas2[[i]],.1))));par(mfrow=c(1,1))
#' 
#' @export
#' 
distreg.sas<- function(ind,drabj,data,vcovfn="vcov",iter=100){
  seed=ind
  set.seed(seed)
  mu = stats::coef(drabj[[ind]])
  if(identical(vcovfn,"vcov")){
    Zigma=stats::vcov(drabj[[ind]])
  }else{
    fn = getExportedValue("sandwich", vcovfn)
    Zigma=fn(drabj[[ind]])
  }
  Matpram=MASS::mvrnorm(n=iter,mu=mu,Sigma=Zigma)
  fitob<-fitdist(Matpram,data)
  return(fitob)
}



#==============================================================================================#
#' Semi-asymptotic counterfactual distribution
#' 
#' \code{distreg_cfa.sas} takes input object from dr_asympar() for counterfactual semi 
#' asymptotic bayesian distribution. This involves taking random draws from the normal 
#' approximation of the posterior at each threshold value.
#' 
#' @param ind index of object in list \code{drabj} (i.e. a threshold value) from which to take draws
#' @param drabj object from dr_asympar()
#' @param data dataframe, first column is the outcome
#' @param cft column vector of counterfactual treatment
#' @param cfIND the column index(indices) of treatment variable(s) to replace with \code{cft} 
#' in \code{data0}
#' @param vcovfn a string denoting the function to extract the variance-covariance. Defaults at
#' "vcov". Other variance-covariance estimators in the sandwich package are usable.
#' @param iter number of draws to simulate
#' @return fitob vector of random draws from density of F(yo) using semi-asymptotic BDR
#' 
#' @examples 
#' y = faithful$waiting
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' qtaus = quantile(y,c(0.05,0.25,0.5,0.75,0.95))
#' drabj<- dr_asympar(y=y,x=x,thresh = qtaus); data = data.frame(y,x)
#' cfIND=2 #Note: the first column is the outcome variable. 
#' cft=0.95*data[,cfIND] # a decrease by 5%
#' cfa.sasobj<- distreg_cfa.sas(ind=2,drabj,data,cft,cfIND,vcovfn="vcov")
#' par(mfrow=c(1,2)); plot(density(cfa.sasobj$original,.1),main="Original")
#' plot(density(cfa.sasobj$counterfactual,.1),main="Counterfactual"); par(mfrow=c(1,1))
#' 
#' @export
#' 
distreg_cfa.sas<- function(ind,drabj,data,cft,cfIND,vcovfn="vcov",iter=100){
  seed=ind
  set.seed(seed)
  mu = stats::coef(drabj[[ind]])
  if(identical(vcovfn,"vcov")){
    Zigma=stats::vcov(drabj[[ind]])
  }else{
    fn = getExportedValue("sandwich", vcovfn)
    Zigma=fn(drabj[[ind]])
  }
  Matpram=MASS::mvrnorm(n=iter,mu=mu,Sigma=Zigma)
  
  data.cf<- data
  data.cf[,cfIND]<- cft
  fitob.o<-fitdist(Matpram,data)
  fitob.cf<-fitdist(Matpram,data.cf)
  robj<- list(original=fitob.o,counterfactual=fitob.cf,Matpram=Matpram)
  return(robj)
}

#==============================================================================================#
#' Asymptotic distribution regression
#' 
#' \code{distreg.asymp} takes input object from dr_asympar() for asymptotic bayesian distribution.
#' 
#' @param ind index of object in list \code{drabj} (i.e. a threshold value) from which to take draws
#' @param drabj object from dr_asympar()
#' @param data dataframe, first column is the outcome
#' @param vcovfn a string denoting the function to extract the variance-covariance. Defaults at
#' "vcov". Other variance-covariance estimators in the sandwich package are usable.
#' @param ... additional input to pass to \code{vcovfn}
#' @return a mean \code{Fhat} and a variance \code{varF}
#' 
#' @examples 
#' y = faithful$waiting
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' qtaus = quantile(y,c(0.05,0.25,0.5,0.75,0.95))
#' drabj<- dr_asympar(y=y,x=x,thresh = qtaus); data = data.frame(y,x)
#' (asymp.obj<- distreg.asymp(ind=2,drabj,data,vcovfn="vcov"))
#' 
#' @export
#' 
distreg.asymp<- function(ind,drabj,data,vcovfn="vcov",...){
  N = nrow(data)
  mu = stats::coef(drabj[[ind]]); k = length(mu)
  if(identical(vcovfn,"vcov")){
    Zigma=stats::vcov(drabj[[ind]],...)
  }else{
    fn = getExportedValue("sandwich", vcovfn)
    Zigma=fn(drabj[[ind]],...)
  }
  Zigma=as.matrix(Zigma)
  mn = fitdist(matrix(mu,nrow = 1),data) #mean fitted distribution at threshold
  rk = 0
  dmat<-as.matrix(cbind(1,data[,-1])) #obtain design matrix
  lcomb = stats::dlogis(dmat%*%matrix(mu,ncol=1))
  
  for(i in 1:N){
    zk = matrix(as.numeric(lcomb[i]*dmat[i,]),nrow = 1)
    rk=rk+(zk%*%Zigma%*%t(zk))
  }
  vr = rk/(N-k); #adjust for degrees of freedom
  list(Fhat=mn,varF=vr)
}
#==============================================================================================#
#' Joint asymptotic mutivariate density of parameters
#' 
#' \code{jdpar.asymp} takes input object from dr_asympar() for asymptotic bayesian distribution.
#' It returns objects for joint mutivariate density of parameters across several thresholds.
#' Check for positive definiteness of the covariance matrix, else exclude thresholds yielding
#' negative eigen values.
#' 
#' @param drabj object from dr_asympar()
#' @param data dataframe, first column is the outcome
#' @param jdF logical to return joint density of F(yo) across thresholds in drabj
#' @param vcovfn a string denoting the function to extract the variance-covariance. Defaults at
#' "vcov". Other variance-covariance estimators in the sandwich package are usable.
#' @param ... additional input to pass to \code{vcovfn}
#' @return mean vector Theta and variance-covariance matrix vcovpar of parameters across 
#' thresholds and if \code{jdF=TRUE}, 
#' a mean vector \code{mnF} and a variance-covariance matrix \code{vcovF} of F(yo)
#' 
#' @examples 
#' y = faithful$waiting
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' qtaus = quantile(y,c(0.05,0.25,0.5,0.75,0.95))
#' drabj<- dr_asympar(y=y,x=x,thresh = qtaus); data = data.frame(y,x)
#' (drjasy = jdpar.asymp(drabj=drabj,data=data,jdF=TRUE))
#' 
#' @export
#' 
jdpar.asymp<- function(drabj,data,jdF=FALSE, vcovfn="vcovHC",...){
  k = length(drabj[[1]]$coefficients)
  G = length(drabj)
  N = length(drabj[[1]]$fitted.values)
  if(jdF){
    matjdF = matrix(NA,G,G)
    mnF = rep(NA,G)
  }
  
  if(identical(vcovfn,"vcov")){
    vcfn = getExportedValue("stats", vcovfn)
  }else{
    vcfn = getExportedValue("sandwich", vcovfn)
  }
  #initialise null matrix to store variance-covariance matrix
  # draw from the joint distribution of parameters using a gibbs sampler
  matcov<- matrix(NA,(G*k),(G*k))
  vecpar<- rep(NA,(G*k))
  for(g in 1:G){
    id=c(((g-1)*k +1):(g*k))
    vecpar[id] = stats::coef(drabj[[g]])
    scg = sandwich::estfun(drabj[[g]])
    for(h in 1:G){
      jd=c(((h-1)*k +1):(h*k))
      if(h!=g){
        sch = sandwich::estfun(drabj[[h]])
        scgh = 0
        for(i in 1:N){
          scgh = scgh + scg[i,]%*%t(scg[i,])
        }
        scgh = scgh/N
        matcov[id,jd] = stats::vcov(drabj[[g]],...)%*%scgh%*%stats::vcov(drabj[[h]],...)
        matcov[jd,id] = matcov[jd,id] #by symmetry of the var-cov
      }else if(g==h){
        matcov[id,jd] = vcfn(drabj[[g]],...)
      }
      if(jdF){
        mug = stats::coef(drabj[[g]])
        mng = fitdist(matrix(mug,nrow = 1),data) #mean fitted distribution at threshold
        mnF[g]=mng
        rk = 0
        dmat<-as.matrix(cbind(1,data[,-1])) #obtain design matrix
        lcombg = stats::dlogis(dmat%*%matrix(mug,ncol=1))
        if(g!=h){
          muh = stats::coef(drabj[[h]])
          lcombh = stats::dlogis(dmat%*%matrix(muh,ncol=1))
          for(i in 1:N){
            zk = matrix(as.numeric(lcombg[i]*dmat[i,]),nrow = 1)
            vk = matrix(as.numeric(lcombh[i]*dmat[i,]),nrow = 1)
            tzk = t(vk)
            rk=rk+(zk%*%matcov[id,jd]%*%tzk)
          }
        }else{
          for(i in 1:N){
            zk = matrix(as.numeric(lcombg[i]*dmat[i,]),nrow = 1)
            tzk = t(zk)
            rk=rk+(zk%*%matcov[id,jd]%*%tzk)
          }
        }
        
        matjdF[g,h] = rk/N;
      }
    }
  }
  varpar=0.5*(matcov + t(matcov)) #ensure symmetry
  if(!jdF){
    ans=list(Theta=vecpar,vcovpar=varpar)
  }else{
    matjdF=0.5*(matjdF + t(matjdF))
    ans=list(Theta=vecpar,vcovpar=varpar,mnF=mnF,vcovF=matjdF)
  }
  return(ans)
}






#==============================================================================================#
