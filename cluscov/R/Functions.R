#===============================================================================#

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#===============================================================================#
# Notes :
# when Eror in FUN(X[[i]],...) : shows, run devtools::load_all(".")
#
# 1. use the current R Development Version (that will eventually become 3.4)
# 2. Run the tools::package_native_routine_registration_skeleton(".") and copy and
#    paste the full output in a packagename_init.c file to be put in src/
# 3. update NAMESPACE, verifying that useDynLib(packagename, .registration = TRUE)
# 4. If necessary, replace the exportPattern with export( list of object to be exported )
#===============================================================================#

#' Linear regression via coordinate descent with covariate clustering
#'
#' Covariate assignment to k clusters using the coordinate descent algorithm. This
#' function is a wrapper for the \code{C} function \code{linreg_coord_clus}
#'
#' @param Y vector of outcome variable
#' @param X matrix of covariates. Should not include 1's for the intercept
#' @param k number of clusters
#' @param coefs vector of coefficients as starting values. Should not include the intercept.
#' @param clus vector of covariate cluster assignments as starting values
#' @param clusmns vector k cluster parameter centers
#' @param nC first nC-1 covariates in X not to cluster. Must be at least 1 for the intercept
#' @param x a logical for returning the design matrix
#' @return clus cluster assignments
#' @return coefs vector of coefficients as starting values
#' @return clusmns vector of cluster means
#'
#'
#' @examples
#' set.seed(14) #Generate data
#' N = 1000; (bets = rep(-2:2,4)); p = length(bets); X = matrix(rnorm(N*p),N,p)
#' Y = cbind(1,X)%*%matrix(c(0.5,bets),ncol = 1)
#' begin_v<- rep(NA,p)
#' for (j in 1:p) {
#'  begin_v[j] = stats::coef(lm(Y~X[,j]))[2]
#' }
#' set.seed(12); klus_obj<- kmeans(begin_v,centers = 5)
#' linrclus(Y,X,k=5,coefs=c(0,begin_v),clus=klus_obj$cluster,clusmns=klus_obj$centers)

#' @useDynLib cluscov linreg_coord_clus
#' @export

linrclus<- function(Y,X,k,coefs,clus,clusmns,nC=1,x=FALSE){
  X = cbind(1,X)
  nrX = as.integer(nrow(X)); ncX = as.integer(ncol(X)); k = as.integer(k)
  Xdot = apply(X, 2, function(x)sum(x^2)); clus=as.integer(c(clus-1)); nC=as.integer(nC)
  ans=.C("linreg_coord_clus",as.double(Y),Xm=as.double(X),coefs=as.double(coefs),clus=as.integer(clus),
         klus=as.integer(c(0,clus)),double(nrX),as.double(Xdot),clusmns=as.double(clusmns),
         nrX,ncX,k,nC,PACKAGE = "cluscov")
  if(!x){
    rval<- list(clus=(ans$clus+1),coefs=ans$coefs,clusmns=ans$clusmns)
  }else{
  rval<- list(clus=(ans$clus+1),coefs=ans$coefs,clusmns=ans$clusmns,X=matrix(ans$Xm,nrow = nrX))
  }
  return(rval)
}

#=============================================================================================>
#' Linear regression via coordinate descent with covariate clustering
#'
#' This function is a wrapper for \code{linrclus}. It requires less input.
#'
#' @param Y vector of outcome variable
#' @param X matrix of covariates. Should not include 1's for the intercept
#' @param k number of clusters
#' @param nC first nC-1 covariates in X not to cluster. Must be at least 1 for the intercept
#' @param ... additional parameters to be passed to \link[stats]{lm}
#'
#' @return \code{mobj}  the low dimension \link[stats]{lm} regression  object
#' @return \code{clus}  cluster assignments of covariates (excluding the first nC
#' covariates - including the intercept 1)
#'
#' @examples
#' set.seed(14) #Generate data
#' N = 1000; (bets = rep(-2:2,4)); p = length(bets); X = matrix(rnorm(N*p),N,p)
#' Y = cbind(1,X)%*%matrix(c(0.5,bets),ncol = 1)
#' CCRls.coord(Y,X,k=5,nC=1)
#' @export
CCRls.coord<- function(Y,X,k,nC=1,...){
  p = ncol(X); n = nrow(X)
  bet_vec<- rep(NA,p) # vector to store parameters, excludes intercept
  # initialise parameters
  for (j in 1:p) {
    coefs<- stats::coef(stats::lm.fit(as.matrix(cbind(1,X[,j])),Y,...))
    bet_vec[j]<- coefs[2]
  }
  clus=dcluspar(k,bet_vec)
  uniclus<- unique(clus)
  clusmns<- rep(NA,k)
  for (j in 1:k) {clusmns[j]<-mean(bet_vec[clus==uniclus[j]]) }
  ans=linrclus(Y,X,k,c(0,bet_vec),clus,clusmns,nC,x=TRUE)
  X1 <- matrix(NA,n,k); Xnc=ans$X[,(1:nC)]; Xc = ans$X[,-c(1:nC)]
  for(j in 1: k){ X1[,j] <- apply(as.matrix(Xc[,which(clus == j)]),1,sum)}
  model1 <- stats::lm(Y~as.matrix(cbind(Xnc,X1)[,-1]),...)
  list(mobj=model1,clus=clus)
}

#===================================================================================>
#' Integer Golden Search Minimisation
#'
#' This function conducts an integer golden search minimisation of a univariate function.
#'
#' @param fn  function to be minimised. \strong{fn} should return a list, with \strong{fval}
#' as the function value.
#' @param interval a vector of length two containing the minimum and maximum interger
#' values within which to search for the minimiser.
#' @param  tol the tolerance level. Defaults at 1
#'
#' @return k minimiser of \code{fn()}
#' @return crit the minimum
#' @return iter total number of iterations
#' @return iterfn total number of function evaluations of \code{fn()}
#' @return fobj an object of the function minimisation
#' @return key a logical for warning if \code{fobj} may not correspond to \code{k}
#'
#' @examples
#' set.seed(14) #Generate data
#' N = 1000; (bets = rep(-2:2,4)); p = length(bets); X = matrix(rnorm(N*p),N,p)
#' Y = cbind(1,X)%*%matrix(c(0.5,bets),ncol = 1)
#' fn=function(k){du=CCRls.coord(Y,X,k=k,nC=1)
#' return(list(fval=BIC(du$mobj),obj=du))}
#' goldopt(fn=fn,interval=c(2,7),tol=1)
#' @export

goldopt<- function(fn,interval,tol=1){
  a=min(interval); b = max(interval); key=0
  xvals<- c(0); xvals[1]<- a; xvals[2]<- b
  fvals<- c(0)
  faobj<-fn(a); fa = faobj$fval; fbobj<-fn(b); fb = fbobj$fval;
  fvals[1]<- fa; fvals[2]<- fb
  cnt=2; # set counter to 2 for function evaluations
  phi<- (1+sqrt(5))/2

  c = ceiling(b - (b-a)/phi); d = floor(a + (b-a)/phi);
  if(any(xvals==c)){
    c=xvals[which(xvals==c)]; fc=fvals[which(xvals==c)]
    warning("Function object may not correspond to optimal number of clusters")
    key=1
  }else{
    cnt=cnt+1
    fcobj<- fn(c); fc=fcobj$fval
  }
  if(any(xvals==d)){
    d=xvals[which(xvals==d)]; fd=fvals[which(xvals==d)];
    warning("Function object may not correspond to optimal number of clusters")
    key=1
  }else{
    cnt=cnt+1
    fdobj<- fn(d); fd=fdobj$fval
  }

  l = 1; # set counter for iterations
  message("iter = ",l,"\n");arreter = 0
  while(abs(c-d)>tol & arreter==0){# while c d
    if(fc<fd){
      b=d; fb=fd; fbobj=fdobj
      d=c;fd=fc; fdobj=fcobj
      c = ceiling(b-(b-a)/phi);
      if(any(xvals==c)){
        c=xvals[which(xvals==c)]; fc=fvals[which(xvals==c)]
        warning("Function object may not correspond to optimal number of clusters")
        key=1
      }else{
        cnt=cnt+1
        fcobj<- fn(c); fc=fcobj$fval
      }

    }else if(fc>fd){
      a=c; fa = fc; faobj=fcobj;
      c=d; fc=fd; fcobj = fdobj
      d = floor(a + (b-a)/phi);
      if(any(xvals==d)){
        d=xvals[which(xvals==d)]; fd=fvals[which(xvals==d)]
        warning("Function object may not correspond to optimal number of clusters")
        key=1
      }else{
        cnt=cnt+1
        fdobj<- fn(d); fd=fdobj$fval
      }
    }else{
      arreter=1
    }
    l=l+1; message("iter = ",l,"\n")
  }

  optx=ifelse(fc>fd,d,c);
  if(fc>fd){
    fobj=fdobj
  }else{
    fobj=fcobj
  }
  res=list(k=optx,crit=min(fd,fc),iter=l,iterfn=cnt,fobj=fobj,key=key)
  return(res)
}
#=============================================================================================>
#' Model criterion function
#'
#' A generic S3 function as wrapper for internal R routines for classes of models implemented
#' in this package. See details \link{c_chmod} for the list of classes supported.
#'
#' @param object the object to be passed to the concrete class constructor \code{chmod}
#' @param ... additional paramters to be passed to the internal routine
#'
#' @export

chmod<- function(object,...) UseMethod("chmod")

##==================================================================================================>
#' Concrete class constructor
#'
#' A function for constructing functions for concrete classes of models for the \code{chmod()} family of
#'  of functions.
#'
#' @param Y vector of the outcome variable
#' @param X matrix of covariates; excepting intercepts 1's
#'
#' @param modclass the class of model. Currently, "lm" for linear regression, "logit" (logit model),
#' "qreg" (quantile regression), "probit" (probit model), "gammainverse" (gamma with inverse link),
#' "gammalog" (gamma with log link), "poissonlog" (poisson model with log link),
#' "poissonidentity" (poisson with identity link), "poissonsqrt" (poisson with sqrt link),
#' "negbin" (negative binomial) are supported.
#'
#' @return object an object list with class attribute modclass.
#'
#' @export

c_chmod<- function(Y, X, modclass="lm"){ #assign class of object
    object<- list(Y,X)
    class(object)<- modclass
    names(object)<- c("Y","X")
  object
}

#=============================================================================================>
#' Regression - lm class
#'
#' A linear regression implementation for the "lm" class. It uses \code{\link[stats]{lm}}
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "lm"
#' @param ... additional parameters to be passed to \code{\link[stats]{lm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="lm"))
#' @export

chmod.lm<- function(object,...){
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::lm(object$Y~.,data = dat,...)
}

#=============================================================================================>
#' Regression - logit class
#'
#' A logit regression implementation for the "logit" class. It uses \code{\link[stats]{glm}}
#' with the binomial link function set to "logit"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "logit"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height<=50,X=women$weight,modclass="logit"))
#' @export

chmod.logit<- function(object,...){
  fam = stats::binomial(link = "logit")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(object$Y~.,family = fam, data = dat,...)
}

#=============================================================================================>
#' Regression - qreg class
#'
#' A quantile regression implementation for the "qreg" class. It uses \code{\link[quantreg]{rq}}
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "qreg"
#' @param ... additional parameters to be passed to \code{\link[quantreg]{rq}}, for example
#' \code{tau}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="qreg"),tau=0.45)
#' @export

chmod.qreg<- function(object,...){
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  quantreg::rq(object$Y~.,data = dat,...)
}
#=============================================================================================>
#' Regression - probit class
#'
#' A probit regression implementation for the "probit" class. It uses \code{\link[stats]{glm}}
#' with the binomial link set to "probit"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "probit"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height<=50,X=women$weight,modclass="probit"))
#' @export

chmod.probit<- function(object,...){
  fam = stats::binomial(link = "probit")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(object$Y~.,family = fam, data = dat,...)
}
#=============================================================================================>
#' Regression - gammainverse class
#'
#' A gamma regression implementation for the "gammainverse" class. It uses \code{\link[stats]{glm}}
#' with the Gamma link function set to "inverse"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "probit"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="gammainverse"))
#' @export

chmod.gammainverse<- function(object,...){
  fam = stats::Gamma(link = "inverse")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(object$Y~.,family = fam, data = dat,...)
}
#=============================================================================================>
#' Regression - gammalog class
#'
#' A gamma regression implementation for the "gammalog" class. It uses \code{\link[stats]{glm}}
#' with the Gamma link function set to "log"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "probit"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="gammalog"))
#' @export

chmod.gammalog<- function(object,...){
  fam = stats::Gamma(link = "log")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(object$Y~.,family = fam, data = dat,...)
}
#=============================================================================================>
#' Regression - poissonlog class
#'
#' A poisson regression implementation for the "poissonlog" class. It uses \code{\link[stats]{glm}}
#' with the poisson link function set to "log"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "poissonlog"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="poissonlog"))
#' @export

chmod.poissonlog<- function(object,...){
  fam = stats::poisson(link = "log")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(Y~.,family = fam,data = dat,...)
}
#=============================================================================================>
#' Regression - poissonidentity class
#'
#' A poisson regression implementation for the "poissonidentity" class. It uses \code{\link[stats]{glm}}
#' with the poisson link function set to "identity"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "poissonidentity"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="poissonidentity"))
#' @export

chmod.poissonidentity<- function(object,...){
  fam = stats::poisson(link = "identity")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(Y~.,family = fam,data = dat,...)
}
#=============================================================================================>
#' Regression - poissonsqrt class
#'
#' A poisson regression implementation for the "poissonsqrt" class. It uses \code{\link[stats]{glm}}
#' with the poisson link function set to "sqrt"
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "poissonsqrt"
#' @param ... additional parameters to be passed to \code{\link[stats]{glm}}
#'
#' @return fitted model object
#' @examples
#' chmod(c_chmod(Y=women$height,X=women$weight,modclass="poissonsqrt"))
#' @export

chmod.poissonsqrt<- function(object,...){
  fam = stats::poisson(link = "sqrt")
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  stats::glm(Y~.,family = fam,data = dat,...)
}

#=============================================================================================>
#' Regression - negbin class
#'
#' A negative binomial regression implementation for the "negbin" class. It uses \code{\link[MASS]{glm.nb}}
#'
#' @param object a list of Y - outcome variable and X - design matrix of class "negbin"
#' @param ... additional parameters to be passed to \code{\link[MASS]{glm.nb}}
#'
#' @return fitted model object
#'
#' @export

chmod.negbin<- function(object,...){
  dat = data.frame(object$Y,object$X); names(dat)[1]="Y"
  MASS::glm.nb(object$Y~.,data = dat,...)
}

#===================================================================================>
#' Construct a network design matrix
#'
#' This function creates the design matrix for a latent network structure using a balanced
#' panel
#'
#' @param datf  the entire data frame of balanced panel with NT rows of unit-time
#' observations
#' @param  Y  dependent variable in the data frame datf
#' @param  X  the covariate(s) generating spillovers
#' @param  Wi  other unit-varying (can be time-invariant) control variables
#' @param  W  global variables. these are only time varying but are common to all units.
#' eg. GDP
#' for individual/state-level data. Note that W has to be a vector of length T so cannot be
#' in the data frame datf
#' @param  panvar  the panel variable eg. unique person/firm identifiers
#' @param  tvar  time variable, eg. years
#' @param  factors  a vector of characters of factors in the data
#' @param scaling  a logical indicating whether non-discrete covariates should be scaled by their standard deviations
#' @param  unicons  a logical indicating whether to include unit-specific constant term

#' @return  Y  vector of dependent variables
#' @return  X  a block matrix of spillover matrix (\eqn{TN} x \eqn{N^2} )
#' @return  Wm  a matrix corresponding to covariate Wi
#' @return  Wf  a matrix of dummies corresponding to factors
#' @export
#'
netdat<- function(datf,Y,X,Wi,W=NULL,panvar,tvar,factors,scaling=TRUE,unicons=TRUE){
  if(any(is.na(data.frame(datf[panvar],datf[tvar])))){stop("NA in panel or time variables unallowed")}
  dat<- datf[order(datf[panvar],datf[tvar]),] #sort data into a panel by units and time
  Nd = nrow(unique(datf[panvar])) # extract number of units
  Td = nrow(unique(datf[tvar])) # extract the number of time periods
  fmat<- dat[factors]
  # check if panel is balanced
  if(dim(dat)[1]!=(Nd*Td)){stop("Unbalanced panel")}
  Xj = matrix(unlist(dat[X]),ncol = Nd) # the covariate(s) generating spillovers or externalities
  # write code to scale continuous covariates and store scales.
  if(scaling){
    fn<- function(x) {stats::sd(stats::na.omit(x))*sqrt(length(which(!is.na(x)))/length(x))}
    vX<- apply(Xj,2,fn);
    if(any(vX<(10^-6))){message("some units lack variation in x. consider removing them.")}


    if(unicons){
      Xm = kronecker(diag(1,Nd),cbind(1,(Xj/vX)))
    }else{
      Xm = kronecker(diag(1,Nd),(Xj/vX))
    } # a (TdxNd) x (Nd^2) block matrix
  }else{
    if(unicons){
      Xm = kronecker(diag(1,Nd),cbind(1,Xj))
    }else{
      Xm = kronecker(diag(1,Nd),Xj)
    } # a (TdxNd) x (Nd^2) block matrix
  }

  if(all(dim(Xm)!=c((Td*Nd),(Nd^2)))){stop("Network data creation failed.")}

  if(scaling){
    vWm = apply(dat[Wi],2,fn);
    if(any(vWm<(10^-6))){message("some units lack variation in covariates. consider removing them.")}
    Wm = dat[Wi]/vWm
  }else{
    Wm = dat[Wi]
  }

  Ym = unlist(dat[Y]); zY=rep(1,length(Ym)); zY[!is.na(Ym)]<- Ym[!is.na(Ym)]
  fmod<- apply(fmat, 2, as.factor) #convert the columns of fmat into factors
  Wf=stats::model.matrix(zY~.,data.frame(zY,fmod))[,-1] # remove the constant term
  # combine terms
  #datDM<-data.frame(Ym,Xm,Wm,Wf); names(datDM)[1]<- "Y"
  if(scaling){
    res=list(Y=Ym,X=Xm,Wm=Wm,Wf=Wf,sdX=vX,sdWm=vWm)
  }else{
    res=list(Y=Ym,X=Xm,Wm=Wm,Wf=Wf)
  }
  return(res)
}


#=============================================================================================>
#' Clustering of vector elements
#'
#' A deterministic clustering device of vector elements into k clusters
#'
#' @param k  number of clusters
#' @param vec  the vector of real valued elements
#'
#' @return clus  integer assignment of corresponding elements in vec in up to k clusters
#' @examples
#' set.seed(2); (v=c(rnorm(4,0,0.5),rnorm(3,3,0.5))[sample(1:7)])
#' dcluspar(k=2,vec = v)
#' @export
#'
dcluspar<- function(k,vec){
  svec=sort(vec)
  dsvec=diff(svec)
  idub = which(rank(-dsvec)<k)
  idlb = 1+idub
  lb=svec[idlb]
  ub=svec[idub]
  lb = c(min(vec),lb); ub = c(ub,max(vec))
  clus = rep(1,length(vec))
  for (j in 1:k) {
    clus[which(vec>=lb[j] & vec<=ub[j])]=j
  }
  clus
}
#=============================================================================================>
#' Sequential CCR
#'
#' \code{CCRls} runs regressions with potentially more covariates than observations.
#' See \code{c_chmod()} for the list of models supported.
#'
#' @param Y vector of dependent variable Y
#' @param X design matrix (without intercept)
#' @param kap maximum number of parameters to estimate in each active sequential step,
#' as a fraction of the less of total number of observations n or number of covariates p.
#' i.e. \eqn{min(n,p)}
#' @param modclass a string denoting the desired the class of model. See \link{c_chmod} for details.
#' @param tol level of tolerance for convergence; default \code{tol=1e-6}
#' @param reltol a logical for relative tolerance instead of level. Defaults at TRUE
#' @param rndcov seed for randomising assignment of covariates to partitions; default \code{NULL}
#' @param report number of iterations after which to report progress; default \code{NULL}
#' @param ... additional arguments to be passed to the model
#'
#' @return \code{betas}  parameter estimates (intercept first),
#' @return \code{iter}  number of iterations,
#' @return \code{dev}  increment in the objective function value at convergence
#' @return \code{fval} objective function value at convergence
#'
#' @examples
#' set.seed(14) #Generate data
#' N = 1000; (bets = rep(-2:2,4)); p = length(bets); X = matrix(rnorm(N*p),N,p)
#' Y = cbind(1,X)%*%matrix(c(0.5,bets),ncol = 1)
#' CCRls(Y,X,kap=0.1,modclass="lm",tol=1e-6,reltol=TRUE,rndcov=NULL,report=8)
#' @export
CCRls<- function(Y,X,kap=0.1,modclass="lm",tol=1e-6,reltol=TRUE,rndcov=NULL,report=NULL,...){
  p = ncol(X)
  n = nrow(X)
  bet_vec<- rep(NA,p) # vector to store parameters, excludes intercept
  asz = 1e-20
  if(kap<(1/n)){stop(paste("kap must be at least 1/n =",I(1/n)))}
  slc<- floor(kap*min(c(n,p))) # maximum size of a local covariate partition
  lcls<- ceiling((1:p)/slc) # assign covariates to partitions
  nlcls<- max(lcls) #number of partitions of covariates
  if(!is.null(rndcov)){set.seed(rndcov);lcls<- sample(lcls,p)}

  # initialise parameters
  bet0<- 0 #initialise intercept
  for (j in 1:p) {
    coefs<- stats::coef(chmod(object=c_chmod(Y, X[,j], modclass=modclass),...))
    bet_vec[j]<- coefs[2]
  }

  val0<- Inf; val1<- 0; l<- 0; dev=(val0-val1)

  while((dev>tol)){
    if(l>0){
      val0<- val1; obj0<- obj1;
      coefs<- stats::coef(obj1)
      bet_vec[IDls]<- coefs[c(2:(1+nIDls))] # local parameter updating
      bet0<- coefs[1]                       # local parameter updating, intercept
      bet_vec[-IDls]<- utils::tail(coefs,n=1)*bet_vec[-IDls] #non-local parameter updating
    }

    l<- l + 1; IND<- l - (ceiling(l/nlcls)-1)*nlcls;

    IDls<- which(lcls==IND); nIDls<- length(IDls)
    # construct (local) design matrix
    XB_ = X[,-IDls]%*%matrix(bet_vec[-IDls],ncol = 1)
    Xl = X[,IDls]
    XX = data.frame(Xl,XB_)
    obj1 = chmod(object=c_chmod(Y, as.matrix(XX), modclass=modclass),...)
    val1<- -stats::logLik(obj1)
    if(!is.null(report)){if(l%%report==0){ message("Iter =",l,"fval =",val1,"\n")}}
    if(reltol){dev=(val0-val1)/(asz+abs(val0))}else{dev=(val0-val1)}
    if(l==1){dev=1} # asz in denominator to avoid dividing by zero
  }
  list(betas=c(bet0,bet_vec),iter=l,dev=dev,fval=val0)
}

#=============================================================================================>
#' Sequential CCR with k clusters
#'
#' \code{CCRseqk} runs regressions with potentially more covariates than observations with
#' \code{k} clusters. See \code{c_chmod()} for the list of models supported.
#'
#' @param Y vector of dependent variable Y
#' @param X design matrix (without intercept)
#' @param k number of clusters
#' @param nC first \code{nC-1} columns in \code{X} not to cluster
#' @param kap maximum number of parameters to estimate in each active sequential step,
#' as a fraction of the less of total number of observations n or number of covariates p.
#' i.e. \eqn{min(n,p)}
#' @param modclass a string denoting the desired the class of model. See \link{c_chmod} for details.
#' @param tol level of tolerance for convergence; default \code{tol=1e-6}
#' @param reltol a logical for relative tolerance instead of level. Defaults at TRUE
#' @param rndcov seed for randomising assignment of covariates to partitions; default \code{NULL}
#' @param report number of iterations after which to report progress; default \code{NULL}
#' @param ... additional arguments to be passed to the model
#'
#' @return a list of objects
#' \itemize{
#' \item mobj low dimensional model object of class lm, glm, or rq (depending on \code{modclass})
#' \item clus cluster assignments of covariates
#' \item iter number of iterations
#' \item dev decrease in the function value at convergence
#' }
#'
#' @examples
#' set.seed(14) #Generate data
#' N = 1000; (bets = rep(-2:2,4)/2); p = length(bets); X = matrix(rnorm(N*p),N,p)
#' Y = cbind(1,X)%*%matrix(c(0.5,bets),ncol = 1); nC=1
#' zg=CCRseqk(Y,X,k=5,nC=nC,kap=0.1,modclass="lm",tol=1e-6,reltol=TRUE,rndcov=NULL,report=8)
#' (del=zg$mobj$coefficients) # delta
#' (bets = c(del[1:nC],(del[-c(1:nC)])[zg$clus])) #construct beta
#' @export
CCRseqk<- function(Y,X,k,nC=1,kap=0.1,modclass="lm",tol=1e-6,reltol=TRUE,rndcov=NULL,report=NULL,...){
  p = ncol(X)
  n = nrow(X)
  bet_vec<- rep(NA,p) # vector to store parameters, excludes intercept
  asz = 1e-20
  if(kap<(1/n)){stop(paste("kap must be at least 1/n =",I(1/n)))}
  slc<- max(floor(kap*min(c(n,p))),1) # maximum size of a local covariate partition
  lcls<- ceiling((1:p)/slc) # assign covariates to partitions
  nlcls<- max(lcls) #number of partitions of covariates
  if(!is.null(rndcov)){set.seed(rndcov);lcls<- sample(lcls,p)} #randomise partition assignment?

  # initialise parameters
  for (j in nC:p) {
    coefs<- stats::coef(chmod(object=c_chmod(Y, X[,j], modclass=modclass),...))
    bet_vec[j]<- coefs[2]
  }

  X1 <- matrix(NA,n,k) #initialise X for low dimension regression
  clus=dcluspar(k,bet_vec) #assign to clusters
  #uniclus<- unique(clus)
  if(nC==1){
    Xnc=NULL; Xc = X
  }else if(nC>1){
    Xnc=X[,(1:(nC-1))]; Xc = X[,-c(1:(nC-1))]
  }else{stop(paste("nC must be >= 1. nC = ",nC))}

  for(j in 1: k){ X1[,j] <- apply(as.matrix(Xc[,which(clus == j)]),1,sum)}
  obj0 = chmod(object=c_chmod(Y, as.matrix(cbind(Xnc,X1)), modclass=modclass),...)
  val0<- -stats::logLik(obj0); l<- 0; #val1<- 0; dev=(val0-val1)
  dev = Inf

  # begin while loop
  while((dev>tol)){
    if(l>0){ #update values
      val0<- val1; obj0<- obj1;
      coefs<- stats::coef(obj1)
      bet_vec = coefs[clus]
      bet_vec0 = bet_vec; clus0 = clus
    }
    # identify partition
    l<- l + 1; IND<- l - (ceiling(l/nlcls)-1)*nlcls;

    IDls<- which(lcls==IND); nIDls<- length(IDls)
    # construct (local) design matrix
    XB_ = Xc[,-IDls]%*%matrix(bet_vec[-IDls],ncol = 1)
    X0 = Xc[,IDls]
    XX = data.frame(cbind(Xnc,X0,XB_))
    objC = chmod(object=c_chmod(Y, as.matrix(cbind(XX)), modclass=modclass),...)
    coefs = stats::coef(objC)
    bet_vec[IDls]<- coefs[nC+(1:nIDls)]
    bet_vec[-IDls]<- utils::tail(coefs,n=1)*bet_vec[-IDls] #non-local parameter updating
    #***************
    clus=dcluspar(k,bet_vec) #assign to clusters
    for(j in 1: k){ X1[,j] <- apply(as.matrix(Xc[,which(clus == j)]),1,sum)}
    obj1 = chmod(object=c_chmod(Y, as.matrix(cbind(Xnc,X1)), modclass=modclass),...)
    #***************
    val1<- -stats::logLik(obj1)
    if(!is.null(report)){if(l%%report==0){ message("Iter =",l,"fval =",val1,"\n")}}
    if(reltol){dev=(val0-val1)/(asz+abs(val0))}else{dev=(val0-val1)}
  } #end while loop
  if(l>1){
  rvals=list(mobj=obj0,clus = clus0, iter=l,dev=dev)
  }else{
    rvals=list(mobj=obj0,clus = clus, iter=l,dev=dev)
  }
  return(rvals)
}


#=============================================================================================>
#' Golden Section Search Algorithm
#'
#' Minimising a continuous univariate function using the golden section search algorithm.
#'
#' @param fn the function; should be scalar valued
#' @param interval a vector containing the lower and upper bounds of search
#' @param tol tolerance level for convergence
#'
#' @return a list of objects
#' \itemize{
#' \item k: minimiser
#' \item value: mimimum value
#' \item iter: number of iterations before convergence
#' \item iterfn: number of function evaluations
#'}
#' @examples
#' fn = function(x) (x-1)^2; goldensearch(fn=fn,interval=c(-2,3),tol=1)
#' @export

goldensearch<- function(fn,interval,tol=1){
  a=min(interval); b = max(interval)
  xvals<- c(0); xvals[1]<- a; xvals[2]<- b
  fvals<- c(0)
  fa = fn(a); fb = fn(b);
  fvals[1]<- fa; fvals[2]<- fb
  cnt=2; # set counter to 2 for function evaluations
  phi<- (1+sqrt(5))/2

  c = ceiling(b - (b-a)/phi); d = floor(a + (b-a)/phi);
  if(any(xvals==c)){
    c=xvals[which(xvals==c)]; fc=fvals[which(xvals==c)]
  }else{
    cnt=cnt+1
    fc=fn(c)
  }
  if(any(xvals==d)){
    d=xvals[which(xvals==d)]; fd=fvals[which(xvals==d)]
  }else{
    cnt=cnt+1
    fd=fn(d)
  }


  l = 1; # set counter for iterations
  message("iter = ",l,"\n");arreter = 0
  while(abs(c-d)>tol & arreter==0){# while c d
    if(fc<fd){
      b=d; fb=fd;d=c;fd=fc;
      c = ceiling(b-(b-a)/phi);
      if(any(xvals==c)){
        c=xvals[which(xvals==c)]; fc=fvals[which(xvals==c)]
      }else{
        cnt=cnt+1
        fc=fn(c)
      }

    }else if(fc>fd){
      a=c; fa = fc; c=d; fc=fd
      d = floor(a + (b-a)/phi);
      if(any(xvals==d)){
        d=xvals[which(xvals==d)]; fd=fvals[which(xvals==d)];
      }else{
        cnt=cnt+1
        fd=fn(d)
      }
    }else{
      arreter=1
    }
    l=l+1; message("iter = ",l,"\n")
  }

  optx=ifelse(fc>fd,d,c)
  res=list(k=optx,value=min(fd,fc),iter=l,iterfn=cnt)
  return(res)
}
