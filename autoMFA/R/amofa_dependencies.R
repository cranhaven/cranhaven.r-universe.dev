#' @importFrom stats mahalanobis rnorm cov kmeans setNames
#' @importFrom utils find setTxtProgressBar txtProgressBar
#' @importFrom Rdpack reprompt
#' @importFrom pracma finds

if(getRversion() >= "2.15.1")  utils::globalVariables(c("templocal", "templocal2"))

ffa <- function(X,numFactors,cyc=100,tol=0.00001) {

  # Fast Maximum Likelihood Factor Analysis using EM
  #
  # X - data matrix
  # numFactors - number of factors
  # cyc - maximum number of cycles of EM (default 100)
  # tol - termination tolerance (prop change in likelihood) (default 0.0001)
  #
  # L - factor loadings
  # Ph - diagonal uniquenesses matrix
  # LL - log likelihood curve
  #
  # Iterates until a proportional change < tol in the log likelihood
  # or cyc steps of EM
  #
  # This code is originally written by Z. Ghahramani, my additions and
  # changes are indicated with AAS - Albert Ali Salah

  initWithPCA=1;

  N=length(X[,1]);
  numDims=length(X[1,]);
  tiny=exp(-700);

  #subtract mean from the data
  X=X-(matrix(1,nrow=N,ncol=1)%*%colMeans(X));

  #XX is X^2 divided by the number of samples
  XX=t(X)%*%X/N; #XX is approximately cov(X), but divided by N, instead of N-1. thus XX*N/N-1 = cX

  # diagXX is the diagonal elements of XX, [sum(X_{i1}^2) sum(X_{i2}^2) .. sum(X_{im}^2)] /N
  # with i from 1 to numSamples and m from 1 to numDims
  # hence it is (average of squares of first elements, average of squares of second elements...)
  diagXX=diag(XX);

  cX=cov(X); #the covariance matrix

  scale=as.complex(det(cX))^(1/numDims);

  if (abs(scale) == 0){
    scale = 0.001}
  #AAS - patch
  if (abs(scale) == Inf ){
    scale = 5e300} #Very large number!
  #HK - patch

  scale = Re(scale) #AAS - patch
  # initial factor loadings are taken from a zero-mean Gaussian random distribution.
  # the covariance however, is scaled by a factor, namely (scale/numFactors).
  # because cov(X*f) = cov(X)*f*f

  if (initWithPCA==1){
    eig_temp <- eigen(cX);
    H <- eig_temp$vectors
    D <- diag(eig_temp$values)
    #sort the eigenvalues
    dg = matrix(eig_temp$values)
    eig_sort <- sort(dg,decreasing=TRUE,index.return=TRUE); #now its is descending

    Y <- matrix(eig_sort$x,nrow =length(eig_sort$x))
    I <- eig_sort$ix

    indexMatrix = I[1:numFactors];

    L = matrix(H[,indexMatrix],ncol=length(indexMatrix)) * matrix(replicate(numDims,sqrt(dg[indexMatrix]))) 
  }else{
    L = matrix(rnorm(numDims*numFactors),nrow = numDims, ncol = numFactors)*sqrt(scale/numFactors)
  }
  #Ph is the diagonal entries of the data covariance...
  # he is initializing Ph from the data covariance!
  Ph=matrix(diag(cX));
  Ph = Ph + (Ph<0.00001)*0.00001; #no entry is smaller than 0.00001 AAS

  I=diag(numFactors);

  lik=0; LL=vector();

  #to use in the calculation, log of 2pi^(-2/d)
  const=-numDims/2*log(2*pi);

  for (i in 1:cyc){

    ### E Step ###

    Phd=diag(as.vector(1/Ph)); #Phd is the inverse of phi
    LP=Phd%*%L #inv(Psi)*L
    # by the matrix inversion lemma, MM is the inverse of the covariance matrix of the data;
    MM=Phd-LP%*%solve(I+t(L)%*%LP)%*%t(LP);

    # AAS - patch for Inf or 0 determinants
    regTerm = 1;
    dM=det(MM*regTerm);
    while (dM==0) {
      regTerm = regTerm * 2;
      dM=det(MM%*%regTerm);
    }
    while (dM==Inf) {
      regTerm = regTerm / 2;
      dM=det(MM%*%regTerm);
    }
    if (dM == 0){warning('In FFA: Determinant zero!')}
    logdM = N*.5*log(dM) - N*.5*numDims*log(regTerm);
    #beta is the for the linear projection, beta = L' * inv(phi + LL').
    # the expected value of z given x is beta*x.

    beta=t(L)%*%MM;
    XXbeta=XX%*%t(beta);
    EZZ=I-beta%*%L +beta%*%XXbeta;

    #### Compute log likelihood ####

    oldlik=lik;
    lik=N*const+logdM-0.5*N*sum(diag(MM%*%XX));

    LL=append(LL,lik);

    #### M Step ####

    L=XXbeta%*%solve(EZZ);
    Ph=diagXX-diag(L%*%t(XXbeta));
    Ph = Ph + (Ph<0.0001)*0.0001; #no entry is smaller than 0.00001 AAS

    if (i<=2){
      likbase=lik
    } else if (lik<oldlik){
      warning('In ffa: Violation - Likelihood decreased')
    } else if(abs((lik-oldlik)/(lik))<0.0001){
      break }

  }
  out = list("L" = L, "Ph" = matrix(Ph), "LL" = matrix(LL,nrow = 1))
}

getDescLen <- function(model,modelLogLik,ModelSelCri){

  #Gets the description length in terms of MML (ModelSelCri=1), MDL/BIC
  #(ModelSelCri=2), or AIC given the MoFA model and its log likelihood
  # -----------------------------------------------------------------------
  # Copyleft (2014): Heysem Kaya
  #
  # This software is distributed under the terms
  # of the GNU General Public License Version 3
  #
  # Permission to use, copy, and distribute this software for
  # any purpose without fee is hereby granted, provided that this entire
  # notice is included in all copies of any software which is or includes
  # a copy or modification of this software and in all copies of the
  # supporting documentation for such software.
  # This software is being provided "as is", without any express or
  # implied warranty.  In particular, the authors do not make any
  # representation or warranty of any kind concerning the merchantability
  # of this software or its fitness for any particular purpose."
  # ----------------------------------------------------------------------

  Mu=model$Mu
  numSamples=model$numSamples
  Pi=model$Pi
  Psi=model$Psi
  numFactors=model$numFactors
  numDims <- dim(Mu)[1]
  numMeans <- dim(Mu)[2]
  numparams= matrix(0,ncol = numMeans)
  numFactors_bits=matrix(0,ncol = numMeans)
  logc=log2(2.865064)
  #Rissanen J., Information and Complexity in Statistical Modeling, Springer, 2007, p.15
  for (k in 1:numMeans){
    numparams[k]=numDims*(numFactors[k]+2)+1
    numFactors_bits[k]= getIntCodeLen(numFactors[k])+logc;
  }
  #We keep a matrix for independent Psi however !

  nparsover2 = numparams/2

  code_len_k= getIntCodeLen(numMeans)+logc


  if (ModelSelCri==1) {
    descLength = -modelLogLik*log2(exp(1))+ (sum(nparsover2 * log2(matrix(Pi,nrow=1)))) +
      sum(nparsover2 + 0.5)*log2(numSamples/12)+
      code_len_k+sum(numFactors_bits);
  } else if (ModelSelCri==2) {
    descLength=-modelLogLik + sum(nparsover2)*log(numSamples);
  } else if  (ModelSelCri==3) {
    descLength=-modelLogLik + sum(nparsover2);
  }
  return(list(descLength = descLength, modelLogLik = modelLogLik, numparams = matrix(numparams,ncol = 1)))
}

getIntCodeLen <- function(k){
  # returns the integer code length in a recursive manner.
  # For details see Rissanen's "universal prior for integers"
  # c=2.865064
  if (prod(dim(k))>1){
    stop('call by one K at a time');
  }

  if (k==1){
    len=1;
  } else if (k>1) {
    lk=ceiling(log2(k));
    len=lk+getIntCodeLen(lk);
  } else {
    len=0;
  }
  return(len)
}

# % -----------------------------------------------------------------------
# % Copyleft (2014): Heysem Kaya and Albert Ali Salah
# %
# % This software is distributed under the terms
# % of the GNU General Public License Version 3
# %
# % Permission to use, copy, and distribute this software for
# % any purpose without fee is hereby granted, provided that this entire
# % notice is included in all copies of any software which is or includes
# % a copy or modification of this software and in all copies of the
# % supporting documentation for such software.
# % This software is being provided "as is", without any express or
# % implied warranty.  In particular, the authors do not make any
# % representation or warranty of any kind concerning the merchantability
# % of this software or its fitness for any particular purpose."
# % ----------------------------------------------------------------------

getWeakestLink <- function(Pi,numSamples,numDims,numFactors,MinSoftCount){
  # % [weakestLink] =getWeakestLink(Pi,numSamples,numDims,numFactors,MinSoftCount)
  # % Selects the weakest component whose support is less than
  # % parametric/theoretic threshold

  weakestLink=0;
  if (MinSoftCount>0){
    if (min(Pi*numSamples)<MinSoftCount){
      weakestLink=which.min(Pi*numSamples)
    }
  } else if (MinSoftCount==-1){
    #compute weakness using theoretic value of C_j
    #for all components individually
    ComponentParams=numDims*(numFactors+2)+2; #factors + means + psi + 1 ( pi(k))
    if (min(t(Pi)*numSamples-(ComponentParams/2))<=0){
      weakestLink=which.min(t(Pi)*numSamples-(ComponentParams/2));
    }
  }
  return(weakestLink)
}

loglike2 <- function(data,lambda,psi,mu){
  #loglike2(data,lambda,psi,mu) returns the log likelihood of the factor analysis model for the given data set ->returns a numSamples x 1 matrix!
  # data   numSamples x numDims data
  # lambda numDims x numFactors factor loading matrix
  # psi    numDims x 1          the diagonal of the noise covariance matrix
  # mu     numDims x 1          the mean vector of the noise
  #
  # 24 september 2003 - albert ali salah

  numSamples = dim(data)[1]
  numDims = dim(data)[2]
  numFactors = dim(lambda)[2]

  #find covariance
  Cov = lambda%*%t(lambda) + diag(as.vector(psi)) ###
  eps = 0.0005
  if (Matrix::rankMatrix(Cov) < numDims){
    Cov = Cov + diag(numDims)*eps
  }

  regTerm = 1
  dC = det(Cov)
  while (dC==0){
    regTerm = regTerm * 2
    dC=det(Cov*regTerm)
  }

  while (is.infinite(dC)){
    regTerm = regTerm / 2
    dC=det(Cov*regTerm)
  }

  lTerm = -0.5*numDims*log(2*pi)-0.5*(log(dC)-numDims*log(regTerm))
  like = matrix(lTerm -.5*mahalanobis(data,mu,Cov),ncol = 1 )
  a = like
  return(a)

}

loglike3 <- function(data,lambda,psi,mu,priors,numFactors,regTerm=1){

  #loglike3(data,lambda,psi,mu,priors,numFactors,regTerm) returns the log likelihood of the MFA for the
  # given data set
  # data   numSamples x numDims data
  # lambda numDims x (sum(numFactors) factor loading matrix
  # psi    numDims x numMeans         the diagonal of the noise covariance matrix
  # mu     numDims x numMeans         the mean vector of the noise
  # priors     1 x numMeans           the component priors
  # numFactors 1 x numMeans           the number of factors for each component
  # regTerm    1 x 1                  against zero determinants, it cancels itself out
  #
  # 2 september 2003 - albert ali salah

  numSamples <- dim(data)[1]
  numDims <- dim(data)[2]
  numMeans <- dim(mu)[2]
  sumLike <- 0;
  logtiny <- -700;

  #PATCH
  if (min(priors)==0){
    #find the entries that are zero, set it to some small number, say 0.05
    priors <- (priors==0)*0.05 + priors
    #now the sum is greater than 1, renormalize
    priors <- priors / sum(priors)
  }

  like <-  matrix(0,numSamples,numMeans)
  factorCount <- 0;
  for (k in 1:numMeans){
    lambda_k = lambda[,(factorCount+1):(factorCount+numFactors[k])]
    factorCount = factorCount+numFactors[k]
    #find covariance
    Cov = lambda_k%*%t(lambda_k) + diag(as.vector(psi[,k]))
    eps = 0.0005;

    if (sum(is.infinite(Cov) + is.nan(Cov))>0){
      #problem....
      warning('In loglike3 : An infinite covariance or NaN covariance has occurred');
    }
    if (Matrix::rankMatrix(Cov)[1]<numDims){ #Changed this to use same? method as MATLAB.
      Cov = Cov + diag(numDims)*eps;
    }

    #invCov = inv(Cov);
    regTerm = 1
    dC = det(Cov)
    while (dC==0) {
      regTerm = regTerm * 2
      dC=det(Cov*regTerm)
    }
    while (dC==Inf){
      regTerm = regTerm / 2
      dC=det(Cov*regTerm)
    }
    detCov = sqrt(dC)*sqrt(regTerm^-numDims)

    pd = priors[k]*(detCov)^-1;
    like[,k]= log(pd) -.5*mahalanobis(data,mu[,k],Cov)
  }

  like=logsumexp(like,2);

  like[like<logtiny] = logtiny;

  sumLike = sum(like) - numSamples*numDims*.5*log(2*pi);
}

loglike4 <- function(data,lambda,psi,mu,priors,numFactors,regTerm=1){

  #loglike4(data,lambda,psi,mu,priors,numFactors,regTerm) returns the log likelihood of the MFA for the
  # given data set - returns numSamples x numComponents matrix that needs to be normalized

  # data   numSamples x numDims data
  # lambda numDims x (sum(numFactors) factor loading matrix
  # psi    numDims x numMeans         the diagonal of the noise covariance matrix
  # mu     numDims x numMeans         the mean vector of the noise
  # priors     1 x numMeans           the component priors
  # numFactors 1 x numMeans           the number of factors for each component
  # regTerm    1 x 1                  against zero determinants, it cancels itself out
  # 29 jan 2004 - albert ali salah

  numSamples = dim(data)[1]
  numDims = dim(data)[2]
  numMeans = dim(mu)[2]
  sumLike = 0;
  tiny = exp(-700);

  #PATCH --> Heysem: We can simply remove those components from the mixture
  if (min(priors)==0){
    #find the entries that are zero, set it to some small number, say 0.05
    priors = (priors==0)*0.05 + priors
    #now the sum is greater than 1, renormalize
    priors = priors / sum(priors)
  }


  like = matrix(rep(0,numSamples),nrow=1)
  factorCount = 0;
  a = matrix(rep(0,numSamples*numMeans),nrow=numSamples)

  for (k in 1:numMeans){
    lambda_k = lambda[,(factorCount+1):(factorCount+numFactors[k])]
    factorCount = factorCount+numFactors[k]
    #find covariance
    Cov = lambda_k%*%t(lambda_k) + diag(as.vector(psi[,k]))
    eps = 0.0001

    if (sum(is.infinite(Cov) + is.nan(Cov))>0){
      #problem....
      warning('In loglike4 : Infinite or NaN terms in covariance matrix')
    }
    if (Matrix::rankMatrix(Cov) <numDims ){
      Cov = Cov + diag(numDims)*eps
    }

    # invCov = inv(Cov);
    regTerm = 1
    dC = det(Cov)
    while (dC==0) {
      regTerm = regTerm * 2;
      dC=det(Cov*regTerm);
    }
    while (is.infinite(dC)) {
      regTerm = regTerm / 2
      dC=det(Cov*regTerm)
    }

    coef=log(priors[k]) -(numDims*.5)*log(2*pi) -.5*(log(dC) -numDims*log(regTerm))
    centered <- t(data)- matrix(rep(mu[,k],numSamples),nrow=dim(mu)[1])

      a[,k] = t(coef- .5*mahalanobis(data,mu[,k],Cov)) # (.5*colSums(centered*(pracma::mldivide(Cov,centered)))))
  }
  return(a)
}

logModel <- function(models,Mu,Lambda,Pi,Psi,numFactors,logsTra,dl,action,tLog,tLogN){

  i=length(models)+1
  modelNames <- c("Mu","Lambda","Pi","Psi","numFactors","logsTra","dl","action","tLog","tLogN")
  models[[i]] <- setNames(vector("list", length(modelNames)), modelNames)
  models[[i]]$Mu=Mu;
  models[[i]]$Lambda=Lambda;
  models[[i]]$Pi=Pi;
  models[[i]]$Psi=Psi;
  models[[i]]$numFactors=numFactors;
  models[[i]]$logsTra = logsTra;
  models[[i]]$dl=dl;
  models[[i]]$action=action;
  models[[i]]$tLog=tLog;
  models[[i]]$tLogN=tLogN;

  return(models)
}

logsumexp <- function(L,dim=1){
  # Numerically stable computation of log(sum(exp(L))
  # Heysem Kaya - Usage proposed by ATC

  if (!is.matrix(L)) stop("Please input L as a matrix object")

  #Expects a vector or matrix L.
  mx <- max(L)
  M <- L - mx
  if (dim==1){
    r=matrix(mx+log(colSums(exp(M))),nrow=1)
  } else {
    if(ncol(L) == 1){ r = L } else {
    r=mx+log(matrix(rowSums(exp(M)))) }
  }
  return(r)
}

ones <- function(nrow,ncol){
  out <- matrix(rep(1,nrow*ncol),nrow = nrow)
}

rprod <- function(X,Y){
  # row product
  # function Z=rprod(X,Y)
  # by Z. Ghahramani
  if(length(X[,1]) != length(Y[,1]) || length(Y[1,]) !=1){
    stop('Error in RPROD, dimension mismatch!');
  }

  nrow = dim(X)[1]
  ncol = dim(X)[2]

  Z=zeros(nrow,ncol);

  for (i in 1:length(X[1,])){
    Z[,i]=X[,i]*Y;
  }
  return(Z)
}

softresidual <- function(data,lambda,psi,posteriors,mu){
  # %softresidual(data,lambda,psi,posteriors,mu) takes a data set and the parameters of a factor analyser.
  # % calculates the expected values of the factors, find the difference of these estimates and the
  # % actual data points(called the residuals) and returns the principal component of the residuals
  # % this vector will be used as the initial value of the k+1th factor (we are increasing the number of factors)
  # % and thus concatenated to lambda. psi will be changed accordingly.
  # % data        numSamples x numDims data
  # % lambda      numDims x numFactors factor loading matrix
  # % psi         numDims x 1          the diagonal of the noise covariance matrix
  # % posteriors  numSamples x 1       normalized posteriors for each sample and this component
  # % mu          numDims x 1          the mean vector of the noise
  # %
  # % 8.10.2003 albert ali salah
  # % -----------------------------------------------------------------------
  # % Copyleft (2014): Heysem Kaya and Albert Ali Salah
  # %
  # % This software is distributed under the terms
  # % of the GNU General Public License Version 3
  # %
  # % Permission to use, copy, and distribute this software for
  # % any purpose without fee is hereby granted, provided that this entire
  # % notice is included in all copies of any software which is or includes
  # % a copy or modification of this software and in all copies of the
  # % supporting documentation for such software.
  # % This software is being provided "as is", without any express or
  # % implied warranty.  In particular, the authors do not make any
  # % representation or warranty of any kind concerning the merchantability
  # % of this software or its fitness for any particular purpose."
  # % ----------------------------------------------------------------------

  #
  eps = 1e-16
  numDims = dim(data)[2];

  if (missing(mu)){
    mu = sum(data * matrix(rep(posteriors,numDims),ncol = numDims)) / sum(posteriors)  #soft mean
  }

  if (missing(posteriors)){ posteriors = ones(dim(data)[1],1)
  }

  numSamples = dim(data)[1];
  numFactors = dim(lambda)[2];

  if (dim(mu)[1] == 1){
    mu = t(mu);
  }

  #according to Ghahramani & Hinton E[z] is lambda'(psi+lambda*lambda')^-1*x
  premult_z = t(lambda)%*%solve(diag(as.vector(psi))+lambda%*%t(lambda));

  resids = zeros(numSamples,numDims)

  #find the residual data set
  for (i in 1:numSamples){
    x = matrix(data[i,]) - mu
    z = premult_z%*%x;
    E_x = lambda%*%z;
    resids[i,] = t(x - E_x);
  }

  #find the soft covariance
  resids_mu = colSums(resids * matrix(rep(posteriors,numDims),ncol = numDims)) / sum(posteriors); #mean of resids
  c_data= resids - matrix(rep(resids_mu,numSamples),byrow=TRUE,nrow = numSamples); #centered residuals
  ch_data = c_data * matrix(rep(posteriors^.5,numDims),ncol = numDims); #weight each data point by the square root of posterior
  softCov = t(ch_data)%*%ch_data/(sum(posteriors)-1); #during multiplication, the posterior will be reconstructed

  #Patch heysem 19.10.2012
  if (sum(sum(is.nan(softCov)))>0){
    warning('Warning: softCov contains NaN elems');
    softCov[is.nan(softCov)]=eps;
  }
  if (sum(sum(is.infinite(softCov)))>0){
    warning('Warning: softCov contains inf elems');
    softCov[is.infinite(softCov)]=eps;
  }

  #fit one factor to the residue

  tempEig <- eigen(softCov)
  H <- tempEig$vectors
  D <- tempEig$values
  I <- order(D,decreasing = TRUE)
  #sort the eigenvalues
  ind = H[1,] < 0
  H[,ind] = -H[,ind] #Imposing the condition that the first element should be postive for identifiability.

  indexMatrix = I[1];
  LambdaR = matrix(H[,indexMatrix]*D[indexMatrix]^0.5)

  #combine the two
  newLambda = cbind(lambda,LambdaR);
  newPsi = 0; #dummy

  return(list(resids = resids,newLambda = newLambda,newPsi = newPsi))

}

softsplit_fj <- function(data,lambda,psi,posteriors,mu,verbose){
  #[X1,X2,g,loglik]=softsplit(data,lambda,psi,posteriors,mu) returns two means, their mixture priors & loglik.
  # the split is 1 eigenvector to each side of the mean in the PCA direction
  # data   numSamples x numDims data
  # lambda numDims x numFactors factor loading matrix
  # psi    numDims x 1          the diagonal of the noise covariance matrix
  # mu     numDims x 1          the mean vector of the noise
  # posteriors numSamples x 1  the posteriors of belonging to this component
  #
  # 8.10.2003 albert ali salah
  # updates by heysem kaya: local 2-component MoFA fitting, using a weighted
  # sum of all eigenvectors to initialze the new components in the local
  # fitting

  useMoFA=1;
  useWtEig=1;
  numDims = dim(data)[2]
  if (missing(posteriors)){
    posteriors <- matrix(rep(1,dim(data)[1]))
  }
  if (missing(mu)){
    mu <- colSums(data * matrix(rep(posteriors,numDims),ncol = numDims))/sum(posteriors)
  }

  numFactors=dim(lambda)[2]

  loglik1 = loglike3(data,lambda,psi,mu,1,numFactors);

  if (dim(mu)[1]>1){
    mu=t(mu);
  }

  tiny = exp(-700); #we can use realmin here
  g = zeros(2,1);
  numSamples = dim(data)[1]

  #find the principal direction by soft covariance
  c_data= data - matrix(rep(mu,numSamples),byrow = TRUE,nrow = numSamples)  #centered data
  ch_data = c_data * matrix(rep(posteriors^.5,numDims),byrow = FALSE, ncol = numDims); #weight each data point by the square root of posterior
  softCov = t(ch_data)%*%ch_data/(sum(posteriors)-1); #during multiplication, the posterior will be reconstructed

  #Patch heysem 22.03.2013
  if (sum(sum(is.nan(softCov)))>0){
    warning('softCov contains NaN elems');
    softCov[is.nan(softCov)]=tiny
    #softCov
  }
  if (sum(sum(is.infinite(softCov)))>0){
    warning('softCov contains inf elems');
    softCov[is.infinite(softCov)]=tiny
    #softCov
  }

  tempeig <- eigen(softCov)
  H <- tempeig$vectors
  eigval <- tempeig$values

  #sort the eigenvalues
  Y = matrix(sort(eigval,decreasing = TRUE),ncol=1); #now its is descending
  I <- matrix(order(eigval, decreasing = TRUE),ncol=1)
  # here we compute the weighted sum of all eigenvectors

  ind <- H[1,] < 0
  H[,ind] <- -H[,ind] #Imposing the condition that the first element should be postive for identifiability.

  eigvector = matrix(rowSums(H[,I]%*%diag(as.vector(Y^0.5))),ncol=1);  

  X1 = mu+t(eigvector);
  X2 = mu-t(eigvector);

  maxIter=100;
  Mu=cbind(t(X1),t(X2));
  Pi=matrix(c(0.5,0.5));
  Lambda=cbind(lambda,lambda); #Check dimensions of these
  Psi=cbind(psi,psi); #Check dimensions of these

  numFactors=cbind(numFactors,numFactors)
  
  mfaOUT  <- cont_mfa_fj(data,Mu,Lambda,Psi,Pi,numFactors,Inf,maxIter, verbose = verbose);
  Lambda = mfaOUT$Lambda
  Psi = mfaOUT$Psi
  Mu = mfaOUT$Mu
  Pi = mfaOUT$Pi
  numFactors = unname(mfaOUT$numFactors)
  logs = mfaOUT$logs
  minDL = mfaOUT$minDL

  return(list(Lambda = Lambda,Psi = Psi,Mu = Mu,Pi = Pi,numFactors = numFactors,logs = logs,minDL = minDL))
}

zeros <- function(nrow,ncol){
  #Mimics zeros() from MATLAB.
  #JD September 2020
  out <- matrix(rep(0,nrow*ncol),nrow=nrow)
}



