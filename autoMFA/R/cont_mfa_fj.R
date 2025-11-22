cont_mfa_fj <- function(data,iMu,iLambda,iPsi,iPi,inumFactors,iDL,maxIter=100,tol=0.00001, verbose){


  #
  # Iterates until a proportional change < tol in the log likelihood
  # or maxIter steps of EM
  # -----------------------------------------------------------------------
  # Copyleft (2014): Heysem Kaya and Albert Ali Salah
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
  gamma=0.0001
  Phmin=0.0001
  lambdaSmall = 0.001
  factAnnihThreshold= 0.1
  tempSize= dim(data)
  numSamples = tempSize[1]
  numDims = tempSize[2]
  numMeans = dim(iMu)[2]
  tiny=exp(-700);
  eps = 1e-16

  #initialize Lambda, concatenation of all mofa{k}.Lambda
  Lambda = iLambda
  #initialize Psi
  Psi = iPsi
  #initialize component priors
  Pi = iPi
  #initialize means
  Mu = iMu
  minDL = iDL

  oldDL = iDL
  dl = iDL
  numFactors=inumFactors

  mofaNames <- c("numFactors","EZ","EZZ","Psi","Lambda","Pi")
  mofa <- vector(mode = "list", length = numMeans)
  for (i in 1:numMeans){
    mofa[[i]] <- setNames(vector("list", length(mofaNames)), mofaNames)
  }

  params= matrix(0,ncol = numMeans)

  logs=vector();

  H=matrix(0,nrow = numSamples, ncol = numMeans) # E(w|x)
  factorCount=0;
  for (k in 1:numMeans){
    mofa[[k]]$numFactors = numFactors[k];
    mofa[[k]]$EZ = zeros(numSamples,mofa[[k]]$numFactors);
    mofa[[k]]$EZZ = zeros(numSamples,mofa[[k]]$numFactors);
    mofa[[k]]$Psi = matrix(Psi[,k],ncol = 1);
    mofa[[k]]$Lambda=Lambda[,(factorCount+1):(factorCount+mofa[[k]]$numFactors)];
    mofa[[k]]$Pi = Pi[k];
    factorCount= factorCount + mofa[[k]]$numFactors;
  }
  XX=zeros(numDims*numMeans,numDims);
  s=zeros(numMeans,1);
  const=(2*pi)^(-numDims/2);
  #%%%%%%%%%%%%%%%%%%%%

  modlNames <- c("Lambda","Psi","Mu","Pi","lik","numSamples","numFactors","dl","numparams")
  modl <- vector(mode = "list", length = maxIter)
  for (i in 1:maxIter){
    modl[[i]] <- setNames(vector("list", length(modlNames)), modlNames)
  }

  for (iter in 1:maxIter){
    #%%%% E Step %%%%
    factorCount = 0;
    logversions = zeros(1,numMeans); #for the cases we need to estimate H by its log
    flagFactAn=0;
    params=zeros(numMeans,1);
    for (k in 1:numMeans){
      ks = toString(k);
      I=diag(as.vector(mofa[[k]]$numFactors))
      #Psi_ is the inverse of Psi
      Psi_ = diag(as.vector(1/mofa[[k]]$Psi))
      #the current component Lambda is mofa{k}.Lambda
      LP=Psi_%*% mofa[[k]]$Lambda; 
      auxM1=I+t(mofa[[k]]$Lambda)%*%LP;
      MM=Psi_-t(solve(t(auxM1),t(LP)))%*%t(LP);

      # determinant regularization patch
      regTerm = 1;
      detM = det(MM);
      while (detM==0) {
        regTerm = regTerm * 2;
        detM=det(MM*regTerm);
      }
      while (is.infinite(detM)) {
        regTerm = regTerm / 2;
        detM=det(MM*regTerm);
      }

      dM = sqrt(detM)*sqrt(regTerm^-numDims);

      log_dM = (log(detM)-numDims*log(regTerm))/2;
      Xk=data-ones(numSamples,1)%*%t(matrix(Mu[,k],ncol = 1))
      XM=Xk%*%MM;
      # H are the weighted (by priors Pi) likelihoods
      H[,k]=log(const*Pi[k])+log_dM-(0.5*rowSums(XM*Xk));
      # we have estimated H(:,k) by its log!!
      logversions[k] = 1;

      mofa[[k]]$EZ = XM%*%mofa[[k]]$Lambda;
      auxNormEZ= colSums(mofa[[k]]$EZ^2)/numDims;
      indices=auxNormEZ<=factAnnihThreshold;
      if ( sum(indices)>0 && (mofa[[k]]$numFactors-sum(indices))>=1){  
        # if logversions then better to annihilate the whole components
        # later annihilate factor loading vector if the factors are weak

        flagFactAn=1;
        if(verbose){
        warn_msg <- message(paste('Annihilating weak factors for component ',k))}
        factorsAnnihilated=mofa[[k]]$Lambda[,indices];
        #Psi correction
        diagContribution=diag(factorsAnnihilated%*%t(factorsAnnihilated));
        mofa[[k]]$Psi=mofa[[k]]$Psi+diagContribution;
        #remove corresponding factor loading columns
        mofa[[k]]$Lambda=mofa[[k]]$Lambda[,-indices]
        #update numFactors
        numFactors[k]=numFactors[k]-sum(indices);
        mofa[[k]]$numFactors=mofa[[k]]$numFactors-sum(indices);
        if (mofa[[k]]$numFactors>0){
          auxLambda=mofa[[k]]$Lambda;
          I=diag(mofa[[k]]$numFactors);
        } else {
          #if all components are annihilated then specify an artificial
          #factor with little contribution
          auxLambda=ones(numDims,1)*eps;
          I=eps;
        }
        #re-compute expectations
        #Psi_ is the inverse of Psi

        Psi_ = diag(as.vector(1/mofa[[k]]$Psi))
        LP=Psi_%*% auxLambda;
        auxM1=I+t(auxLambda)%*%LP;
        MM=Psi_ - t(solve(t(auxM1),t(LP))) %*% t(LP)
        XM=Xk%*%MM;
        H[,k]=log(const*Pi[k])+log_dM-(0.5*rowSums(XM*Xk));
        mofa[[k]]$EZ = XM%*%auxLambda;
      }
      params[k]=numDims*(mofa[[k]]$numFactors+2)+1; #%+2 for Psi and Means

    }

    oldDL=dl;
    #check if global Lambda is to be updated
    if (flagFactAn){
      factorCount=0;
      Lambda=zeros(numDims,sum(numFactors));
      for (k in 1:numMeans) {
        Lambda[,(factorCount+1):(factorCount + mofa[[k]]$numFactors)]=mofa[[k]]$Lambda;
        factorCount = factorCount + mofa[[k]]$numFactors;
      }

    }

    # H correction for numerical underflow
    Hsum=logsumexp(H,2);
    Hzero=(Hsum==0);  #the entries of H with zero sum
    Nz=sum(Hzero);  #the number of such entries
    H[Hzero,]=log(tiny/numMeans)*ones(Nz,numMeans);  #replace those with tiny, equal probability
    Hsum[Hzero]=log(tiny)*ones(Nz,1);

    H= H - matrix(rep(Hsum,numMeans),nrow = dim(Hsum)[1]);  
    s_log=logsumexp(H,1); #use logsumexp as intermediate step to eliminate NaN and Inf cond.
    s=exp(s_log);
    s=s+(s==0)*tiny;
    s2=sum(s)+tiny;


    factorCount = 0;
    for (k in 1:numMeans){
      ks = toString(k);
      I = diag(as.vector(mofa[[k]]$numFactors));
      Psi_ = diag(as.vector(1/mofa[[k]]$Psi)) #diag(1/mofa[[k]]$Psi);
      kD=((k-1)*numDims+1):(k*numDims)
      #the current component Lambda is mofa{k}.Lambda
      factorCount = factorCount + mofa[[k]]$numFactors;
      LP=Psi_%*%mofa[[k]]$Lambda;
      auxM1=I+t(mofa[[k]]$Lambda)%*%LP;
      MM=Psi_-t(solve(t(auxM1),t(LP)))%*%t(LP) #singularity alert
      Xk=(data-ones(numSamples,1)%*%t(matrix(Mu[,k],ncol = 1)));
      XX[kD,]=t(rprod(Xk,matrix(exp(H[,k]))))%*%Xk/s[k];
      beta=t(mofa[[k]]$Lambda)%*%MM;
      mofa[[k]]$EZZ= I-beta%*%mofa[[k]]$Lambda +beta%*%XX[kD,]%*%t(beta);
    }

    temp <- logL(data, dim(matrix(numFactors,nrow = 1))[2],
                 pivec = matrix(Pi,nrow = 1), B = Lambda, mu = Mu, D = Psi,
                 numFactors = numFactors)
    lik = temp$logL

    modl[[iter]]$Lambda=Lambda;
    modl[[iter]]$Psi=Psi;
    modl[[iter]]$Mu=Mu;
    modl[[iter]]$Pi=Pi;
    modl[[iter]]$lik=lik;
    modl[[iter]]$numSamples=numSamples;
    modl[[iter]]$numFactors=numFactors;


    tempOut=getDescLen(modl[[iter]],lik,1); ################## need to check this function
    dl = tempOut$descLength
    numparams = tempOut$numparams
    modl[[iter]]$dl=dl;
    modl[[iter]]$numparams=numparams;
    #HK: Log models so as to select the one with maximal loglik upon
    #degeneraiton

    #### log dl ####

    logs=matrix(c(logs,dl),nrow=1);

    if (iter<=2){
      dlbase=dl;
    } else if (dl>oldDL){
      #HK !!! violation !!!
      if(verbose){
      message("In cont_mfa_fj: dl > oldDL.")}
    } else if (abs((dl-oldDL)/(oldDL))<tol){
      break
    }
    
    #### M Step ####
    # means and covariance structure

    Psi=zeros(numDims,numMeans);
    factorCount = 0;
    sumPsi = zeros(numDims,1);

    for (k in 1:numMeans){

      kN=((k-1)*numSamples+1):(k*numSamples);

      T0=rprod(data,matrix(exp(H[,k]))); #T0 is h_ij*x_i in Zubin's paper
      T1=t(T0)%*%cbind(mofa[[k]]$EZ,ones(numSamples,1))  

      XH=t(mofa[[k]]$EZ)%*%exp(H[,k]);   
      T2 <- rbind(cbind(s[k]*mofa[[k]]$EZZ,XH),cbind(t(XH),s[k]))

      # T2 is inverse of sum of EZZ, but h_ij is taken within the matrix.
      #for example s(k) is the entry shown with 1 in the EZZ formula, but here it is sum(h_ij) over j only

      T3 = t(solve(t(T2),t(T1)))
      if (sum(sum(is.nan(T3)))>0){
        warning('In cont_mfa_fj : T3 has NaN entries... Operation rolled back');
        Lambda= iLambda;
        #initialize Psi
        Psi= iPsi;
        #initialize component priors
        Pi=iPi;
        #initialize means
        Mu=iMu;
        minDL=iDL;
        break();
      }
      mofa[[k]]$Lambda=T3[,1:mofa[[k]]$numFactors];
      Lambda[,(factorCount+1):(factorCount+mofa[[k]]$numFactors)]=mofa[[k]]$Lambda;
      factorCount=factorCount+mofa[[k]]$numFactors;
      Mu[,k]=T3[,mofa[[k]]$numFactors+1];
      mofa[[k]]$Psi = matrix(diag(t(T0)%*%data-T3%*%t(T1))/s[k]); 
      #now, sum( Pi(k)*mofa{k}.Psi) will give the good old joint psi...
      sumPsi = sumPsi + Pi[k]*mofa[[k]]$Psi;
      Psi[,k]=mofa[[k]]$Psi;
    };


    # numerical stability corrections
    for (k in 1:numMeans){
      patch1 = (abs(mofa[[k]]$Lambda)<lambdaSmall) * (mofa[[k]]$Lambda<0); #negative small values
      patch2 = (abs(mofa[[k]]$Lambda)<lambdaSmall) * (mofa[[k]]$Lambda>=0); #positive small values
      mofa[[k]]$Lambda = mofa[[k]]$Lambda + patch1*(-lambdaSmall) + patch2*lambdaSmall;
      mofa[[k]]$Psi=(1-gamma)*mofa[[k]]$Psi+gamma*sumPsi; #Here ? Or line below
      # if above line contributes the next line is not needed
      mofa[[k]]$Psi=mofa[[k]]$Psi*(mofa[[k]]$Psi>Phmin)+(mofa[[k]]$Psi<=Phmin)*Phmin;
    }

    Psi = (1-gamma)*Psi+gamma*matrix(rep(sumPsi,numMeans),nrow = dim(sumPsi)[1]) # HK > why lose local manifold info?

    Psi= Psi*(Psi>Phmin)+(Psi<=Phmin)*Phmin; # to avoid zero variances

    patch1 = (abs(Lambda)<lambdaSmall) * (Lambda<0); #negative small values
    patch2 = (abs(Lambda)<lambdaSmall) * (Lambda>=0); #positive small values
    Lambda = Lambda + patch1*(-lambdaSmall) + patch2*lambdaSmall;

    # priors

    Pi=t(s)/s2;
    softCount=Pi*numSamples;

    weakestLink = getWeakestLink(Pi,numSamples,numDims,numFactors,-1);
    flagCompAnnih=0;

    # kill illegitimate components from the weakest on but not the last
    # component
    while (weakestLink>0 && numMeans>1){
      k=weakestLink;
      flagCompAnnih=1;
      mofa <- mofa[-k]; 
      if(verbose){
      message(paste('In cont_mfa_fj: Annihilating component ',k,' on iteration ', iter))} ;
      #this component poses problems, eliminate it...

      params <- params[-k]
      Mu <- matrix(Mu[,-k], ncol = dim(Mu)[2] - 1)
      H <- matrix(H[,-k], ncol = dim(H)[2] - 1)
      numFactors <- numFactors[-k]
      Pi <- Pi[-k] 
      Pi = Pi / sum(Pi);
      #update numMeans
      numMeans = numMeans - 1;

      weakestLink =getWeakestLink(Pi = Pi ,numSamples,numDims,numFactors,-1);
    }   #end while

    factorCount = 0;
    if (flagCompAnnih){
      Lambda=zeros(numDims,sum(numFactors));
      Psi=zeros(numDims,numMeans);
      for (k in 1:numMeans){
        Lambda[,(factorCount+1):(factorCount+mofa[[k]]$numFactors)]=mofa[[k]]$Lambda;
        Psi[,k]=mofa[[k]]$Psi;
        factorCount = factorCount+mofa[[k]]$numFactors;
      }

    }

    if (   sum(sum(is.nan(Psi)))>0 || sum(sum(is.nan(Lambda)))>0 ){
      warning('In cont_mfa_fj : Problem - NaN entries in Lambda and/or Psi');
    } #if problem...


  } #iter
  # when density is >1 in small regions, ML becomes negative!

  #####

  mnLog=min(logs)
  minind = which.min(logs)
  Lambda= modl[[minind]]$Lambda;
  Psi=modl[[minind]]$Psi;
  Mu=modl[[minind]]$Mu;
  Pi=modl[[minind]]$Pi;
  numFactors=modl[[minind]]$numFactors;
  minDL=modl[[minind]]$dl;

  return(list(Lambda = Lambda,Psi = Psi,Mu = Mu,Pi = Pi,numFactors= numFactors,logs = logs, minDL = minDL))

}
