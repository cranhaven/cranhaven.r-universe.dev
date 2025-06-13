updateComponents<-function(Env,Parameters){
  Env$parametervector<-Parameters
  with(Env,{
    if (sum(whichXest)>0){
      beta[whichXest]<-parametervector[whichparametersmean]
      XB<-as.vector(X%*%beta)
    } else if (!exists("XB")){XB<-as.vector(X%*%beta)}
    if (sum(whichZest)>0){
      delta[whichZest]<-parametervector[whichparametersscale]
      ZD<-as.vector(Z%*%delta)
      Std.Dev<-eval({z<-ZD;sdmodel})
      GStd.Dev<-eval({z<-ZD;gsdmodel})
      minStD<-.Machine$double.xmin^(1/2)
      maxStD<-.Machine$double.xmax
      Std.Dev[Std.Dev==0]<-minStD
      Std.Dev[Std.Dev==Inf]<-maxStD
      GStd.Dev[GStd.Dev==Inf]<-maxStD
      if (analhessian){
        HStd.Dev<-eval({z<-ZD;hsdmodel})
        HStd.Dev[HStd.Dev==Inf]<-maxStD
      }
    } else if (!exists("ZD")){
      ZD<-as.vector(Z%*%delta)
      Std.Dev<-eval({z<-ZD;sdmodel})
      minStD<-.Machine$double.xmin^(1/2)
      maxStD<-.Machine$double.xmax
      Std.Dev[Std.Dev==0]<-minStD
      Std.Dev[Std.Dev==Inf]<-maxStD
    }
    if (sum(whichAlphaest)>0){
      threshparam[whichAlphaest]<-parametervector[whichparametersthresh]
      ThresholdMatrix<-getThresholds(outcomeMat,threshparam)
    } else if (!exists("ThresholdMatrix")){ThresholdMatrix<-getThresholds(outcomeMat,threshparam)}
    if (!exists("scorevecs")){
      scorevecs<-matrix(0,nrow = nrow(X),ncol=length(parametervector))
      hessMatrix<-matrix(0,nrow=length(parametervector),ncol=length(parametervector))
    }
  })
}

loglikelihood.oglmx<-function(Env){
  with(Env,{
    etas<-getEtas(ThresholdMatrix,XB,Std.Dev)
    probs<-Probability(etas[[1]],etas[[2]],link = link)
    logprobs<-suppressWarnings(log(probs))
    if (!is.null(w)){
      wll<-sum(w*logprobs)
      ll<-sum(logprobs)
      return(wll)
    }
    ll<-sum(logprobs)
    return(ll)
  })
}

getThresholds<-function(outcomematrix,thresholdvector){
  no.threshold<-length(thresholdvector)
  leftThreshold<-outcomematrix[,2:(no.threshold+1),drop=FALSE]%*%thresholdvector
  leftThreshold[outcomematrix[,1]==1]<- -Inf
  rightThreshold<-outcomematrix[,1:(no.threshold),drop=FALSE]%*%thresholdvector
  rightThreshold[outcomematrix[,no.threshold+1]==1]<- Inf
  return(thresholds=cbind(leftThreshold,rightThreshold))
}



getEtas<-function(thresholds,xb,std.dev){
  etas<-(thresholds-xb)/std.dev
  return(list(eta_1=etas[,2,drop=TRUE],eta_0=etas[,1,drop=TRUE]))
}

Probability<-function(eta_1,eta_0,link){
  ProbFunc<-.cdf.func(link)
  if (!is.na(match(link,c("probit","logit","cauchit")))){
    switchsign<-eta_1>0 & eta_0>=0
    prob<-ifelse(switchsign,ProbFunc(-eta_0)-ProbFunc(-eta_1),ProbFunc(eta_1)-ProbFunc(eta_0))
  } else {
    prob<-ProbFunc(eta_1)-ProbFunc(eta_0)
  }
  return(prob)
}

score_oglmx<-function(Env){
  with(Env,{
    if (sum(whichXest)>0){
      scoMean<-scoreMean(etas[[1]],etas[[2]],Std.Dev,probs,link)
      scorevecs[,whichparametersmean]<-X[,whichXest]*scoMean
    }
    if (sum(whichZest)>0){
      scoVar<-scoreVar(etas[[1]],etas[[2]],Std.Dev,GStd.Dev,probs,link)
      scorevecs[,whichparametersscale]<-Z[,whichZest]*scoVar
    }
    if (sum(whichAlphaest)>0){
      scoThr<-scoreThresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,probs,link)
      scorevecs[,whichparametersthresh]<-scoThr
    }
    if (!is.null(w)){
      scorevecs<-w*scorevecs
    }
    score<-apply(scorevecs,2,sum)
    return(score)
  })
}

hessian_oglmx<-function(Env){
  with(Env,{
    if (sum(whichXest)>0){
      hessMean<-hessMean_Mean(etas[[1]],etas[[2]],Std.Dev,probs,link)-(scoMean^2)
      if (!is.null(w)){hessMean<-w*hessMean}
      hessMatrix[whichparametersmean,whichparametersmean]<-crossprod(X[,whichXest],X[,whichXest]*hessMean)
    }
    if (sum(whichZest)>0){
      hessVar<-hessVar_Var(etas[[1]],etas[[2]],Std.Dev,GStd.Dev,HStd.Dev,probs,link)-(scoVar^2)
      if (!is.null(w)){hessVar<-w*hessVar}
      hessMatrix[whichparametersscale,whichparametersscale]<-crossprod(Z[,whichZest],Z[,whichZest]*hessVar)
    }
    if (sum(whichXest)>0 & sum(whichZest)>0){
     hessMeVa<-hessMean_Var(etas[[1]],etas[[2]],Std.Dev,GStd.Dev,probs,link)-scoVar*scoMean
     if (!is.null(w)){hessMeVa<-w*hessMeVa}
     hessMatrix[whichparametersmean,whichparametersscale]<-crossprod(X[,whichXest],Z[,whichZest]*hessMeVa)
     hessMatrix[whichparametersscale,whichparametersmean]<-t(hessMatrix[whichparametersmean,whichparametersscale])
    }
    
    if (sum(whichAlphaest)>0){
      if (is.null(w)){
        hessMatrix[whichparametersthresh,whichparametersthresh]<-diag(apply(hessThresh_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,probs,link),2,sum),nrow=sum(whichAlphaest))-crossprod(scoThr)
      } else {
        hessMatrix[whichparametersthresh,whichparametersthresh]<-diag(apply(hessThresh_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,probs,link)*w,2,sum),nrow=sum(whichAlphaest))-crossprod(scoThr*w)
      }
    }
    if (sum(whichAlphaest)>0 & sum(whichXest)>0){
      if (is.null(w)){
        hessMatrix[whichparametersmean,whichparametersthresh]<-crossprod(X[,whichXest],hessMean_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,probs,link))-crossprod(scorevecs[,whichparametersmean],scorevecs[,whichparametersthresh])
      } else {
        hessMatrix[whichparametersmean,whichparametersthresh]<-crossprod(X[,whichXest],hessMean_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,probs,link)*w)-crossprod(scorevecs[,whichparametersmean],scorevecs[,whichparametersthresh])
      }
      hessMatrix[whichparametersthresh,whichparametersmean]<-t(hessMatrix[whichparametersmean,whichparametersthresh])
    }
    if (sum(whichAlphaest)>0 & sum(whichZest)>0){
      if (!is.null(w)){
        hessMatrix[whichparametersscale,whichparametersthresh]<-crossprod(Z[,whichZest],hessVar_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,GStd.Dev,probs,link)*w)-crossprod(scorevecs[,whichparametersscale],scorevecs[,whichparametersthresh])
      } else {
        hessMatrix[whichparametersscale,whichparametersthresh]<-crossprod(Z[,whichZest],hessVar_Thresh(whichAlphaest,outcomeMat,etas[[1]],etas[[2]],Std.Dev,GStd.Dev,probs,link))-crossprod(scorevecs[,whichparametersscale],scorevecs[,whichparametersthresh])
      }
      hessMatrix[whichparametersthresh,whichparametersscale]<-t(hessMatrix[whichparametersscale,whichparametersthresh])
    }
    return(hessMatrix)
  })
}