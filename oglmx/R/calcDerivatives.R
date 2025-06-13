
scoreMean<-function(eta_1,eta_0,std.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  gprob<- -(ProbFuncD(eta_1)-ProbFuncD(eta_0))*(1/std.dev)/prob
  if (link=="probit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<-(1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])*eta_0[prob==0 & eta_0>0 & eta_1!=eta_0]
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- -(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])*eta_1[prob==0 & eta_1<0 & eta_1!=eta_0]
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  if (link=="logit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<- (1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- -(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  return(gprob)
}

scoreVar<-function(eta_1,eta_0,std.dev,gstd.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  prod1<-eta_1*ProbFuncD(eta_1)
  prod1[eta_1==Inf]<-0
  prod0<-eta_0*ProbFuncD(eta_0)
  prod0[eta_0==-Inf]<-0
  gprob<- -(gstd.dev/std.dev)*(prod1-prod0)/prob
  if (link=="probit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<-(1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])*(eta_0[prob==0 & eta_0>0 & eta_1!=eta_0])^2
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- -(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])*(eta_1[prob==0 & eta_1<0 & eta_1!=eta_0])^2
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  if (link=="logit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<-(1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])*eta_0[prob==0 & eta_0>0 & eta_1!=eta_0]
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- -(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])*eta_1[prob==0 & eta_1<0 & eta_1!=eta_0]
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  return(gprob)
}

scoreThresh<-function(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  leftcols<-c(2:ncol(outcomematrix))[estThresh]
  leftOutcome<-outcomematrix[,leftcols,drop=FALSE]
  rightcols<-c(1:(ncol(outcomematrix)-1))[estThresh]
  rightOutcome<-outcomematrix[,rightcols,drop=FALSE]
  gprob<- (1/std.dev)*(rightOutcome*ProbFuncD(eta_1)-leftOutcome*ProbFuncD(eta_0))/prob
  if (link=="probit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<- -(leftOutcome*eta_0)[prob==0 & eta_0>0 & eta_1!=eta_0]*(1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- (rightOutcome*eta_1)[prob==0 & eta_1<0 & eta_1!=eta_0]*(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  if (link=="logit"){
    gprob[prob==0 & eta_0>0 & eta_1!=eta_0]<- -(leftOutcome*(eta_0/eta_0))[prob==0 & eta_0>0 & eta_1!=eta_0]*(1/std.dev[prob==0 & eta_0>0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1<0 & eta_1!=eta_0]<- (rightOutcome*(eta_1/eta_1))[prob==0 & eta_1<0 & eta_1!=eta_0]*(1/std.dev[prob==0 & eta_1<0 & eta_1!=eta_0])
    gprob[prob==0 & eta_1==eta_0]<-0
  }
  return(gprob)
}


hessMean_Mean<-function(eta_1,eta_0,std.dev,prob,link){
  ProbFuncDD<-.Dpdf.func(link)
  hprob<-((1/std.dev)^2)*(ProbFuncDD(eta_1)-ProbFuncDD(eta_0))/prob
  if (link=="probit"){
    hprob[prob==0 & eta_0>0]<-((1/std.dev[prob==0 & eta_0>0])^2)*eta_0[prob==0 & eta_0>0]^2
    hprob[prob==0 & eta_1<0]<-((1/std.dev[prob==0 & eta_1<0])^2)*eta_1[prob==0 & eta_1<0]^2
  }
  return(hprob)
}


hessMean_Var<-function(eta_1,eta_0,std.dev,gstd.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  ProbFuncDD<-.Dpdf.func(link)
  prod1<-eta_1*ProbFuncDD(eta_1)
  prod0<-eta_0*ProbFuncDD(eta_0)
  prod1[eta_1==Inf]<-0
  prod0[eta_0==-Inf]<-0
  hprob<- (gstd.dev/std.dev^2)*(ProbFuncD(eta_1)-ProbFuncD(eta_0)+(prod1-prod0))/prob
}


hessVar_Var<-function(eta_1,eta_0,std.dev,gstd.dev,hstd.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  ProbFuncDD<-.Dpdf.func(link)
  prod1D<-eta_1*ProbFuncD(eta_1)
  prod0D<-eta_0*ProbFuncD(eta_0)
  prod1DD<-ProbFuncDD(eta_1)*eta_1^2
  prod0DD<-ProbFuncDD(eta_0)*eta_0^2
  prod1D[eta_1==Inf]<-0
  prod1DD[eta_1==Inf]<-0
  prod0DD[eta_0==-Inf]<-0
  prod0D[eta_0==-Inf]<-0
  hprob<- ((-hstd.dev/std.dev+2*(gstd.dev/std.dev)^2)*(prod1D-prod0D)+
             ((gstd.dev/std.dev)^2)*(prod1DD-prod0DD))/prob
}

hessMean_Thresh<-function(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link){
  hprob<- -hessThresh_Thresh(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link)
}

hessVar_Thresh<-function(estThresh,outcomematrix,eta_1,eta_0,std.dev,gstd.dev,prob,link){
  ProbFuncD<-.pdf.func(link)
  ProbFuncDD<-.Dpdf.func(link)
  leftcols<-c(2:ncol(outcomematrix))[estThresh]
  leftOutcome<-outcomematrix[,leftcols,drop=FALSE]
  rightcols<-c(1:(ncol(outcomematrix)-1))[estThresh]
  rightOutcome<-outcomematrix[,rightcols,drop=FALSE]
  prod1<-as.vector(eta_1*ProbFuncDD(eta_1))
  prod0<-as.vector(eta_0*ProbFuncDD(eta_0))
  prod1[eta_1==Inf]<-0
  prod0[eta_0==-Inf]<-0
  hprob<- -(as.vector(gstd.dev/(std.dev)^2))*(rightOutcome*(as.vector(ProbFuncD(eta_1))+prod1)-leftOutcome*(as.vector(ProbFuncD(eta_0))+prod0))/prob
}

hessThresh_Thresh<-function(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link){
  ProbFuncDD<-.Dpdf.func(link)
  leftcols<-c(2:ncol(outcomematrix))[estThresh]
  leftOutcome<-outcomematrix[,leftcols,drop=FALSE]
  rightcols<-c(1:(ncol(outcomematrix)-1))[estThresh]
  rightOutcome<-outcomematrix[,rightcols,drop=FALSE]
  hprob<-(1/as.vector(std.dev)^2)*(rightOutcome*as.vector(ProbFuncDD(eta_1))-leftOutcome*as.vector(ProbFuncDD(eta_0)))/prob
}
