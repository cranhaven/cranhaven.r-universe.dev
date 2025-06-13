margins.oglmx<-function(object,atmeans=TRUE,AME=FALSE,location=NULL,outcomes="All",
                        ascontinuous=FALSE,Vars=NULL){
  # given object of class oglmx return marginal effects.
  # default return gives the marginal effect evaluated at the mean of the RHS variables (atmeans=TRUE)
  # AME=TRUE calculates the marginal effect at each data point in the sample and averages output, requires
  # the data frames to be returned as output in the call to oglmx
  # location is NULL or a numeric vector of length equal to the number of RHS variables, allows the user to
  # obtain marginal effects for any possible variable combination, rather than just the mean
  # Vars allows the user to specify which variables marginal effects are sought
  # outcomes indicates which outcome the marginal effect is sought
  
  
   #object<-results.oprobhet1
   #atmeans=TRUE
   #AME=FALSE
   #location=NULL
   #outcomes="All"
   #ascontinuous=FALSE
  #dummyzero=FALSE
   #Vars=NULL
  
  if (outcomes=="All"){
    outcomeMat<-diag(object$NOutcomes)
    outcomeNames<-object$Outcomes
  } else {
    outcomeMat<-diag(object$NOutcomes)[match(outcomes,object$outcomes,0L),]
    if (nrow(outcomeMat)==0){
      stop("Invalid Outcome Requested")
    }
    outcomeNames<-outcomes
  }
  
  beta<-object$allparams$beta
  delta<-object$allparams$delta
  
  paramtypes<-attr(object$coefficients,"coefftypes")
  
  whichXest<-object$Est.Parameters[[1]]
  whichZest<-object$Est.Parameters[[2]]
  whichAlphaest<-object$Est.Parameters[[3]]
  
  if (is.null(Vars)){
    betalocs<-c(1:length(beta))
    if (object$Hetero){
      deltalocs<-c(1:length(delta))
    }
  } else {
    betalocs<-match(Vars,names(object$varMeans[[1]]))
    if (object$Hetero){
      deltalocs<-match(Vars,names(object$varMeans[[2]]))
    }
  }
  betalocs<-betalocs[names(object$varMeans[[1]])[betalocs]!="(Intercept)"]
  if (object$Hetero){deltalocs<-deltalocs[names(object$varMeans[[2]])[deltalocs]!="(Intercept)"]}
  
  thresholds<-getThresholds(outcomeMat,object$allparams$threshparam)
  
  if (object$Hetero){
    gsdmodel<-D(object$sdmodel,"z")
    hsdmodel<-D(gsdmodel,"z")
  }
  
  if (atmeans){
    X<-matrix(object$varMeans[[1]],nrow=1)
    Z<-matrix(object$varMeans[[2]],nrow=1)
    MeanValues<-X%*%beta
    Std.Values<-eval({z<-Z%*%delta;object$sdmodel})
    if (object$Hetero){
      GStd.Values<-eval({z<-Z%*%delta;gsdmodel})
      HStd.Values<-eval({z<-Z%*%delta;hsdmodel})
    }
  } else if (!is.null(location)){
    if (!object$Hetero){
      X<-matrix(location,nrow = 1)
      MeanValues<-X%*%beta
      Std.Values<-eval({z<-1%*%delta;object$sdmodel})
    } else {
      if (length(location)!=2){
        stop("Provide location as a list with two elements: a vector of variable values for the mean equation \n
             and a vector of values for the variance equation.")
      }
      X<-matrix(location[[1]],nrow=1)
      Z<-matrix(location[[2]],nrow=1)
      MeanValues<-X%*%object$allparams$beta
      Std.Values<-eval({z<-Z%*%delta;object$sdmodel})
      GStd.Values<-eval({z<-Z%*%delta;gsdmodel})
      HStd.Values<-eval({z<-Z%*%delta;hsdmodel})
    }
  }
  
  if (AME){
    X<-object$modelframes$X
    Z<-object$modelframes$Z
    MeanValues<-X%*%beta
    Std.Values<-eval({z<-Z%*%delta;object$sdmodel})
    if (object$Hetero){
      GStd.Values<-eval({z<-Z%*%delta;gsdmodel})
      HStd.Values<-eval({z<-Z%*%delta;hsdmodel})
    }
  }
  
  if (sum(paramtypes[[3]])>0){
    outcomeMat<-lapply(c(1:nrow(thresholds)),function(x){matrix(outcomeMat[x,],nrow=length(MeanValues),ncol=ncol(outcomeMat),byrow = TRUE)})
  }
  
  etas<-lapply(c(1:nrow(thresholds)),function(x){getEtas.Exp(thresholds[x,],MeanValues,Std.Values)})
  
  # need to find the locations of the beta coefficients that will be used in the continuous calculations
  if (!ascontinuous){
    betalocscon<-betalocs[!(betalocs %in% c(1:length(beta))[object$varBinary[[1]]])]
    betalocsbin<-betalocs[(betalocs %in% c(1:length(beta))[object$varBinary[[1]]])]
    # if (length(object$factorvars)>0){
    #   betafactorlocs<-lapply(c(1:length(object$factorvars)),function(x){match(paste(object$factorvars[x],attr(object$factorvars,"levels")[[x]],sep=""),names(object$varMeans[[1]]),0)})
    # } else {
    #   betafactorlocs<-NULL 
    # }
    
    if (object$Hetero){
      deltalocscon<-deltalocs[!(deltalocs %in% c(1:length(delta))[object$varBinary[[2]]])]
      deltalocsbin<-deltalocs[(deltalocs %in% c(1:length(delta))[object$varBinary[[2]]])]
      # if (length(object$factorvars)>0){
      #   deltafactorlocs<-lapply(c(1:length(object$factorvars)),function(x){match(paste(object$factorvars[x],attr(object$factorvars,"levels")[[x]],sep=""),names(object$varMeans[[2]]),0)})
      # } else {
      #   deltafactorlocs<-NULL 
      # }
    }
  } else {
    betalocscon<-betalocs
    betalocsbin<-numeric()
    if (object$Hetero){
      deltalocscon<-deltalocs
      deltalocsbin<-numeric()
    }
  }
  
  betacont<-beta[betalocscon]
  betabin<-beta[betalocsbin]
  whichXest<-c(1:length(whichXest))[whichXest]
  if (object$Hetero){
    deltacont<-delta[deltalocscon]
    deltabin<-delta[deltalocsbin]
    whichZest<-c(1:length(whichZest))[whichZest]
  }
  
  
  if (length(betacont)>0){
    meanmargin<-lapply(c(1:length(etas)),function(x){continuous.margin.mean(betacont,etas[[x]],object$link,Std.Values)})
    marginsmean_cont<-lapply(meanmargin,function(x){apply(x,2,mean)})
    
    if (sum(paramtypes[[1]])>0){
      d_meanmargin_db<-lapply(etas,function(x){D_continuous.margin.mean_mean(betalocscon,whichXest,X[,whichXest,drop=FALSE],betacont,x,object$link,Std.Values)})
    } else {d_meanmargin_db<-NULL}
    if (sum(paramtypes[[2]])>0){
      d_meanmargin_dd<-lapply(etas,function(x){D_continuous.margin.mean_var(Z[,whichZest,drop=FALSE],betacont,x,object$link,Std.Values,GStd.Values)})
    } else {d_meanmargin_dd<-NULL}
    if (sum(paramtypes[[3]])>0){
      d_meanmargin_da<-lapply(c(1:length(etas)),function(x){D_continuous.margin.mean_alpha(whichAlphaest,outcomeMat[[x]],betacont,etas[[x]],object$link,Std.Values)})
    } else {d_meanmargin_da<-NULL}
    d_meanmargin_cont<-list(d_meanmargin_db,d_meanmargin_dd,d_meanmargin_da)
    d_meanmargin_cont<-d_meanmargin_cont[!sapply(d_meanmargin_cont,is.null)]
    d_meanmargin_cont<-lapply(c(1:length(etas)),function(x){do.call(cbind,lapply(d_meanmargin_cont,function(y){y[[x]]}))})
    
  } else {
    marginsmean_cont<-NULL
    d_meanmargin_cont<-NULL
  }
  
  if (object$Hetero){
    if (length(deltacont)>0){
      sdmargin<-lapply(c(1:length(etas)),function(x){continuous.margin.sd(deltacont,etas[[x]],object$link,Std.Values,GStd.Values)})
      marginsvar_cont<-lapply(sdmargin,function(x){apply(x,2,mean)})
      
      if (sum(paramtypes[[1]])>0){
        d_varmargin_db<-lapply(etas,function(x){D_continuous.margin.var_mean(X[,whichXest,drop=FALSE],deltacont,x,object$link,Std.Values,GStd.Values)})
      } else {d_varmargin_db<-NULL}
      if (sum(paramtypes[[2]])>0){
        d_varmargin_dd<-lapply(etas,function(x){D_continuous.margin.var_var(deltalocscon,whichZest,Z[,whichZest,drop=FALSE],deltacont,x,object$link,Std.Values,GStd.Values,HStd.Values)})
      } else {d_varmargin_dd<-NULL}
      if (sum(paramtypes[[3]])>0){
        d_varmargin_da<-lapply(c(1:length(etas)),function(x){D_continuous.margin.var_alpha(whichAlphaest,outcomeMat[[x]],deltacont,etas[[x]],object$link,Std.Values,GStd.Values)})
      } else {d_varmargin_da<-NULL}
      d_varmargin_cont<-list(d_varmargin_db,d_varmargin_dd,d_varmargin_da)
      d_varmargin_cont<-d_varmargin_cont[!sapply(d_varmargin_cont,is.null)]
      d_varmargin_cont<-lapply(c(1:length(etas)),function(x){do.call(cbind,lapply(d_varmargin_cont,function(y){y[[x]]}))})
      
    } else {
      marginsvar_cont<-NULL
      d_varmargin_cont<-NULL
    }
    
    # need to collect together margins and terms for the calculation of standard errors that occur in both the mean
    # and variance equations
    
    # check that there are variables to be collected that are in both the mean and variance equation
    BothEq<-object$BothEq[(object$BothEq$meanandvarLOC %in% betalocscon),,drop=FALSE] 
    if (nrow(BothEq)>0){
      Xrows<-match(BothEq$meanandvarLOC,betalocscon)
      Zrows<-match(BothEq$meanandvarLOCZ,deltalocscon)
      marginsmeanvar_cont<-lapply(c(1:length(etas)),function(x){marginsmean_cont[[x]][Xrows]+marginsvar_cont[[x]][Zrows]})
      marginsmean_cont<-lapply(c(1:length(etas)),function(x){marginsmean_cont[[x]][-Xrows]})
      marginsvar_cont<-lapply(c(1:length(etas)),function(x){marginsvar_cont[[x]][-Zrows]})
      d_meanvarmargin_cont<-lapply(c(1:length(etas)),function(x){d_meanmargin_cont[[x]][Xrows,,drop=FALSE]+d_varmargin_cont[[x]][Zrows,,drop=FALSE]})
      d_meanmargin_cont<-lapply(c(1:length(etas)),function(x){d_meanmargin_cont[[x]][-Xrows,,drop=FALSE]})
      d_varmargin_cont<-lapply(c(1:length(etas)),function(x){d_varmargin_cont[[x]][-Zrows,,drop=FALSE]})
      
    } else {
      d_meanvarmargin_cont<-NULL
      marginsmeanvar_cont<-NULL
    }
  } else {
    marginsvar_cont<-NULL
    d_varmargin_cont<-NULL
    d_meanvarmargin_cont<-NULL
    marginsmeanvar_cont<-NULL
  }
  
  # now for binary variables, need to separate variables into three categories:
  #     1. Only mean
  #     2. Only variance
  #     3. Both mean variance
  if (object$Hetero){
    BothEq<-object$BothEq[(object$BothEq$meanandvarLOC %in% betalocsbin),,drop=FALSE]
    if (nrow(BothEq)>0){
      # take the locations that are in BothEq out of the vectors to be collected
      betalocsbin<-betalocsbin[!(betalocsbin %in% BothEq$meanandvarLOC)]
      deltalocsbin<-deltalocsbin[!(deltalocsbin %in% BothEq$meanandvarLOCZ)]
    }
  }
  
  if (length(betalocsbin)>0){
    marginsmean_Disc<-lapply(etas,function(x){discrete.margin_meanonly(beta,X,betalocsbin,x,object$link,Std.Values)})
    if (sum(paramtypes[[1]])>0){
      d_meanmargin_bin_db<-lapply(marginsmean_Disc,function(x){D_discrete.margin_meanonly.mean(betalocsbin,whichXest,X,attr(x,"inputetas"),object$link,Std.Values)})
    } else {d_meanmargin_bin_db<-NULL}
    if (sum(paramtypes[[2]])>0){
      d_meanmargin_bin_dd<-lapply(marginsmean_Disc,function(x){D_discrete.margin_mean.var(whichZest,Z,attr(x,"inputetas"),object$link,Std.Values,GStd.Values)})
    } else {d_meanmargin_bin_dd<-NULL}
    if (sum(paramtypes[[3]])>0){
      d_meanmargin_bin_da<-lapply(c(1:length(etas)),function(x){D_discrete.margin_mean.alpha(whichAlphaest,outcomeMat[[x]],attr(marginsmean_Disc[[x]],"inputetas"),Std.Values,object$link)})
    } else {d_meanmargin_bin_da<-NULL}
    d_meanmargin_bin<-list(d_meanmargin_bin_db,d_meanmargin_bin_dd,d_meanmargin_bin_da)
    d_meanmargin_bin<-d_meanmargin_bin[!sapply(d_meanmargin_bin,is.null)]
    d_meanmargin_bin<-lapply(c(1:length(etas)),function(x){do.call(cbind,lapply(d_meanmargin_bin,function(y){y[[x]]}))})
  } else {
    marginsmean_Disc<-NULL
    d_meanmargin_bin<-NULL
  }
  
  if (object$Hetero){
    if (length(deltalocsbin)>0){
      marginsvar_Disc<-lapply(etas,function(x){discrete.margin_varonly(delta,Z,deltalocsbin,object$sdmodel,x,object$link,Std.Values)})
      if (sum(paramtypes[[1]])>0){
        d_varmargin_bin_db<-lapply(marginsvar_Disc,function(x){D_discrete.margin_var.mean(whichXest,X,attr(x,"inputetas"),object$link,attr(x,"StdDevs"))})
      } else {d_varmargin_bin_db<-NULL}
      if (sum(paramtypes[[2]])>0){
        d_varmargin_bin_dd<-lapply(marginsvar_Disc,function(x){D_discrete.margin_varonly.var(deltalocsbin,whichZest,Z,attr(x,"inputetas"),attr(x,"ZDbinaries"),object$link,attr(x,"StdDevs"),gsdmodel)})
      } else {d_varmargin_bin_dd<-NULL}
      if (sum(paramtypes[[3]])>0){
        d_varmargin_bin_da<-lapply(c(1:length(etas)),function(x){D_discrete.margin_var.alpha(whichAlphaest,outcomeMat[[x]],attr(marginsvar_Disc[[x]],"inputetas"),attr(marginsvar_Disc[[x]],"StdDevs"),object$link)})
      } else {d_varmargin_bin_da<-NULL}
      d_varmargin_bin<-list(d_varmargin_bin_db,d_varmargin_bin_dd,d_varmargin_bin_da)
      d_varmargin_bin<-d_varmargin_bin[!sapply(d_varmargin_bin,is.null)]
      d_varmargin_bin<-lapply(c(1:length(etas)),function(x){do.call(cbind,lapply(d_varmargin_bin,function(y){y[[x]]}))})
    } else {
      marginsvar_Disc<-NULL
      d_varmargin_bin<-NULL
    }
    
    if (nrow(BothEq)>0){
      marginsmeanvar_Disc<-lapply(etas,function(x){discrete.margin_both(beta,X,delta,Z,BothEq,object$sdmodel,x,object$link,Std.Values)})
      if (sum(paramtypes[[1]])>0){
        d_meanvarmargin_bin_db<-lapply(marginsmeanvar_Disc,function(x){D_discrete.margin_meanvar.mean(whichXest,X,BothEq,attr(x,"inputetas"),attr(x,"StdDevs"),object$link)})
      } else {d_meanvarmargin_bin_db<-NULL}
      if (sum(paramtypes[[2]])>0){
        d_meanvarmargin_bin_dd<-lapply(marginsmeanvar_Disc,function(x){D_discrete.margin_meanvar.var(whichZest,Z,BothEq,attr(x,"inputetas"),attr(x,"ZDbinaries"),object$link,attr(x,"StdDevs"),gsdmodel)})
      } else {d_meanvarmargin_bin_dd<-NULL}
      if (sum(paramtypes[[3]])>0){
        d_meanvarmargin_bin_da<-lapply(c(1:length(etas)),function(x){D_discrete.margin_var.alpha(whichAlphaest,outcomeMat[[x]],attr(marginsmeanvar_Disc[[x]],"inputetas"),attr(marginsmeanvar_Disc[[x]],"StdDevs"),object$link)})
      } else {d_meanvarmargin_bin_da<-NULL}
      d_meanvarmargin_bin<-list(d_meanvarmargin_bin_db,d_meanvarmargin_bin_dd,d_meanvarmargin_bin_da)
      d_meanvarmargin_bin<-d_meanvarmargin_bin[!sapply(d_meanvarmargin_bin,is.null)]
      d_meanvarmargin_bin<-lapply(c(1:length(etas)),function(x){do.call(cbind,lapply(d_meanvarmargin_bin,function(y){y[[x]]}))})
    } else {
      marginsmeanvar_Disc<-NULL
      d_meanvarmargin_bin<-NULL
    }
    
  } else {
    marginsvar_Disc<-NULL
    d_varmargin_bin<-NULL
    marginsmeanvar_Disc<-NULL
    d_meanvarmargin_bin<-NULL
  }
  allmargins<-list(marginsmeanvar_Disc,marginsmeanvar_cont,marginsmean_Disc,marginsmean_cont,marginsvar_Disc,marginsvar_cont)
  keeplist<-sapply(allmargins,is.null)
  allmargins<-lapply(c(1:length(outcomeNames)),function(x){do.call(c,lapply(allmargins[!keeplist],function(y){y[[x]]}))})
  alld_margins<-list(d_meanvarmargin_bin,d_meanvarmargin_cont,d_meanmargin_bin,d_meanmargin_cont,d_varmargin_bin,d_varmargin_cont)[!keeplist]
  alld_margins<-lapply(c(1:length(outcomeNames)),function(x){do.call(rbind,lapply(alld_margins,function(y){y[[x]]}))})
  
  vcovmat<-vcov.oglmx(object)
  StdErrors<-lapply(alld_margins,function(x){calcMEstdErrors(x,vcovmat)})
  TValues<-lapply(c(1:length(StdErrors)),function(x){allmargins[[x]]/StdErrors[[x]]})
  PValues<-lapply(TValues,function(x){2*pnorm(-abs(x))})
  
  Xnames<-names(object$varMeans[[1]])
  marginnames<-character()
  if (object$Hetero){
    Znames<-names(object$varMeans[[2]])
    marginnames<-c(marginnames,BothEq$meanandvarNAME)
    marginnames<-c(marginnames,Xnames[betalocscon[betalocscon %in% object$BothEq$meanandvarLOC]])
    marginnames<-c(marginnames,Xnames[betalocsbin])
    marginnames<-c(marginnames,Xnames[betalocscon[!(betalocscon %in% object$BothEq$meanandvarLOC)]])
    marginnames<-c(marginnames,Znames[deltalocsbin])
    marginnames<-c(marginnames,Znames[deltalocscon[!(deltalocscon %in% object$BothEq$meanandvarLOCZ)]])
  } else {
    marginnames<-c(marginnames,Xnames[betalocsbin])
    marginnames<-c(marginnames,Xnames[betalocscon])
  }
  
  output<-lapply(c(1:length(StdErrors)),function(x){cbind("Marg. Eff"=allmargins[[x]],"Std. error"=StdErrors[[x]],
                                                          "t value"=TValues[[x]],"Pr(>|t|)"=PValues[[x]])})
  output<-lapply(output,function(x){rownames(x)<-marginnames; x})
  names(output)<-outcomeNames
  class(output)<-c("margins.oglmx")
  output
}

getEtas.Exp<-function(thresholds,xb_matrix,sd_matrix){
  eta1numerator<-apply(xb_matrix,2,function(x){thresholds[2]-x})
  eta0numerator<-apply(xb_matrix,2,function(x){thresholds[1]-x})
  etas1<-eta1numerator/sd_matrix
  etas0<-eta0numerator/sd_matrix
  return(list(etas1,etas0))
}

continuous.margin.mean<-function(paramvec,etas,link,std.dev){
    margineffect<- scoreMean(etas[[1]],etas[[2]],std.dev,1,link)%*%paramvec
    return(margineffect)
}

D_continuous.margin.mean_mean<-function(whichMargins,whichXest,X,paramvec,etas,link,std.dev){
  firstterm<-scoreMean(etas[[1]],etas[[2]],std.dev,1,link)
  ftmatrix<-t(sapply(whichMargins,function(x){as.numeric(whichXest %in% x)}))
  d_margineffect1<- ftmatrix*mean(firstterm)
  #  return((ProbFuncDD(etas[[1]])-ProbFuncDD(etas[[2]]))/(std.dev^2))
  secondterm<-X*as.vector(hessMean_Mean(etas[[1]],etas[[2]],std.dev,1,link))
  d_margineffect2<- (matrix(paramvec,ncol=nrow(X),nrow=length(paramvec))%*%secondterm)/length(etas[[1]])
  #return(list(d_margineffect1,d_margineffect2))
  d_margineffect<- d_margineffect1+d_margineffect2
  return(d_margineffect)
}


D_continuous.margin.mean_var<-function(Z,paramvec,etas,link,std.dev,gstd.dev){
  d_margineffect<-(matrix(paramvec,ncol=nrow(Z),nrow=length(paramvec))%*%(Z*as.vector(hessMean_Var(etas[[1]],etas[[2]],std.dev,gstd.dev,1,link))))/length(etas[[1]])
  return(d_margineffect)
}

D_continuous.margin.mean_alpha<-function(estThresh,outcomematrix,paramvec,etas,link,std.dev){
  d_margineffect<- (matrix(paramvec,ncol=length(etas[[1]]),nrow=length(paramvec))%*%hessMean_Thresh(estThresh,outcomematrix,etas[[1]],etas[[2]],std.dev,1,link))/length(etas[[1]])
  return(d_margineffect)
}

continuous.margin.sd<-function(paramvec,etas,link,std.dev,gstd.dev){
  margineffect<- scoreVar(etas[[1]],etas[[2]],std.dev,gstd.dev,1,link)%*%paramvec
  return(margineffect)
}

D_continuous.margin.var_mean<-function(X,paramvec,etas,link,std.dev,gstd.dev){
  d_margineffect<-(matrix(paramvec,ncol=nrow(X),nrow=length(paramvec))%*%(X*as.vector(hessMean_Var(etas[[1]],etas[[2]],std.dev,gstd.dev,1,link))))/length(etas[[1]])
  return(d_margineffect)
}


D_continuous.margin.var_var<-function(whichMargins,whichZest,Z,paramvec,etas,link,std.dev,gstd.dev,hstd.dev){
  firstterm<-scoreVar(etas[[1]],etas[[2]],std.dev,gstd.dev,1,link)
  ftmatrix<-t(sapply(whichMargins,function(x){as.numeric(whichZest %in% x)}))
  d_margineffect1<- -ftmatrix*mean(firstterm)
  secondterm<-Z*as.vector(hessVar_Var(etas[[1]],etas[[2]],std.dev,gstd.dev,hstd.dev,1,link))
  d_margineffect2<- (matrix(paramvec,ncol=nrow(Z),nrow=length(paramvec))%*%secondterm)/length(etas[[1]])
  d_margineffect<- d_margineffect1+d_margineffect2
  return(d_margineffect)
}

D_continuous.margin.var_alpha<-function(estThresh,outcomematrix,paramvec,etas,link,std.dev,gstd.dev){
  d_margineffect<-(matrix(paramvec,ncol=length(etas[[1]]),nrow=length(paramvec))%*%hessVar_Thresh(estThresh,outcomematrix,etas[[1]],etas[[2]],std.dev,gstd.dev,1,link))/length(etas[[1]])
  return(d_margineffect)
}

discrete.margin_meanonly<-function(beta,X,whichVars,etas,link,std.dev){
  ProbFunc<-.cdf.func(link)
  XBmarginremoved<-t(apply(X[,whichVars,drop=FALSE],1,function(x){x*beta[whichVars]}))/as.vector(std.dev)
  etas1Untreated<-as.vector(etas[[1]])+XBmarginremoved
  etas0Untreated<-as.vector(etas[[2]])+XBmarginremoved
  TreatEffects<-t(beta[whichVars]%*%t(std.dev))
  etas1Treated<-etas1Untreated-TreatEffects
  etas0Treated<-etas0Untreated-TreatEffects
  margineffect<-ProbFunc(etas1Treated)-ProbFunc(etas0Treated)-(ProbFunc(etas1Untreated)-ProbFunc(etas0Untreated))
  margineffect<-apply(margineffect,2,mean)
  attr(margineffect,"inputetas")<-list(etas1Treated=etas1Treated,etas0Treated=etas0Treated,etas1Untreated=etas1Untreated,etas0Untreated=etas0Untreated)
  return(margineffect)
}

discrete.margin_varonly<-function(delta,Z,whichVars,sdmodel,etas,link,std.dev){
  ProbFunc<-.cdf.func(link)
  ZDmarginremoved<-as.vector(Z%*%delta)-t(apply(Z[,whichVars,drop=FALSE],1,function(x){x*delta[whichVars]}))
  ZDUntreated<-ZDmarginremoved
  StdUntreated<-eval({z<-ZDUntreated;sdmodel})
  TreatEffects<-matrix(delta[whichVars],ncol=length(whichVars),nrow=nrow(ZDmarginremoved),byrow = TRUE)
  ZDTreated<-ZDmarginremoved+TreatEffects
  StdTreated<-eval({z<-ZDTreated;sdmodel})
  etas1Untreated<-as.vector(etas[[1]])*as.vector(std.dev)/StdUntreated
  etas0Untreated<-as.vector(etas[[2]])*as.vector(std.dev)/StdUntreated
  etas1Treated<-as.vector(etas[[1]])*as.vector(std.dev)/StdTreated
  etas0Treated<-as.vector(etas[[2]])*as.vector(std.dev)/StdTreated
  margineffect<-ProbFunc(etas1Treated)-ProbFunc(etas0Treated)-(ProbFunc(etas1Untreated)-ProbFunc(etas0Untreated))
  margineffect<-apply(margineffect,2,mean)
  attr(margineffect,"inputetas")<-list(etas1Treated=etas1Treated,etas0Treated=etas0Treated,etas1Untreated=etas1Untreated,etas0Untreated=etas0Untreated)
  attr(margineffect,"ZDbinaries")<-list(ZDTreated=ZDTreated,ZDUntreated=ZDUntreated)
  attr(margineffect,"StdDevs")<-list(StdTreated=StdTreated,StdUntreated=StdUntreated)
  return(margineffect)
}

discrete.margin_both<-function(beta,X,delta,Z,BothEqLocs,sdmodel,etas,link,std.dev){
  ProbFunc<-.cdf.func(link)
  XBmarginremoved<-t(apply(X[,BothEqLocs$meanandvarLOC,drop=FALSE],1,function(x){x*beta[BothEqLocs$meanandvarLOC]}))
  etas1NumUntreated<-as.vector(etas[[1]])*as.vector(std.dev)+XBmarginremoved
  etas0NumUntreated<-as.vector(etas[[2]])*as.vector(std.dev)+XBmarginremoved
  TreatEffectsMean<-matrix(beta[BothEqLocs$meanandvarLOC],ncol=length(BothEqLocs$meanandvarLOC),nrow=nrow(XBmarginremoved),byrow = TRUE)
  etas1NumTreated<-etas1NumUntreated-TreatEffectsMean
  etas0NumTreated<-etas0NumUntreated-TreatEffectsMean
  ZDmarginremoved<-as.vector(Z%*%delta)-t(apply(Z[,BothEqLocs$meanandvarLOCZ,drop=FALSE],1,function(x){x*delta[BothEqLocs$meanandvarLOCZ]}))
  ZDUntreated<-ZDmarginremoved
  StdUntreated<-eval({z<-ZDmarginremoved;sdmodel})
  TreatEffectsVar<-matrix(delta[BothEqLocs$meanandvarLOCZ],ncol=length(BothEqLocs$meanandvarLOCZ),nrow=nrow(ZDmarginremoved),byrow = TRUE)
  ZDTreated<-ZDmarginremoved+TreatEffectsVar
  StdTreated<-eval({z<-ZDTreated;sdmodel})
  etas1Untreated<-etas1NumUntreated/StdUntreated
  etas0Untreated<-etas0NumUntreated/StdUntreated
  etas1Treated<-etas1NumTreated/StdTreated
  etas0Treated<-etas0NumTreated/StdTreated
  margineffect<-ProbFunc(etas1Treated)-ProbFunc(etas0Treated)-(ProbFunc(etas1Untreated)-ProbFunc(etas0Untreated))
  margineffect<-apply(margineffect,2,mean)
  attr(margineffect,"inputetas")<-list(etas1Treated=etas1Treated,etas0Treated=etas0Treated,etas1Untreated=etas1Untreated,etas0Untreated=etas0Untreated)
  attr(margineffect,"ZDbinaries")<-list(ZDTreated=ZDTreated,ZDUntreated=ZDUntreated)
  attr(margineffect,"StdDevs")<-list(StdTreated=StdTreated,StdUntreated=StdUntreated)
  return(margineffect)
}

D_discrete.margin_meanonly.mean<-function(whichVars,whichXest,X,fouretas,link,std.dev){
  # whichVars says which elements of the beta vector correspond to binary variables
  # whichXest says which elements of the beta vector are estimated
  TreatScore<-scoreMean(fouretas[[1]],fouretas[[2]],as.vector(std.dev),1,link)
  UntreatScore<-scoreMean(fouretas[[3]],fouretas[[4]],as.vector(std.dev),1,link)
  UntreatedX<-lapply(whichVars,function(x){X[,x]<-0;X})
  TreatedX<-lapply(whichVars,function(x){X[,x]<-1;X})
  d_margineffect<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(TreatedX[[x]][,whichXest,drop=FALSE]*TreatScore[,x]-UntreatedX[[x]][,whichXest,drop=FALSE]*UntreatScore[,x],2,mean)}))
  return(d_margineffect)
}

D_discrete.margin_mean.var<-function(whichZest,Z,fouretas,link,std.dev,gstd.dev){
  TreatScore<-scoreVar(fouretas[[1]],fouretas[[2]],as.vector(std.dev),as.vector(gstd.dev),1,link)
  UntreatScore<-scoreVar(fouretas[[3]],fouretas[[4]],as.vector(std.dev),as.vector(gstd.dev),1,link)
  d_margineffect<-t(sapply(c(1:ncol(TreatScore)),function(x){apply(Z[,whichZest,drop=FALSE]*(TreatScore[,x]-UntreatScore[,x]),2,mean)}))
  return(d_margineffect)
}

D_discrete.margin_mean.alpha<-function(estThresh,outcomematrix,fouretas,std.dev,link){
  TreatScore<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[1]][,x],fouretas[[2]][,x],as.vector(std.dev),1,link),2,mean)}))
  UntreatScore<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[3]][,x],fouretas[[4]][,x],as.vector(std.dev),1,link),2,mean)}))
  d_margineffect<-TreatScore-UntreatScore
}


D_discrete.margin_var.mean<-function(whichXest,X,fouretas,link,StdDevs){
  TreatScore<-scoreMean(fouretas[[1]],fouretas[[2]],StdDevs[[1]],1,link)
  UntreatScore<-scoreMean(fouretas[[3]],fouretas[[4]],StdDevs[[2]],1,link)
  d_margineffect<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(X[,whichXest,drop=FALSE]*TreatScore[,x]-X[,whichXest,drop=FALSE]*UntreatScore[,x],2,mean)}))
  return(d_margineffect)
}

D_discrete.margin_varonly.var<-function(whichVars,whichZest,Z,fouretas,ZDinputs,link,StdDevs,gsdmodel){
  GStdTreated<-eval({z<-ZDinputs[[1]];gsdmodel})
  GStdUntreated<-eval({z<-ZDinputs[[2]];gsdmodel})
  TreatScore<-scoreVar(fouretas[[1]],fouretas[[2]],StdDevs[[1]],GStdTreated,1,link)
  UntreatScore<-scoreVar(fouretas[[3]],fouretas[[4]],StdDevs[[2]],GStdUntreated,1,link)
  UntreatedZ<-lapply(whichVars,function(x){Z[,x]<-0;Z})
  TreatedZ<-lapply(whichVars,function(x){Z[,x]<-1;Z})
  d_margineffect<-t(sapply(c(1:length(whichVars)),function(x){apply(TreatedZ[[x]][,whichZest,drop=FALSE]*TreatScore[,x]-UntreatedZ[[x]][,whichZest,drop=FALSE]*UntreatScore[,x],2,mean)}))
  return(d_margineffect)
}

D_discrete.margin_var.alpha<-function(estThresh,outcomematrix,fouretas,StdDevs,link){
  if (sum(estThresh)>1){
    TreatScore<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[1]][,x],fouretas[[2]][,x],StdDevs[[1]][,x],1,link),2,mean)}))
    UntreatScore<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[3]][,x],fouretas[[4]][,x],StdDevs[[2]][,x],1,link),2,mean)}))
  } else {
    TreatScore<-sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[1]][,x],fouretas[[2]][,x],StdDevs[[1]][,x],1,link),2,mean)})
    UntreatScore<-sapply(c(1:ncol(fouretas[[1]])),function(x){apply(scoreThresh(estThresh,outcomematrix,fouretas[[3]][,x],fouretas[[4]][,x],StdDevs[[2]][,x],1,link),2,mean)})
  }
  d_margineffect<-TreatScore-UntreatScore
}

D_discrete.margin_meanvar.mean<-function(whichXest,X,BothEqLocs,fouretas,StdDevs,link){
  TreatScore<-scoreMean(fouretas[[1]],fouretas[[2]],StdDevs[[1]],1,link)
  UntreatScore<-scoreMean(fouretas[[3]],fouretas[[4]],StdDevs[[2]],1,link)
  whichVars<-BothEqLocs$meanandvarLOC
  UntreatedX<-lapply(whichVars,function(x){X[,x]<-0;X})
  TreatedX<-lapply(whichVars,function(x){X[,x]<-1;X})
  d_margineffect<-t(sapply(c(1:ncol(fouretas[[1]])),function(x){apply(TreatedX[[x]][,whichXest,drop=FALSE]*TreatScore[,x]-UntreatedX[[x]][,whichXest,drop=FALSE]*UntreatScore[,x],2,mean)}))
  return(d_margineffect)
}

D_discrete.margin_meanvar.var<-function(whichZest,Z,BothEqLocs,fouretas,ZDinputs,link,StdDevs,gsdmodel){
  GStdTreated<-eval({z<-ZDinputs[[1]];gsdmodel})
  GStdUntreated<-eval({z<-ZDinputs[[2]];gsdmodel})
  TreatScore<-scoreVar(fouretas[[1]],fouretas[[2]],StdDevs[[1]],GStdTreated,1,link)
  UntreatScore<-scoreVar(fouretas[[3]],fouretas[[4]],StdDevs[[2]],GStdUntreated,1,link)
  whichVars<-BothEqLocs$meanandvarLOCZ
  UntreatedZ<-lapply(whichVars,function(x){Z[,x]<-0;Z})
  TreatedZ<-lapply(whichVars,function(x){Z[,x]<-1;Z})
  d_margineffect<-t(sapply(c(1:length(whichVars)),function(x){apply(TreatedZ[[x]][,whichZest,drop=FALSE]*TreatScore[,x]-UntreatedZ[[x]][,whichZest,drop=FALSE]*UntreatScore[,x],2,mean)}))
  return(d_margineffect)
}

calcMEstdErrors<-function(derivME,estHess){
  StdErrors<-sapply(c(1:nrow(derivME)),function(x){(derivME[x, ,drop=FALSE]%*%estHess%*%t(derivME[x, ,drop=FALSE]))^0.5})
}
