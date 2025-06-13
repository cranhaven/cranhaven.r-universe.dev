mergeformulas<-function(formula1,formula2){
  termsFM1<-terms(formula1)
  termsFM2<-terms(formula2)
  rhsvarsFM1<-attr(termsFM1,"term.labels")
  rhsvarsFM2<-attr(termsFM2,"term.labels")
  newterms<-rhsvarsFM2[!(rhsvarsFM2 %in% rhsvarsFM1)]
  if (length(newterms)>1){
    if (attr(termsFM1,"intercept")==0 & attr(termsFM2,"intercept")==1){
      updateform<-as.formula(paste("~.+1+",paste(newterms,collapse = "+")))
    } else {
      updateform<-as.formula(paste("~.+",paste(newterms,collapse = "+")))
    }
    finalformula<-update(formula1,updateform)
    return(finalformula)
  } else {
    return(formula1)
  }
}



.checkbinary<-function(x){
  if (sum(x==0)+sum(x==1)==length(x) & sum(x==0)>0 & sum(x==1)>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


formula.oglmx<-function(x, ...){
  # extract the formula for an oglmx object
  # for use to apply a model name in lrtest
  if (is.null(x$formula[[2]])){
    value<-x$formula[[1]]
  } else {
    # collect the names from the terms output
    # from the mean equation, include response term
    termsFM1<-terms(x$formula[[1]])
    termsFM2<-terms(x$formula[[2]])
    rhsvarsFM2<-attr(termsFM2,"term.labels")
    updateform<-as.formula(paste("~.|",paste(rhsvarsFM2,collapse="+")))
    value<-update.formula(x$formula[[1]],updateform)
  }
  return(value)
}



calcstartvalues<-function(whichparameter,gfunc,threshvec){
  meanstart<-numeric(sum(whichparameter[[1]]))
  varstart<-numeric(sum(whichparameter[[2]]))
  threshstart<-numeric(sum(whichparameter[[3]]))
  if (length(varstart)>0){
    # need to ensure that the start values are such that there is positive variance
    # setting all values equal to zero would not work in the case of a linear standard deviation function
    # assume the first estimated variance parameter is a constant, set so that the initial standard deviation is 0.5
    calcstartdelta<-function(x){eval({z<-x;gfunc})-0.5}
    varstart[1]<-uniroot(calcstartdelta,c(-10,10),extendInt="yes")$root
  }
  # threshold values are more complicated is some values are provided and some aren't
  if (length(threshstart)>0){
    nthresh<-length(threshvec)
    if (all(is.na(threshvec))){
      threshstart<-seq(from=-0.6*(nthresh/2),to=0.6*(nthresh/2),length.out=nthresh)
    } else {
      # when there is just one specified threshold can just choose an arbitrary gap between thresholds
      if (sum(!is.na(threshvec))==1){
        threshstart<-threshvec[!is.na(threshvec)]+0.5*((nthresh+2)/(nthresh))*(c(1:nthresh)-c(1:nthresh)[!is.na(threshvec)])[is.na(threshvec)]
      } else {
        # when there are two or more need to interpolate between given thresholds and extrapolate on the extremes
        tempthreshvec<-threshvec
        restrictabove<-c(sapply(c(1:(nthresh-1)),function(x){!all(is.na(threshvec[(x+1):nthresh]))}),FALSE)
        restrictbelow<-c(FALSE,sapply(c(2:nthresh),function(x){!all(is.na(threshvec[1:x-1]))}))
        tointerpol<-restrictabove & restrictbelow & is.na(threshvec)
        if (sum(tointerpol)>0){
          ranges<-sapply(c(1:nthresh)[tointerpol],function(x){c(max(threshvec[1:(x-1)],na.rm = TRUE),min(threshvec[(x+1):nthresh],na.rm=TRUE))})
          # need to fill in values between extremes
          if (sum(tointerpol)==1){ # list the extremes between points to interpolate
            interpoints<-ranges
          } else {interpoints<-unique(t(ranges))}
          counts<-apply(interpoints,2,function(x){sum(apply(ranges,2,function(y){all(y==x)}))})
          values<-do.call(c,lapply(c(1:ncol(interpoints)),function(x){seq(from=interpoints[1,x],to=interpoints[2,x],length.out = counts[x]+2)[c(-1,-(counts[x]+2))]}))
          tempthreshvec[tointerpol]<-values
        } else {values<-numeric()}
        # having filled in intermediate points can extrapolate out linearly for extremes
        toextrapbelow<-!restrictbelow & is.na(threshvec)
        bpoints<-sum(toextrapbelow)
        if (bpoints>0){
          bvalues<-tempthreshvec[bpoints+1]-sapply(c(bpoints:1),function(x){x*(tempthreshvec[bpoints+2]-tempthreshvec[bpoints+1])})
        } else {bvalues<-numeric()}
        toextrapabove<-!restrictabove & is.na(threshvec)
        apoints<-sum(toextrapabove)
        if (apoints>0){
          avalues<-tempthreshvec[nthresh-apoints]+sapply(c(1:apoints),function(x){x*(tempthreshvec[nthresh-apoints]-tempthreshvec[nthresh-1-apoints])})
        } else {avalues<-numeric()}
        threshstart<-c(bvalues,values,avalues)
      }
    }
    
  }
  return(c(meanstart,varstart,threshstart))
}

calcBHHHmatrix<-function(Env){
  with(Env,{
    BHHHmatrix<-crossprod(scorevecs)
    return(BHHHmatrix)
  })
}
