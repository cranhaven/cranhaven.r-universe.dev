##
##  scaleboot: R package for multiscale bootstrap
##  Copyright (C) 2006-2008 Hidetoshi Shimodaira
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
######################################################################
### MAIN: SUMMARY


##
## extracting elements
##

sbexttab <- function(x,i) {
  list(character=x$character[i,], head=x$head, value=x$value[i,])
}

"[.summary.scalebootv" <- function(x, i, ...)
  structure(NextMethod("["),
            pvalues = attr(x,"pvalues"),
            spvalues = attr(x,"spvalues"),
            lambda = attr(x,"lambda"),
            table = sbexttab(attr(x,"table"),i)
            )
##
## scaleboot
##

summary.scaleboot <- function(object,models=names(object$fi),
                              k=3,sk=k,s=1,sp=-1,
                              hypothesis=c("auto","null","alternative"),
                              type=c("Frequentist","Bayesian"),...) {
## note: ... is not passed to any further, but only to avoid error
##                 "S3 generic/method consistency ... WARNING"

  ## option
  op <- sboptions()

  ## object type
  class(object) <- c("summary.scaleboot",class(object))

  ## p-values
  pvnames <- paste("k",k,sep=".")
  spvnames <- paste("sk",sk,sep=".")
  if(!is.numeric(type)) {
    type <- match.arg(type)
    lambda <- switch(type, Bayesian=0, Frequentist=1)
  } else lambda <- type

  ## save parameters for extrapolation
  object$parex <- list(k=k,sk=sk,s=s,sp=sp,lambda=lambda)
  names(object$parex$k) <- pvnames; names(object$parex$sk) <- spvnames

  ## models
  if(is.numeric(models)) models <- names(object$fi)[models]
  
  ## restrict models
  a <- match(models,names(object$fi))
  a <- a[!is.na(a)]
  if(length(a)>0) object$fi <- object$fi[a] else object$fi <- NULL
  models <- names(object$fi)

  ## no model fitting
  if(is.null(object$fi)) {
    pv <- rep(object$raw$pv,length(pvnames))
    pe <- rep(object$raw$pe,length(pvnames))
    names(pv) <- names(pe) <- pvnames
    spv <- rep(object$raw$pv,length(spvnames))
    spe <- rep(object$raw$pe,length(spvnames))
    names(spv) <- names(spe) <- spvnames
    object$best <- list(model="raw",aic=0,pv=pv,pe=pe,spv=spv,spe=spe,betapar=NULL)
    object$average <- list(model="raw",w=structure(1,names="raw"),pv=pv,pe=pe,spv=spv,spe=spe,betapar=NULL)
    object$hypothesis <- ""
    return(object)
  }

  ## corrected p-values and (beta0,beta1)
  pv <- pe <- matrix(NA,length(models),length(k),
                     dimnames=list(models,pvnames))
#  dv <- de <- structure(rep(0,length(models)),names=models) # beta0 (value and sd)
  betapar <- vector("list",length(models)); names(betapar) <- models  # c(beta0,beta1) and its var matrix
  for(i in seq(along=models)) {
    m <- models[[i]]
    f <- object$fi[[m]]
    psi <- sbpsiget(f$model)
    for(j in seq(along=k)) {
      y <- sbpv1(f,psi,k=k[[j]],s=s,sp=sp,lambda=lambda)
      pv[i,j] <- y$pv
      pe[i,j] <- y$pe
    }
    betapar[[i]] <- sbbetapar1(f,psi,s=s,lambda=lambda)
#    dv[i] <- f$par[1]*f$mag[1]
#    de[i] <- sqrt(f$var[1,1])*f$mag[1]
  }
  object$pv <- pv # p-value
  object$pe <- pe # (sd)
#  object$dv <- dv # beta0
#  object$de <- de # (sd)
  object$betapar <- betapar # beta0, beta1

  ## chisq p-value (experimental, unsupported)
  if(!is.null(f <- object$fi$sphe.3)) {
    y <- sbpv1(f,sbpsiget(f$model),k=0,s=s,sp=sp)
    object$chisq <- y
  }

  ## find the best model
  aic <- sapply(object$fi,"[[","aic")
  aic0 <- min(aic)
  ibest <- which(aic==aic0)[1]
  model <- models[[ibest]]
  pvbest <- pv[ibest,]; pebest <- pe[ibest,]
  names(pvbest) <- names(pebest) <- pvnames
#  object$best <- list(model=model,aic=aic0,pv=pvbest,pe=pebest,dv=dv[[i]],de=de[[i]])
  object$best <- list(model=model,aic=aic0,pv=pvbest,pe=pebest,betapar=betapar[[ibest]])

  ## average by akaike weights
  w <- exp(-(aic-aic0)/2) # akaike weights
  w <- w/sum(w)
  u <- w>op$th.aicw # ignore small values
  w <- w[u]/sum(w[u])
  pvave <- apply(w*pv[u,,drop=F],2,sum)
  peave <- apply(w*pe[u,,drop=F],2,sum)
#  dvave <- sum(w*dv[u])
#  deave <- sum(w*de[u])
  betaparave <- list(par=apply(w*t(sapply(betapar[u],"[[","par")),2,sum),
    var=matrix(apply(w*t(sapply(betapar[u],"[[","var")),2,sum),2,2))
#  object$average <- list(w=w,pv=pvave,pe=peave,dv=dvave,de=deave)
  object$average <- list(w=w,pv=pvave,pe=peave,betapar=betaparave)

  ##########################################
  ### selective inference
  ###
  ## determine if the region is hypothesis or alternative
  hypothesis <- match.arg(hypothesis)
  if(hypothesis == "auto") {
    if(object$average$betapar$par[1]>0) hypothesis <- "null" else hypothesis <- "alternative"
  }
  object$hypothesis <- hypothesis

  ## selective p-values
  spv <- spe <- matrix(NA,length(models),length(sk),dimnames=list(models,spvnames))
  for(i in seq(along=models)) {
    m <- models[[i]]
    f <- object$fi[[m]]
    psi <- sbpsiget(f$model)
    for(j in seq(along=sk)) {
      y <- sbspv1(f,psi,k=sk[[j]],s=s,sp=sp,hypothesis=hypothesis,lambda=lambda)
      spv[i,j] <- y$spv
      spe[i,j] <- y$spe
    }
  }
  object$spv <- spv # p-value
  object$spe <- spe # (sd)

  spvbest <- spv[ibest,]; spebest <- spe[ibest,]
  names(spvbest) <- names(spebest) <- spvnames
  object$best$spv = spvbest;  object$best$spe = spebest

  spvave <- apply(w*spv[u,,drop=F],2,sum)
  speave <- apply(w*spe[u,,drop=F],2,sum)
  object$average$spv=spvave;  object$average$spe=speave

  object
}

### corrected p-value 1
## fit : output of optims (includes var, mag)
## psi : function(beta,s,k)
## k : degree for corrected p-value (default: k=1)
## s : sigma^2 for corrected p-value (default: s=1)
## sp : sigma^2 for prediction (default: sp=-1)
## lambda : mixing bayes (lambda=0) and freq (lambda=1)

sbpv1 <- function(fit,psi,k=1,s=1,sp=-1,lambda=0) {
  pval <- function(par) pnorm(-psi(fit$mag*par,s,k=k,sp=sp,lambda=lambda))
  pv <- pval(fit$par)
  h <- nderiv(pval,fit$par)
  pe <- sqrtx(h %*% fit$var %*% h)
  list(pv=pv,pe=pe)
}

### selective inference p-value 1
## hypothesis: "null" or "alternative"
sbspv1 <- function(fit,psi,k=1,s=1,sp=-1,hypothesis="null",lambda=0) {
  pval <- function(par) {
    z0 = psi(fit$mag*par,s,k=k,sp=0,lambda=lambda)
    z1 = psi(fit$mag*par,s,k=k,sp=sp,lambda=lambda)
    if(hypothesis=="null") {
      if(z0>=0) pnorm(-z1)/pnorm(-z1+z0)
      else 1.0
    } else {
      if(z0<=0) 1 - pnorm(z1)/pnorm(z1-z0)
      else 0.0
    }
  }
  pv <- pval(fit$par)
  if(0<pv && pv<1) {
    h <- nderiv(pval,fit$par)
    pe <- sqrtx(h %*% fit$var %*% h)
  } else pe <- 0.0
  list(spv=pv,spe=pe)
}

### estimating beta0, beta1 by fitting linear model to the estimated model
##
##  By taylor expansion of psi at s for getting the linear model:
##  psi(sigma2) = beta0 + beta1 * sigma2
##
sbbetapar1 <- function(fit,psi,s=1,lambda=0) {
  fbeta <- function(par) {
      beta0 <- psi(fit$mag*par,s,k=2,sp=0,lambda=lambda)
      beta1 <- psi(fit$mag*par,s,k=2,sp=1,lambda=lambda) - beta0
      beta <- c(beta0,beta1); names(beta) <- c("beta0", "beta1")
      beta
  }
  beta <- fbeta(fit$par)
  h <- nderiv(fbeta,fit$par)
  var <- h %*% fit$var %*% t(h)
  list(par=beta,var=var)
}

sbgetbetapar1 <- function(x) {
  if(is.null(x)){
    sd <- beta <- c(NA,NA) ; names(beta) <- c("beta0","beta1")
  } else {
    beta <- x$par
    sd <- sqrt(diag(x$var))
  }
  list(beta=beta,sd=sd)
}

## print
print.summary.scaleboot <- function(x,sort.by=c("aic","none"),verbose=FALSE,...) {
  ## verbose
  if(verbose) print.scaleboot(x,sort.by=sort.by,...)
  
  ### raw
  a <- catpval(x$raw$pv,x$raw$pe)
  if(is.null(x$raw$s)) x$raw$s <- NA
  cat("\nRaw Bootstrap Probability (scale=",round(x$raw$s,3),
      ") : ",a$value,"\n",sep="")

  ## in case no fitting
  if(is.null(x$fi)) {
    cat("\nNo Model Fitting\n")
    return(invisible(x))
  }

  ## chisq
  if(!is.null(x$chisq)) {
    a <- catpval(x$chisq$pv,x$chisq$pe)
    cat("\nChisquare P-value: ",a$value)
    f <- x$fi$sphe.3
    p <- parsphere(f$par*f$mag)
    cat(" ; v=",format(p[1],digits=3),
        ", a=",format(p[2],digits=3),
        ", nu=",format(p[3],digits=3),"\n",sep="")
  }

  ## corrected p-values (for models, and also for the best and average)
  pvs <- rbind(x$best$pv,x$average$pv,x$pv)
  pes <- rbind(x$best$pe,x$average$pe,x$pe)
  rownames(pvs)[1:2] <- rownames(pes)[1:2] <- c("best","average")

  ## selective inference
  spvs <- rbind(x$best$spv,x$average$spv,x$spv)
  spes <- rbind(x$best$spe,x$average$spe,x$spe)
  rownames(spvs)[1:2] <- rownames(spes)[1:2] <- c("best","average")

  ## beta0, beta1
  betabest <- sbgetbetapar1(x$best$betapar)
  betaaverage <- sbgetbetapar1(x$average$betapar)
  betaall <- c(list(best=betabest,average=betaaverage),lapply(x$betapar, sbgetbetapar1))
#  dvs <- c(x$best$dv,x$average$dv,x$dv)
#  des <- c(x$best$de,x$average$de,x$de)
#  names(dvs)[1:2] <- names(des)[1:2] <- c("best","average")

  ## prepare table
  pval <- matrix("",nrow(pvs),ncol(pvs))
  dimnames(pval) <- dimnames(pvs)
  for(i in seq(length=ncol(pvs))) {
    a <- catpval(pvs[,i],pes[,i],lambda=x$parex$lambda)
    pval[,i] <- a$value
  }
  spval <- matrix("",nrow(spvs),ncol(spvs))
  dimnames(spval) <- dimnames(spvs)
  for(i in seq(length=ncol(spvs))) {
    a <- catpval(spvs[,i],spes[,i],lambda=x$parex$lambda)
    spval[,i] <- a$value
  }
  
  cat("\nHypothesis:",x$hypothesis,"\n")
  cat("\nCorrected P-values for Models (",a$name,",",a$lambda,"):\n",sep="")
  pvalbest <- pval[1:2,,drop=F] # for best and average
  pval <- pval[-(1:2),,drop=F] # for models
  spvalbest <- spval[1:2,,drop=F] # for best and average
  spval <- spval[-(1:2),,drop=F] # for models

  ## beta0, beta1 table
#  beta0 <- myformat(c(pi,dvs),c(pi,des),digits=2)[-1]
#  beta0best <- beta0[1:2]
#  beta0 <- beta0[-(1:2)]
  beta <- matrix("",length(betaall), length(betaall[[1]]$beta))
  dimnames(beta) <- list(names(betaall), names(betaall[[1]]$beta))
  for(i in seq(length=length(betaall))) {
    beta[i,] <- myformat(c(pi,betaall[[i]]$beta),c(pi,betaall[[i]]$sd),digits=2)[-1]
  }
  betabest <- beta[1:2,,drop=F]
  beta <- beta[-(1:2),,drop=F]

  ## aic and akaike weights
  aicval <- sbaic(x)
  aic <- myformat(c(pi,aicval),digits=2)[-1]
  weight <- aic; weight[] <- ""
  a <- catpval(x$average$w)$value
  weight[names(a)] <- a

  ## sort
#  tab <- cbind(pval,beta0,aic,weight) # to be catmat
  tab <- cbind(pval,spval,beta,aic,weight) # to be catmat
  sort.by <- match.arg(sort.by)
  j <- switch(sort.by,
              none=1:length(aicval),
              aic=order(aicval))
  tabj <- tab[j,]

  ## print the table
  catmat(tabj)

  ## best model
  cat("\nBest Model: ",x$best$model,"\n")

  ## the bottom line
  cat("\nCorrected P-values by the Best Model and by Akaike Weights Averaging:\n")
#  beta0 <- beta0best
#  catmat(cbind(pvalbest,beta0))
  tabbest <- cbind(pvalbest, spvalbest, betabest)
  catmat(tabbest)
  cat("\n")

  invisible(list(table.sort = tabj, table.best = tabbest))
}


##
## scalebootv
##

summary.scalebootv <- function(object,models=attr(object,"models"),k=3,sk=k,
                               hypothesis="auto",type="Frequentist",
                               select="average",...) {
  for(i in seq(along=object)) object[[i]] <- summary(object[[i]],models,k=k,sk=sk,hypothesis=hypothesis,type=type,...)
  class(object) <- c("summary.scalebootv",class(object))
  attr(object,"models") <- models
  attr(object,"pvalues") <- paste("k",k,sep=".")
  attr(object,"spvalues") <- paste("sk",k,sep=".")
  attr(object,"lambda") <- object[[1]]$parex$lambda
  a <- formatting.summary.scalebootv(object,select=select) 
  attr(object,"table") <- a
  object
}


selectpv <- function(x,select) {
  models <- attr(x,"models")
  select <- match.arg(select,c("best","average",models))
  if(select=="best") {
    pvpe <- lapply(x,"[[","best")
    model <- format(sapply(pvpe,"[[","model"))
    aic <- format(round(sapply(pvpe,"[[","aic"),digits=2))
    outaic <- cbind(model,aic)
    selna <- "the Best Model"
  } else if(select=="average") {
    pvpe <- lapply(x,function(s)
       list(model=s$best$model,weight=s$average$w[s$best$model],
            pv=s$average$pv,pe=s$average$pe,spv=s$average$spv,spe=s$average$spe,
            betapar=s$average$betapar))
    model <- format(sapply(pvpe,"[[","model"))
    weight <- catpval(sapply(pvpe,"[[","weight"))$value
    outaic <- cbind(model,weight)
    selna <- "Akaike Weights Averaging"
  } else {
    pvpe <- lapply(x, function(s)
      if(!is.null(s$fi)) list(model=select,aic=s$fi[[select]]$aic,
                              pv=s$pv[select,],pe=s$pe[select,],
                              spv=s$spv[select,],spe=s$spe[select,])
      else s$best)
    model <- format(sapply(pvpe,"[[","model"))
    aic <- format(round(sapply(pvpe,"[[","aic"),digits=2))
    outaic <- cbind(model,aic)
    selna <- select
  }
  
  list(pvpe=pvpe,select=select,name=selna,outaic=outaic)
}

print.summary.scalebootv <- function(x,...) {
  tab = attr(x,"table")
  cat("\n",tab$head,"\n",sep="")
  catmat(tab$character)
  invisible(x)
}

### internal:  prepare pvalue table (in values and character)
formatting.summary.scalebootv <- function(x,select="average") {
  ## extract information
  pvalues <- attr(x,"pvalues")
  spvalues <- attr(x,"spvalues")
  lambda <- attr(x,"lambda")
  raws <- lapply(x,"[[","raw")

  ## prepare table containers for p-values
  out <- matrix("",length(x),1+length(pvalues)+length(spvalues)+3,
              dimnames=list(names(x),c("raw",pvalues,spvalues,"beta0","beta1","hypothesis")))

  ## numerical values for sorting
  outval <- matrix(0,length(x),1+length(pvalues)+length(spvalues)+2,
                   dimnames=list(names(x),c("raw",pvalues,spvalues,"beta0","beta1")))

  ## which p-values to be printed?
  selpv <- selectpv(x,select)
  out <- cbind(out,selpv$outaic)

  ## fill-in values
  pv <- sapply(raws,"[[","pv")
  pe <- sapply(raws,"[[","pe")
  out[,"raw"] <- catpval(pv,pe)$value
  outval[,"raw"] <- pv
  for(p in pvalues) {
    pv <- sapply(selpv$pvpe,function(b) b$pv[[p]])
    pe <- sapply(selpv$pvpe,function(b) b$pe[[p]])
    a <- catpval(pv,pe,lambda=lambda)
    out[,p] <- a$value
    outval[,p] <- pv
  }
  for(p in spvalues) {
    spv <- sapply(selpv$pvpe,function(b) b$spv[[p]])
    spe <- sapply(selpv$pvpe,function(b) b$spe[[p]])
    a <- catpval(spv,spe,lambda=lambda)
    out[,p] <- a$value
    outval[,p] <- spv
  }
  out[,"hypothesis"] <- sapply(x,"[[","hypothesis")

#  dv <- sapply(selpv$pvpe,"[[","dv")
#  de <- sapply(selpv$pvpe,"[[","de")
#  de[de>10] <- NA
#  out[,"beta0"] <- myformat(c(pi,dv),c(pi,de),digits=2)[-1]
  beta <- lapply(selpv$pvpe,function(a) sbgetbetapar1(a$betapar))
  betav <- sapply(beta,"[[","beta")
  betae <- sapply(beta,"[[","sd")
  outval[,"beta0"] <- betav[1,]
  outval[,"beta1"] <- betav[2,]
  out[,"beta0"] <- myformat(c(pi,betav[1,]),c(pi,betae[1,]),digits=2)[-1]
  out[,"beta1"] <- myformat(c(pi,betav[2,]),c(pi,betae[2,]),digits=2)[-1]

  ## sort and print
  head=paste("Corrected P-values by ",selpv$name," (",a$name,",",a$lambda,"):",sep="")

  #  invisible(x)
  list(character=out, head=head, value=outval)
}

#######
##
## extract p-values & (beta0,beta1)

## general
sbpval <- function(x,...) UseMethod("sbpval")

## scaleboot
sbpval.summary.scaleboot <- function(x,
                                     select=c("average","best","all"),...) {
  select <- match.arg(select)
  y <- switch(select,
              average=x$average,
              best=x$best,
              all=x)
  pv <- y$pv
  pe <- y$pe
  spv <- y$spv
  spe <- y$spe
  if(select=="all") {
    pvalue <- cbind(pv,spv)
    sd <- cbind(pe,spe)
    xx <- lapply(y$betapar,sbgetbetapar1)
    beta=list(beta=t(sapply(xx,"[[","beta")),sd=t(sapply(xx,"[[","sd")))
  } else {
    pvalue <- c(pv,spv)
    sd <- c(pe,spe)
    beta <- sbgetbetapar1(y$betapar)
  }
  list(pvalue=pvalue,pvalue.sd=sd,hypothesis=x$hypothesis,beta=beta$beta,beta.sd=beta$sd)
}

## scalebootv
sbpval.summary.scalebootv <- function(x,...) {
  y <- lapply(x,sbpval,...)
  pvalue <- sapply(y,"[[","pvalue")
  pvalue.sd <- sapply(y,"[[","pvalue.sd")
  hypothesis <- sapply(y,"[[","hypothesis")
  beta <- sapply(y,"[[","beta")
  beta.sd <- sapply(y,"[[","beta.sd")
  list(pvalue=pvalue,pvalue.sd=pvalue.sd,hypothesis=hypothesis,beta=beta,beta.sd=beta.sd)
}
