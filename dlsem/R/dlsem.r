defpar <- par()[setdiff(names(par()),c("cin","cra","csi","cxy","din","page"))]

#########################################
#
###  Available constrained lag shapes ###
#
# "ecq": endpoint-constrained quadratic
#  "qd": quadratic decreasing
#  "ld": linearly decreasing
# "gam": gamma
#
#########################################


# lag weights (internal use only)
lagwei <- function(theta,lag,type) {
  res <- c()
  xa <- 0  #####  
  for(i in 1:length(lag)) {
    if(type=="ecq") {
      if(lag[i]<theta[1] | lag[i]>theta[2]) {
        res[i] <- 0
        } else {
        res[i] <- -4/(theta[2]-theta[1]+2)^2*(lag[i]^2-(theta[1]+theta[2])*lag[i]+(theta[1]-1)*(theta[2]+1))
        }
      } else if(type=="qd") {
      if(lag[i]<theta[1] | lag[i]>theta[2]) {
        res[i] <- 0
        } else {
        res[i] <- (lag[i]^2-2*(theta[2]+1)*lag[i]+(theta[2]+1)^2)/(theta[2]-theta[1]+1)^2
        }
      } else if(type=="ld") {
      if(lag[i]<theta[1] | lag[i]>theta[2]) {
        res[i] <- 0
        } else {
        res[i] <- (theta[2]+1-lag[i])/(theta[2]+1-theta[1])
        }
      } else if(type %in% c(4,"gam")) {
      if(lag[i]>=xa) {
        bnum <- (lag[i]-xa+1)^(theta[1]/(1-theta[1]))*theta[2]^(lag[i]-xa)
        xM <- (theta[1]/(theta[1]-1))/log(theta[2])+xa-1
        bden <- (xM-xa+1)^(theta[1]/(1-theta[1]))*theta[2]^(xM-xa)
        res[i] <- bnum/bden
        } else {
        res[i] <- 0   
        }
      }
    }
  names(res) <- lag
  if(type %in% c(4,"gam")) {
    if(sum(res,na.rm=T)==0) res[1] <- 1
    }
  res
  }

# generate lagged instances of a variable (internal use only)
genLag <- function(x,maxlag,past=F) {
  if(maxlag>0) {
    if(past==T) x[which(is.na(x))] <- mean(x,na.rm=T)
    out <- x
    for(w in 1:maxlag) {
      if(w<length(x)) {
        if(past==F) {
          wx <- c(rep(NA,w),x[1:(length(x)-w)])
          } else {
          wx <- c(rep(mean(x,na.rm=T),w),x[1:(length(x)-w)])       
          }
        } else {
        if(past==F) {
          wx <- rep(NA,length(x))
          } else {
          wx <- rep(mean(x,na.rm=T),length(x))          
          }
        }
      out <- cbind(out,wx)        
      }
    colnames(out) <- NULL
    out
    } else {
    x
    }
  }

# distributed-lag transformation (internal use only)
Zmat <- function(x,type,theta,nlag) {
  if(type=="none") {
    matrix(x,ncol=1)
    } else {
    if(type=="gam") {
      xa <- 0  #####
      bhat <- gamlead(theta[1],theta[2])
      if(is.null(nlag)) {
        laglim <- min(bhat,trunc(2/3*length(x)))
        } else {
        laglim <- min(nlag,trunc(2/3*length(x)))
        }
      } else {
      if(is.null(nlag)) {
        laglim <- min(theta[2],trunc(2/3*length(x)))
        } else {
        laglim <- min(nlag,trunc(2/3*length(x)))
        }
      }
    H <- lagwei(theta,0:laglim,type)
    as.numeric(genLag(x,laglim)%*%matrix(H,ncol=1))
    }
  }

# compute the lag limit (internal use only)
findLagLim <- function(data,group=NULL) {
  if(is.null(group)) {
    trunc(nrow(na.omit(data))*2/3)
    } else {
    auxlaglim <- c()
    gruppi <- levels(factor(data[,group]))
    for(i in 1:length(gruppi)) {
      auxlaglim[i] <- nrow(na.omit(data[which(data[,group]==gruppi[i]),]))
      }
    trunc(min(auxlaglim,na.rm=T)*2/3)
    }
  }

# ECQ constructor
ecq <- function(x,a,b,x.group=NULL,nlag=NULL) {
  if(missing(a)==F & (length(a)!=1 || !is.numeric(a) || a<0 || a!=round(a))) stop("Argument 'a' must be a non-negative integer value",call.=F)
  if(missing(b)==F & (length(b)!=1 || !is.numeric(b) || b<0 || b!=round(b))) stop("Argument 'b' must be a non-negative integer value",call.=F)
  if(a>b) stop("Argument 'a' must be no greater than argument 'b'",call.=F)
  if(!is.null(nlag) & (length(nlag)!=1 || !is.numeric(nlag) || nlag<0 || nlag!=round(nlag))) stop("Argument 'nlag' must be a non-negative integer value",call.=F)
  if(is.null(x.group)) {
    res <- Zmat(x,"ecq",c(a,b),nlag)
    } else {
    res <- c()
    gruppi <- levels(factor(x.group))
    for(i in 1:length(gruppi)) {
      auxind <- which(x.group==gruppi[i])
      ires <- Zmat(x[auxind],"ecq",c(a,b),nlag)
      res[auxind] <- ires
      }
    }
  res
  }

# QD constructor
qd <- function(x,a,b,x.group=NULL,nlag=NULL) {
  if(missing(a)==F & (length(a)!=1 || !is.numeric(a) || a<0 || a!=round(a))) stop("Argument 'a' must be a non-negative integer value",call.=F)
  if(missing(b)==F & (length(b)!=1 || !is.numeric(b) || b<0 || b!=round(b))) stop("Argument 'b' must be a non-negative integer value",call.=F)
  if(a>b) stop("Argument 'a' must be no greater than argument 'b'",call.=F)
  if(!is.null(nlag) & (length(nlag)!=1 || !is.numeric(nlag) || nlag<0 || nlag!=round(nlag))) stop("Argument 'nlag' must be a non-negative integer value",call.=F)
  if(is.null(x.group)) {
    res <- Zmat(x,"qd",c(a,b),nlag)
    } else {
    res <- c()  
    gruppi <- levels(factor(x.group))
    for(i in 1:length(gruppi)) {
      auxind <- which(x.group==gruppi[i])
      ires <- Zmat(x[auxind],"qd",c(a,b),nlag)
      res[auxind] <- ires
      }
    }
  res
  }

# LD constructor
ld <- function(x,a,b,x.group=NULL,nlag=NULL) {
  if(missing(a)==F & (length(a)!=1 || !is.numeric(a) || a<0 || a!=round(a))) stop("Argument 'a' must be a non-negative integer value",call.=F)
  if(missing(b)==F & (length(b)!=1 || !is.numeric(b) || b<0 || b!=round(b))) stop("Argument 'b' must be a non-negative integer value",call.=F)
  if(a>b) stop("Argument 'a' must be no greater than argument 'b'",call.=F)
  if(!is.null(nlag) & (length(nlag)!=1 || !is.numeric(nlag) || nlag<0 || nlag!=round(nlag))) stop("Argument 'nlag' must be a non-negative integer value",call.=F)
  if(is.null(x.group)) {
    res <- Zmat(x,"ld",c(a,b),nlag)
    } else {
    res <- c()  
    gruppi <- levels(factor(x.group))
    for(i in 1:length(gruppi)) {
      auxind <- which(x.group==gruppi[i])
      ires <- Zmat(x[auxind],"ld",c(a,b),nlag)
      res[auxind] <- ires
      }
    }
  res
  }

# GAM constructor
gam <- function(x,a,b,x.group=NULL,nlag=NULL) {
  if(missing(a)==F & (length(a)!=1 || !is.numeric(a) || a<=0 || a>=1)) stop("Argument 'a' must be a value in the interval (0,1)",call.=F)
  if(missing(b)==F & (length(b)!=1 || !is.numeric(b) || b<=0 || b>=1)) stop("Argument 'b' must be a value in the interval (0,1)",call.=F)
  if(!is.null(nlag) & (length(nlag)!=1 || !is.numeric(nlag) || nlag<0 || nlag!=round(nlag))) stop("Argument 'nlag' must be a non-negative integer value",call.=F)
  if(is.null(x.group)) {
    res <- Zmat(x,"gam",c(a,b),nlag)
    } else {
    res <- c()
    gruppi <- levels(factor(x.group))
    for(i in 1:length(gruppi)) {
      auxind <- which(x.group==gruppi[i])
      ires <- Zmat(x[auxind],"gam",c(a,b),nlag)
      res[auxind] <- ires
      }
    }
  res
  }

# unconstrained constructor (internal use only)
uncons <- function(x,a,b,x.group=NULL) {
  if(missing(a)==F & (length(a)!=1 || !is.numeric(a) || a<0 || a!=round(a))) stop("Argument 'a' must be a non-negative integer value",call.=F)
  if(missing(b)==F & (length(b)!=1 || !is.numeric(b) || b<0 || b!=round(b))) stop("Argument 'b' must be a non-negative integer value",call.=F)
  if(a>b) stop("Argument 'a' must be no greater than argument 'b'",call.=F)
  if(is.null(x.group)) {
    if(b>0) {
      res <- x
      for(i in 1:b) {
        res <- cbind(res,c(rep(NA,i),x[1:(length(x)-i)]))
        }
      colnames(res) <- 0:b
      } else {
      res <- x
      }
    } else {
    res <- c()  
    gruppi <- levels(factor(x.group))
    for(i in 1:length(gruppi)) {
      auxind <- which(x.group==gruppi[i])
      ires <- idat <- x[auxind]
      for(j in 1:b) {
        ires <- cbind(ires,c(rep(NA,j),idat[1:(length(auxind)-j)]))
        }
      res <- rbind(res,ires)
      }
    colnames(res) <- 0:b
    }
  res[,(a+1):(b+1)]
  }

# check if a variable is quantitative (internal use only)
isQuant <- function(x) {
  if(is.numeric(x)) {
    if(identical(sort(unique(na.omit(x))),c(0,1))) F else T
    } else {
    F
    }
  }

# get info within brackets (internal use only)
getWBrk <- function(x) {
  regmatches(x, gregexpr("(?<=\\().*?(?=\\))",x,perl=T))[[1]]
  #xstr <- strsplit(x,"\\(")[[1]]
  #if(length(xstr)==2) {
  #  gsub("\\)","",xstr[2])
  #  } else {
  #  paste(paste(gsub("\\)","",xstr[-1]),collapse="("),paste(rep(")",length(xstr)-2),collapse=""),sep="")
  #  }
  }

# scan formula (internal use only)
scanForm <- function(x,warn=F) {
  auxform <- gsub(" ","",formula(x))[-1]
  ynam <- gsub(" ","",auxform[1])
  auX <- gsub(" ","",strsplit(gsub("-","+",auxform[2]),"\\+")[[1]])
  auX <- setdiff(auX,c("1",""))
  if(length(auX)>0) {
    lnames <- ltype <- rep(NA,length(auX))
    lpar <- list()
    for(i in 1:length(auX)) {        
      if(nchar(auX[i])>0) {
        # check deprecated constructors  <-------------------- !!
        if(identical("quec.lag(",substr(auX[i],1,9))) {
          auX[i] <- gsub("^quec.lag\\(","ecq\\(",auX[i])
          if(warn==T) warning("The constructor 'quec.lag()' is deprecated, 'ecq' was used instead",call.=F)
          }
        if(identical("qdec.lag(",substr(auX[i],1,9))) {
          auX[i] <- gsub("^qdec.lag\\(","qd\\(",auX[i])
          if(warn==T) warning("The constructor 'qdec.lag()' is deprecated, 'qd' was used instead",call.=F)
          }
        if(identical("gamma.lag(",substr(auX[i],1,10))) {
          auX[i] <- gsub("^gamma.lag\\(","gam\\(",auX[i])
          if(warn==T) warning("The constructor 'gamma.lag()' is deprecated, 'gam' was used instead",call.=F)
          }
        if(identical("gamm.lag(",substr(auX[i],1,9))) {
          auX[i] <- gsub("^gamm.lag\\(","gam\\(",auX[i])
          if(warn==T) warning("The constructor 'gamm.lag()' is deprecated, 'gam' was used instead",call.=F)
          }
        # find parameters in lag shape constructors
        if(strsplit(auX[i],"\\(")[[1]][1] %in% c("ecq","qd","ld","gam")) {
          istr <- strsplit(getWBrk(auX[i]),",")[[1]]
          ltype[i] <- strsplit(auX[i],"\\(")[[1]][1]
          lnames[i] <- istr[1]
          #auX[i] <- istr[1]
          lpar[[i]] <- as.numeric(istr[2:3])
          } else {
          lnames[i] <- auX[i]
          ltype[i] <- "none"
          lpar[[i]] <- NA
          }
        }
      }
    names(lpar) <- names(ltype) <- lnames
    } else {
    lpar <- ltype <- c()
    }
  list(y=ynam,X=auX,ltype=ltype,lpar=lpar)
  }

# lead lag of a gamma lag shape (internal use only)
gamlead <- function(delta,lambda,tol=1e-4) {
  m <- ceiling(delta/((delta-1)*log(lambda)))
  res <- m
  b <- lagwei(c(delta,lambda),res,"gam")
  ind <- 1
  while(b>tol && ind<10*m) {
    res <- res+1
    b <- lagwei(c(delta,lambda),res,"gam")
    ind <- ind+1
    }
  res
  }

# default gamma lag shape (internal use only)
gamdefault <- function(maxlag) {
  G <- matrix(
  c(0.115, 0.005,
    0.05, 0.02,
    0.46, 0.04,
    0.42, 0.08,
    0.18, 0.13,
    0.32, 0.17,
    0.35, 0.21,
    0.35, 0.25,
    0.47, 0.28,
    0.51, 0.31,
    0.38, 0.35,
    0.47, 0.37,
    0.48, 0.40,
    0.48, 0.42,
    0.43, 0.45,
    0.53, 0.46,
    0.51, 0.48),byrow=T,ncol=2)
  if(maxlag<18) {
    G[max(1,maxlag),]
    } else {
    c(0.5,0.5)
    }
  }

# generate gamma parameters (internal use only)
gammaParGen <- function(by) {
  xseq <- seq(0+by,1-by,by=by)
  auxmat <- as.matrix(expand.grid(xseq,xseq))
  limmat <- c()
  for(i in 1:nrow(auxmat)) {
    limmat <- c(limmat,gamlead(auxmat[i,1],auxmat[i,2]))
    }
  res <- cbind(auxmat,limmat)
  colnames(res) <- c("delta","lambda","lead_lag")
  res
  }

# generate search grid (internal use only)
searchGrid <- function(mings,maxgs,minwd,maxld,lag.type,gammaMat) {
  if(lag.type=="gam") {
    gammaMat[which(gammaMat[,3]<=maxld & gammaMat[,3]>=minwd),1:2]
    } else {
    auxmat <- c()
    for(i in 0:maxld) {
      for(j in i:maxld) { 
        if(i>=mings & i<=maxgs & j-i>=minwd) auxmat <- rbind(auxmat,c(i,j))
        }   
      }
    auxmat
    }
  }

# create lm formula (internal use only)
creatForm <- function(y,X,group,type,theta,nlag) {
  xnam <- c()
  if(length(X)>0) {
    nomi <- setdiff(names(theta),group)
    for(i in 1:length(nomi)) {
      ilab <- nomi[i]
      if(type[ilab]=="none") {
        xnam[i] <- ilab
        } else {
        ixnam <- paste(type[ilab],"(",ilab,",",theta[[ilab]][1],",",theta[[ilab]][2],sep="")
        if(!is.null(group)) {
          if(!is.null(nlag)) {
            ixnam <- paste(ixnam,",",group,",",nlag,sep="")
            } else {
            ixnam <- paste(ixnam,",",group,sep="")  
            }
          } else {
          if(!is.null(nlag)) ixnam <- paste(ixnam,",,",nlag,sep="")
          }
        xnam[i] <- paste(ixnam,")",sep="")
        }
      }
    }
  if(is.null(group)) {
    if(length(X)>0) {
      res <- paste(y,"~",paste(xnam,collapse="+"),sep="")    
      } else {
      res <- paste(y,"~1",sep="")
      }
    } else {
    if(length(X)>0) {
      res <- paste(y,"~-1+",group,"+",paste(xnam,collapse="+"),sep="")    
      } else {
      res <- paste(y,"~-1+",group,sep="")
      }
    }
  formula(res)
  }

# extract the name from a lag shape (internal use only)
extrName <- function(x) {
  auxstr <- strsplit(x,"\\(")[[1]]
  if(auxstr[1] %in% c("ecq","qd","ld","gam")) {
    gsub("\\)","",strsplit(auxstr[2],",")[[1]][1])
    } else {
    x  
    }
  }

# get levels of variables in data (internal use only)
getLev <- function(data) {
  auxq <- sapply(data,isQuant)
  res <- list()
  for(i in 1:length(auxq)) {
    if(auxq[i]==T) {
      res[[i]] <- NA
      } else {
      res[[i]] <- paste(colnames(data)[i],levels(factor(data[,i])),sep="")
      }
    }
  names(res) <- colnames(data)
  res
  }

# deseasonalization (internal use only)
deSeas <- function(x,seas,group,data) {
  res <- data
  if(is.null(group)) {
    xseas <- factor(rep(1:seas,length=nrow(data)))
    if(nlevels(xseas)>=2 && min(table(xseas))>=2) {
      for(i in 1:length(x)) {
        iform <- paste(x[i],"~xseas",sep="")
        imod <- lm(formula(iform),data=data)
        res[,x[i]] <- mean(data[,x[i]],na.rm=T)+residuals(imod)
        }
      }
    } else {
    gruppi <- levels(factor(data[,group]))
    for(w in 1:length(gruppi)) {
      auxind <- which(data[,group]==gruppi[w])
      xseas <- factor(rep(1:seas,length=length(auxind)))
      if(nlevels(xseas)>=2 && min(table(xseas))>=2) {
        for(i in 1:length(x)) {
          iform <- paste(x[i],"~xseas",sep="")
          imod <- lm(formula(iform),data=data[auxind,])
          res[auxind,x[i]] <- mean(data[auxind,x[i]],na.rm=T)+residuals(imod)
          }
        }
      }
    }
  res
  }

# perform ols estimation (internal use only)
doLS <- function(formula,group,data) {
  formOK <- formula
  Xm0 <- model.matrix(formOK,data=data)
  auxdel <- names(which(apply(Xm0,2,var)==0))
  if(length(auxdel)>0) {
    auxlev <- getLev(data)
    x2del <- c()
    for(i in 1:length(auxdel)) {
      if(auxdel[i] %in% colnames(data)) {
        x2del <- c(x2del,auxdel[i])
        } else {
        auxf <- sapply(auxlev,function(z){auxdel[i] %in% z})
        x2del <- c(x2del,names(which(auxf==T)))
        }
      }
    x2del <- setdiff(x2del,group)
    if(length(x2del)>0) {
      auxform <- scanForm(formula)
      if(is.null(group)) auxsep <- "" else auxsep <- "-1+"
      formOK <- formula(paste(auxform$y,"~",auxsep,paste(setdiff(auxform$X,x2del),collapse="+"),sep=""))
      }
    }
  res <- lm(formOK,data=data)
  res$call$formula <- formOK
  res
  }

# fit a distributed-lag linear regression (internal use only)
dlaglm <- function(formula,group,data,adapt,no.select,min.gestation,max.gestation,min.width,max.lead,sign,ndiff,gamma.by,mess,nblank) {
  auxscan <- scanForm(formula)
  y <- auxscan$y
  if(length(auxscan$X)>0) {
    lagPar <- auxscan$lpar
    lagType <- auxscan$ltype
    lagNam <- names(lagPar)
    no.lag <- names(which(lagType=="none"))
    xOK <- c(no.select,no.lag)
    xtest <- setdiff(lagNam,xOK)
    if(length(xtest)==0) adapt <- F
    if(adapt==F) {
      warn1 <- warn2 <- 0
      for(i in 1:length(lagPar)) {
        ilimit <- findLagLim(data[,c(y,lagNam[i],group)],group=group)
        if(lagType[i] %in% c("ecq","qd","ld")) {
          if(lagPar[[i]][1]!=round(lagPar[[i]][1])) {
            lagPar[[i]][1] <- round(lagPar[[i]][1])
            warn1 <- warn1+1
            }
          if(lagPar[[i]][1]<0) {
            lagPar[[i]][1] <- 0
            warn1 <- warn1+1
            }
          if(lagPar[[i]][2]!=round(lagPar[[i]][2])) {
            lagPar[[i]][2] <- round(lagPar[[i]][2])
            warn1 <- warn1+1
            }
          if(lagPar[[i]][2]<0) {
            lagPar[[i]][2] <- 0
            warn1 <- warn1+1
            }
          if(lagPar[[i]][1]>lagPar[[i]][2]) {
            lagPar[[i]][2] <- lagPar[[i]][1]
            warn1 <- warn1+1
            }
          if(lagPar[[i]][1]>ilimit) {
            lagPar[[i]][1] <- ilimit
            warn2 <- warn2+1
            }
          if(lagPar[[i]][2]>ilimit) {
            lagPar[[i]][2] <- ilimit
            warn2 <- warn2+1
            }
          } else if(lagType[i]=="gam") {
          if(lagPar[[i]][1]<=0) {
            lagPar[[i]][1] <- 0.01
            warn1 <- warn1+1
            }
          if(lagPar[[i]][1]>=1) {
            lagPar[[i]][1] <- 0.99
            warn1 <- warn1+1
            }
          if(lagPar[[i]][2]<=0) {
            lagPar[[i]][2] <- 0.01
            warn1 <- warn1+1
            }
          if(lagPar[[i]][2]>=1) {
            lagPar[[i]][2] <- 0.99
            warn1 <- warn1+1
            }
          iglim <- gamlead(lagPar[[i]][1],lagPar[[i]][2])
          if(iglim>ilimit) {
            lagPar[[i]] <- gamdefault(ilimit)
            warn2 <- warn2+1
            }
          }
        }
      if(warn1>0) warning("Invalid lag shapes in the regression of '",y,"' replaced with the nearest valid ones",call.=F)
      if(warn2>0) warning("Too large lead lags in the regression of '",y,"' replaced with the maximum possible ones",call.=F)
      if(!is.null(mess)) {
        iblchar <- ""
        if(nblank>0) iblchar <- paste(rep(" ",nblank),collapse="")
        cat('\r')
        cat(paste(mess,iblchar,sep=""))
        flush.console()
        }
      formOK <- creatForm(y,names(lagPar),group,lagType,lagPar,NULL)
      modOK <- doLS(formula=formOK,group=group,data=data)
      } else {
      if(sum(lagType=="gam")>0) {
        gammaMat <- gammaParGen(gamma.by)
        } else {
        gammaMat <- NULL  
        }
      bestPar <- vector("list",length=length(lagNam))
      names(bestPar) <- lagNam
      consList <- list()
      for(i in 1:length(xtest)) {
        imings <- min.gestation[xtest[i]]
        imaxgs <- max.gestation[xtest[i]]
        iminwd <- min.width[xtest[i]]
        imaxld <- max.lead[xtest[i]]
        icons <- searchGrid(imings,imaxgs,iminwd,imaxld,lagType[xtest[i]],gammaMat)
        if(lagType[[xtest[i]]]=="gam") {
          igamdef <- gamdefault(imaxld)
          if(nrow(icons)==0) icons <- igamdef
          bestPar[[xtest[i]]] <- igamdef
          } else {
          bestPar[[xtest[i]]] <- c(imings,imaxld)
          }
        consList[[i]] <- icons
        }
      names(consList) <- xtest
      nittVet <- sapply(consList,nrow)
      nittTot <- 0
      for(i in 1:length(nittVet)) {
        nittTot <- nittTot+sum(nittVet[i:length(nittVet)])
        }
      fine <- nitt <- 0
      while(fine==0) {
        xtest <- setdiff(lagNam,xOK)
        ntest <- length(xtest)
        if(ntest>0) {
          currentBIC <- rep(NA,ntest) 
          currentPar <- vector("list",length=length(xtest))
          names(currentBIC) <- names(currentPar) <- xtest
          for(i in 1:ntest) {
            auxcons <- consList[[xtest[i]]]
            if(nrow(auxcons)==0) auxcons <- matrix(lagPar[[xtest[i]]],nrow=1)
            mse0 <- bhat0 <- c()
            for(j in 1:nrow(auxcons)) {
              nitt <- nitt+1
              iperc <- round(100*nitt/nittTot)
              if(!is.null(mess) && iperc%%5==0) {
                iblchar <- ""
                if(nblank>0) iblchar <- paste(rep(" ",nblank),collapse="")
                imess <- paste(mess," ... ",iperc,"%",iblchar,sep="")
                cat('\r')
                cat(imess)
                flush.console()
                }
              #
              testType <- lagType
              #testType[setdiff(xtest,xtest[i])] <- "none"  #####
              testPar <- bestPar
              testPar[[xtest[i]]] <- auxcons[j,]
              #
              form0 <- creatForm(y,names(testPar),group,testType,testPar,max.lead[xtest[i]])  #####
              #form0 <- creatForm(y,names(testPar),group,testType,testPar,NULL)
              #
              mod0 <- doLS(formula=form0,group=group,data=data)
              est0 <- mod0$coefficients
              ixall <- names(est0)
              iauxlab <- sapply(ixall,extrName)
              ixlab <- ixall[which(iauxlab==xtest[i])]
              #
              if(length(ixlab)>0 && ixlab %in% ixall) { 
                bhat0[j] <- est0[ixlab]
                mse0[j] <- mean(residuals(mod0)^2)
                } else {
                bhat0[j] <- NA
                mse0[j] <- Inf
                }
              }
            isign <- sign[xtest[i]]
            #if(!is.null(isign)) {
            if(isign!=F) {
              if(isign=="+") {
                auxsign <- which(bhat0>0)
                } else {
                auxsign <- which(bhat0<0)                  
                }
              if(length(auxsign)>0) {
                auxbest <- auxsign[which.min(mse0[auxsign])]
                } else {
                auxbest <- which.min(mse0)
                }
              } else {
              auxbest <- which.min(mse0)
              }
            currentPar[[xtest[i]]] <- auxcons[auxbest,]
            currentBIC[xtest[i]] <- mse0[auxbest]
            }
          xnew <- names(currentBIC)[which.min(currentBIC)]
          bestPar[[xnew]] <- currentPar[[xnew]]
          xOK <- c(xOK,xnew)
          } else {
          fine <- 1
          }
        }
      formOK <- creatForm(y,names(bestPar),group,lagType,bestPar,NULL)  #####
      modOK <- doLS(formula=formOK,group=group,data=data)
      }
    } else {
    if(is.null(group)) {
      formOK <- formula(paste(y,"~1",sep=""))
      } else {
      formOK <- formula(paste(y,"~-1+",group,sep=""))        
      }
    modOK <- doLS(formula=formOK,group=group,data=data)
    }
  modOK
  }

# HAC weights (internal use only)
W_hac <- function(Xmat,res,maxlag) {
  n <- nrow(Xmat)
  p <- ncol(Xmat)
  W <- matrix(0,nrow=p,ncol=p)
  for(i in 1:n) {
    W <- W+res[i]^2*Xmat[i,]%*%t(Xmat[i,])
    }
  if(maxlag>0) {
    for(j in 1:maxlag) {
      wi <- 0
      for(i in (j+1):n) {
        wi <- wi+res[i]*res[i-j]*(Xmat[i,]%*%t(Xmat[i-j,])+Xmat[i-j,]%*%t(Xmat[i,]))
        }
      W <- W+(1-j/(1+maxlag))*wi
      }
    }
  W
  }

# apply HAC covariance matrix to a lm object
lmHAC <- function(x,group=NULL) {
  if(("lm" %in% class(x))==F & ("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'lm' or 'dlsem'",call.=F)
  if("lm" %in% class(x)) {
    x$vcov <- doHAC(x=x,group=group)
    class(x) <- c("hac","lm")
    } else {
    vcovL <- lapply(x$estimate,doHAC,group=group)
    for(i in 1:length(x$estimate)) {
      x$estimate[[i]] <- vcovL[[i]]
      class(x$estimate[[i]]) <- c("hac","lm")
      }
    }
  x
  }

# HAC for class 'lm' (internal use only)
doHAC <- function(x,group) {
  Xmat <- model.matrix(x)
  Smat <- summary(x)$cov.unscaled
  Xmat <- Xmat[,colnames(Smat),drop=F]  ## delete collinear terms
  n <- nrow(Xmat)
  p <- ncol(Xmat)
  res <- x$residuals
  if(!is.null(group) && is.na(group)) group <- NULL
  if(is.null(group)) {
    maxlag <- ar(res)$order
    W <- W_hac(Xmat,res,maxlag)
    } else {
    glev <- x$xlevels[[group]]
    gnam <- paste(group,glev,sep="")
    Wsum <- matrix(0,nrow=ncol(Xmat),ncol=ncol(Xmat))
    W <- matrix(0,nrow=p,ncol=p)
    maxlag <- c()
    for(i in 1:length(gnam)) {
      if(gnam[i] %in% colnames(Xmat)) {
        iind <- names(which(Xmat[,gnam[i]]==1))
        } else {
        iind <- names(which(apply(Xmat[,gnam[setdiff(1:length(gnam),i)]],1,sum)==0))
        }
      maxlag[i] <- ar(res[iind])$order
      W <- W+W_hac(Xmat[iind,],res[iind],maxlag[i])
      }
    names(maxlag) <- gnam
    }
  out <- n/(n-p)*Smat%*%W%*%Smat
  attr(out,"max.lag") <- maxlag
  out
  }

# vcov method for class 'hac'
vcov.hac <- function(object,...)  {
  object$vcov
  }

# summary method for class 'hac'
summary.hac <- function(object,...)  {
  res <- summary.lm(object)
  res$coefficients[colnames(object$vcov),2] <- sqrt(diag(object$vcov))
  res$coefficients[,3] <- res$coefficients[,1]/res$coefficients[,2]
  res$coefficients[,4] <- 2*pt(-abs(res$coefficients[,3]),object$df.residual)
  res
  }

# confint method for class 'hac'
confint.hac <- function(object,parm,level=0.95,...) {
  summ <- summary(object)$coefficients
  quan <- qt((1+level)/2,object$df.residual)
  res <- cbind(summ[,1]-quan*summ[,2],summ[,1]+quan*summ[,2])
  colnames(res) <- paste(100*c(1-level,1+level)/2," %",sep="")
  res
  }
  
# compute lag effects of a covariate (internal use only)
lagEff <- function(model,x,cumul,lag) {
  formstr <- strsplit(gsub(" ","",as.character(model$call$formula)[3]),"\\+")[[1]]
  xall <- names(model$coefficients)
  auxlab <- sapply(xall,extrName)
  xlab <- xall[which(auxlab==x)]
  auxscan <- scanForm(model$call)
  if(auxscan$ltype[x] %in% c("ecq","qd","ld")) {
    sx <- auxscan$lpar[[x]][1]
    dx <- auxscan$lpar[[x]][2]
    imu <- model$coefficients[xlab]
    icov <- vcov(model)[xlab,xlab]
    if(is.null(lag)) {
      xgrid <- 0:dx
      } else {
      xgrid <- lag
      }
    iH <- matrix(lagwei(c(sx,dx),xgrid,auxscan$ltype[x]),ncol=1)
    } else if(auxscan$ltype[x]=="gam") {
    delta <- auxscan$lpar[[x]][1]
    lambda <- auxscan$lpar[[x]][2]
    ilim <- c(0,gamlead(delta,lambda))  #####
    imu <- model$coefficients[xlab]
    icov <- vcov(model)[xlab,xlab]
    if(is.null(lag)) {
      xa <- 0  #####
      xgrid <- 0:ilim[2]
      } else {
      xgrid <- lag
      }
    iH <- matrix(lagwei(c(delta,lambda),xgrid,"gam"),ncol=1)
    idel <- setdiff(xgrid,ilim[1]:ilim[2])
    if(length(idel)>0) iH[sapply(idel,function(z){which(xgrid==z)}),] <- 0
    } else {  
    xgrid <- 0  
    imu <- model$coefficients[x]
    icov <- vcov(model)[x,x]
    iH <- matrix(1,nrow=1,ncol=1)
    }
  ibhat <- iH%*%imu
  ibse <- sqrt(diag(iH%*%icov%*%t(iH)))
  #
  out <- cbind(ibhat,ibse)
  rownames(out) <- xgrid
  colnames(out) <- c("estimate","std. err.")
  if(cumul==T) out <- cumulCalc(out)
  out
  }

# apply differentiation (internal use only)
applyDiff <- function(x,group,data,k) {
  #
  diffFun <- function(z,k) {
    if(k>0 & k<length(z)) {
      res <- z
      for(i in 1:k) {
        res <- res-c(NA,res[1:(length(res)-1)])
        }
      res
      } else if(k<=0) {
      z
      } else {
      rep(NA,length(z))
      }
    }
  #
  diffdat <- data
  if(is.null(group)) {
    for(w in 1:length(x)) {
      if(isQuant(data[,x[w]])) {
        wdat <- data[,x[w]]
        diffdat[,x[w]] <- diffFun(wdat,k[w])      
        }
      }
    } else {
    data[,group] <- factor(data[,group])
    gruppi <- levels(factor(data[,group]))
    for(i in 1:length(gruppi)) {
      auxind <- which(data[,group]==gruppi[i])
      for(w in 1:length(x)) {
        if(isQuant(data[,x[w]])) {
          wdat <- data[auxind,x[w]]
          diffdat[auxind,x[w]] <- diffFun(wdat,k[w])
          }
        }
      } 
    }
  diffdat
  }

# check the time variable (internal use only)
isTimeVar <- function(x) {
  #
  standarDates <- function(string) {
    patterns = c('[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]','[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]','[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
    formatdates = c('%Y/%m/%d','%d/%m/%Y','%Y-%m-%d')
    standardformat='%d/%m/%Y'
    for(i in 1:3){
      if(grepl(patterns[i], string)){
        aux=as.Date(string,format=formatdates[i])
        if(!is.na(aux)){
          format(aux, standardformat)
          }
        }
      }
    F
    }
  #
  res <- T
  if(!is.numeric(x)) {
    if(F %in% sapply(x,standarDates)) res <- F
    }
  res
  }

# unit root test
unirootTest <- function(x=NULL,group=NULL,time=NULL,data,test=NULL,log=FALSE) {
  if(missing(data)==F & !identical(class(data),"data.frame")) stop("Argument 'data' must be a data.frame",call.=F)
  if(!is.null(x)) { 
    x2del <- c()
    for(i in 1:length(x)) {
      if((x[i] %in% colnames(data))==F) {
        warning("Variable '",x[i],"' not found in data and ignored",call.=F)
        x2del <- c(x2del,x[i])
        } else {
        if(isQuant(data[,x[i]])==F) {
          warning("'",x[i],"' is not a quantitative variable and was ignored",call.=F)
          x2del <- c(x2del,x[i])
          }
        }
      }
    x <- setdiff(x,x2del)
    if(length(x)<1) stop("No quantitative variables provided to argument 'x'",call.=F)
    } else {
    allnam <- setdiff(colnames(data),c(group,time))
    for(i in 1:length(allnam)) {
      if(isQuant(data[,allnam[i]])) x <- c(x,allnam[i])
      }
    }
  if(!is.null(group)) {
    if(is.na(group)) group <- NULL
    if(length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
    if((group %in% colnames(data))==F) stop("Variable '",group,"' provided to argument 'group' not found in data",call.=F)
    if(group %in% x) stop("Variable '",group,"' is provided to both arguments 'x' and 'group'",call.=F)
    if(group %in% time) stop("Variable '",group,"' is provided to both arguments 'group' and 'time'",call.=F)
    data[,group] <- factor(data[,group])
    gruppi <- levels(data[,group])
    if(length(gruppi)<2) stop("The group factor must have at least 2 unique values",call.=F)
    n <- min(table(data[,group]))
    if(n<3) stop("There must be at least 3 observations per group",call.=F)
    } else {
    n <- nrow(data)
    if(n<3) stop("There must be at least 3 observations",call.=F)  
    }
  if(!is.null(time)) {
    if(is.na(time)) time <- NULL
    if(length(time)!=1) stop("Argument 'time' must be of length 1",call.=F)
    if((time %in% colnames(data))==F) stop("Variable '",time,"' provided to argument 'time' not found in data",call.=F)
    if(time %in% x) stop("Variable '",time,"' is provided to both arguments 'x' and 'time'",call.=F)
    if(time %in% group) stop("Variable '",time,"' is provided to both arguments 'group' and 'time'",call.=F)
    if(isTimeVar(data[,time])==F) stop("The time variable is neither numeric nor a date",call.=F)
    if(is.null(group)) {
      if(sum(duplicated(data[,time]))>0) stop("The time variable has duplicated values",call.=F)
      } else {
      timesplit <- split(data[,time],data[,group])
      if(sum(sapply(timesplit,function(z){sum(duplicated(z))}))>0) stop("The time variable has duplicated values",call.=F)  
      }
    }
  if(is.null(test)) {
    test <- ifelse(n<100,"kpss","adf")
    } else {
    if((test %in% c("kpss","adf"))==F) stop("Argument 'test' must be one among 'kpss' or 'adf'",call.=F)  
    }
  if(identical(log,T)) {
    for(i in 1:length(x)) {
      if(sum(data[,x[i]]<=0,na.rm=T)>0) {
        warning("Logarithmic transformation not applied to variable '",x[i],"'",call.=F)
        } else {
        data[,x[i]] <- log(data[,x[i]])
        }
      }
    } else if(!identical(log,F)) {
    for(i in 1:length(log)) {
      if((log[i] %in% colnames(data))==F) {
        warning("Variable '",log[i],"' provided to argument 'log' not found in data",call.=F)
        } else if(log[i] %in% c(group,time)) {
        warning("Logarithmic transformation not applied to variable '",log[i],"'",call.=F)
        } else {
        if(sum(data[,log[i]]<=0,na.rm=T)>0) {
          warning("Logarithmic transformation not applied to variable '",log[i],"'",call.=F)
          } else {
          data[,log[i]] <- log(data[,log[i]])  
          }
        }
      }
    }
  urtFun(x,group,time,data,test,log)
  }
  
# interface for unit root test (internal use only)
urtFun <- function(x,group,time,data,test,log) {
  if(!is.null(group)) {
    g.id <- as.numeric(data[,group])
    glab <- levels(data[,group])
    } else {
    g.id <- glab <- rep(1,nrow(data))
    }
  data[which(abs(data[,x])==Inf),x] <- NA
  if(!is.null(time)) {
    for(i in 1:length(g.id)) {
      auxind <- which(g.id==glab[i])
      idat <- data[auxind,]
      data[auxind,] <- idat[order(idat[,time]),]
      }
    }
  res <- vector("list",length=length(x))
  for(i in 1:length(x)) {
    if(test=="adf") {
      if(is.null(group)) {
        ikmax <- trunc((length(na.omit(data[,x[i]]))-1)^(1/3))
        } else {
        ik <- table(na.omit(data[,c(x[i],group)])[,group])
        ikmax <- sapply(ik,function(z){trunc((z-1)^(1/3))})
        }
      res[[i]] <- doURT(data[,x[i]],g.id=g.id,test="adf",glab=glab,par=ikmax)
      } else {
      res[[i]] <- doURT(data[,x[i]],g.id=g.id,test="kpss",glab=glab,par=c(F,T))  #####
      }
    }
  names(res) <- x
  if(!is.null(group)) attr(res,"group") <- group
  attr(res,"test") <- test
  class(res) <- "unirootTest"
  res
  }

# adf test (internal use only)
adft <- function(x,kmax) {
  #
  doADF <- function(k) {
    y <- diff(x)
    n <- length(y)
    k <- k+1
    z <- embed(y,k)
    yt <- z[,1]
    xt1 <- x[k:n]
    tt <- k:n
    if(k>1) {
      yt1 <- z[,2:k,drop=F]
      res <- lm(yt~xt1+tt+yt1)
      } else {
      res <- lm(yt~xt1+tt)
      }
    res.sum <- summary(res)$coefficients
    if(nrow(res.sum)>=2) {
      STAT <- res.sum[2,1]/res.sum[2,2]
      table <- -1*cbind(c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96),
                        c(3.95, 3.8, 3.73, 3.69, 3.68, 3.66),
                        c(3.6, 3.5, 3.45, 3.43, 3.42, 3.41),
                        c(3.24, 3.18, 3.15, 3.13, 3.13, 3.12),
                        c(1.14, 1.19, 1.22, 1.23, 1.24, 1.25),
                        c(0.8, 0.87, 0.9, 0.92, 0.93, 0.94),
                        c(0.5, 0.58, 0.62, 0.64, 0.65, 0.66),
                        c(0.15, 0.24, 0.28, 0.31, 0.32, 0.33))
      tablen <- dim(table)[2]
      tableT <- c(25, 50, 100, 250, 500, 1e+05)
      tablep <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
      tableipl <- numeric(tablen)
      for(i in (1:tablen)) {
        tableipl[i] <- approx(tableT,table[,i],n,rule=2)$y
        }
      PVAL <- approx(tableipl,tablep,STAT,rule=2)$y
      } else {
      STAT <- PVAL <- NA
      }
    c(STAT,PVAL)
    }
  #
  k <- kmax
  res <- doADF(k)
  while(is.na(res[1])||(abs(res[1])>1.6 & k>0)) {
    k <- k-1
    res <- doADF(k)
    }
  list(statistic=res[1],lag.order=k,p.value=res[2])
  }

# kpss test (internal use only)
kpsst <- function (x,trend,lshort) {
  n <- length(x)
  if(trend==T) {
    t <- 1:n
    e <- residuals(lm(x ~ t))
    table <- c(0.216, 0.176, 0.146, 0.119)
    } else {
    e <- residuals(lm(x ~ 1))
    table <- c(0.739, 0.574, 0.463, 0.347)
    }
  tablep <- c(0.01, 0.025, 0.05, 0.1)
  s <- cumsum(e)
  eta <- sum(s^2)/(n^2)
  s2 <- sum(e^2)/n
  if(lshort==T) {
    l <- trunc(4*(n/100)^0.25)
    } else {
    l <- trunc(12*(n/100)^0.25)
    }
  k <- 0
  for(i in 1:l) {
    ik <- 0
    for(j in (i+1):n) {
      ik <- ik+e[j]*e[j-i]
      }
    k <- k+(1-i/(l+1))*ik
    }
  STAT <- eta/(s2+2*k/n)
  PVAL <- approx(table,tablep,STAT,rule=2)$y
  list(statistic=STAT,lag.order=l,p.value=PVAL)
  }

# function for uniroot test (internal use only)  
doURT <- function(x,g.id,test,glab,par) {
  gruppi <- sort(unique(g.id))
  auxord <- auxstat <- auxp <- nwm <- c()
  h0 <- ifelse(test=="adf","unit root","stationarity")
  auxwarn <- options()$warn
  options(warn=-1)
  for(i in 1:length(gruppi)) {
    auxind <- which(g.id==gruppi[i])
    auxdat <- na.omit(splRecons(x[auxind]))
    nwm[i] <- length(auxdat)
    if(length(auxdat)>4 && var(auxdat)>0) {
      if(test=="adf") {
        auxurt <- adft(auxdat,kmax=par[i])
        } else {
        auxurt <- kpsst(auxdat,trend=par[1],lshort=par[2])  
        }
      auxstat[i] <- auxurt$statistic
      auxp[i] <- auxurt$p.value
      auxord[i] <- auxurt$lag.order
      } else {
      auxstat[i] <- auxp[i] <- auxord[i] <- NA
      }
    }
  if(length(auxstat)>1) names(auxstat) <- names(nwm) <- names(auxord) <- glab
  auxp[which(auxp>1)] <- 1
  auxp[which(auxp<0)] <- 0
  if(length(auxp)==1) {
    res <- list(statistic=auxstat,lag.order=auxord,n=nwm,null=h0,z.value=auxstat,p.value=auxp)
    } else {
    m <- length(auxp)
    logp <- qnorm(auxp)
    rhat <- 1-var(logp)
    rstar <- max(rhat,-1/(m-1))
    auxz <- sum(logp)/sqrt(m*(1+(m-1)*(rstar+0.2*sqrt(2/(m+1))*(1-rstar))))
    #auxz <- sum(logp)/sqrt(m)
    auxpval <- 2*pnorm(-abs(auxz))
    res <- list(statistic=auxstat,lag.order=auxord,n=nwm,null=h0,z.value=auxz,p.value=auxpval)
    }
  options(warn=auxwarn)
  res
  }

# print method for class 'unirootTest'
print.unirootTest <- function(x,...) {
  if(attr(x,"test")=="adf") {
    cat("ADF test (null hypothesis is unit root)","\n")
    } else {
    cat("KPSS test (null hypothesis is stationarity)","\n")
    }
  res <- sapply(x,function(z){z$p.value})
  print(round(res,4))
  }

# estimate parameters of the imputation model (internal use only)
impuFit <- function(xcont,xqual,group,data) {
  res <- G <- list()
  logL <- 0
  z.names <- c(group,xqual)
  for(i in 1:length(xcont)) {
    if(i==1) {
      if(is.null(z.names)) ipar <- "1" else ipar <- z.names  
      } else {
      ipar <- c(xcont[1:(i-1)],z.names)        
      }
    iform <- formula(paste(xcont[i],"~",paste(ipar,collapse="+"),sep=""))
    imod <- lm(iform,data=data)
    imod$call$formula <- iform
    logL <- logL-0.5*AIC(imod,k=0)
    res[[i]] <- imod
    G[[i]] <- ipar
    }
  names(res) <- names(G) <- xcont
  list(hat=res,G=G,logL=logL)
  }

# predict missing values from the imputation model (internal use only)
impuPred <- function(mod,data) {
  est <- mod$hat
  G <- mod$G
  res <- data
  auxwarn <- options()$warn
  options(warn=-1)
  for(i in 1:length(est)) {
    iest <- est[[i]]
    inam <- names(est)[i]
    ipar <- G[[inam]]
    ina <- which(is.na(data[,inam]))
    if(length(ina)>0) {res[ina,inam] <- predict(iest,res[ina,ipar,drop=F])}
    }
  options(warn=auxwarn)
  res
  }

# linear interpolation (internal use only)
linImp <- function(x) {
  res <- x
  auxNA <- which(is.na(x))
  if(length(auxNA)>0) {
    naL <- split(auxNA,cumsum(c(1,diff(auxNA)!=1)))
    for(i in 1:length(naL)) {
      ina <- naL[[i]]
      x1 <- min(ina)-1
      x2 <- max(ina)+1
      y1 <- x[x1]
      y2 <- x[x2]
      b <- (y2-y1)/(x2-x1)
      a <- y1-b*x1
      res[ina] <- a+b*ina
      }
    }
  res
  }

# spline reconstruction (internal use only)
splRecons <- function(x) {
  res <- x
  auxNA <- which(is.na(x))
  if(length(auxNA)>0&length(auxNA)<length(x)) {
    auxO <- which(!is.na(x))
    auxI <- intersect(auxNA,min(auxO):max(auxO))
    yI <- spline(1:length(x),x,xout=1:length(x))
    res[auxI] <- yI$y[auxI]
    }
  res
  }

# lag order determination (internal use only)
arFind <- function(x,group,data) {
  if(!is.null(group)) {
    data[,group] <- factor(data[,group])
    gruppi <- levels(data[,group])    
    res <- rep(0,length(x))
    for(w in 1:length(gruppi)) {
      auxind <- which(data[,group]==gruppi[w])
      for(i in 1:length(x)) {
        ix <- na.omit(splRecons(data[auxind,x[i]]))
        if(var(ix)>0) res[i] <- ar(na.omit(ix))$order
        }
      }    
    } else {
    res <- c()
    for(i in 1:length(x)) {
      ix <- na.omit(splRecons(data[,x[i]]))
      if(var(ix)>0) res[i] <- ar(na.omit(ix))$order
      }
    }
  res
  }

# add lagged instances (internal use only)
addLags <- function(x,group=NULL,data,k=0) {
  if(k>0) {
    if(is.null(group)) {
      for(i in 1:length(x)) {
        for(j in 1:k) {
          data[,paste(x[i],"_lag",j,sep="")] <- c(rep(NA,j),data[1:(nrow(data)-j),x[i]])
          }
        }
      } else {
      gruppi <- levels(factor(data[,group]))
      for(w in 1:length(gruppi)) {
        auxind <- which(data[,group]==gruppi[w])
        for(i in 1:length(x)) {
          for(j in 1:k) {
            data[auxind,paste(x[i],"_lag",j,sep="")] <- c(rep(NA,j),data[1:(length(auxind)-j),x[i]])
            }
          }
        }      
      }
    }
  data
  }

# imputation of missing data (internal use only)
EM.imputation <- function(xcont,xqual,group,data,tol,maxiter,quiet=F) {
  nmiss <- apply(data[,xcont],2,function(v){sum(is.na(v))})
  xcont <- xcont[order(nmiss)]
  currentDat <- data
  for(i in 1:length(xcont)) {
    currentDat[which(is.na(currentDat[,xcont[i]])),xcont[i]] <- mean(data[,xcont[i]],na.rm=T)
    }
  currentFit <- impuFit(xcont=xcont,xqual=xqual,group=group,data=currentDat)
  currentLik <- -Inf
  fine <- forcend <- 0
  count <- 1
  if(quiet==F) {
    cat("Starting EM...")
    flush.console() 
    }
  while(fine==0) {
    newDat <- impuPred(currentFit,data)
    newFit <- impuFit(xcont=xcont,xqual=xqual,group=group,data=newDat)
    newLik <- newFit$logL
    if(quiet==F) {
      cat('\r',"EM iteration ",count,". Log-likelihood: ",newLik,sep="")
      flush.console() 
      }
    if(newLik<currentLik) {
      newLik <- currentLik
      newDat <- currentDat
      #warning("Forced stop of EM algorithm because likelihood has decreased",call.=F)
      fine <- 1
      } else {
      if(newLik-currentLik>tol & count<maxiter) {
        currentFit <- newFit
        currentLik <- newLik
        currentDat <- newDat
        count <- count+1
        } else {
        fine <- 1
        if(count>=maxiter) forcend <- 1
        }
      }
    }
  if(quiet==F) {
    if(forcend==0) {
      cat('\r',"EM converged after ",count," iterations. Log-likelihood: ",newLik,sep="","\n")
      } else {
      cat('\r',"EM stopped after ",maxiter," iterations. Log-likelihood: ",newLik,sep="","\n")      
      }
    }
  newDat
  }

# function to plot a lag shape (internal use only)
makeShape <- function(bmat,maxlag,cumul,bcum,conf,ylim,title) {
  if(!is.null(maxlag)) {
    maxlag <- maxlag-1  #####
    ymlag <- max(as.numeric(rownames(bmat)))
    if(maxlag>=ymlag) {
      if(cumul==F) {
        addmat <- matrix(0,nrow=maxlag-ymlag+1,ncol=3)
        } else {
        addmat <- matrix(bmat[nrow(bmat),],nrow=maxlag-ymlag+1,ncol=3,byrow=T)
        }
      bmat <- rbind(bmat,addmat)
      rownames(bmat) <- -1:(maxlag+1)
      } else {                
      bmat <- bmat[1:(which(rownames(bmat)==as.character(maxlag))+1),]
      }
    auxNZ <- which(bmat[,1]!=0)
    } else {
    auxNZ <- which(bmat[,1]!=0)
    if(cumul==F) {
      if(nrow(bmat)>max(auxNZ)+1) {
        bmat <- bmat[1:(max(auxNZ)+1),]
        }
      } else {                          
      auxCm <- min(intersect(which(diff(bmat[,1])==0)+1,auxNZ))
      if(nrow(bmat)>auxCm) {
        bmat <- bmat[1:auxCm,]
        }
      auxNZ <- which(bmat[,1]!=0)
      }
    }
  xaux <- as.numeric(rownames(bmat))  #####
  xaux <- c(xaux,max(xaux)+1)
  #
  if(is.null(ylim)) {
    upLim <- 1.05*max(bmat)
    lowLim <- 1.05*min(bmat)
    upLim <- max(abs(c(upLim,lowLim)))
    lowLim <- -max(abs(c(upLim,lowLim)))
    } else {
    lowLim <- ylim[1]
    upLim <- ylim[2]
    }
  auxs <- which(bmat[,1]!=0)
  bmat_s <- bmat[c(max(1,min(auxs)-1):min(nrow(bmat),max(auxs)+1)),]
  bval <- as.numeric(rownames(bmat_s))
  #
  m0 <- lm(bmat_s[which(bmat_s[,1]!=0),1]~bval[which(bmat_s[,1]!=0)])
  smooth <- ifelse(sum(residuals(m0)^2)<0.001,F,T)
  #
  if(smooth==T) {
    xgrid <- sort(unique(c(bval,seq(min(bval),max(bval),length=100))))
    ygrid <- cbind(spline(bval,bmat_s[,1],xout=xgrid)$y,spline(bval,bmat_s[,2],xout=xgrid)$y,spline(bval,bmat_s[,3],xout=xgrid)$y)
    } else {
    xgrid <- bval
    ygrid <- bmat_s
    }
  #
  auxsgn <- sign(bmat[auxs,1])
  if(sum(auxsgn==-1)==0) {
    auxdel <- which(ygrid[,1]<0)
    } else if(sum(auxsgn==1)==0) {
    auxdel <- which(ygrid[,1]>0)
    } else {
    auxdel <- c()  
    }
  if(length(auxdel)>0) {
    auxInt <- c()
    for(i in 1:length(bval)) {
      auxInt[i] <- which(xgrid==bval[i])
      }
    for(i in 1:length(auxdel)) {
      isx <- auxInt[max(which(auxInt<=auxdel[i]))]
      idx <- auxInt[min(which(auxInt>=auxdel[i]))]
      ygrid[isx:idx,] <- NA
      }
    ygrid[c(1,nrow(ygrid)),] <- 0
    for(j in 1:ncol(ygrid)) {
      ygrid[,j] <- linImp(ygrid[,j])
      }
    }
  #
  plot(0,type="n",xlim=c(min(xaux),max(xaux)),ylim=c(lowLim,upLim),yaxs="i",xaxs="i",cex.lab=1.2,
    lwd=2,xaxt="n",yaxt="n",xlab="Lag",ylab="Coefficient",main=title,cex.main=1.2) 
  if(cumul==T) mtext("cumulative lag shape",cex=0.9)
  polygon(c(xgrid,rev(xgrid)),c(ygrid[,2],rev(ygrid[,3])),border=NA,col="grey80")
  yaxaux <- seq(lowLim,upLim,length=21)
  ylabaux <- signif(yaxaux,3)
  ylabaux[11] <- 0
  xaxaux <- seq(min(xaux),max(xaux))
  auxby <- max(1,round((max(xaux)-min(xaux)+1)/30))
  xlabaux1 <- xlabaux2 <- seq(min(xaux),max(xaux),by=auxby)
  xlabaux2[c(1,length(xlabaux1))] <- NA
  abline(h=yaxaux,v=seq(min(xaux),max(xaux),by=auxby),col="grey75",lty=2)
  abline(h=0,lty=2,col="grey35")                                        
  lines(ygrid[,1]~xgrid,col="grey40",lty=2)
  #
  auxpoi <- which(bmat[,1]!=0)
  auxpoiOK <- max(1,(min(auxpoi)-1)):min(nrow(bmat),(max(auxpoi)+1))
  points(bmat[auxpoiOK,1]~as.numeric(names(bmat[,1]))[auxpoiOK],col="grey35",lty=2,cex=0.6)
  #
  axis(1,at=xlabaux1,labels=xlabaux2,cex.axis=1.1)
  axis(2,at=yaxaux,labels=ylabaux,cex.axis=1.1)
  confLeg <- paste("   ",conf*100,"% CI: (",bcum[2],", ",bcum[3],")",sep="")      
  if(max(bmat[,1])>0) {
    legpos <- "bottomright"
    } else {
    legpos <- "topright"
    }                                       
  est <- bmat[,1]
  if(cumul==T) {
    newest <- est[1]
    for(i in 2:length(est)) {
      newest[i] <- est[i]-est[i-1]
      } 
    est <- newest
    }
  minlag <- min(as.numeric(rownames(bmat)[which(est!=0)]))
  maxlag <- max(as.numeric(rownames(bmat)[which(est!=0)]))             
  legend(legpos,legend=c(paste("Relevant lags: ",minlag," to ",maxlag,sep=""),paste("Cumulative coefficient: ",bcum[1],sep=""),confLeg),bty="n",cex=1.1)
  box()
  }

# estimated lag shapes
lagShapes <- function(x,cumul=FALSE) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(length(cumul)!=1 || !is.logical(cumul)) stop("Argument 'cumul' must be a logical value",call.=F)
  G <- makeGraph(x)$full.graph
  est <- x$estimate
  nomi <- names(x$estimate)
  pset <- getStruct(x)
  res <- vector("list",length=length(nomi))
  names(res) <- nomi
  for(i in 1:length(nomi)) {
    ires <- list()
    ipar <- pset[[nomi[i]]]
    if(length(ipar)>0) {
      for(j in 1:length(ipar)) {
        ijtab <- lagEff(est[[nomi[i]]],x=ipar[j],lag=NULL,cumul=cumul)
        if(cumul==F) {
          ijtabOK <- rbind(ijtab,c(0,0))
          } else {
          ijtabOK <- rbind(ijtab,ijtab[nrow(ijtab),])
          }
        rownames(ijtabOK)[nrow(ijtabOK)] <- nrow(ijtabOK)-1
        ires[[j]] <- ijtabOK
        }
      names(ires) <- ipar
      }
    res[[i]] <- ires
    }
  res
  }

# plot the lag shape associated to an overall causal effect or a path
lagPlot <- function(x,from=NULL,to=NULL,path=NULL,maxlag=NULL,cumul=FALSE,conf=0.95,use.ns=FALSE,ylim=NULL,title=NULL) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(!is.null(maxlag) && (length(maxlag)!=1 || !is.numeric(maxlag) || maxlag<=0 || maxlag!=round(maxlag))) stop("Argument 'maxlag' must be a positive integer number",call.=F)
  if(length(cumul)!=1 || !is.logical(cumul)) stop("Argument 'cumul' must be a logical value",call.=F)
  if(length(conf)!=1 || !is.numeric(conf) || conf<=0 || conf>=1) stop("Argument 'conf' must be a real number in the interval (0,1)",call.=F)
  if(length(use.ns)!=1 || !is.logical(use.ns)) stop("Argument 'use.ns' must be a logical value",call.=F)
  if(!is.null(ylim) && (length(ylim)!=2 || ylim[1]>=ylim[2])) stop("Invalid argument 'ylim'",call.=F)
  #
  if(!is.null(from) && !is.na(from) && (is.null(to) & is.null(path))) {
    path <- from
    auxstr <- strsplit(path,"\\*")[[1]]
    if(length(auxstr)<2) {
      stop("Argument 'to' is missing",call.=F)
      } else {  
      from <- NULL
      }
    } else {
    if(is.null(from)||is.null(to)) {
      auxstr <- strsplit(path,"\\*")[[1]]
      if(length(auxstr)<2) stop("Invalid path length",call.=F)
      from <- to <- NULL
      } else {
      path <- NULL
      }
    }
  #
  if(is.null(path) && (is.null(from) || is.na(from))) stop("Argument 'from' is missing",call.=F)
  if(is.null(path) && (is.null(to) || is.na(to))) stop("Argument 'to' is missing",call.=F)
  xedgF <- edgeMat(x,conf=conf,full=T)
  xedg <- edgeMat(x,conf=conf,full=F)
  if(is.null(path)) {                        
    auxpa <- causalEff(x,from=from,to=to,lag=NULL,cumul=cumul,conf=conf,use.ns=use.ns)$overall
    } else {
    path <- gsub(" ","",path)
    auxstr <- strsplit(path,"\\*")[[1]]
    pathchk <- setdiff(auxstr,names(x$estimate))
    if(length(pathchk)>0) stop("Unknown variable '",pathchk[1],"' in the path",call.=F)
    from <- auxstr[1]
    to <- rev(auxstr)[1] 
    isIn <- isInF <- 1
    for(i in 2:length(auxstr)) {
      isIn <- isIn*length(which(xedg[,1]==auxstr[i-1] & xedg[,2]==auxstr[i]))
      isInF <- isInF*length(which(xedgF[,1]==auxstr[i-1] & xedgF[,2]==auxstr[i]))
      }
    if((use.ns==T & isInF>0) | isIn>0) {
      auxpa <- causalEff(x,from=from,to=to,cumul=cumul,conf=conf,use.ns=use.ns)
      auxpa <- auxpa[[path]] 
      } else {
      #if(isInF>0) {
      #  stop("Path not found. Try to reduce 'conf' or to set 'use.ns' to TRUE",call.=F)
      #  } else {
      #  stop("Inexistent path",call.=F)
      #  }
      auxpa <- NULL
      }
    }
  if(!is.null(auxpa)) {
    yaux <- rbind(rep(0,ncol(auxpa)),auxpa)
    rownames(yaux) <- c(-1:(nrow(yaux)-2))
    if(is.null(title)) {
      if(is.null(path)) {
        title <- paste(to," ~ ",from,sep="")
        } else {
        title <- paste(auxstr,collapse=" * ")
        }
      }
    bmat <- yaux[,c(1,3,4)]
    if(cumul==F) {
      auxbcum <- causalEff(x,from=from,to=to,cumul=T,conf=conf,use.ns=use.ns)$overall
      bcum <- signif(auxbcum[nrow(auxbcum),c(1,3,4)],5)
      } else {
      bcum <- signif(bmat[nrow(bmat),],5)
      }
    makeShape(bmat=bmat,maxlag=maxlag,cumul=cumul,bcum=bcum,conf=conf,ylim=ylim,title=title)
    } else {
    NULL  
    }
  }

# check if integer (internal use only)
intCheck <- function(x) {
  if(is.numeric(x) && x[1]==round(x[1]) && x[1]>=0) T else F
  }

# adjust global control options (internal use only)
gconAdj <- function(x) {
  nomi <- names(x)
  warn1 <- warn2 <- 0
  if(!is.null(x) && !is.list(x)) warn2 <- warn2+1
  unknam <- setdiff(nomi,c("adapt","min.gestation","max.gestation","min.width","max.lead","sign"))
  if(length(unknam)>0) {
    x <- x[setdiff(nomi,unknam)]
    warning("Some components with unknown names in argument 'global.control' were ignored",call.=F)
    }
  #
  if("adapt" %in% nomi) {
    xad <- x$adapt
    if(is.null(xad)) xad <- F
    if(is.logical(xad)) {
      if(length(xad)>1) warn1 <- warn1+1
      xad <- xad[1]
      } else {
      xad <- F
      warn2 <- warn2+1
      }
    } else {
    xad <- F
    }
  #
  if("min.gestation" %in% nomi) {
    xming <- x$min.gestation
    if(is.null(xming)) xming <- 0
    if(length(xming)>1) warn1 <- warn1+1
    xming <- xming[1]
    if(intCheck(xming)==F) {
      xming <- 0
      warn2 <- warn2+1
      }
    } else {
    xming <- 0
    }
  #
  if("max.gestation" %in% nomi) {
    xmaxg <- x$max.gestation
    if(is.null(xmaxg)) xmaxg <- Inf
    if(length(xmaxg)>1 | xmaxg<xming) warn1 <- warn1+1
    xmaxg <- max(xming,xmaxg[1])
    if(intCheck(xmaxg)==F) {
      xmaxg <- Inf
      warn2 <- warn2+1
      }
    } else {
    xmaxg <- Inf
    }
  #
  if("max.lead" %in% nomi) {
    xml <- x$max.lead
    if(is.null(xml)) xml <- Inf
    if(length(xml)>1) warn1 <- warn1+1
    xml <- xml[1]
    if(intCheck(xml)==F) {
      xml <- Inf
      warn2 <- warn2+1
      }
    } else {
    xml <- Inf
    }
  if(xml<Inf & xmaxg==Inf) xmaxg <- xml
  #
  if("min.width" %in% nomi) {
    xminw <- x$min.width
    if(is.null(xminw)) xminw <- 0
    if(length(xminw)>1) warn1 <- warn1+1
    xminw <- xminw[1]
    if(intCheck(xminw)==F) {
      xminw <- 0
      warn2 <- warn2+1
      }
    } else {
    xminw <- 0
    }
  #
  if("sign" %in% nomi) {
    xsg <- x$sign
    if(is.null(xsg)) xsg <- F
    if(xsg[1] %in% c("+","-")) {
      if(length(xsg)>1) warn1 <- warn1+1
      xsg <- xsg[1]
      } else {
      xsg <- F
      warn2 <- warn2+1
      }
    } else {
    xsg <- F
    }
  #
  if(xming>xml) {
    warn2 <- warn2+1
    xming <- xml
    }
  if(xmaxg>xml) {
    warn2 <- warn2+1
    xmaxg <- xml
    }
  if(xminw>xml) {
    warn2 <- warn2+1
    xminw <- xml
    }
  #
  if(warn1>0) warning("Some components in argument 'global.control' had length >1 and only the first element was used",call.=F)
  if(warn2>0) warning("Some invalid or uncoherent components in argument 'global.control' were adjusted for coherence",call.=F)
  list(adapt=xad,min.gestation=xming,max.gestation=xmaxg,min.width=xminw,max.lead=xml,sign=xsg)
  }

# adjust local control options (internal use only)
lconAdj <- function(x,gcon,pset) {
  warn1 <- warn2 <- 0
  nomi <- names(pset)
  if(!is.null(x) && !is.list(x)) warn2 <- warn2+1
  xad <- vector("list",length=length(nomi))
  names(xad) <- nomi
  for(j in 1:length(nomi)) {
    if("adapt" %in% names(x) && nomi[j] %in% names(x[["adapt"]])) {
      xad[[j]] <- x$adapt[[nomi[j]]]
      if(is.null(xad[[j]])) xad[[j]] <- gcon$adapt
      if(is.logical(xad[[j]])) {
        if(length(xad[[j]])>1) warn1 <- warn1+1
        xad[[j]] <- xad[[j]][1]
        } else {
        xad[[j]] <- gcon$adapt
        warn2 <- warn2+1
        }
      } else {
      xad[[j]] <- gcon$adapt
      }
    }
  #
  comp <- c("min.gestation","max.gestation","min.width","max.lead","sign")
  res <- vector("list",length=length(comp))
  names(res) <- comp
  for(i in 1:length(comp)) {
    ires <- vector("list",length=length(nomi))
    names(ires) <- nomi
    for(j in 1:length(nomi)) {
      ijpset <- pset[[nomi[[j]]]]
      ijres <- c()
      if(length(ijpset)>0) {
        for(k in 1:length(ijpset)) {
          if(comp[i] %in% names(x) && nomi[j] %in% names(x[[comp[i]]])) {
            ijval <- x[[comp[i]]][[nomi[[j]]]]
            if(ijpset[k] %in% names(ijval)) {
              ijres[k] <- ijval[ijpset[k]]
              if(is.null(ijres[k])) ijres[k] <- gcon[[comp[i]]]
              if(comp[i]=="sign") {
                ijkch <- ijres[k] %in% c("+","-")
                } else {
                ijkch <- intCheck(ijres[k]) 
                }
              if(ijkch==F) {
                ijres[k] <- gcon[[comp[i]]]
                warn2 <- warn2+1
                }
              } else {
              ijres[k] <- gcon[[comp[i]]]  #####
              }
            } else {
            ijres[k] <- gcon[[comp[i]]]
            }
          }
        names(ijres) <- ijpset
        ires[[j]] <- ijres
        }
      }
    res[[i]] <- ires
    }
  if(warn1>0) warning("Some components in argument 'local.control' had length >1 and only the first element was used",call.=F)
  if(warn2>0) warning("Some invalid or uncoherent components in argument 'local.control' were adjusted for coherence",call.=F)
  c(adapt=list(xad),res)
  }

# check missing values (internal use only)
checkNA <- function(x,group,data) {
  if(sum(!is.na(data[,x]))<3) stop("Variable '",x,"' has less than 3 observed values",call.=F)
  if(!is.null(group)) {
    gruppi <- levels(factor(data[,group]))
    for(i in 1:length(gruppi)) {
      auxind <- which(data[,group]==gruppi[i])
      if(sum(!is.na(data[auxind,x]))<1) {
        stop("Variable '",x,"' has no observed values in group '",gruppi[i],"'",call.=F)
        }
      }
    }
  }

# check variable name (internal use only)
checkName <- function(x) {
  res <- T
  if((substr(x,1,1) %in% c(letters,toupper(letters)))==F) res <- F
  if(length(grep("[-\\+\\*\\/]",x))>0) res <- F
  res
  }

# find operator in a variable name (internal use only)
findOp <- function(x) {
  if(gregexpr("\\(",x)[[1]][1]>0) {
    strsplit(x,"\\(")[[1]][1]
    } else {
    NULL 
    }
  }

# automated full model code
autoCode <- function(var.names,lag.type="ecq") {
  if(missing(var.names)==F & length(var.names)<2) stop("Argument 'var.names' must be at least of length 2",call.=F)
  if(length(lag.type)!=1) stop("Argument 'lag.type' must be of length 1",call.=F)
  if((lag.type %in% c("ecq","qd","ld","gam"))==F) stop("Argument 'lag.type' must be one among 'ecq', 'qd', 'ld' and 'gam'",call.=F)
  res <- list()
  if(checkName(var.names[1])==F) stop("'",var.names[1]," 'is not a valid variable name",call.=F)
  res[[1]] <- formula(paste(var.names[1],"~1",sep=""))
  for(i in 2:length(var.names)) {
    iy <- var.names[i]
    if(checkName(iy)==F) stop("'",iy," 'is not a valid variable name",call.=F)
    iX <- var.names[1:(i-1)]
    res[[i]] <- formula(paste(iy,"~",paste(paste(lag.type,"(",iX,",,)",sep=""),collapse="+"),sep=""))    
    }
  res
  }

# adjust diff options (internal use only)
diffoptAdj <- function(x) {
  warn1 <- warn2 <- 0
  if(!is.null(x) && !is.list(x)) warn2 <- warn2+1
  nomi <- names(x)
  unknam <- setdiff(nomi,c("test","maxdiff","ndiff"))
  if(length(unknam)>0) {
    x <- x[setdiff(nomi,unknam)]
    warning("Some components with unknown names in argument 'diff.options' were ignored",call.=F)
    }
  # 
  if("test" %in% nomi) {
    test <- x$test
    if(is.null(test)) test <- NULL
    if(length(test)>1) warn1 <- warn1+1
    test <- test[1]
    if(!is.null(test) && (test %in% c("kpss","adf"))==F) {
      test <- NULL
      warn2 <- warn2+1
      }
    } else {
    test <- NULL  
    }
  #
  if("maxdiff" %in% nomi) {
    maxdiff <- x$maxdiff
    if(is.null(maxdiff)) maxdiff <- 2
    if(length(maxdiff)>1) warn1 <- warn1+1
    maxdiff <- maxdiff[1]
    if(intCheck(maxdiff)==F) {
      maxdiff <- 2
      warn2 <- warn2+1
      }
    } else {
    maxdiff <- 2
    }
  #
  if("ndiff" %in% nomi) {
    ndiff <- x$ndiff
    if(length(ndiff)>1) warn1 <- warn1+1
    ndiff <- ndiff[1]
    if(!is.null(ndiff) && intCheck(ndiff)==F) {
      ndiff <- NULL
      warn2 <- warn2+1
      }
    } else {
    ndiff <- NULL
    }
  #
  if(warn1>0) warning("Some components in argument 'diff.options' had length >1 and only the first element was used",call.=F)
  if(warn2>0) warning("Some invalid or uncoherent components in argument 'diff.options' were adjusted",call.=F)
  list(test=test,maxdiff=maxdiff,ndiff=ndiff)
  }

# adjust imput options (internal use only)
impoptAdj <- function(x) {
  warn1 <- warn2 <- 0
  if(!is.null(x) && !is.list(x)) warn2 <- warn2+1
  nomi <- names(x)
  unknam <- setdiff(nomi,c("tol","maxiter","maxlag","no.imput"))
  if(length(unknam)>0) {
    x <- x[setdiff(nomi,unknam)]
    warning("Some components with unknown names in argument 'imput.options' were ignored",call.=F)
    }
  # 
  if("no.imput" %in% nomi) {
    noimp <- x$no.imput
    } else {
    noimp <- NULL  
    }
  #
  if("tol" %in% nomi) {
    tol <- x$tol
    if(is.null(tol)) tol <- 0.0001
    if(length(tol)>1) warn1 <- warn1+1
    tol <- tol[1]
    if(tol<=0) {
      tol <- 0.0001
      warn2 <- warn2+1
      }
    } else {
    tol <- 0.0001
    }
  #
  if("maxiter" %in% nomi) {
    maxiter <- x$maxiter
    if(is.null(maxiter)) maxiter <- 500
    if(length(maxiter)>1) warn1 <- warn1+1
    maxiter <- maxiter[1]
    if(intCheck(maxiter)==F) {
      maxiter <- 500
      warn2 <- warn2+1
      }
    } else {
    maxiter <- 500
    }
  #
  if("maxlag" %in% nomi) {
    maxlag <- x$maxlag
    if(is.null(maxlag)) maxlag <- 2
    if(length(maxlag)>1) warn1 <- warn1+1
    maxlag <- maxlag[1]
    if(intCheck(maxlag)==F) {
      maxlag <- 2
      warn2 <- warn2+1
      }
    } else {
    maxlag <- 2
    }
  #
  if(warn1>0) warning("Some components in argument 'imput.options' had length >1 and only the first element was used",call.=F)
  if(warn2>0) warning("Some invalid components in argument 'imput.options' were adjusted",call.=F)
  list(tol=tol,maxiter=maxiter,maxlag=maxlag,no.imput=noimp)
  }

# preprocessing
preProcess <- function(x=NULL,group=NULL,time=NULL,seas=NULL,data,log=FALSE,
  diff.options=list(test=NULL,maxdiff=2,ndiff=NULL),
  imput.options=list(tol=0.0001,maxiter=500,maxlag=2,no.imput=NULL),quiet=FALSE) {
  #
  if(missing(data)==F & !identical(class(data),"data.frame")) stop("Argument 'data' must be a data.frame",call.=F)  
  if(!is.null(group) && length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
  if(!is.null(group) && is.na(group)) group <- NULL
  if(!is.null(seas) && (length(seas)!=1 || !is.numeric(seas) || seas<=0 || seas!=round(seas))) stop("Argument 'seas' must be a positive integer value",call.=F)
  if(!is.null(group)) {
    if(length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
    if((group %in% colnames(data))==F) stop("Variable '",group,"' provided to argument 'group' not found in data",call.=F)
    gruppi <- levels(factor(data[,group]))
    if(length(gruppi)<2) stop("The group factor must have at least 2 unique values",call.=F)
    data[,group] <- factor(data[,group])
    if(min(table(data[,group]))<3) stop("There must be at least 3 observations per group",call.=F)
    data <- data[order(data[,group]),]
    } else {
    if(nrow(data)<3) stop("There must be at least 3 observations",call.=F)  
    }
  if(!is.null(time)) {
    if(is.na(time)) time <- NULL
    if(length(time)!=1) stop("Argument 'time' must be of length 1",call.=F)
    if((time %in% colnames(data))==F) stop("Variable '",time,"' provided to argument 'time' not found in data",call.=F)
    if(time %in% group) stop("Variable '",time,"' is provided to both arguments 'group' and 'time'",call.=F)
    if(isTimeVar(data[,time])==F) stop("The time variable is neither numeric nor a date",call.=F)
    if(is.null(group)) {
      if(sum(duplicated(data[,time]))>0) stop("The time variable has duplicated values",call.=F)
      } else {
      timesplit <- split(data[,time],data[,group])
      if(sum(sapply(timesplit,function(z){sum(duplicated(z))}))>0) stop("The time variable has duplicated values",call.=F)  
      }
    #
    if(!is.null(group)) {
      for(i in 1:length(gruppi)) {
        iind <- which(data[,group]==gruppi[i])
        idat <- data[iind,]
        data[iind,] <- idat[order(idat[,time]),]
        }
      } else {
      data <- data[order(data[,time]),]
      }
    #
    }
  if(!is.null(group) && length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
  if(!is.null(group) && (group %in% colnames(data))==F) stop("Variable '",group,"' not found in data",sep="",call.=F)
  if(length(quiet)!=1 || !is.logical(quiet)) stop("Argument 'quiet' must be a logical value",call.=F)
  #if(!is.null(group)) {
  #  data[,group] <- factor(data[,group])
  #  gruppi <- levels(data[,group])
  #  }
  if(!is.null(x)) {
    x2del <- c()
    for(i in 1:length(x)) {
      if((x[i] %in% colnames(data))==F) {
        if(quiet==F) warning("Variable '",x[i],"' not found in data and ignored",call.=F)
        x2del <- c(x2del,x[i])
        }
      }
    x <- setdiff(x,x2del)
    if(length(x)<1) stop("No valid variables provided to argument 'x'",call.=F)
    } else {
    x <- setdiff(colnames(data),c(group,time))
    }
  if(length(quiet)!=1 || !is.logical(quiet)) stop("Argument 'quiet' must be a logical value",call.=F)
  #
  diff.options <- diffoptAdj(diff.options)
  if(is.null(diff.options$test)) {
    if(is.null(group)) {
      n <- nrow(data)
      } else {
      n <- min(table(data[,group]))  
      }
    diff.options$test <- ifelse(n>100,"adf","kpss")
    }
  ndiff <- diff.options$ndiff
  imput.options <- impoptAdj(imput.options)
  #
  if(!is.null(time)) {
    if(!is.null(group)) {
      for(i in 1:length(gruppi)) {
        iind <- which(data[,group]==gruppi[i])
        idat <- data[iind,]
        data[iind,] <- idat[order(idat[,time]),]
        }
      } else {
      data <- data[order(data[,time]),]
      }
    }
  nodenam <- x
  xfact <- c()
  for(i in 1:length(nodenam)) {
    if(isQuant(data[,nodenam[i]])==F) {
      if(sum(is.na(data[,nodenam[i]]))>0) stop("Variable ",nodenam[i]," is qualitative and contains missing values",sep="",call.=F)
      xfact <- c(xfact,nodenam[i])
      data[,nodenam[i]] <- factor(data[,nodenam[i]])
      }
    }
  # deseasonalization
  if(!is.null(seas) && seas>1) data[,nodenam] <- deSeas(setdiff(nodenam,xfact),seas=seas,group=group,data=data)
  # log transformation
  logOK <- c()
  if(identical(log,T)) {
    logtest <- setdiff(nodenam,xfact)            
    if(length(logtest)>0) {
      for(i in 1:length(logtest)) {
        if(sum(data[,logtest[i]]<=0,na.rm=T)>0) {
          if(quiet==F) warning("Logarithmic transformation not applied to variable '",logtest[i],"'",call.=F)      
          } else {
          data[,logtest[i]] <- log(data[,logtest[i]])
          logOK <- c(logOK,logtest[i])
          }
        }
      }
    } else if(!identical(log,F)) {
    for(i in 1:length(log)) {
      if((log[i] %in% colnames(data))==F) {
        if(quiet==F) warning("Variable '",log[i],"' provided to argument 'log' not found in data",call.=F)
        } else if(log[i] %in% c(group,time)) {
        if(quiet==F) warning("Logarithmic transformation not applied to variable '",log[i],"'",call.=F)
        } else {
        if(sum(data[,log[i]]<=0,na.rm=T)>0) {
          if(quiet==F) warning("Logarithmic transformation not applied to variable '",log[i],"'",call.=F)
          } else {
          data[,log[i]] <- log(data[,log[i]])  
          }
        }
      }
    }
  # imputation
  auxna <- apply(data[,nodenam],1,function(x){sum(is.na(x))})
  if(sum(auxna)>0) {
    auxOK <- unname(which(auxna<length(nodenam))) 
    if(sum(is.na(data[auxOK,]))>0 && imput.options$maxiter>0) {
      #
      auxch <- setdiff(imput.options$no.imput,colnames(data))
      if(length(auxch)>0) warning("Variable '",auxch[1],"' provided to component 'no.imput' in argument 'imput.options' not found in data and ignored",call.=F)
      #
      x2imp <- setdiff(nodenam,c(imput.options$no.imput,xfact))
      if(length(x2imp)>0) {
        for(i in 1:length(x2imp)) {
          nachk <- checkNA(x2imp[i],group,data[auxOK,])
          }
        nIm <- sum(is.na(data[,x2imp]))
        if(nIm>0) {
          emdat <- data[,c(group,xfact,x2imp),drop=F]
          for(i in 1:length(x2imp)) {
            iord <- min(arFind(x2imp[i],group,data),imput.options$maxlag)
            if(iord>0) {
              idx <- addLags(x=x2imp[i],data=data,k=iord)
              emdat <- cbind(emdat,idx) 
              }
            }
          filldat <- EM.imputation(xcont=x2imp,xqual=xfact,group=group,data=emdat[auxOK,],tol=imput.options$tol,maxiter=imput.options$maxiter,quiet=quiet)
          data[auxOK,c(group,xfact,x2imp)] <- filldat[,c(group,xfact,x2imp)]
          }
        }
      } else {
      if(quiet==F && imput.options$maxiter==0) cat("Imputation not performed","\n")  
      }
    }
  # differentiation
  difftest <- setdiff(nodenam,xfact)
  if(is.null(ndiff)) {
    ndiff <- 0
    if(length(difftest)>0 & diff.options$maxdiff>0) {
      fine <- 0
      if(quiet==F) cat("Checking stationarity ...")
      flush.console()
      data <- applyDiff(x=difftest,group=group,data=data,k=rep(0,length(difftest)))
      while(fine==0) {
        auxp <- c()
        urtList <- urtFun(x=difftest,group=group,time=NULL,data=data,test=diff.options$test,log=F)
        for(i in 1:length(difftest)) {
          ipvl <- urtList[[i]]$p.value
          if(is.null(ipvl)) {
            auxp[i] <- 0
            } else {
            if(is.na(ipvl)) {
              auxp[i] <- 0
              } else {
              auxp[i] <- ipvl
              }
            }
          }
        if(diff.options$test=="adf") {
          nUR <- length(which(auxp>0.05))
          } else {
          nUR <- length(which(auxp<0.05))
          }
        if(nUR>0) {
          if(ndiff<diff.options$maxdiff) {
            ndiff <- ndiff+1
            data <- applyDiff(x=difftest,group=group,data=data,k=rep(1,length(difftest)))                                        
            } else {
            fine <- 1
            }
          } else {
          fine <- 1
          }
        }
      } else {
      urtList <- NULL
      }
    } else {
    data <- applyDiff(x=difftest,group=group,data=data,k=rep(ndiff,length(difftest)))
    }
  if(quiet==F) {
    cat('\r')
    if(ndiff>0) {
      cat("Order",ndiff,"differentiation performed","\n")
      } else {
      cat("Differentiation not performed","\n")
      }
    }
  res <- data[,c(group,time,x),drop=F]
  attr(res,"log") <- logOK
  attr(res,"ndiff") <- ndiff
  res
  }

# check collinearity (internal use only)
collCheck <- function(y,x,group,data) {
  form <- paste(y,"~",paste(c(group,x),collapse="+"),sep="")
  m0 <- lm(formula(form),data=data)
  names(which(is.na(m0$coefficients)))
  }

# fit a dlsem
dlsem <- function(model.code,group=NULL,time=NULL,exogenous=NULL,data,
  hac=TRUE,gamma.by=0.05,global.control=NULL,local.control=NULL,log=FALSE,
  diff.options=list(test=NULL,maxdiff=2,ndiff=NULL),
  imput.options=list(tol=0.0001,maxiter=500,maxlag=2,no.imput=NULL),quiet=FALSE) {
  #
  if(missing(model.code)==F & (!is.list(model.code) || length(model.code)==0 || sum(sapply(model.code,class)!="formula")>0)) stop("Argument 'model code' must be a list of formulas",call.=F)
  if(missing(data)==F & !identical(class(data),"data.frame")) stop("Argument 'data' must be a data.frame",call.=F)
  #nameOfData <- deparse(substitute(data))
  if(!is.null(group) && length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
  if(!is.null(group) && is.na(group)) group <- NULL
  if(!is.null(group)) {
    if(length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
    if((group %in% colnames(data))==F) stop("Variable '",group,"' provided to argument 'group' not found in data",call.=F)
    if(group %in% exogenous) stop("Variable '",group,"' is provided to both arguments 'group' and 'exogenous'",call.=F)
    gruppi <- levels(factor(data[,group]))
    if(length(gruppi)<2) stop("The group factor must have at least 2 unique values",call.=F)
    data[,group] <- factor(data[,group])
    if(min(table(data[,group]))<3) stop("There must be at least 3 observations per group",call.=F)
    data <- data[order(data[,group]),]
    } else {
    if(nrow(data)<3) stop("There must be at least 3 observations",call.=F)  
    }
  if(!is.null(time)) {
    if(is.na(time)) time <- NULL
    if(length(time)!=1) stop("Argument 'time' must be of length 1",call.=F)
    if((time %in% colnames(data))==F) stop("Variable '",time,"' provided to argument 'time' not found in data",call.=F)
    if(time %in% group) stop("Variable '",time,"' is provided to both arguments 'group' and 'time'",call.=F)
    if(time %in% exogenous) stop("Variable '",time,"' is provided to both arguments 'time' and 'exogenous'",call.=F)
    if(isTimeVar(data[,time])==F) stop("The time variable is neither numeric nor a date",call.=F)
    if(is.null(group)) {
      if(sum(duplicated(data[,time]))>0) stop("The time variable has duplicated values",call.=F)
      } else {
      timesplit <- split(data[,time],data[,group])
      if(sum(sapply(timesplit,function(z){sum(duplicated(z))}))>0) stop("The time variable has duplicated values",call.=F)  
      }
    #
    if(!is.null(group)) {
      for(i in 1:length(gruppi)) {
        iind <- which(data[,group]==gruppi[i])
        idat <- data[iind,]
        data[iind,] <- idat[order(idat[,time]),]
        }
      } else {
      data <- data[order(data[,time]),]
      }
    }
  if(!is.null(exogenous) && identical(NA,exogenous)) exogenous <- NULL
  if(!is.null(group) && length(group)!=1) stop("Argument 'group' must be of length 1",call.=F)
  if(!is.null(group) && (group %in% colnames(data))==F) stop("Variable '",group,"' not found in data",sep="",call.=F)
  if(length(hac)!=1 || !is.logical(hac)) stop("Argument 'hac' must be a logical value",call.=F)
  if(length(gamma.by)!=1 || !is.numeric(gamma.by) || gamma.by<=0 || gamma.by>=1) stop("Argument 'gamma.by' must be a real number in the interval (0,1)",call.=F)
  if(length(quiet)!=1 || !is.logical(quiet)) stop("Argument 'quiet' must be a logical value",call.=F)
  rownames(data) <- 1:nrow(data)
  estL <- pset <- list()
  for(i in 1:length(model.code)) {
    if(sum(grepl("\\-",model.code[[i]]))>0) stop("Invalid character '-' in 'model.code', regression of '",model.code[[i]][2],"'",call.=F)
    if(sum(grepl("\\:",model.code[[i]]))>0) stop("Invalid character ':' in 'model.code', regression of '",model.code[[i]][2],"'",call.=F) #####
    if(sum(grepl("\\*",model.code[[i]]))>0) stop("Invalid character '*' in 'model.code', regression of '",model.code[[i]][2],"'",call.=F) #####
    }
  for(i in 1:length(model.code)) {
    iscan <- scanForm(model.code[[i]],warn=T)
    ynam <- iscan$y    
    if(checkName(ynam)==F) stop("'",ynam,"' is not a valid variable name",call.=F)
    #
    if((ynam %in% colnames(data))==F) {
      auxop <- findOp(ynam)
      if(is.null(auxop)) {
        stop("Variable '",ynam,"' not found in data",call.=F)
        } else {
        stop("Operator ",auxop,"() not allowed in 'model.code'",call.=F) 
        }
      }
    #
    ipar <- names(iscan$ltype)
    if(length(ipar)==0) ipar <- character(0)
    pset[[i]] <- ipar
    names(model.code)[i] <- names(pset)[i] <- ynam
    if(length(ipar)>0) {
      if(!is.null(group)) {
        if(group %in% ipar) stop("Variable '",group,"' is defined as a group factor and appears in 'model.code'",call.=F) 
        }
      #
      auxfun <- setdiff(ipar,colnames(data))
      if(length(auxfun)>0) {
        auxop <- findOp(auxfun[1])
        if(is.null(auxop)) {
          stop("Variable '",auxfun[1]," not found in data",call.=F)
          } else {
          stop("Operator ",auxop,"() not allowed in 'model.code'",call.=F) 
          }
        }
      #
      auxexo <- intersect(ipar,exogenous)
      if(length(auxexo)>0) stop("Variable '",auxexo[1],"' appears both in 'model.code' and in 'exogenous'",call.=F)
      auxdupl <- duplicated(ipar)
      if(sum(auxdupl)>0) stop("Duplicated covariate '",ipar[auxdupl][1],"' in 'model.code', regression of '",model.code[[i]][2],"'",call.=F)
      }
    auxcoll <- collCheck(ynam,c(ipar,exogenous),group,data)
    if(length(auxcoll)>0) stop("Collinearity problem with covariate '",auxcoll[1],"' in the regression of '",model.code[[i]][2],"'",call.=F)
    }
  auxdupl <- duplicated(names(model.code))
  if(sum(auxdupl)>0) stop("Duplicated response variable '",names(model.code)[auxdupl][1],"' in 'model.code'",call.=F)
  nodenam <- unique(c(names(pset),unlist(pset)))
  auxadd <- setdiff(nodenam,names(model.code))
  if(length(auxadd)>0) {
    for(i in length(auxadd):1) {
      model.code <- c(formula(paste(auxadd[i],"~1",sep="")),model.code)
      names(model.code)[1] <- auxadd[i]
      pset[[length(pset)+1]] <- character(0) 
      names(pset)[length(pset)] <- auxadd[i]
      }
    }
  if(length(nodenam)<2) stop("The model cannot contain less than 2 endogenous variables",call.=F)
  auxvar <- setdiff(c(nodenam,exogenous),colnames(data))
  if(length(auxvar)>0) {
    stop("Variable '",auxvar[1],"' not found in data",sep="",call.=F)
    }
  for(i in 1:length(nodenam)) {
    if(isQuant(data[,nodenam[i]])==F) stop("Qualitative variables cannot appear in 'model.code': ",nodenam[i],sep="",call.=F)
    }
  if(!is.null(exogenous)) {
    for(i in 1:length(exogenous)) {
      if(isQuant(data[,exogenous[i]])==F) {
        if(sum(is.na(data[,exogenous[i]]))>0) stop("Qualitative variables cannot contain missing values: ",exogenous[i],sep="",call.=F)
        }
      }                                                                                                                                                                          
    }
  G <- new("graphNEL",nodes=names(pset),edgemode="directed")    
  for(i in 1:length(pset)) {
    if(length(pset[[i]])>0) {
      for(j in 1:length(pset[[i]])) {
        G <- addEdge(pset[[i]][j],names(pset)[i],G,1)
        }
      }
    }          
  topG <- topOrder(G)
  if(is.null(topG)) stop("The DAG contains directed cycles",call.=F)  
  global.control <- gconAdj(global.control)
  local.control <- lconAdj(local.control,global.control,pset)
  nodenam <- c(exogenous,topG)
  #
  #ndiff <- attr(data,"ndiff")
  #if(is.null(ndiff)) {
  #  ndiff <- 0
  #  #if(quiet==F) {
  #  #  pre <- winDialog("yesno",message="Data have not been preprocessed. Do you want to apply preprocessing with default settings?")
  #  #  if(identical(pre,"YES")) data <- preProcess(nodenam,group=group,time=time,log=T,data=data)
  #  #  }
  #  warning("It seems that data have not been preprocessed",call.=F)
  #  }
  #
  # preprocessing
  data_orig <- data
  data <- preProcess(x=nodenam,group=group,time=time,seas=NULL,data=data,
    log=log,diff.options=diff.options,imput.options=imput.options,quiet=quiet)
  #
  nomi <- c()
  optList <- callList <- lparList <- codeList <- vector("list",length=length(model.code))
  if(quiet==F) cat("Starting estimation ...")
  flush.console()
  for(i in 1:length(model.code)) {
    nomi[i] <- as.character(model.code[[i]])[2]
    if(quiet==F) {
      if(i>1) {
        inbl0 <- nchar(imess)
        } else {
        inbl0 <- 0  
        }
      imess <- paste("Estimating regression ",i,"/",length(model.code)," (",nomi[i],")",sep="")
      inbl <- max(0,inbl0-nchar(imess)+2)
      } else {
      imess <- NULL
      inbl <- 0
      }
    if(is.null(exogenous)) {
      iform <- model.code[[i]]
      } else {
      iform <- formula(paste(as.character(model.code[[i]])[2],"~",paste(exogenous,collapse="+"),"+",as.character(model.code[[i]])[3],sep=""))
      }
    iad <- local.control[["adapt"]][nomi[i]]
    ipar <- pset[[nomi[i]]]
    iscan <- scanForm(model.code[[i]])
    ilimit <- Inf
    if(length(ipar)>0) {
      #
      ilagtype <- iscan$ltype
      ilagnam <- names(ilagtype[which(ilagtype!="none")])
      if(length(ilagnam)>0) {
        ilimit <- findLagLim(data[,c(nomi[i],ilagnam,group)],group=group)
        if(sum(sapply(iscan$lpar,function(z){sum(is.na(z))}))>0) iad <- T
        }
      #
      iming <- rep(min(ilimit,global.control$min.gestation),length(ipar))
      iges <- rep(min(ilimit,global.control$max.gestation),length(ipar))
      iwd <- rep(min(ilimit,global.control$min.width),length(ipar))
      ilead <- rep(min(ilimit,global.control$max.lead),length(ipar))
      isg <- rep(global.control$sign,length(ipar))
      names(iming) <- names(iges) <- names(iwd) <- names(ilead) <- names(isg) <- ipar
      #
      } else {
      iming <- iges <- iwd <- ilead <- isg <- c()
      }
    #
    iauxming <- local.control$min.gestation[[nomi[i]]]
    iming[names(iauxming)] <- sapply(iauxming,function(z){min(ilimit,z)})
    iauxges <- local.control$max.gestation[[nomi[i]]]
    iges[names(iauxges)] <- sapply(iauxges,function(z){min(ilimit,z)})
    iauxwd <- local.control$min.width[[nomi[i]]]
    iwd[names(iauxwd)] <- sapply(iauxwd,function(z){min(ilimit,z)})
    iauxlead <- local.control$max.lead[[nomi[i]]]
    ilead[names(iauxlead)] <- sapply(iauxlead,function(z){min(ilimit,z)})
    iauxsg <- local.control$sign[[nomi[i]]]
    isg[names(iauxsg)] <- iauxsg
    #
    if(iad==T) optList[[i]] <- list(adapt=iad,min.gestation=iming,max.gestation=iges,min.width=iwd,max.lead=ilead,sign=isg)
    imod <- dlaglm(iform,group=group,data=data,adapt=iad,no.select=exogenous,min.gestation=iming,max.gestation=iges,min.width=iwd,max.lead=ilead,sign=isg,ndiff=diff.options$ndiff,gamma.by=gamma.by,mess=imess,nblank=inbl)
    callList[[i]] <- icall <- imod$call$formula
    iscan <- scanForm(icall)
    ixnam <- setdiff(names(iscan$ltype),c(group,time,exogenous))
    codeList[[i]] <- creatForm(nomi[i],ixnam,NULL,iscan$ltype[ixnam],iscan$lpar[ixnam],NULL)
    if(length(ixnam)>0) {
      ilpar <- data.frame(iscan$ltype[ixnam],do.call(rbind,iscan$lpar[ixnam]))
      colnames(ilpar) <- c("type","par1","par2")
      lparList[[i]] <- ilpar
      }
    if(hac==T) {
      imod$vcov <- doHAC(imod,group=group)
      class(imod) <- c("hac","lm")
      } else {
      imod$vcov <- vcov(imod)
      }
    estL[[i]] <- imod
    }
  names(optList) <- names(lparList) <- nomi
  if(quiet==F) {
    blchar <- ""
    if(iad==F) {
      if(nchar(imess)>=20) blchar <- paste(rep(" ",nchar(imess)-20),collapse="")
      } else {
      if(nchar(imess)>=11) blchar <- paste(rep(" ",nchar(imess)-11),collapse="")
      }
    cat('\r')
    cat("Estimation completed",blchar,"\n")
    }
  names(estL) <- nomi
  # fitted and residuals
  epsL <- fitL <- list()
  for(i in 1:length(estL)) {
    epsL[[i]] <- residuals(estL[[i]])
    fitL[[i]] <- fitted(estL[[i]])
    }
  epsOK <- data.frame(do.call(cbind,lapply(epsL,formatFit,n=nrow(data))))
  fitOK <- data.frame(do.call(cbind,lapply(fitL,formatFit,n=nrow(data))))
  if(!is.null(time)) {
    epsOK <- cbind(data[,time],epsOK)
    fitOK <- cbind(data[,time],fitOK)
    } 
  if(!is.null(group)) {
    epsOK <- cbind(data[,group],epsOK)
    fitOK <- cbind(data[,group],fitOK)
    }
  colnames(epsOK) <- colnames(fitOK) <- c(group,time,names(estL))
  # autocorrelation order
  if(hac==T) {
    acOrder <- lapply(estL,function(z){attr(z$vcov,"max.lag")})
    } else {
    if(is.null(group)) {
      acOrder <- c()
      for(i in 1:length(estL)) {
        acOrder[i] <- ar(na.omit(epsOK[,names(estL)[i]]))$order
        }
      names(acOrder) <- names(estL)
      } else {
      acOrder <- list()
      for(i in 1:length(estL)) {
        iac <- c()
        for(j in 1:length(gruppi)) {
          iind <- which(epsOK[,group]==gruppi[j])
          iac[j] <- ar(na.omit(epsOK[iind,names(estL)[i]]))$order
          }
        names(iac) <- paste(group,gruppi,sep="")
        acOrder[[i]] <- iac
        }
      names(acOrder) <- names(estL)
      }
    }
  # output
  out <- list(estimate=estL,model.code=codeList,call=callList,lag.par=lparList,exogenous=exogenous,group=group,time=time,
    log=attr(data,"log"),ndiff=attr(data,"ndiff"),
    data=data[,c(group,time,nodenam)],data.orig=data_orig,
    fitted=fitOK,residuals=epsOK,autocorr=acOrder,hac=hac,adaptation=optList)
  class(out) <- "dlsem"
  out
  }

# automated plots of lag shapes
auto.lagPlot <- function(x,cumul=FALSE,conf=0.95,plotDir=NULL) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(length(cumul)!=1 || !is.logical(cumul)) stop("Argument 'cumul' must be a logical value",call.=F)
  if(length(conf)!=1 || !is.numeric(conf) || conf<=0 || conf>=1) stop("Argument 'conf' must be a real number in the interval (0,1)",call.=F)
  if(is.null(plotDir)) plotDir <- getwd()
  for(i in 1:length(x$estimate)) {  
    scan0 <- scanForm(x$estimate[[i]]$call$formula)
    inam <- scan0$y
    isumm <- summary(x$estimate[[inam]])$coefficients
    ilagged <- names(scan0$ltype)[which(scan0$ltype!="none")]
    ilab <- sapply(rownames(isumm),extrName)
    if(length(ilagged)>0) {                                  
      for(j in 1:length(ilagged)) {
        ijlab <- names(ilab)[which(ilab==ilagged[j])]
        if(isumm[ijlab,4]<=1-conf) {
          pdf(file.path(plotDir,paste(inam,"~",ilagged[j],".pdf",sep="")))
          lagPlot(x,path=paste(ilagged[j],"*",inam,sep=""),cumul=cumul,conf=conf)
          dev.off()
          }
        }
      }
    }
  cat("Plots saved in ",plotDir,"\n",sep="")
  }

# print method for class 'dlsem'
print.dlsem <- function(x,...) {
  cat("A distributed-lag linear structural equation model","\n")
  n.e <- sum(sapply(chldsets(makeGraph(x)$graph),length))
  N.e <- sum(sapply(chldsets(makeGraph(x)$full.graph),length))
  if(!is.null(x$group)) {
    ngr <- length(x$estimate[[1]]$xlevels[[x$group]])
    cat(" Number of groups: ",ngr,"\n",sep="")
    } else {
    cat(" No groups","\n")
    }
  cat(" Number of endogenous variables: ",length(x$estimate),"\n",sep="")
  if(!is.null(x$exogenous)) {
    cat(" Number of exogenous variables: ",length(x$exogenous),"\n",sep="")
    } else {
    cat(" No exogenous variables","\n")
    }
  if(n.e>0) {
    cat(" ",n.e,"/",N.e," significant edges at 5% level","\n",sep="")
    } else {
    cat(" No significant edges at 5% level","\n")  
    }
  }

# format summary table (internal use only)
formatSumm <- function(x,newnam) {
  if(newnam==T) {
    colnam <- c("theta","se(theta)","t value","Pr(>|t|)","")
    } else {
    colnam <- c(colnames(x),"")
    }
  auxp <- rep("",nrow(x))
  pval <- x[,ncol(x)]
  auxp[which(pval<0.1)] <- "."
  auxp[which(pval<0.05)] <- "*"
  auxp[which(pval<0.01)] <- "**"
  auxp[which(pval<0.001)] <- "***"
  res <- data.frame(x,auxp)
  colnames(res) <- colnam
  res
  }

# summary method for class 'dlsem'
summary.dlsem <- function(object,...) {
  elev <- glev <- c()
  enam <- object$exogenous 
  xcat <- object$estimate[[1]]$xlevels
  if(!is.null(enam)) {
    for(i in 1:length(enam)) {
      if((enam[i] %in% names(xcat))==F) {
        elev <- c(elev,enam[i]) 
        } else {
        elev <- c(elev,paste(enam[i],xcat[[enam[i]]],sep=""))
        }
      }
    }
  if(!is.null(object$group)) {
    glev <- paste(object$group,xcat[[object$group]],sep="")
    }
  estim <- object$estimate
  summList <- summList_e <- summList_g <- vector("list",length=length(estim))
  names(summList) <- names(summList_e) <- names(summList_g) <- names(estim)
  summS <- data.frame(matrix(nrow=length(estim),ncol=3))
  rownames(summS) <- names(estim)
  colnames(summS) <- c("Std. Dev.","df","Rsq")
  for(i in 1:length(estim)) {
    isumm <- summary(estim[[i]])
    iB <- isumm$coefficients
    inam <- setdiff(rownames(iB),c("(Intercept)",elev,glev))
    if(length(inam)>0) {
      iendo <- formatSumm(iB[inam,,drop=F],newnam=F)
      rownames(iendo) <- inam
      summList[[i]] <- iendo
      }
    if(length(elev)>0) {
      elevOK <- intersect(rownames(iB),elev)
      if(length(elevOK)>0) {
        summList_e[[i]] <- formatSumm(iB[elevOK,,drop=F],newnam=F)
        } else {
        #summList_e[[i]] <- NULL
        }
      }
    if(length(glev)>0) {
      summList_g[[i]] <- formatSumm(iB[intersect(glev,rownames(iB)),,drop=F],newnam=F)   
      } else {
      summList_g[[i]] <- formatSumm(iB["(Intercept)",,drop=F],newnam=F)          
      }
    summS[i,] <- c(isumm$sigma,estim[[i]]$df.residual,isumm$r.squared)
    }
  OUT <- list(endogenous=summList,exogenous=summList_e,intercepts=summList_g,errors=summS)
  class(OUT) <- "summary.dlsem"
  OUT
  }

# print method for class summary.dlsem
print.summary.dlsem <- function(x,...) {
  cat("ENDOGENOUS PART","\n","\n")
  for(i in 1:length(x$endogenous)) {
    cat("Response: ",names(x$endogenous)[i],sep="","\n")
    isumm <- x$endogenous[[i]]
    if(!is.null(isumm)) {
      print(isumm)
      } else {
      cat("-","\n")
      }
    if(i<length(x$endogenous)) cat("\n")
    }
  cat("\n","\n")
  cat("EXOGENOUS PART","\n")
  if(sum(sapply(x$exogenous,is.null))==0) {
    cat("\n")
    for(i in 1:length(x$exogenous)) {
      cat("Response: ",names(x$exogenous)[i],sep="","\n")
      print(x$exogenous[[i]])
      cat("\n")
      }
    } else {
    cat(" -","\n","\n")
    }
  fitI <- x$gof
  cat("\n")
  cat("INTERCEPTS","\n")
  if(sum(sapply(x$intercepts,is.null))==0) {
    cat("\n")
    for(i in 1:length(x$intercepts)) {
      cat("Response: ",names(x$intercepts)[i],sep="","\n")
      print(x$intercepts[[i]])
      cat("\n")
      }
    } else {
    cat(" -","\n","\n")
    }
  cat("\n")
  cat("ERRORS","\n")
  print(x$errors)
  }

# nobs method for class 'dlsem'
nobs.dlsem <- function(object,...) {
  sapply(object$estimate,nobs)
  }

# coef method for class 'dlsem'
coef.dlsem <- function(object,...) {
  lapply(object$estimate,coef)
  }

# vcov method for class 'dlsem'
vcov.dlsem <- function(object,...) {
  lapply(object$estimate,vcov)
  }

# confint method for class 'dlsem'
confint.dlsem <- function(object,parm,level=0.95,...) {
  lapply(object$estimate,confint)
  }

# logLik method for class 'dlsem'
logLik.dlsem <- function(object,...) {
  lapply(object$estimate,logLik)
  }

# compare several models
compareModels <- function(x) {
  if(!identical(class(x),"list") || length(x)<2 || sum(sapply(x,function(y){!identical(class(y),"dlsem")}))>0) stop("The argument must be a list of 2 or more objects of class 'dlsem'",call.=F)
  for(i in 2:length(x)) {
    if(!identical(x[[i-1]]$data,x[[i]]$data)) stop("The models were estimated from different data",call.=F)
    }
  nomi <- names(x[[1]]$estimate)
  resL <- lapply(x,function(y){y$residuals})
  isNA <- do.call(cbind,lapply(resL,function(y){apply(y,1,function(z){1*(sum(is.na(z))>0)})}))
  ind <- which(apply(isNA,1,sum)==0)
  n <- length(ind)
  ic <- data.frame(matrix(nrow=length(x),ncol=4))
  colnames(ic) <- c("logLik","p","AIC","BIC")
  rownames(ic) <- names(x)  
  for(i in 1:length(x)) {
    inpar <- sapply(x[[i]]$estimate,function(y){extractAIC(y)[1]})
    ires <- resL[[i]]
    inam <- names(x[[i]]$estimate)
    irss <- c()
    for(j in 1:length(inam)) {
      irss[j] <- sum(ires[ind,inam[j]]^2)
      }
    is2 <- irss/(n-inpar)
    ilogL <- -log(sqrt(2*pi*is2))-irss/(2*is2)
    ic[i,1] <- sum(ilogL)
    ic[i,2] <- sum(inpar)
    }
  ic[,3] <- -2*ic[,1]+2*ic[,2]
  ic[,4] <- -2*ic[,1]+ic[,2]*log(n)
  ic
  }

# format fitted or residuals (internal use only)
formatFit <- function(x,n) {
  res <- rep(NA,n)
  names(res) <- 1:n
  res[names(x)] <- x
  res
  }

# fitted method for class 'dlsem'
fitted.dlsem <- function(object,...) {
  object$fitted
  }

# residuals method for class 'dlsem'
residuals.dlsem <- function(object,...) {
  object$residuals
  }

# predict method for class 'dlsem'
predict.dlsem <- function(object,newdata=NULL,...) {  
  res <- c()
  nomi <- names(object$estimate)
  if(!is.null(newdata)) {
    oldata <- object$data
    ndata <- newdata
    xnam <- intersect(colnames(oldata),colnames(newdata))
    rownames(ndata) <- NULL
    dataOK <- rbind(oldata[,xnam],ndata[,xnam])
    } else {
    dataOK <- NULL
    }
  for(i in 1:length(object$estimate)) {
    ipred <- predict(object$estimate[[i]],dataOK)
    iN <- max(as.numeric(names(ipred)))
    ipredOK <- rep(NA,iN)
    names(ipredOK) <- as.character(1:iN)
    ipredOK[names(ipred)] <- ipred
    res <- cbind(res,ipredOK)
    }
  res <- data.frame(res)
  if(!is.null(object$group)) {
    res <- cbind(object$fitted[,object$group],res)
    }
  colnames(res) <- c(object$group,nomi)
  if(!is.null(newdata)) {
    resOK <- res[(nrow(oldata)+1):(nrow(oldata)+nrow(newdata)),]
    rownames(resOK) <- rownames(newdata)
    resOK
    } else {
    res
    }
  }

# get structure (auxiliary)
getStruct <- function(x) {
  G <- x$model.code
  xnam <- c()
  res <- list()
  for(i in 1:length(G)) {
    iform <- scanForm(G[[i]])
    if(length(iform$X)>0) {
      res[[i]] <- names(iform$ltype)
      } else {
      res[[i]] <- character(0)
      }
    xnam[i] <- iform$y
    }
  names(res) <- xnam
  res
  }

# draw sample from a dlsem
drawSample <- function(x,n) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(missing(n)==F & (length(n)!=1 || !is.numeric(n) || n<=0 || n!=round(n))) stop("Argument 'n' must be a positive integer value",call.=F)
  laglim <- findLagLim(x$data,group=x$group)
  nOK <- n+laglim
  pset <- getStruct(x)
  auxwarn <- options()$warn
  options(warn=-1)
  if(is.null(x$group)) {
    res <- data.frame(matrix(nrow=nOK,ncol=length(pset)))
    colnames(res) <- names(pset)
    if(!is.null(x$exogenous)) {
      #exdat <- c()
      #xcat <- x$estimate[[1]]$xlevels
      #xnum <- setdiff(x$exogenous,names(xcat))
      #exv1 <- exv2 <- matrix(nrow=1,ncol=0)
      #if(length(xnum)>0) exv1 <- matrix(colMeans(x$data[,xnum,drop=F],na.rm=T),nrow=1)
      #if(length(xcat)>0) exv2 <- matrix(sapply(xcat,function(y){y[1]}),nrow=1)
      #exv <- data.frame(exv1,exv2)
      #for(i in 1:nOK) exdat <- rbind(exdat,exv)
      #colnames(exdat) <- c(xnum,names(xcat))
      #if(length(xcat)>0) {
      #  for(i in 1:length(xcat)) {
      #    exdat[,names(xcat)[i]] <- factor(exdat[,names(xcat)[i]],levels=xcat[[i]])
      #    }
      #  }
      ind <- rownames(x$data)[nrow(x$data)]
      exdat <- x$data[rep(ind,each=nOK),x$exogenous,drop=F]
      } else {
      exdat <- data.frame(matrix(nrow=nOK,ncol=0))  
      }
    for(i in 1:length(pset)) {
      inam <- names(pset)[i]
      imod <- x$estimate[[inam]]
      isd <- summary(imod)$sigma
      ipar <- pset[[i]]
      if(length(ipar)==0) {
        res[,i] <- rnorm(nOK,predict(imod,exdat),isd)
        } else {
        ipdat <- cbind(exdat,res[,ipar,drop=F])
        res[,i] <- rnorm(nOK,predict(imod,ipdat),isd)
        }
      }
    resOK <- cbind(exdat,res)[(nOK-n+1):nOK,]
    #resOK <- cbind(h=1:n,cbind(exdat,res)[(nOK-n+1):nOK,])
    } else {
    ng <- nlevels(factor(x$data[,x$group]))
    res <- data.frame(matrix(nrow=nOK*ng,ncol=length(pset)))
    colnames(res) <- names(pset)
    datL <- split(x$data,x$data[,x$group])
    ind <- sapply(datL,function(y){rownames(y)[nrow(y)]})
    if(!is.null(x$exogenous)) {
      #exdat <- c()
      #xcat0 <- x$estimate[[1]]$xlevels
      #xcat <- xcat0[setdiff(names(xcat0),x$group)]
      #xnum <- setdiff(x$exogenous,names(xcat))
      #exv <- list()
      #for(i in 1:length(datL)) {
      #  iexv1 <- iexv2 <- matrix(nrow=1,ncol=0)
      #  if(length(xnum)>0) iexv1 <- matrix(colMeans(datL[[i]][,xnum,drop=F],na.rm=T),nrow=1)
      #  if(length(xcat)>0) iexv2 <- matrix(sapply(xcat,function(y){y[1]}),nrow=1)
      #  exv[[i]] <- data.frame(names(datL)[i],iexv1,iexv2)
      #  }
      #for(i in 1:length(exv)) for(j in 1:nOK) exdat <- rbind(exdat,exv[[i]])
      #colnames(exdat) <- c(x$group,xnum,names(xcat))
      #for(i in 1:length(xcat0)) {
      #  exdat[,names(xcat0)[i]] <- factor(exdat[,names(xcat0)[i]],levels=xcat0[[i]])
      #  }
      exdat <- x$data[rep(ind,each=nOK),c(x$group,x$exogenous),drop=F]
      } else {
      exdat <- x$data[rep(ind,each=nOK),x$group,drop=F]  
      }
    for(i in 1:length(pset)) {
      inam <- names(pset)[i]
      imod <- x$estimate[[inam]]
      isd <- summary(imod)$sigma
      ipar <- pset[[i]]
      if(length(ipar)==0) {
        res[,i] <- rnorm(nOK*ng,predict(imod,exdat),isd)
        } else {
        ipdat <- cbind(exdat,res[,ipar,drop=F])
        res[,i] <- rnorm(nOK*ng,predict(imod,ipdat),isd)
        }
      }
    res <- cbind(exdat,res)
    indOK <- c()
    for(i in 1:ng) indOK <- c(indOK,(nOK-n+1+nOK*(i-1)):(nOK*i))
    resOK <- res[indOK,]
    #resOK <- cbind(h=rep(1:n,ng),res[indOK,])
    }
  options(warn=auxwarn)
  rownames(resOK) <- NULL
  resOK
  }

# create graph object from dlsem (internal use only)
makeGraph <- function(x,conf=0.95) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(length(conf)!=1 || !is.numeric(conf) || conf<=0 || conf>=1) stop("Argument 'conf' must be a real number in the interval (0,1)",call.=F)
  nomi <- names(x$estimate)
  G0 <- G <- new("graphNEL",nodes=nomi,edgemode="directed")
  eSign <- c()
  for(i in 1:length(nomi)) {
    isumm <- summary(x$estimate[[nomi[i]]])$coefficients
    auxnam <- rownames(isumm)
    for(j in 1:length(auxnam)) {
      auxsg <- sign(isumm[auxnam[j],1])
      ijnam <- extrName(auxnam[j])
      if(ijnam %in% nomi) {        
        G0 <- addEdge(ijnam,nomi[i],G0,1)
        if(isumm[auxnam[j],4]<1-conf) {
          G <- addEdge(ijnam,nomi[i],G,1)
          if(auxsg>0) {
            eSign <- c(eSign,"+")
            } else {
            eSign <- c(eSign,"-")
            }
          } else {
          eSign <- c(eSign,"")
          }
        names(eSign)[length(eSign)] <- paste(ijnam,"~",nomi[i],sep="")              
        }
      }
    }
  list(graph=G,full.graph=G0,sign=eSign)
  }

# convert into class 'graphNEL'
as.graphNEL <- function(x,conf=0.95,use.ns=FALSE) {
  if(use.ns==F) {
    makeGraph(x,conf=conf)$graph
    } else {
    makeGraph(x,conf=conf)$full.graph
    }
  }

# compute the coefficient associated to each edge at different time lags (internal use only)
edgeCoeff <- function(x,lag=NULL) {
  nomi <- names(x$estimate)
  laglen <- c()
  for(i in 1:length(nomi)) {
    isumm <- summary(x$estimate[[nomi[i]]])$coefficients
    auxnam <- rownames(isumm)
    for(j in 1:length(auxnam)) {
      ijnam <- extrName(auxnam[j])
      if(ijnam %in% nomi) {
        cumb <- lagEff(model=x$estimate[[nomi[i]]],x=ijnam,cumul=F,lag=NULL)
        laglen <- c(laglen,rownames(cumb)[nrow(cumb)])
        }
      }
    }
  meL <- max(as.numeric(laglen))
  lagOK <- sort(unique(c(lag,0:meL)))
  bList <- vector("list",length=length(lagOK))
  for(i in 1:length(nomi)) {
    isumm <- summary(x$estimate[[nomi[i]]])$coefficients
    auxnam <- rownames(isumm)
    for(j in 1:length(auxnam)) {
      ijnam <- extrName(auxnam[j])
      if(ijnam %in% nomi) {
        for(w in 1:length(lagOK)) {
          ijw_eff <- lagEff(model=x$estimate[[nomi[i]]],x=ijnam,cumul=F,lag=lagOK[w])
          bList[[w]] <- rbind(bList[[w]],ijw_eff)
          rownames(bList[[w]])[nrow(bList[[w]])] <- paste(ijnam,"~",nomi[i],sep="")
          }
        }
      }
    if(!is.null(bList)) names(bList) <- lagOK
    }
  for(i in 1:length(bList)) {
    auxnam <- rownames(bList[[i]])
    newnam <- c()
    for(j in 1:length(auxnam)) {
      newnam[j] <- paste(rev(strsplit(auxnam[j],"~")[[1]]),collapse="~")
      }
    rownames(bList[[i]]) <- newnam
    }
  for(i in 1:length(bList)) {
    colnames(bList[[i]]) <- c("estimate","std. err.")
    }
  if(is.null(lag)) {
    bList
    } else {
    voidB <- bList[[1]]
    voidB[] <- 0
    bList2 <- vector("list",length=length(lag))
    names(bList2) <- lag
    for(i in 1:length(bList2)) {
      if(lag[i]<=max(lagOK)) {
        bList2[[i]] <- bList[[as.character(lag[i])]]
        } else {
        bList2[[i]] <- voidB
        }
      }
    bList2
    }
  }

# residual plots for class dlsem
residualPlot <- function(x,type="fr") {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(length(type)!=1 || (type %in% c("fr","qq","ts","ac"))==F) stop("Argument 'type' must one among 'fr','qq', 'ts' and 'ac'",call.=F)
  res <- residuals(x)
  nomi <- setdiff(colnames(res),c(x$group,x$time))
  par(mfrow=n2mfrow(length(nomi)))
  if(type %in% c("ts","ac")) {
    if(!is.null(x$time)) {
      tnam <- x$fitted[,x$time]  
      } else {
      if(is.null(x$group)) {
        tnam <- 1:length(res)
        } else {
        tnam <- do.call(c,(by(x$fitted[,x$group],x$fitted[,x$group],function(z){1:length(z)})))
        }
      }
    }
  if(type=="qq") {
    for(i in 1:length(nomi)) {
      qqnorm(res[,nomi[i]],main=nomi[i])
      qqline(res[,nomi[i]])
      }
    } else if(type=="ts") {
    for(i in 1:length(nomi)) {
      if(is.null(x$group)) {
        plot(res[,nomi[i]],xlab="Time",type="l",ylab="Residual",main=nomi[i])
        } else {
        ires <- do.call(rbind,by(res[,nomi[i]],tnam,quantile,na.rm=T))
        inam <- as.numeric(rownames(ires))
        plot(inam,ires[,3],lwd=2,ylim=range(ires,na.rm=T),xlab="Time",type="l",ylab="Residual",main=nomi[i])
        abline(h=0,col="red")
        lines(inam,ires[,1])
        lines(inam,ires[,2],lty=2)
        lines(inam,ires[,4],lty=2)
        lines(inam,ires[,5])
        }
      }
    } else if(type=="ac") {
    acCalc <- function(z) {
      res <- c(acf(z,plot=F)$acf)
      names(res) <- 0:(length(res)-1)
      res
      }
    for(i in 1:length(nomi)) {
      if(is.null(x$group)) {
        iacf <- acCalc(res[,nomi[i]])
        plot(as.numeric(names(iacf)),iacf,type="n",xlab="Lag",ylab="Residual",main=nomi[i],las=1)        
        grid(col="grey90")
        lines(as.numeric(names(iacf)),iacf)
        box()
        } else {
        auxO <- which(!is.na(res[,nomi[i]]))
        acMat <- do.call(rbind,by(res[auxO,nomi[i]],tnam[auxO],acCalc))
        ires <- t(apply(acMat,2,quantile))
        inam <- as.numeric(rownames(ires))
        plot(inam,ires[,3],type="n",ylim=c(-1,1),xlab="Lag",ylab="Auto-corr.",main=nomi[i],las=1)
        grid(col="grey90")
        lines(inam,ires[,3],lwd=2)
        abline(h=0,col="red")
        lines(inam,ires[,1])
        lines(inam,ires[,2],lty=2)
        lines(inam,ires[,4],lty=2)
        lines(inam,ires[,5])
        box()
        }
      } 
    } else {
    fit <- x$fitted
    for(i in 1:length(nomi)) {
      plot(fit[,nomi[i]],res[,nomi[i]],xlab="Fitted value",ylab="Residual",main=nomi[i])
      abline(h=0)
      }
    }
  par(mfrow=c(1,1))
  }

# plot method for class dlsem
plot.dlsem <- function(x,conf=0.95,style=2,node.col=NULL,font.col=NULL,border.col=NULL,edge.col=NULL,edge.lab=NULL,...) {
  if((style %in% c(0,1,2))==F) stop("Argument 'style' must be either '0' (plain), '1' (significance shown), or '2' (signs shown)",call.=F)
  G <- makeGraph(x,conf=conf)
  nomi <- nodes(G$full.graph)  
  #
  cutString <- function(x,l) {
    n <- nchar(x)
    k <- ceiling(n/l)
    res <- c()
    for(i in 1:k) {
      res[i] <- substr(x,1+(i-1)*l,i*l)
      }
    paste(res,collapse="\n")
    }
  #
  nAttr <- list()
  nAttr$shape <- rep("ellipse",length(nomi))
  nAttr$fontsize <- rep(14,length(nomi))
  nAttr$height <- rep(2.5,length(nomi))
  nAttr$width <- rep(4,length(nomi))
  nAttr$label <- sapply(nomi,cutString,l=12)  ##### maximum number of characters: 12
  for(i in 1:length(nAttr)) {
    names(nAttr[[i]]) <- nomi
    }
  if(!is.null(node.col)) nAttr$fillcolor <- node.col
  if(!is.null(font.col)) nAttr$fontcolor <- font.col
  if(!is.null(border.col)) nAttr$color <- border.col
  eAttr <- list()                                                     
  if(!is.null(edge.lab)) {
    eAttr$label <- edge.lab
    #eAttr$labelfontsize <- rep(14,length(edge.lab))
    #names(eAttr$labelfontsize) <- names(edge.lab)
    }
  if(!is.null(edge.col)) {
    eAttr$color <- edge.col     
    eAttr$color[which(G$sign=="")] <- NA
    } else {
    eCol <- G$sign
    if(style==1) {
      eCol[which(G$sign=="+")] <- "grey30"
      eCol[which(G$sign=="-")] <- "grey30"
      eCol[which(G$sign=="")] <- "grey75"
      } else if(style==2) {
      eCol[which(G$sign=="+")] <- "green4"
      eCol[which(G$sign=="-")] <- "tomato3"
      eCol[which(G$sign=="")] <- "grey75"
      } else {
      eCol[] <- "grey30"
      }
    eAttr[[length(eAttr)+1]] <- eCol
    }
  names(eAttr)[length(eAttr)] <- "color"
  #eStyl <- G$sign
  #eStyl[which(G$sign=="+")] <- "solid"
  #eStyl[which(G$sign=="-")] <- "dashed"
  #eAttr[[length(eAttr)+1]] <- eStyl
  #names(eAttr)[length(eAttr)] <- "style"
  #
  #
  par(xpd=T)
  plot(G$full.graph,"dot",nodeAttrs=nAttr,edgeAttrs=eAttr,attrs=list(edge=list(color="grey25",arrowsize=0.4)),...)
  par(defpar)
  }   

# find the child sets (internal use only)
chldsets <- function(x) {
  pset <- inEdges(x)
  findchld <- function(xname,ps) {names(which(sapply(ps,function(z){xname %in% z})==T))}
  nomi <- names(pset)
  res <- lapply(nomi,findchld,ps=pset)
  names(res) <- nomi
  res
  }

# node markov blanket (internal use only)
nodeMB <- function(nodeName,G) {
  pset <- inEdges(G)
  auxch <- chldsets(G)[[nodeName]]
  auxpar <- pset[[nodeName]]
  auxp <- c()
  if(length(auxch)>0) {
    for(i in 1:length(auxch)) {
      auxp <- c(auxp,pset[[auxch[i]]])
      }
    }
  setdiff(unique(c(auxpar,auxch,auxp)),nodeName)
  }

# node ancestors (internal use only)
nodeAnces <- function(nodeName,G) {
  eList <- inEdges(G)
  xpar <- aux.xpar <- eList[[nodeName]]
  while(length(aux.xpar)>0) {
    newpar <- c()
    for(i in 1:length(aux.xpar)) {
      newpar <- c(newpar,eList[[aux.xpar[i]]])
      }
    xpar <- unique(c(xpar,newpar))
    aux.xpar <- newpar
    }
  unique(xpar)
  }

# node descendants (internal use only)
nodeDescen <- function(nodeName,G) {
  eList <- chldsets(G)
  xpar <- aux.xpar <- eList[[nodeName]]
  while(length(aux.xpar)>0) {
    newpar <- c()
    for(i in 1:length(aux.xpar)) {
      newpar <- c(newpar,eList[[aux.xpar[i]]])
      }
    xpar <- c(xpar,newpar)
    aux.xpar <- newpar
    }
  unique(xpar)
  }

# find topological order (internal use only)
topOrder <- function(G) {
  parSet <- inEdges(G)
  nomi <- names(parSet)
  L <- c()
  S <- nomi[which(sapply(parSet,length)==0)] 
  while(length(S)>0) {
    xaux <- S[1]
    S <- setdiff(S,xaux)
    L <- c(L,xaux)
    sch <- c()
    for(j in 1:length(parSet)) {
      if(xaux %in% parSet[[j]]) sch <- c(sch,nomi[j])
      }
    if(length(sch)>0) {
      for(j in 1:length(sch)) {
        parSet[[sch[j]]] <- setdiff(parSet[[sch[j]]],xaux)
        if(length(parSet[[sch[j]]])==0) S <- c(S,sch[j])  
        }
      }
    }
  if(sum(sapply(parSet,length))==0) L else NULL
  }

# ancestral graph (internal use only)
angraph <- function(x,G) {
  xanc <- unlist(sapply(x,nodeAnces,G))
  xOK <- unique(c(x,xanc))
  subGraph(xOK,G)
  }

# moral graph (internal use only)
morgraph <- function(G) {
  pset <- inEdges(G)
  nomi <- names(pset)
  for(i in 1:length(nomi)) {
    ipar <- pset[[nomi[i]]]
    if(length(ipar)>2) {
      for(j in 1:(length(ipar)-1)) {
        for(w in (j+1):length(ipar)) {
          if((ipar[j] %in% pset[[ipar[w]]])==F) G <- addEdge(ipar[j],ipar[w],G,1)
          }
        }
      }
    }
  newpset <- inEdges(G)  
  W <- new("graphNEL",nodes=nomi,edgemode="undirected")    
  for(i in 1:length(nomi)) {
    ips <- newpset[[i]]
    if(length(ips)>0) {
      for(j in 1:length(ips)) {
        W <- addEdge(ips[j],nomi[i],W,1) 
        }
      }
    }  
  W
  }

# check conditional independence
isIndep <- function(x,var1=NULL,var2=NULL,given=NULL,conf=0.95,use.ns=FALSE) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(is.null(var1) || is.na(var1)) stop("Argument 'var1' is missing",call.=F)
  if(!is.null(var1) && length(var1)!=1) stop("Argument 'var1' must be of length 1",call.=F)
  if(is.null(var2) || is.na(var2)) stop("Argument 'var2' is missing",call.=F)  
  if(!is.null(var2) && length(var2)!=1) stop("Argument 'var2' must be of length 1",call.=F)
  if(!is.null(given) && is.na(given)) stop("Argument 'given' is missing",call.=F)
  if(length(conf)!=1 || !is.numeric(conf) || conf<=0 || conf>=1) stop("Argument 'conf' must be a real number in the interval (0,1)",call.=F)
  if(length(use.ns)!=1 || !is.logical(use.ns)) stop("Argument 'use.ns' must be a logical value",call.=F)
  Gobj <- makeGraph(x,conf=conf)
  if(use.ns==F) {
    G <- Gobj$graph
    } else {
    G <- Gobj$full.graph
    }
  nomi <- nodes(G)
  auxcheck <- setdiff(c(var1,var2,given),nomi)
  if(length(auxcheck)>0) stop("Unknown variable '",auxcheck[1],"'",sep="",call.=F)
  if(length(unlist(chldsets(G)))>0) {
    Gm <- morgraph(angraph(c(var1,var2,given),G))
    pset <- chldsets(Gm)                      
    xedg <- c()
    for(i in 1:length(pset)) {
      if(length(pset[[i]])>0) {
        for(j in 1:length(pset[[i]])) {
          xedg <- rbind(xedg,c(names(pset)[i],pset[[i]][j])) 
          }
        }
      }                              
    if(!is.null(xedg)) {
      xedg <- rbind(xedg,xedg[,2:1])
      nomi <- sort(unique(xedg))
      borde <- var1
      reached <- c()
      neighb <- function(x,node) {
        Ne <- c(x[which(x[,1]==node),2],x[which(x[,2]==node),1])
        sort(unique(Ne))
        }                     
      while(length(borde)>0) {
        reached <- c(reached,borde)
        fan_borde <- c()
        for(i in 1:length(borde)) {                      
          auxne <- neighb(xedg,borde[i])                 
          fan_borde <- c(fan_borde,auxne) 
          }                              
        borde <- setdiff(fan_borde,c(reached,given))   
        if(length(intersect(borde,var2))>0) break()
        }
      ifelse(length(borde)>0, res <- F, res <- T)
      } else {
      res <- T
      }
    } else {
    res <- T
    }
  res
  }

# matrix of edges (internal use only)
edgeMat <- function(x,conf,full) {
  if(full==T) {
    xedg <- chldsets(makeGraph(x,conf=conf)$full.graph)
    } else {
    xedg <- chldsets(makeGraph(x,conf=conf)$graph)
    }
  res <- c()
  if(length(xedg)>0) {
    for(i in 1:length(xedg)) {
      if(length(xedg[[i]])>0) {
        res <- rbind(res,cbind(rep(names(xedg)[i],length(xedg[[i]])),xedg[[i]]))
        }
      }
    }
  res
  }

# find directed path (internal use only)
dpathFind <- function(G,from,to) {
  auxedg <- chldsets(G)
  if(to %in% nodeDescen(from,G)) {
    auxmat <- c()
    for(i in 1:length(auxedg)) {
      if(length(auxedg[[i]])>0) {
        for(j in 1:length(auxedg[[i]])) {
          auxmat <- rbind(auxmat,c(names(auxedg)[i],auxedg[[i]][j]))
          }
        }
      }
    auxchld <- auxmat[which(auxmat[,1]==from),2]
    pathList <- list()
    for(i in 1:length(auxchld)) pathList[[i]] <- c(from,auxchld[i])
    endcheck <- function(x) {
      res <- rep(0,length(x))                 
      for(i in 1:length(x)) {                
        if(rev(x[[i]])[1]==to) res[i] <- 1
        }
      res
      }                        
    isOK <- endcheck(pathList)               
    while(sum(isOK)<length(isOK)) {
      auxind <- which(isOK==0)[1]         
      auxchld <- auxmat[which(auxmat[,1]==rev(pathList[[auxind]])[1]),2]
      auxpath <- pathList[[auxind]]
      if(length(auxchld)>0) {
        pathList[[auxind]] <- c(auxpath,auxchld[1])
        if(length(auxchld)>1) {
          for(i in 2:length(auxchld)) {
            pathList[[length(pathList)+1]] <- c(auxpath,auxchld[i])
            }
          }   
        } else {
        pathList <- pathList[-auxind]
        }                       
      isOK <- endcheck(pathList)  
      }
    pathList
    } else {
    NULL
    }
  } 

# find lag to sum (internal use only)
findlag2sum <- function(x,lag) {
  g <- length(x)                             
  out <- list()
  for(w in 1:length(lag)) {
    mycode <- "res <- c(); "
    for(i in 1:g) {
      mycode <- paste(mycode,"for(k",i," in 1:length(x[[",i,"]])) { ; ",sep="")
      }
    mycode <- paste(mycode,paste("xaux <- c(",paste("x[[",1:g,"]][k",1:g,"]",collapse=",",sep=""),"); ",sep=""),sep="")
    mycode <- paste(mycode,"if(sum(xaux)==lag[w]) { res <- rbind(res,xaux) }; ",sep="")
    mycode <- paste(mycode,paste(rep("}",length(x)),collapse="; "),sep="")
    eval(parse(text=mycode))
    if(is.null(res)) {
      res <- matrix(nrow=0,ncol=g)
      } else {
      rownames(res) <- NULL
      }
    colnames(res) <- names(x)             
    out[[w]] <- res
    }
  names(out) <- lag
  out
  }


# table for cumulative lag shape (internal use only)
cumulCalc <- function(x) {
  #serr <- x[which.max(abs(x[,1])),2]
  #wei <- x[,1]/max(abs(x[,1]))
  se_cum <- c()
  for(i in 1:nrow(x)) {
    #se_cum[i] <- sqrt(sum(wei[1:i]^2))*serr
    se_cum[i] <- sqrt(sum(x[1:i,2]^2))
    }
  res <- x
  res[,1] <- cumsum(x[,1])
  res[,2] <- se_cum
  res
  }   

# computation of causal effects
causalEff <- function(x,from=NULL,to=NULL,lag=NULL,cumul=FALSE,conf=0.95,use.ns=FALSE) {
  if(("dlsem" %in% class(x))==F) stop("Argument 'x' must be an object of class 'dlsem'",call.=F)
  if(is.null(from) || is.na(from)) stop("Argument 'from' is missing",call.=F)
  if(is.null(to) || is.na(to)) stop("Argument 'to' is missing",call.=F)
  if(!is.character(from)) stop("Invalid argument 'from'",call.=F)
  if(!is.character(to)) stop("Invalid argument 'to'",call.=F)
  if(length(to)!=1) stop("Argument 'to' must be of length 1",call.=F)
  if(length(cumul)!=1 || !is.logical(cumul)) stop("Argument 'cumul' must be a logical value",call.=F)
  if(length(conf)!=1 || !is.numeric(conf) || conf<=0 || conf>=1) stop("Argument 'conf' must be a real number in the interval (0,1)",call.=F)
  if(length(use.ns)!=1 || !is.logical(use.ns)) stop("Argument 'use.ns' must be a logical value",call.=F)
  if(!is.null(lag)) {
    for(i in 1:length(lag)) {
      if(!is.numeric(lag[i]) || is.na(lag[i]) || lag[i]<0 || lag[i]!=round(lag[i])) stop("Argument 'lag' must contain non-negative integer numbers only",call.=F)
      }
    }
  auxcheck <- setdiff(c(from,to),names(x$estimate))
  if(length(auxcheck)>0) {
    auxcntx <- intersect(auxcheck,x$exogenous)
    if(length(auxcntx)>0) {
      stop("Variable '",auxcntx[1],"' is exogenous and cannot appear in argument 'from' or 'to'",call.=F)
      } else {
      stop("Unknown variable '",auxcheck[1],"'",sep="",call.=F)
      }
    }
  Gobj <- makeGraph(x,conf=conf)
  if(use.ns==F) {
    G <- Gobj$graph
    } else {
    G <- Gobj$full.graph
    }
  nomi <- nodes(G)
  if(length(setdiff(from,nomi))>0) stop("Unknown variable '",setdiff(from,nomi)[1],"'",sep="",call.=F)
  if((to %in% nomi)==F) stop("Unknown variable '",to,"'",sep="",call.=F)
  isOK <- rep(1,length(from))
  for(i in 1:length(from)) {
    if((to %in% nodeDescen(from[i],G))==F) isOK[i] <- 0
    }
  ###
  pset <- inEdges(G)
  for(i in 1:length(from)) {
    ipa <- pset[[from[[i]]]]
    if(length(ipa)>0) {
      for(j in 1:length(ipa)) {
        G <- removeEdge(ipa[j],from[i],G)
        }
      }
    }  
  ###
  if(sum(isOK)>0) {
    from <- from[which(isOK==1)]
    mycol1 <- mycol2 <- rep(NA,length(nomi))
    names(mycol1) <- names(mycol2) <- nomi
    nodemed <- c()
    pathList <- vector("list",length=length(from))
    for(i in 1:length(from)) {
      pathList[[i]] <- dpathFind(G,from=from[i],to=to)
      nodemed <- c(nodemed,setdiff(unlist(pathList[[i]]),c(from,to)))
      }
    nodemed <- unique(nodemed)
    names(pathList) <- from
    auxdel <- which(sapply(pathList,is.null)==T)
    if(length(auxdel)>0) {
      pathList <- pathList[-auxdel]
      from <- from[-auxdel]
      }
    nodecond <- setdiff(unlist(pset[c(to,nodemed)]),c(from,nodemed))
    nodebarr <- setdiff(nomi,c(from,to,nodemed,nodecond))
    mycol1[c(from,to)] <- mycol2[c(from,to)] <- "grey20"
    mycol1[nodemed] <- mycol2[nodemed] <- "grey20"
    mycol1[nodecond] <- "navy"
    mycol2[nodecond] <- "grey70"
    mycol1[nodebarr] <- mycol2[nodebarr] <- "grey70"
    xedg <- chldsets(G)
    ednam <- list()
    for(i in 1:length(xedg)) {
      if(length(xedg[[i]])>0) ednam[[i]] <- paste(names(xedg)[i],"~",xedg[[i]],sep="")
      }
    ednam <- unlist(ednam)
    eddel <- c()
    for(i in 1:length(nodebarr)) {
      eddel <- c(eddel,paste(nodebarr[i],"~",setdiff(nomi,nodebarr[i]),sep=""),paste(setdiff(nomi,nodebarr[i]),"~",nodebarr[i],sep=""))
      }
    for(i in 1:length(nodecond)) {
      eddel <- c(eddel,paste(nodecond[i],"~",setdiff(nomi,nodecond[i]),sep=""),paste(setdiff(nomi,nodecond[i]),"~",nodecond[i],sep=""))
      }
    edcol <- rep("grey70",length(Gobj$sign))
    names(edcol) <- names(Gobj$sign)
    edcol[intersect(setdiff(ednam,eddel),names(which(Gobj$sign=="+")))] <- "green4"
    edcol[intersect(setdiff(ednam,eddel),names(which(Gobj$sign=="-")))] <- "tomato3"
    newPathList <- list()
    for(i in 1:length(pathList)) {
      newPathList <- c(newPathList,pathList[[i]])
      }                                                        
    laglen <- list()
    for(i in 1:length(newPathList)) {
      jlaglen <- list()
      for(j in 2:length(newPathList[[i]])) {
        auxnam <- paste(newPathList[[i]][j],"~",newPathList[[i]][j-1],sep="")
        auxeff <- lagEff(model=x$estimate[[newPathList[[i]][j]]],x=newPathList[[i]][j-1],cumul=F,lag=NULL)
        auxpos <- which(auxeff[,1]!=0)
        if(length(auxpos)>0) {
          jlaglen[[j-1]] <- as.numeric(rownames(auxeff)[auxpos])
          } else {
          jlaglen[[j-1]] <- NA
          }
        }
      laglen[[i]] <- c(min(jlaglen[[1]],na.rm=T),sum(sapply(jlaglen,max,na.rm=T)))
      }
    meL <- max(unlist(laglen))+1        
    lagOK <- 0:meL
    quan <- -qnorm((1-conf)/2)
    #
    sd_calc <- function(muvet,sdvet) { sqrt(prod(muvet^2+sdvet^2)-prod(muvet^2)) }
    #
    bhat <- edgeCoeff(x,lag=lagOK)
    outList <- list()                       
    for(i in 1:length(newPathList)) {           
      outList[[i]] <- matrix(nrow=length(lagOK),ncol=2)
      rownames(outList[[i]]) <- lagOK
      colnames(outList[[i]]) <- c("estimate","std. error")
      auxbetalag <- list()
      for(j in 2:length(newPathList[[i]])) {
        auxnam <- paste(newPathList[[i]][j],"~",newPathList[[i]][j-1],sep="")
        auxeff <- lagEff(model=x$estimate[[newPathList[[i]][j]]],x=newPathList[[i]][j-1],cumul=F,lag=lagOK)
        auxpos <- which(auxeff[,1]!=0)
        if(length(auxpos)>0) {                                 
          auxbetalag[[j-1]] <- as.numeric(rownames(auxeff)[auxpos])
          } else {
          auxbetalag[[j-1]] <- 0
          }
        names(auxbetalag)[j-1] <- auxnam
        }
      lagsumMat <- findlag2sum(auxbetalag,lagOK)
      for(j in 1:length(lagsumMat)) {
        auxlag <- as.character(lagOK[j])                           
        auxind <- lagsumMat[[j]]                          
        if(nrow(auxind)>0) {
          auxres <- array(dim=c(nrow(auxind),ncol(auxind),2))
          for(w1 in 1:nrow(auxind)) {                          
            for(w2 in 1:ncol(auxind)) {
              auxres[w1,w2,1] <- bhat[[as.character(auxind[w1,w2])]][colnames(auxind)[w2],1]
              auxres[w1,w2,2] <- bhat[[as.character(auxind[w1,w2])]][colnames(auxind)[w2],2]
              }
            }                                                           
          muprod <- sdprod <- c()
          for(w in 1:nrow(auxind)) {                               
            muprod[w] <- prod(auxres[w,,1])                              
            sdprod[w] <- sd_calc(auxres[w,,1],auxres[w,,2])
            }
          mupath <- sum(muprod)
          sdpath <- sqrt(sum(sdprod^2))
          outList[[i]][j,] <- c(mupath,sdpath)
          } else {
          outList[[i]][j,] <- rep(0,2)
          }
        }
      }
    names(outList) <- sapply(newPathList,function(x){paste(x,collapse="*")})
    out <- matrix(nrow=length(lagOK),ncol=2)
    rownames(out) <- lagOK
    colnames(out) <- c("estimate","std. error")
    for(j in 1:length(lagOK)) {
      auxover <- rep(0,2)
      for(i in 1:length(outList)) {
        auxover <- auxover+outList[[i]][j,]
        }
      out[j,] <- auxover
      }
    outList[[length(outList)+1]] <- out
    names(outList)[[length(outList)]] <- "overall"
    if(cumul==T) {
      for(i in 1:length(outList)) {
        if(nrow(outList[[i]])>1) outList[[i]] <- cumulCalc(outList[[i]])
        }    
      }
    #
    for(i in 1:length(outList)) {
      imu <- outList[[i]][,1]
      isd <- outList[[i]][,2]
      outList[[i]] <- cbind(imu,isd,imu-quan*isd,imu+quan*isd)
      colnames(outList[[i]]) <- c("estimate","std. err.",paste(c("lower ","upper "),conf*100,"%",sep=""))
      }
    #
    if(is.null(lag)) {
      outList
      } else {
      lagSel <- lag
      auxOlag <- which(lag>max(lagOK))
      if(length(auxOlag)>0) lagSel[auxOlag] <- max(lagOK)  
      outList2 <- outList
      for(i in 1:length(outList)) {
        outList2[[i]] <- outList[[i]][as.character(lagSel),,drop=F]
        rownames(outList2[[i]]) <- lag
        }
      outList2
      }
    } else {
    auxanc <- nodeAnces(to,makeGraph(x)$full.graph)
    if(length(intersect(from,auxanc))>0) {
      #stop("No paths found connecting the selected variables. Try to reduce 'conf' or to set 'use.ns' to TRUE",call.=F)
      #} else {
      #stop("No paths exist connecting the selected variables",call.=F)
      NULL
      }
    }
  }
