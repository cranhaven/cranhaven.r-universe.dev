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
###
### INTERNAL FUNCTIONS
###

######################################################################
### INTERNAL: CONFIDENCE INTERVAL

### find a root of y(x) == y0
## assume y(x) is monotone
##
## x0 : a starting value for search
## y0,x,y : to define the equation
## w : weight for lsfit
## tol : tolerance for chekcing monotonicity

sbfindroot <- function(x0,y0,x,y,w=rep(1,length(x)),tol=0) {
  ## determine if increasing or decreasing
  f <- lsfit(x,y,w)
  if(f$coef[2]<0) {
    y <- -y  # modify as increasing
    y0 <- -y0
  }

  ## prepare
  o <- order(x)
  x <- x[o]; y <- y[o]; w <- w[o]
  n <- length(x)
  i0 <- sum(x<=x0)
  if(i0==0 || i0==n) return(NULL)
  i1 <- i0; i2 <- i0+1
  ymin <- y[i1]; ymax <- y[i2]
  
  ## find a range which includes y[i1] <= y0 <= y[i2]
  for(k in 1:1000) { # should terminate, but for safty
    if(y[i1]>y0) {
      i1 <- i1 - 1; if(i1<1) return(NULL)
      if(y[i1] < ymin) ymin <- y[i1]
      else if(y[i1]-ymin>tol) return(NULL)
    } else {
      i1 <- i1 + 1; if(i1>n) return(NULL)
      if(y[i1]>y0) i1 <- i1 - 1
    }
    if(y[i2]<y0) {
      i2 <- i2 + 1; if(i2>n) return(NULL)
      if(y[i2] > ymax) ymax <- y[i2]
      else if(ymax-y[i2]>tol) return(NULL)
    } else {
      i2 <- i2 - 1; if(i2<1) return(NULL)
      if(y[i2]<y0) i2 <- i2 +1
    }
    if(i1>=i2) return(NULL)
    if(y[i1]<=y0 && y0<=y[i2]) break
  }

  ## linear interpolation
  xv <- as.numeric(x[i1] + (y0-y[i1])* (x[i2]-x[i1])/(y[i2]-y[i1]))
  ye <- min(abs(y0-y[c(i1,i2)]))  # absolute error in y
  xe <- min(abs(xv-x[c(i1,i2)]))  # absolute error in x
  re <- ye*sqrt((w[i1]+w[i2])/2)  # relative error in y
  return(list(xv=xv,xe=xe,ye=ye,re=re))
}


######################################################################
### MISC: NUMERICAL FUNCTIONS

## optims: Optimization with Multiple Initial Values
##
## Description:
##   Apply optim to muliple initial values and
##   return the minimum of the values.
##
## Arguments:
##  coefs : n x m matrix. m sets of initial values of  n dimensions.
##  fn : function to be minimized.
##  ... : passed to optim.
##
## Value:
##  a list of the same structure as the optim function.
##  value : the minimum returned value.
##  init : the initial value for the minimum returned value.
optims <- function(coefs,fn,chkcoef=NULL,eps=1e-3,...) {
  coef0 <- pos <- numeric(0) # used as global variables in this function
  op <- sboptions()
  if(op$debug) {
    cat("########## optims\n")
    print(coefs)
  }
  
  ## objective function with constraints
  fn1 <- function(coef) {
    ## coef0 is used as a container, where only pos are changed    
    coef0[pos] <- coef
    fn(coef0)
  }
  ## optim1: one-dimensional optimization; length(pos)==1
  optim1 <- function(coef) {
    fit <- optimize(fn1,c(coef-10,coef+10))
    x0 <- fit$minimum
    f0 <- fn1(x0);
    fp <- fn1(x0+eps); fm <- fn1(x0-eps)
    hess <- ((fp-f0)-(f0-fm))/(eps*eps)
    list(par=x0,value=f0,hessian=hess)
  }
  ## optim2: switch optim1 and optim depending on length(pos)
  optim2 <- function(coef) {
    len <- length(coef)
    if(len!=length(pos)) stop("internal error")
    if(len>=2) {
      f <- optim(coef,fn1,hessian=T,...)
    } else if(len==1) {
      f <- optim1(coef)
    } else {
      return(list(par=NULL,value=fn(coef0)))
    }
    f
  }

  ## use each column as a initial parameter vector
  coefs <- as.matrix(coefs)
  m <- ncol(coefs); len <- nrow(coefs)
  fit0 <- list(value=Inf)
  for(i in 1:m) {
    coef0 <- init <- coefs[,i]
    pos <- 1:len
    mask <- rep(T,len)
    if(is.null(chkcoef)) {
      fit <- optim2(coef0) # unconstrained optimizaiton
    } else {
      for(j in 1:(len+1)) { # should not be repeated more than len+1
        par <- coef0[pos]
        fit <- optim2(par)
        if(length(pos)>0) coef0[pos] <- fit$par
        y <- chkcoef(coef0)
        if(op$debug) {
          cat("##### ",j,"\n")
          print(pos)
          print(mask)
          print(coef0)
          print(y)
          print(fit)
        }
        if(is.null(y)) break
        coef0 <- y$par
        if(all(y$mask == mask)) break
        mask <- y$mask; pos <- which(y$mask)        
      }
      fit$par <- coef0
    }
    fit$init <- init
    fit$pos <- pos
    fit$mask <- mask
    if(fit$value<=fit0$value) fit0 <- fit
  }
  if(op$debug) {
    cat("#####\n")
    print(fit0)
  }
  fit0$var <- matrix(0,len,len)
  if(length(fit0$pos)>=1) fit0$var[fit0$pos,fit0$pos] <- solvex(fit0$hessian)
  names(fit0$par) <- rownames(coefs)

  fit0
}

## solvex: a stable inverse of symmetric matrix
solvex <- function(x,tol=1e-16) {
  e <- eigen(x,symmetric=T) # eigen values and vectors
  val <- e$val # eigen values
  ##  u <- abs(val) < tol # almost zero values
  u <- val < tol
  ## val[u] <- 0 # set to zero (genaralized inverse)
  val[u] <- NA # set to NA
  ##  val[u] <- 1/tol # set to a large number
  val[!u] <- 1/val[!u] # take the inverse
  a <- e$vec %*% diag(val,nrow=length(val)) %*% t(e$vec)
  dimnames(a) <- dimnames(x)
  a
}

## logx : allow negative x (linear extrapolation for x smaller than 1e-10)
## print(log(1e-10),digits=15)
## [1] -23.0258509299405
logx <- function(x) {
  a <- x<1e-10
  if(sum(a)>0) {
    x[a] <-  x[a]*1e10 - 24.0258509299405
    x[!a] <- log(x[!a])
  } else {
    x <- log(x)
  }
  x
}

## sqrtx : The square root of x allowing negative x values
sqrtx <- function(x) {
  x <- drop(x); sign(x)*sqrt(abs(x)) 
}

## expsx and logsx (exp for large x, linear for small x)
expsx <- function(x) sign(x)*(exp(abs(x))-1)
logsx <- function(x) sign(x)*log(abs(x)+1)

## qnorm2
qnorm2 <- function(x) {
  x[x>1] <- 1
  x[x<0] <- 0
  qnorm(x)
}

## sbbpxtab : convert (bp,bpm) to a table of bp's
##
## bpx = (bp,bpm)
## bp = Pr( obs = (1,?) )
## bpm = c( Pr(obs=(?,1)), Pr(obs=(1,1)) )
##
## x[1,1] = Pr( obs=(0,0) )
## x[1,2] = Pr( obs=(0,1) )
## x[2,1] = Pr( obs=(1,0) )
## x[2,2] = Pr( obs=(1,1) )
##
sbbpxtab <- function(bpx) {
  x <- matrix(0,2,2)
  x[2,2] <- bpx[3]
  x[1,2] <- bpx[2] - bpx[3]
  x[2,1] <- bpx[1] - bpx[3]
  x[1,1] <- 1 - (bpx[1]+bpx[2]-bpx[3])
  x
}


## prnorm2 : probability
##
## zi ~ N(0,1), i=1,2; correlation=r
## calculate P(z1 < a1 & z2 < a2)
prnorm2 <- function(a1,a2,r,tol=1e-6) 
  pmvnorm(mean=c(0,0),sigma=matrix(c(1,r,r,1),2,2),
          lower=rep(-Inf,2),upper=c(a1,a2),maxpts=1e7,abseps=tol)

## denorm2 : density
denorm2 <- function(a1,a2,r) {
  b <- 1/(1-r^2)
  exp( -0.5*b*((a1-a2)^2 + 2*(1-r)*a1*a2) )*sqrt(b)*0.5/pi
}


## prnorm4
##
## zi ~ N(0,1), i=1,2; correlation=r
## calculate P(b1 < z1 < a1 & b2 < z2 < a2)
prnorm4 <- function(a1,a2,b1,b2,r,tol=1e-6) {
  if(b1>=a1 || b2>=a2) return(0)
  pmvnorm(mean=c(0,0),sigma=matrix(c(1,r,r,1),2,2),
          lower=c(b1,b2),upper=c(a1,a2),maxpts=1e7,abseps=tol)
}

## weighted average of z-values
wsumzval <- function(z,w) {
  i0 <- order(-w)[1] # argmax(w)
  wsum <- function(x) {
    lowtail <- x[i0] < 0
    qnorm(sum(w*pnorm(x,lower.tail=lowtail)),lower.tail=lowtail)
  }
  if(is.array(z)) apply(z,1:(length(dim(z))-1),wsum)
  else wsum(z)
}

## wzval for difference models (poa and sia)
## out=-qnorm(pnorm(-z1) + w*pnorm(-z2))
## =qnorm2(pnorm(z1) - w*pnorm(-z2) ) 
wzval <- function(z1,z2,w) {
  if(z1>0) {
    p <- pnorm(-z1) + w*pnorm(-z2)
    if(p>1) p <- 2-p
    else if(p<0) p <- 0
    -qnorm(p)
  } else {
    p <- pnorm(z1) - w*pnorm(-z2)
    if(p<0) p <- -p
    else if(p>1) p <- 1
    qnorm(p)
  }
}

## nderiv: numerical derivative
nderiv <- function(fn,x,k=1,eps=1e-3) {
  if(k==1) {
    apply(diag(length(x))*eps,1,
          function(h) 0.5*(fn(x+h)-fn(x-h))/eps)
  } else if(k<=0) {
    fn(x)
  } else {
    apply(diag(length(x))*eps,1,
          function(h) 0.5*(nderiv(fn,x+h,k-1)-nderiv(fn,x-h,k-1))/eps)
  }
}

## sebp: standard error of bootstrap probability
sebp <- function(bp,nb) sqrt(bp*(1-bp)/nb)

######################################################################
### MISC: TEXT FORMAT FUNCTIONS

## myformat
myformat <- function(x,s=NULL,digits=6,h=NULL) {
  ux <- is.na(x); x <- round(x,digits)
  x <- format(x,nsmall=digits)
  x[ux] <- "" # print nothing for NA
  y <- format(x,justify="right")
  if(!is.null(s)) {
    us <- is.na(s); s <- round(s,digits)    
    s <- format(s,nsmall=digits)
    s[us] <- "oo" # infinity sign for NA
    lp <- rep("(",length(s)); rp <- rep(")",length(s))
    ## print nothing for which x is NA
    s[ux] <- ""; lp[ux] <- " "; rp[ux] <- " "; 
    y <- paste(y," ",lp,format(s,justify="right"),rp,sep="")
  }
  if(!is.null(h)) y <- format(c(h,y))
  y
}

## catmat
catmat <- function(x,rn=rownames(x),cn=colnames(x),sep=" ",file="") {
  y <- x
  if(!is.null(rn)) y <- cbind(format(rn),y)
  if(!is.null(cn)) {
    if(!is.null(rn)) cn <- c("",cn)
    y <- rbind(cn,y)
    y <- apply(y,2,function(a) format(a))
  }
  apply(y,1,function(a) cat(a,"\n",sep=sep,file=file,append=T))
  invisible(y)
}

## catpval
## internal for print pval
catpval <- function(pv,pe,digits=NULL,lambda=NULL) {
  op <- sboptions()
  if(is.null(digits)) digits <- op$digits.pval
  if(op$percent) {
    name <- "percent"
    if(missing(pe)) value <- myformat(100*pv,digits=digits)
    else value <- myformat(100*pv,100*pe,digits=digits)    
  } else {
    name <- "probability"
    if(missing(pe)) value <- myformat(pv,digits=digits+2)
    else value <- myformat(pv,pe,digits=digits+2)
  }
  if(!is.null(lambda)) {
    if(lambda==0) lambda <- "Bayesian"
    else if(lambda==1) lambda <- "Frequentist"
  }
  
  list(name=name,value=value,lambda=lambda)
}



######################################################################
### MISC: OTHER FUNCTIONS

### transpose of matrix
## output: t(x) for a matrix
trmat <- function(x,col=T) {
  if(is.matrix(x)) t(x)
  else if(col) as.matrix(x) # column vector
  else t(as.matrix(x)) # row vector
}

### matrix as is
## output: x for a matrix
asmat <- function(x,col=F) {
  if(is.matrix(x)) x
  else if(col) as.matrix(x) # column vector
  else t(as.matrix(x)) # row vector
}

### simplist
## output: simplified vector or matrix of list
simplist <- function(a) sapply(a,function(x) x)

### column-wise apply (with same dimensions)
capply <- function(x,fun,...) {
  y <- apply(x,2,fun,...)
  if(is.vector(y))  y <- t(as.matrix(y))
  dimnames(y) <- dimnames(x)
  y
}

### calc max diff
##
## y[i] := max_{j neq i} x[j] - x[i]
##
maxdif <- function(x) {
  i1 <- which.max(x)  # the largest element
  x <- -x + x[i1]
  x[i1] <- -min(x[-i1])  # the second largest value
  x
}

### calc assmaxdif
##
## y[[i]][j] := max_{k neq ass[[i]]} x[k] - x[ass[[i]][j]]
##
assmaxdif <-  function(x,a) {
  y <- vector("list",length(a))
  names(y) <- names(a)
  for(i in seq(along=a))  y[[i]] <- max(x[ -a[[i]] ]) - x[ a[[i]] ]
  y
}

### sum of row vectors
##
## x = matrix (array of row vectors)
## i = indices (for rows)
##
sumrow <- function(x,i) {
  apply(x[i,,drop=F],2,mean)*nrow(x)
}

### weighted sum of row vectors
##
## x = matrix (array of row vectors)
## w = weight vector (for rows)
##
wsumrow <- function(x,w) {
  apply(w*x,2,sum)*nrow(x)/sum(w)
}

### split numbers
## x: vector of interges
## m: number of splits
##
## output: y = matrix of m*length(x)
## such that apply(y,2,sum) = x
## y[i,] is approximately x/m
splitnum <- function(x,m) {
  n <- length(x)
  xm <- floor(x/m)
  y <- t(matrix(xm,n,m)) # initial distribution
  d <- x - apply(y,2,sum) # left over
  i <- 0
  for(j in 1:n) if(d[j]>0) {
    i <- 1+(seq(from=1+i[length(i)],length=d[j])-1) %% m
    y[i,j] <- y[i,j]+1
  }
  y
}

