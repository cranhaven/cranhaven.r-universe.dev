plotCoef <- function(beta,norm,lambda,df,dev,label=FALSE,
                     xvar=c("norm","lambda","dev"),
                     xlab=iname,...){

  which <- nonzeroCoef(beta)
  switch(length(which)+1,#we add one to make switch work
         "0"={warning("No plot produced since all coefficients zero")
              return()
         },
         "1"=warning("1 or less nonzero coefficients; classo plot is not meaningful")
  )

  # ------------------------------------------------ #

  beta <- as.matrix(beta[which,,drop=FALSE])
  xvar <- match.arg(xvar)
  switch(xvar,
         "norm"={ index=if(missing(norm))apply(abs(beta),2,sum)else norm
                   iname="L1 Norm (Modulus)"
                   approx.f=1 },
         "lambda"= { index=log(lambda)
                     iname= expression(Log(lambda))
                     approx.f=0 },
        "dev"= {
          index=dev
          iname="Fraction Deviance Explained"
          approx.f=1
        }
  )

  # ------------------------------------------------ #
  
  dotlist <- list(...)
  type <- dotlist$type
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
  if (is.null(type)) {
    # Real part:
    matplot(index, t(Re(beta)), lty = 1, xlab = xlab, ylab = "Coefficients (Re)", type = "l", ...)
    
    which <- nonzeroCoef(Re(beta))
    atdf <- pretty(index)
    prettydf <- approx(x=index,y=df,xout=atdf,rule=2,method="constant",f=approx.f)$y
    axis(3,at=atdf,labels=prettydf,tcl=NA)
    if(label){
      nnz <- length(which)
      xpos <- max(index)
      pos <- 4
      if(xvar=="lambda"){
        xpos <- min(index)
        pos <- 2
      }
      xpos <- rep(xpos,nnz)
      ypos <- abs(Re(beta)[,ncol(Re(beta))])
      text(xpos,ypos,paste(which),cex=.5,pos=pos)
    }
    
    # ------------------------------------------------ #
    # Imaginary part:
    matplot(index, t(Im(beta)), lty = 1, xlab = xlab, ylab = "Coefficients (Im)", type = "l", ...)
    
    which <- nonzeroCoef(Im(beta))
    atdf <- pretty(index)
    prettydf <- approx(x=index,y=df,xout=atdf,rule=2,method="constant",f=approx.f)$y
    axis(3,at=atdf,labels=prettydf,tcl=NA)
    if(label){
      nnz <- length(which)
      xpos <- max(index)
      pos <- 4
      if(xvar=="lambda"){
        xpos <- min(index)
        pos <- 2
      }
      xpos <- rep(xpos,nnz)
      ypos <- abs(Im(beta)[,ncol(Im(beta))])
      text(xpos,ypos,paste(which),cex=.5,pos=pos)
    }
    
  } else {
    # Real part:
    matplot(index, t(Re(beta)), lty = 1, xlab = xlab, ylab = "Coefficients (Re)", ...)
    
    which <- nonzeroCoef(Re(beta))
    atdf <- pretty(index)
    prettydf <- approx(x=index,y=df,xout=atdf,rule=2,method="constant",f=approx.f)$y
    axis(3,at=atdf,labels=prettydf,tcl=NA)
    if(label){
      nnz <- length(which)
      xpos <- max(index)
      pos <- 4
      if(xvar=="lambda"){
        xpos <- min(index)
        pos <- 2
      }
      xpos <- rep(xpos,nnz)
      ypos <- abs(Re(beta)[,ncol(Re(beta))])
      text(xpos,ypos,paste(which),cex=.5,pos=pos)
    }
    
    # ------------------------------------------------ #
    # Imaginary part:
    matplot(index, t(Im(beta)), lty = 1, xlab = xlab, ylab = "Coefficients (Im)", ...)
    
    which <- nonzeroCoef(Im(beta))
    atdf <- pretty(index)
    prettydf <- approx(x=index,y=df,xout=atdf,rule=2,method="constant",f=approx.f)$y
    axis(3,at=atdf,labels=prettydf,tcl=NA)
    if(label){
      nnz <- length(which)
      xpos <- max(index)
      pos <- 4
      if(xvar=="lambda"){
        xpos <- min(index)
        pos <- 2
      }
      xpos <- rep(xpos,nnz)
      ypos <- abs(Im(beta)[,ncol(Im(beta))])
      text(xpos,ypos,paste(which),cex=.5,pos=pos)
    }
  }

}
