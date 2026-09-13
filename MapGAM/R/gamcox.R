#***********************************************************************************
#
# Fit a Cox Additive Model with a Two-Dimensional Smooth
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
gamcox <- function(formula, data, subset, weights = NULL, span=0.5, I.span=0.2, degree=1, 
                   loess.trace="exact", Maxiter=40, tol=1e-7)
  {
    call <- match.call()
    if (missing(data)) 
      data <- environment(formula)
    mf <- match.call()
    m <- match(c("formula", "data","subset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mt <- if (missing(data)) 
      terms(formula, c("lo"))
    else terms(formula, c("lo"), data = data)
    mf$formula <- mt
    mf <- eval(mf, parent.frame())
    order = attr(mt,"specials")$lo
    ## If no smooth term, then Coxph model
    if(is.null(order)){
      if(missing(weights))
      fit = coxph(formula,data[subset,])
      else fit = eval(substitute(coxph(formula,data,weights,subset)))
      rslt <- fit
      rslt$additive.predictors = fit$linear.predictors
      if(!is.null(fit$coefficients)){
        rslt$deviance = -2*fit$loglik[2]
        rslt$var=fit$var
        rslt$aic = AIC(fit)
        rslt$df.residual = fit$n - if(!is.null(fit$coefficients))length(fit$coefficients) else 0
      }else{
        rslt$deviance = -2*fit$loglik
        rslt$df.residual = fit$n
      }
      class(rslt)<-"coxph"
      return(rslt)
    }else{  ### else Cox additive model
        ncols <- attr(mf[[order]],"ncols")
        
        ## extract coords name, span and degree
        spanf = if(!missing(span)) span else NULL
        degreef = if(!missing(degree)) degree else NULL
 #      next line not working properly when lo(.,.) term is listed first
 #      los <- attr(mt,"term.labels")[order-1]   
  		los <- untangle.specials(terms(formula,specials="lo"), "lo")$vars   # error fix  
        los <- gsub("[[:blank:]]","",los)
        start <- gregexpr("\\(",los)[[1]]; end <-rev(gregexpr("\\)",los)[[1]]) 
        losub <- substr(los,start[1]+1,end[1]-1)
        parts <- strsplit(losub,"\\([^()]*\\)(*SKIP)(*F)|\\h*,\\h*", perl=T)
        coords.name <- parts[[1]][1:2]
        if(is.null(dim(mf[[order]]))|dim(mf[[order]])[2]<2) 
          stop("Two predictors must be specified in lo()")
        if(ncols>2)
          warning(paste("Only the first two variables", paste(coords.name,collapse=" and "),"will be used for smoothing"))
        mf[[order]] <- mf[[order]][,1:ncols]
        colnames(mf[[order]]) <- coords.name
        if(length(parts[[1]])>2){
          for(i in 3:length(parts[[1]]))
            eval(parse(text=parts[[1]][i]))
        }
        if(!is.null(spanf))if(!all.equal(spanf,span)) 
          warning(paste("span size of",span,"in the formula will be used instead of value of argument sp=", spanf))  # error fix
        if(!is.null(span)) sp = span
        if(!is.null(degreef))if(degreef!=degree)
          warning(paste("degree=", degree,"in the formula will be used instead of value of argument degree=", degreef))
      
        Y = list(time=mf[[1]][,1],event = mf[[1]][,2])
        X.matrix <- if(!is.empty.model(mt))model.matrix(mt,mf,contrasts)[,-1]
        smooth.frame = mf[[order]]
      }
    if(!is.null(weights)){
      if(any(weights<0)) 
        stop("Negative weights not allowed")
      weights = weights[subset]
    }
    fit = gamcox.fit(Y,X.matrix,smooth.frame,weights,span,I.span,degree,loess.trace, Maxiter, tol)
    rslt <- fit
    rslt$formula=formula
    rslt$terms=mt
    rslt$xlevels = .getXlevels(mt,mf)
    rslt$data <- data
    rslt$span <- span
    rslt$I.span <- I.span
    rslt$Maxiter <- Maxiter
    rslt$tol<- tol
    rslt$degree <- degree
    rslt$loess.trace = loess.trace
    rslt$X=X.matrix
    rslt$Y = Y
    rslt$smooth.frame = mf[[order]]
    rslt$weights = if(missing(weights))NULL else weights
    rslt$call <- call
    class(rslt)<-"gamcox"
    rslt
  }