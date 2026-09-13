#***********************************************************************************
#
# Fit a Generalized Additive Model with a Bivariate Smooth and Make Predictions
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
modgam <- function (formula, rgrid, data, subset, offset, family = binomial(), permute = 0, 
                  conditional = TRUE, pointwise = FALSE, m = "adjusted", sp = seq(0.05,0.95,0.05), 
                  degree = 1, keep = FALSE, type=c("spatial","all"), reference = "median", 
                  se.fit = FALSE, alpha = 0.05, verbose = TRUE, ...)
{
  call <- match.call()
  # for back-compatibility, allow data frame to be passed as first argument
  if(!missing(formula) && is.data.frame(formula)) {
    offstr = ''
    if(!missing(offset)){offstr = substr(call['offset'],1,nchar(call['offset']))}
    if (!missing(data)) stop("data can be entered through only one argument (formula or data)")
    if(missing(type)){
      type = "all"
      if(is.character(family))
        if(family =="survival")
          type = "spatial"
    }
    return(modgam(rgrid=rgrid, data=formula, subset=subset, offset=offstr, family=family, permute=permute,
                    conditional = conditional, pointwise = pointwise, m = m, sp = sp, degree = degree, 
                    keep = keep, type=type, reference = reference, se.fit = se.fit, alpha = alpha, 
                    verbose = verbose, ...))
    
  }
  if(!missing(formula)){
    #if formula provided
    if (missing(data)) 
      data <- environment(formula)
    mf <- match.call()
    m <- match(c("formula", "data", "subset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mt <- if (missing(data)) 
      terms(formula, c("Surv","lo"))
    else terms(formula, c("Surv","lo"), data = data)
    surv = !is.null(attr(mt,"specials")$Surv)
    order = attr(mt,"specials")$lo
    if(surv)
      family = "survival"
    if(is.character(family))
     if(!surv & tolower(family)[1]=="survival")
        stop("formula misspecified for survival data")
    mf$formula <- mt
    mf <- eval(mf, parent.frame())
    ncols <- attr(mf[[order]],"ncols")
    if(is.null(order) | ncols<2)
      stop("Two parameters must specified in lo() in the formula")
    if(length(order)>1)
      stop("Only one smoothing function could be included in the model")
    
    ### Extract coords names, span and length
    span = NULL
    degreef = if(!missing(degree)) degree else NULL
    los <- attr(mt,"term.labels")[order-1]
    los <- gsub("[[:blank:]]","",los)
    start <- gregexpr("\\(",los)[[1]]
    end <-rev(gregexpr("\\)",los)[[1]]) 
    losub <- substr(los, start[1]+1, end[1]-1)
    parts <- strsplit(losub,"\\([^()]*\\)(*SKIP)(*F)|\\h*,\\h*", perl=T)
    coords.name <- parts[[1]][1:2]
    if(ncols>2)
      warning(paste("Only the first two variables", paste(coords.name,collapse=" and "),"will be used for smoothing"))
    if(length(parts[[1]])>2){
      for(i in 3:length(parts[[1]]))
        eval(parse(text=parts[[1]][i]))
    }
    if(!missing(sp)&!is.null(span)&conditional)if(!all(sp==span)) 
      warning(paste("span size of", 
                    if(length(span)==1) span else paste("(",paste(span,collapse=","),")"),
                    "in the formula will be used instead of value of argument sp=", sp))
    if(!is.null(span)) sp = span
    if(!is.null(degreef))
      if(degreef!=degree)
        warning(paste("degree=", degree,"in the formula will be used instead of value of argument degree=", degreef))
    if(length(mf)==2) m = "unadjusted" else m="adjusted"
    data <- data[subset,]
  }
  ## Recognize family
  if(missing(offset))
    offset = ''
  dim.min = 3; surv = FALSE
  if(is.character(family)){
     if(tolower(family)== "survival") {	 
  	   dim.min = 4; surv = TRUE
     }
  }
  if(dim.min==3){
    if(is.character(family)) 
      family = get(family, mode = "function", envir = parent.frame())
    if (is.function(family)) family = family()
    if (is.null(family$family)) {
      print(family)
      stop("`family' not recognized")
    }
  }
  if (is.null(sp)) sp = seq(0.05,0.95,0.05)      # for back-compatibility
  if (length(sp)==1 && !conditional) {
 	  warning("User-specified fixed span size is ignored for conditional = FALSE") 
	  sp = seq(0.05,0.95,0.05)
  }
  data.name = names(data)
  data.dim = length(data.name)
  
  ## Re-organize the data structure
  if(missing(formula)){
    if(data.dim < dim.min) stop(paste("data must include at least", dim.min,"columns"))
    if(tolower(m)=="adjusted"){
       if(data.dim == dim.min){
         warning("cannot run adjusted model without additional covariates in data")
         m = "unadjusted"
       }
       data = data[subset,]
    }else if(tolower(m) == "unadjusted" | tolower(m) == "crude"){
       dimension = 1:dim.min
       if(offset!=''){
         for(j in 1:length(data.name)){
           if(grepl(data.name[j],offset)==1){
             break
           }
         }
         dimension = c(dimension,j)
       }
       data = data[subset,dimension]
       data.name=data.name[dimension]
       data.dim = length(dimension)
    }else stop(paste("model type", m, "not recognized"))
    coords.name <- data.name[2:3+surv]
  }
  index <- match(coords.name,names(rgrid))
  if(any(is.na(index)))
    stop("spatial variables in rgrid are not consistent with data")
  
  ## Find the optimal span if needed
  if(length(sp)>1) {
    spans <- sp
    sp <- eval(substitute(optspan(formula, data, offset, spans, m, family, verbose, degree,...)))
  }
  fmla <- toformula(formula,data,m,surv,sp,degree,TRUE,offset)
  fmla.0 <- toformula(formula,data,m,surv,sp,degree,FALSE,offset)
  if (verbose) {
    cat(paste("The ", tolower(m), " model is: ", sep = ""), 
        fill = T)
    print(fmla, showEnv = F)
    if(!surv)
      cat(paste(c("Family:", "Link:"), family[1:2]), fill = T)
    else cat(paste("\nspan: ",sp,"\n"))
  }
  if(missing(type)&!surv) type = "all"
  else type = type[1]
  # Warning regarding change in default behavior for gaussian models
  if (tolower(as.character(family)[1]) == "gaussian" & reference == "median" & type == "all") 
  	warning("MapGAM now uses reference='median' by default; for predicted values use reference='none'") 

  # Fit the model and make predictions
  if(surv){
    model <- eval(substitute(gamcox(fmla,data=as.data.frame(data),...)))
    nrows <- dim(model$X)[1]
    pred <- predict(model,rgrid,se.fit,type,reference,alpha,verbose,)
    model.0 <- eval(substitute(gamcox(fmla.0,data=as.data.frame(data),...)))
    nrows.0 <- model.0$n    
  }else{
    model <- eval(substitute(gam(fmla,family=family,data=as.data.frame(data),...)))
    nrows <- length(model$y)
    pred <- mypredict.gam(model,rgrid,se.fit,type,reference,alpha,verbose)
    model.0 <- eval(substitute(gam(fmla.0,family=family,data=as.data.frame(data),...)))
    nrows.0 <- length(model.0$y)
  }
  
  dev = model.0$deviance-model$deviance; df = model.0$df.residual - model$df.residual
  rslt <- list(grid = rgrid[,index],family=family, type=type,span=sp,gamobj = model, predobj = pred,
               fit = pred$pred, exp.fit = exp(pred$pred),
               global.pvalue = if(nrows == nrows.0) 1-pchisq(dev,df=df))
  if(se.fit){
    rslt$se = pred$se
    rslt$conf.low = pred$conf.low
    rslt$conf.high = pred$conf.high
    rslt$exp.conf.low= exp(pred$conf.low)
    rslt$exp.conf.high = exp(pred$conf.high)
  }
  
  ### Permutation test
  if (permute > 0) {
    grid.N = nrow(rgrid)
    data.N = nrow(data)
    if (keep & pointwise) 
      permrslt = matrix(NA, grid.N, permute - 1)
    ptranks = rep(1, grid.N)
    devstat = rep(NA, permute)
    ucspans = rep(NA, permute)		
    ucspans[1] = sp
    devstat[1] = model.0$deviance-model$deviance
    coords = data[, coords.name]
    m.data = data
    for (i in 2:permute) {
      index.perm = sample(1:data.N, replace = F)
      m.data[, coords.name] = coords[index.perm, ]
      if (!conditional) {
        ucsp = optspan(formula, m.data, spans, m, family = family, verbose=F, ...)
        ucspans[i] = ucsp
        fmla <- toformula(formula, m.data, m, surv, ucsp, degree,TRUE,offset)
      }
      if(surv){
        m.gam <- gamcox(fmla,data=as.data.frame(m.data),span=sp,degree=degree,...)
        if(pointwise) m.pred <- predict(m.gam,rgrid,FALSE,type,reference)$pred
      }else{
        m.gam <- gam(fmla,family=family,data=as.data.frame(m.data),...)
        if(pointwise) m.pred <- mypredict.gam(m.gam,rgrid,FALSE,type,reference)$pred
      }
      devstat[i] = model.0$deviance - m.gam$deviance
      if(pointwise){
        temprslt = m.pred
        ptranks = ptranks + (rslt$fit > temprslt)*(!is.na(rslt$fit))*(!is.na(temprslt))
        if (keep)
          permrslt[, i - 1] = temprslt
      }
      if (verbose && i%%10 == 0)
        cat(paste("Permutation", i, "of", permute), fill = TRUE)
    }
    devglobp = (permute - rank(devstat)[1])/permute
    globprint = if (devglobp == 0) 
      paste("<", round(1/permute, 3), sep = "")
    else devglobp
    cat(paste("The global statistic for the ", tolower(m), 
              " model is ", globprint, sep = ""), fill = TRUE)
    rslt$global.permt = devglobp
    if(pointwise) rslt$pointwise.permt = as.vector(ptranks/permute)
    if (keep) {
      rslt$permutations = permrslt
      rslt$deviances.permt = devstat
      if (!conditional) rslt$span.permt = ucspans
    }
    rslt$N.permt <- permute
  }
  rslt$call <- call
  class(rslt) <- "modgam"
  return(rslt)
}
