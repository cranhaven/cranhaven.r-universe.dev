#***********************************************************************************
#
# Determine the Optimal Span Size for modgam()
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
optspan <- function (formula, data, offset, spans=seq(0.05,0.95,by=0.05), m="adjusted",
                     family=binomial(), verbose = TRUE, degree=1,...)
{ 
  suppressWarnings({    # lo.wam generates repeated warning messages at some span sizes
  # for back-compatibility, allow data frame to be passed as first argument
  if(!missing(formula) && is.data.frame(formula)) {
    offstr = ''
    if(!missing(offset)){offstr = substr(call['offset'],1,nchar(call['offset']))}
    if (!missing(data)) 
      stop("data can be entered through only one argument (formula or data)")
    return(optspan(data=formula, offset=offstr, spans=spans, m=m, family=family, verbose=verbose,
                   degree=degree,...))
  }
  if(!missing(formula) && !is.data.frame(formula)){
    # if formula is specified
    if (missing(data)) 
      data <- environment(formula)
    mf <- match.call()
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mt <- if (missing(data)) 
      terms(formula, c("Surv","lo"))
    else terms(formula, c("Surv","lo"), data = data)
    surv = !is.null(attr(mt,"specials")$Surv)
    order = attr(mt,"specials")$lo
    if(surv){
      family = "survival"
    }
    if(is.character(family))
      if(!surv & tolower(family)[1]=="survival")
        stop("formula misspecified for survival data")
    mf$formula <- mt
    mf <- eval(mf, parent.frame())
    ncols <- attr(mf[[order]],"ncols")
    if(is.null(order) | ncols<2)
      stop("Two parameters must specified in lo() in the formula")
    if(length(order)>1)
      stop("Only one smoothing function can be included in the model")
    
    ## Extract coords names
 #   next line not working properly when lo(.,.) term is listed first
 #   los <- attr(mt,"term.labels")[order-1-surv]
	los <- untangle.specials(terms(formula,specials="lo"), "lo")$vars   # error fix
    los <- gsub("[[:blank:]]","",los)
    start <- gregexpr("\\(",los)[[1]]; end <-rev(gregexpr("\\)",los)[[1]]) 
    losub <- substr(los,start[1]+1,end[1]-1)
    parts <- strsplit(losub,"\\([^()]*\\)(*SKIP)(*F)|\\h*,\\h*", perl=T)
    coords.name <- parts[[1]][1:2]
    if(ncols>2)
      warning(paste("Only the first two variables", paste(coords.name,collapse=" and "),"will be used for smoothing"))
    
    if(length(mf)==2) m = "unadjusted" else m="adjusted"
  }
  if(missing(offset))
    offset = ''
  ## Recognize family
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
  
  ## Re-organize the data structure
  data.name = colnames(data)
  data.dim = length(data.name)
  if(missing(formula)){
    if(data.dim < dim.min) stop(paste("data must include at least", dim.min,"columns"))
    if(tolower(m)=="adjusted"){
      if(data.dim == dim.min){
        warning("cannot run adjusted model without additional covariates in data")
        m = "unadjusted"
      }
    }else if(tolower(m) == "unadjusted" | tolower(m) == "crude"){
      data = data[,1:dim.min]
      data.name=data.name[1:dim.min]
      data.dim = dim.min
    }else stop(paste("model type", m, "not recognized"))
    coords.name <- data.name[2:3+surv]
  }
  
  ## fits gams with all specified spans
  N.span = length(spans)
  span.aic = rep(0,N.span)
  for (S in 1:N.span) {
      span = spans[S]
      fmla = toformula(formula,data, m,surv, span=span, degree=degree,TRUE,offset)
      if(surv)
        fit = eval(substitute(gamcox(fmla,data,span=span,degree=degree,...)))
      else
        fit = eval(substitute(gam(fmla,family=family,data=data,...)))
      span.aic[S] = fit$aic
      if (verbose) cat(paste("The AIC for span=", format(span, nsmall = 2), 
          " is ", format(fit$aic, nsmall = 2), "." , sep = ""), 
          fill = TRUE)
  }
  sp = spans[which.min(span.aic)]
  if(verbose) cat(paste("The optimal span is",sp,"\n"))
  return(sp)
  })
}
