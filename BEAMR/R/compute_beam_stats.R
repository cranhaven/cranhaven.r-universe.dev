#' Compute bootstrap model coefficients for BEAM
#'
#' @param beam.data Result of prep.beam.data
#' @param beam.specs A data.frame of strings with columns name, mtx, mdl (string with R model with mtx.row)
#' @param stdize Logical whether to standardize (center and scale) predictors or not. Default is TRUE.
#'
#' @returns A beam.stats object, which is a list with beam.stats (the association matrices), the beam.specs, and the beam.data
#' @importFrom stats coef
#' @importFrom stats sd
#' @importFrom stats lm
#' @importFrom stats nlminb
#' @importFrom stats model.frame
#' @importFrom stats binomial
#' @importFrom stats glm
#' @importFrom stats model.matrix
#' @importFrom survival coxph
#' @importFrom survival coxph.control
#' @importFrom survival Surv
#' @importFrom MASS ginv
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split_1
#' @export
#'
#' @examples
#' data(beam_dat_sm)
#' data(beam_specs_sm)
#' test.beam.stats <- compute_beam_stats(beam.data=beam_dat_sm,
#'                                       beam.specs=beam_specs_sm, stdize=TRUE)
compute_beam_stats=function(beam.data, beam.specs,stdize=TRUE)
{
  ##############################
  # Check data
  if(!inherits(beam.data, "beam.data")){
    stop("Error: beam.data must be the result of prep.beam.data.")
  }

  #print(beam.data)
  #print(beam.specs)

  ##################################
  # check the specs
  mtx.names=names(beam.data$mtx.data)
  main.clms=colnames(beam.data$main.data)
  spec.check=check_beam_specs(beam.specs,
                              mtx.names)

  n.spec=nrow(beam.specs)
  boot.index=beam.data$boot.index
  #print(class(boot.index))
  main.data=beam.data$main.data

  beam.stats=vector("list",n.spec)
  for (i in 1:n.spec)                    # loop over specifications
  {
    message(paste("Working on analysis",
                  i,"of",n.spec,":",date()))
    message(beam.specs[i,c("mtx","mdl")])
    mtx.name=beam.specs[i,"mtx"]
    mtx=beam.data$mtx.data[[mtx.name]]
    mdl=beam.specs[i,"mdl"]
    x.clm=paste0(mtx.name,".clm")

    #print(boot.index)
    beam.stats[[i]]=boot_model_coefs(boot.index,
                                     mtx,
                                     main.data,
                                     x.clm,
                                     mdl,
                                     stdize=stdize)
  }

  names(beam.stats)=beam.specs[,"name"]

  res=list(beam.stats=beam.stats,
           beam.specs=beam.specs,
           beam.data=beam.data)

  class(res)="beam.stats"

  return(res)
}

###############################################
# compute bootstrap model coefficient matrix

boot_model_coefs=function(boot.index,
                          X,
                          main.data,
                          x.clm,
                          mdl,
                          stdize,
                          mess.freq=10)

{
  #print(dim(X))
  m=nrow(X)
  b=nrow(boot.index)
  #print(b)
  #print(head(boot.index))
  #print(dim(boot.index))
  res=matrix(NA,m,b)
  rownames(res)=rownames(X)
  colnames(res)=paste0("boot_",0:(b-1))
  for (i in 1:b)
  {
    if (((i-1)%%mess.freq)==0)
      message(paste("  Working on bootstrap",
                    i-1,"of",b,":",date()))
    boot.ind=boot.index[i,]
    #print(boot.ind)
    boot.data=main.data[boot.ind,]
    x.index=boot.data[,x.clm]
    #print(x.index)
    boot.X=X[,x.index]
    #print(dim(boot.X))
    res[,i]=row_model_coefs(boot.X,boot.data,mdl,stdize)
  }
  return(res)
}

#########################################
# For each row of X, fit a model and get the first coefficient

row_model_coefs=function(X,main.data,mdl,stdize)

{
  res=apply(X,1,model_coef,
            main.data=main.data,
            mdl=mdl,
            stdize=stdize)
  return(res)
}

############################################
# Get the first non-intercept coefficient for a fitted model

model_coef=function(x,main.data,mdl,stdize)

{
  res=NA
  sdx=sd(x,na.rm=TRUE)
  if (is.na(sdx)) return(0)
  if (sdx==0) return(0)

  mnx=mean(x,na.rm=TRUE)
  if(stdize){
    main.data$mtx.row=(x-mnx)/sdx
  }
  else{
    main.data$mtx.row=x
  }

  # check output before fitting cox models
  # check output before fitting cox models
  if(grepl("coxphf", mdl))
  {
    #model.terms=get_model_terms(model.fit)
    formula <- parse(text=mdl)
    form.char <- as.character(formula)
    form.in <- stringr::str_extract_all(form.char,  "(?<=\\().+?(?=\\))")
    form.in.spl <- stringr::str_split_1(form.in[[1]], "\\,")
    mdl.frm <- stats::model.frame(formula=form.in.spl[1], data=main.data)
    #mdl.frm

    evtm=mdl.frm[,1][,1]
    event=mdl.frm[,1][,2]
    x.temp=mdl.frm[,grep("mtx.row", colnames(mdl.frm))]

    # make sure adjustment factor variables don't have empty levels
    if(ncol(mdl.frm)>2){
      adj.vars <- mdl.frm[,-c(1,2),drop=FALSE]
      for(j in 1:ncol(adj.vars)){
        if(is.factor(adj.vars[,j])){
          main.data <- main.data[which(!is.na(main.data$mtx.row)),]
          main.data[,which(colnames(main.data)==colnames(adj.vars)[j])] <- droplevels(main.data[,which(colnames(main.data)==colnames(adj.vars)[j])])
        }
      }
    }

    # make sure design matrix is full rank
    form.in.spl2 <- stringr::str_split_1(form.in.spl[[1]], "\\~")
    dm.form <- formula(paste0("~", form.in.spl2[2]))
    dm <- stats::model.matrix(dm.form, main.data)
    dm.rnk <- qr(dm)$rank
    if(dm.rnk < min(ncol(dm), nrow(dm))) return(0)

    # no events
    if (is.null(event)) return(0)
    if (mean(event,na.rm=TRUE)==0) return(0)

    # no variability in event time
    if ((sd(evtm,na.rm=TRUE)==0)&(mean(event,na.rm=TRUE)==1)) return(0)

    # no variability in x
    if (sd(x.temp,na.rm=TRUE)==0) return(0)

    # no variability in x among those with observation times greater than or equal to shortest uncensored event time
    evtm.cns <- cbind.data.frame(evtm, event)
    evtm.or <- evtm.cns[order(evtm, decreasing=FALSE),]
    evtm.or.evnt <- evtm.or[which(evtm.or$event==1),]
    min.time <- evtm.or.evnt[which.min(evtm.or.evnt$evtm),1]
    x.obs.mog <- x.temp[evtm>=min.time]
    if(sd(x.obs.mog)==0) return(0)

    # check for monotone likelihood
    x.event=range(x.temp[event>0],na.rm=TRUE)
    x.none=range(x.temp[event==0],na.rm=TRUE)
    if(sd(x.event,na.rm=TRUE)==0){
      model.fit=try(eval(parse(text=mdl)))
      if(inherits(model.fit, "try-error")){
        return(0) # if x for cases with event has no variability, return 0 if model can't be fit?
      }
      else{
        beta=model.fit$beta
        mtx.ind <- which(names(beta)=="mtx.row")
        res <- beta[mtx.ind]
        return(res)
      }
    }

    # Check for failure due to inability to fit Firth penalty
    model.fit=try(eval(parse(text=mdl)))
    if(inherits(model.fit, "try-error")){
      return(0) # if firth penalty can't be found, return 0
    }

    # Do we need to check this in coxphf if we're already accounting for montone likelihood?
    #if (all(x.none>=x.event[2])) return(-Inf) # if x for cases with no event is always less than x for cases with event
    #if (all(x.none<=x.event[1])) return(Inf)  #
  }


  model.fit=try(eval(parse(text=mdl)))


  if (class(model.fit)[1]!="try-error")
  {
    beta=coef(model.fit)
    int=(names(beta)=="(Intercept)")
    beta=beta[!int]
    res=beta[1]
  }

  #################################
  # special case of coxphf2
  if(inherits(model.fit, "coxphf2")){
    beta=model.fit$beta
    mtx.ind <- which(names(beta)=="mtx.row")
    res <- beta[mtx.ind]
  }

  #################################
  # special case of logistf2
  if(inherits(model.fit, "logistf2")){
    formula <- parse(text=mdl)
    form.char <- as.character(formula)
    form.in <- stringr::str_extract_all(form.char,  "(?<=\\().+?(?=\\))")
    form.in.spl <- stringr::str_split_1(form.in[[1]], "\\,")
    mdl.frm <- stats::model.frame(formula=form.in.spl[1], data=main.data)

    #model.terms=get_model_terms(model.fit)
    y.temp=mdl.frm[,1]
    x.temp=mdl.frm[,2]

    # no variability in x or y after removing NAs
    sdx=sd(x.temp,na.rm=TRUE)
    if (sdx==0) res=0
    if(inherits(y.temp, "factor")){
      if(all(duplicated(y.temp))){
        res=0
      }
    }
    else{
      sdy=sd(y.temp,na.rm=TRUE)
      if (sdy==0) res=0
    }

    # If doesn't fail, return beta
    beta=model.fit$beta
    mtx.ind <- which(names(beta)=="mtx.row")
    res <- beta[mtx.ind]
  }

  #################################
  # special cases in coxph
  if(inherits(model.fit,"coxph"))
  {
    model.terms=get_model_terms(model.fit)

    evtm=model.fit$model[,model.terms[1]][,1]
    event=model.fit$model[,model.terms[1]][,2]
    x.temp=model.fit$model[,model.terms[2]]

    # no events
    if (is.null(event)) return(0)
    if (mean(event,na.rm=TRUE)==0) return(0)

    # no variability in event time
    if ((sd(evtm,na.rm=TRUE)==0)&(mean(event,na.rm=TRUE)==1)) return(0)

    # no variability in x
    if (sd(x.temp,na.rm=TRUE)==0) return(0)

    # check for monotone likelihood
    x.event=range(x.temp[event>0],na.rm=TRUE)
    x.none=range(x.temp[event==0],na.rm=TRUE)

    if (all(x.none>=x.event[2])) return(-Inf) # if x for cases with no event is always less than x for cases with event
    if (all(x.none<=x.event[1])) return(Inf)  #
  }

  #######################################
  # special cases in lm
  if (inherits(model.fit, "lm"))
  {
    model.terms=get_model_terms(model.fit)
    y.temp=model.fit$model[,model.terms[1]]
    x.temp=model.fit$model[,model.terms[2]]

    # no variability in x or y after removing NAs
    sdx=sd(x.temp,na.rm=TRUE)
    if (sdx==0) res=0
    if(inherits(y.temp, "factor")){
      if(all(duplicated(y.temp))){
        res=0
      }
    }
    else{
      sdy=sd(y.temp,na.rm=TRUE)
      if (sdy==0) res=0
    }
  }


  #if (class(model.fit)[1]=="try-error")
  if(inherits(model.fit, "try-error"))
  {
    #res=0
    message("Error fitting model!")
    # print(mdl)
    # message("Error fitting model, mtx.row summarized below:")
    # print(summary(main.data$mtx.row))
    # message("Error fitting model, summary of data shown below: ")
    # print(summary(main.data))
  }

  return(res)
}

#######################################
# Get the terms of a model

get_model_terms=function(model.fit)

{
  model.terms=deparse(attr(model.fit$terms,"variables"))
  model.terms=unlist(strsplit(model.terms,split=","))
  model.terms=gsub("list(","",model.terms,fixed=TRUE)
  model.terms=gsub(")","",model.terms,fixed=TRUE)
  model.terms=gsub(" ","",model.terms)
  return(model.terms)
}

#################################################
# Find Firth-penalized coefficients in Cox model
# in case of monotone likelihood

coxphf2 <- function(formula, data, model=TRUE){
  if(model){
    col.names <- as.character(formula)
    model <- data[,which(colnames(data) %in% col.names[-1])]
  }
  else{
    model <- NULL
  }
  fit <- firth.coxph(data=data, form=formula, use.pen=TRUE)
  res <- list(model, fit, fit$par)
  names(res) <- c("model", "fit", "beta")
  class(res) <- "coxphf2"
  return(res)
}

firth.neg.logL=function(beta,data,form,use.pen=FALSE)
{
  suppressWarnings({
    cox.res0=coxph(form,data=data,init=beta,control=coxph.control(iter.max=0)) # cox logL at beta
  }) # suppress expected warning from coxph to get initial values
  cox.res0$call$formula <- form
  cox.res0$call$data <- as.data.frame(data)
  logL=cox.res0$loglik[1]
  #fpen=0.5*log(sum(diag(ginv(cox.res0$var))))
  detail <- try(coxph.detail(cox.res0))
  if(inherits(detail, "try-error")){
   stop("Unable to calculate Firth penalty!")
  }
  if(inherits(detail$imat, "array")){
    det.list <- lapply(seq(dim(detail$imat)[3]), function(x) detail$imat[ , , x])
    inf <- Reduce("+", det.list)
    fpen=0.5*log(det(inf))
  }
  else{
    inf <- sum(detail$imat)
    fpen=0.5*log(inf)
  }


  if (!use.pen) fpen=0

  res=-logL-fpen
  #print(c(beta=beta,logL=logL,firth=fpen,pen.logL=logL+fpen))
  return(res)
}

firth.coxph=function(data,form,use.pen=TRUE)
{
  suppressWarnings({
    cox.fit=coxph(form,data=data,control=coxph.control(iter.max=0))
  }) # suppress expected warning from coxph to get initial values
  res=nlminb(start=coef(cox.fit),
             objective=firth.neg.logL,
             data=data,form=form,
             use.pen=use.pen)
  return(res)
}

###########################################################
# Find Firth-penalized coefficients in logistic model
# in case of monotone likelihood

# formula <- MRDbin~mtx.row
# data <- main.data
logistf2 <- function(formula, data, model=TRUE){
  if(model){
    col.names <- unlist(strsplit(as.character(formula), split=" "))
    model <- data[,which(colnames(data) %in% col.names)]
  }
  else{
    model <- NULL
  }
  fit <- firth.logistic(dset=data, form=formula, firth=TRUE)
  res <- list(model, fit, fit$coef)
  names(res) <- c("model", "fit", "beta")
  class(res) <- "logistf2"
  return(res)
}

nlogL.logistic=function(beta,y,mdl.mtx,firth=FALSE)

{
  # Add extra step to filter mdl.mtx for factor covariates
  mdl.mtx2 <- mdl.mtx[, which(colnames(mdl.mtx)%in%names(beta))]
  lnr.cmb=mdl.mtx2%*%beta                                                  # compute linear predictors
  y.hat=exp(lnr.cmb)/(1+exp(lnr.cmb))                                     # compute predicted probabilities
  logL=sum(log(y.hat[y==1]))+sum(log(1-y.hat[y==0]))              # logistic regression likelihood
  I.mtx=t(mdl.mtx2)%*%diag(as.vector(y.hat*(1-y.hat)))%*%mdl.mtx2           # Equation 5.20 of Agretsi (2002)
  pen=firth*0.5*log(det(I.mtx))                                           # Firth penalty term (Heinze & Schemper 2002; PMID 12210625)
  return(-logL-pen)                                                       # (penalized) likelihood for input beta & data
}

# form <- formula
# dset=data
# firth=TRUE
firth.logistic=function(form,dset,firth=TRUE)

{
  suppressWarnings({
    glm.res=stats::glm(form,data=dset,family=binomial)                             # use glm for initial fit
  }) # suppress expected warnings from glm fit for initial values
  mdl.mtx=model.matrix(glm.res$model, data=dset)                                     # extract model matrix
  y.obs=glm.res$y                                                         # extract observed y
  nlm.res=stats::nlminb(start=glm.res$coefficients,                              # fit model with firth-penalized likelihood
                        objective=nlogL.logistic,
                        y=y.obs,mdl.mtx=mdl.mtx,firth=firth)
  res=list(coef=nlm.res$par,
           loglik=-nlm.res$objective,
           converged=(nlm.res$converge==0))
  return(res)
}


