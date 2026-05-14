#############################################################
##  This file has all functions required for regplot v 1.0. #
#############################################################

## extract main items for polr models
polr_1 <- function(reg){
  
  ## this function to extract  required parameter from o polr object
  ## for ordinal logistic regressioo
  ## pretty much a copy of glm_1()
  
    variable_names <- attr(reg$terms,"term.labels")
  ## variable_names <- gsub("=","",variable_names)
   ##   

 
coefficients <- reg$coefficients

       x <- model.matrix( formula(reg), data=reg$model )


   xlevels <- reg$xlevels 
   
## use length of xlevels to indicate wthere any factors in the model
  hasfactors <- (length(xlevels)>0)

 ## note even though polr model,  design matrix has "intercept"
  ## polr doesnt  allow "no intercept" model
    nointercept_term <- FALSE
#     
#     ## problem: here  a varaible may be expression with a * in it. 
#     ##  mistaken as an interaction  in defining isinteraction[]
#     ## is there a better way to identify interaction? 

    names(coefficients) <- gsub(" \\* ",":",names(coefficients))
      
# 

## intercpts are included as last terms
ncoef <- length(coefficients)

 SEbeta <- sqrt(diag(vcov(reg)))[1:ncoef]
      
 names(SEbeta) <- names(coefficients)
 ## patch so that coefficients looks like a regular logistic
 ## model with initial intercept 0 term
 coefficients <- c(0,coefficients)
 names(coefficients)[1] <- "(Intercept)"
 SEbeta <- c(SEbeta,sqrt(diag(vcov(reg)))[1+ncoef])

 Pval   <- 2*(1-pnorm(abs(coefficients/SEbeta)) )
 
   ## check on weights model  for glm, lm,
  lw <- ncol(reg$model)
  lw <- which(colnames(reg$model)=="(weights)")
  weighted <- FALSE
  W <- NULL
  if(length(lw)==1 ){
    message("Replicate integer weights assumed")
    weighted <- TRUE 
    
    W <- floor(reg$model[,lw])
    if(any(  W-reg$model[,lw] !=0) ){
      message("Note: non-integer weights have been floored")
    }
     }
  yvar <- names(reg$model)[1]
## does it have cbind() syntax? eg. cbind(cases,controls)??
test <- grep(pattern="cbind\\(",x=yvar)
if(length(test > 0) ){
## later: think this works: needs checking  
 ##return(message(yvar," outcome syntax is not supported by regplot") )
}

  ##note Sreg bug detected here for lm model with
  ##  y`~  0 + x1:x2  no intercept no main effect. 
  ##  Sreg$coefficients[,1] returns no names()
  ## for safety use reg$coefficients 
  ## coefficients <- Sreg$coefficients[,1]
   
  Lcoefficients <- coefficients -1.96*SEbeta
  Ucoefficients <- coefficients +1.96*SEbeta
 
  ##coefficients <- reg$coefficients
  ## patch in possibility of zero beta.  Throws factor variables!

  coefficients <- ifelse(coefficients==0,0.0001,coefficients)
 

  offset <- !is.null(reg$offset) 

  offsetvar <- NULL

  if(offset){
   ## how to get offset varaivle name?
   ## and:  o gives offset item number, which is extracted from as.character()! 
  
    o <- which(names(reg$call) == "offset")
    
    if(length(o) == 0){
      
      ##offset=.. not included in call. offset() must be specified in formula
      message(" \"offset()\" specified in formula. Please use  \"offset=\" to specify the offset")
    }
    else
    {
     offsetvar <- as.character(reg$call)[o]}

        
     ## create a redundant Pvalue
    Pval <- c(Pval,1)
  }
    
    
    actualdata <- model.frame(reg)
    #  
    if(offset){
      ## last column of actual data is "(offset)".
      ## rename with offsetvar
      ## WHY?
      o <- which(names(actualdata)=="(offset)")
      names(actualdata)[o] <- offsetvar
}
   
intercepts <- reg$zeta  
XXX <- unlist(strsplit(names(intercepts),split="|",fixed=TRUE))
XXX <- unique(XXX)
XXX <- paste0(yvar,"<=",XXX)
names(intercepts) <- XXX[1:length(intercepts)]

R <- list(xlevels,weighted,W,yvar,coefficients,
          offset,SEbeta,Lcoefficients,Ucoefficients,variable_names,
          actualdata,Pval,x,offsetvar,intercepts)
return(R)
}  #end polr_1 function

##################################################
## extract main items for glm, lm  glm.nb models
## and rms equivalents ols, Glm
glm_1 <- function(reg){
  
  # ## problem:
  # 
  # how to retrieve variables of the regression when they are functions?
  #   eg.  y ~ bs(x)  the underlying values of x not returned with reg$model
  # 
  # see https://stackoverflow.com/questions/22921765/way-to-extract-data-from-lm-object-before-function-is-applied
  # 
  # for a possible solution, but cant make it work
  #  eval(getCall(reg)$data,environment(formula(reg))
  
     #}
    variable_names <- attr(reg$terms,"term.labels")
  ## variable_names <- gsub("=","",variable_names)

  rms       <- (class(reg)[2]=="rms")
  if(is.na(rms)) rms <- FALSE
  ols.rms <-  (class(reg)[1] == "ols" )
  Glm.rms <-  (class(reg)[1] == "Glm")
  lrm.rms <-  (class(reg)[1] == "lrm")
coefficients <- reg$coefficients

  #3   x <- model.matrix( formula(reg), data=reg$model )
    if(ols.rms | lrm.rms){
       x <- model.matrix(reg)}
    else
      {
       x <- model.matrix( formula(reg), data=reg$model )
       }
 
    # ## no re$xlevels for rms:ols , use Design attribute
   if(ols.rms | Glm.rms | lrm.rms) { 
     xlevels <- reg$Design$parms
     }
 # 
  else
  {
  xlevels <- reg$xlevels 
  }

 if(rms){
## this is crucial patch in rms to ensure 
   ## coefficent names link to variable names correctly
   ## the same as they are in non-rms models
## (same code is used in glm_() rms models   
   names(coefficients) <- colnames(x)
      }
     
 
## use length of xlevels to indicate wthere any factors in the model
  hasfactors <- (length(xlevels)>0)

        
    nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")
    if(nointercept_term){message("Note:  model without an intercept")}

     if(rms & nointercept_term & hasfactors){
       
       ##  rms with n intercept behaves unlike glm().
       ## if the model has factor terms. model.matrix returns
       ## additional parameter in first columm, as is needed for the 
       ## glm() parameterisation. 
       
       ## try patching by removing first col of x
       x <- x[,-1]
     } 
## dont need Sreg
##  if(!rms) Sreg <- summary(reg)


     

  ## patch for interactions in rms models have coefficients  "sex * age",
  ## rather than  sex:age.  Patch in ":
    
    ## problem: here  a varaible may be expression with a * in it. 
    ##  mistaken as an interaction  in defining isinteraction[]
    ## is there a better way to identify interaction? 

    names(coefficients) <- gsub(" \\* ",":",names(coefficients))
      

if(rms){
st <- grep(pattern= "strat\\(" , variable_names)

if(length(st) >0){
  return(message("strat() not supported in ", class(reg)[1]," formula"))
}

# 
#         s <- which( substr(start=1,stop=6,colnames(x)) == "strat("  )
#         x <- x[,-s]


##  strip "=" from names(coeficients) 
##browser()
## is this nevcessary at
## avoid  stripping "==" 
# xxx <- gsub(pattern= "==" , replacement="@@", x=names(coefficients) )
# xxx <- gsub(pattern= "=" , replacement="", x=xxx )
# names(coefficients) <- gsub(pattern= "@@" , replacement="==", x=xxx )
#       
## alt  way, get from  colnmaes(x), but this doesn't work if there
## are NA  beta coefficients
  ##???      names(coefficients) <- colnames(x)
      
 }

## need to add in term for intercept. But dont know SE of
     ## intercept term . Make arbitray 0?

      SEbeta <- sqrt(diag(vcov(reg)))
      
      names(SEbeta) <- names(coefficients)
      Pval <- 2*(1-pnorm(abs(coefficients/SEbeta)) )
    #   
    # nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")
    # if(nointercept_term){message("Note:  model without an intercept")}
   ##note: nointercept is indicator, from henceforth, of whether 
    ## nointercept in sense of glm() model with no intercept
   # if(rms) nointercept <- FALSE
##  }
    
    
  # else  #if(rms | TRUE)
  # { SEbeta <- Sreg$coefficients[,2]
  # Pval <- Sreg$coefficients[,4] }

#  # ## no re$xlevels for rms:ols , use Design attribute
#    if(ols.rms) { xlevels <- reg$Design$values}
#  # 
#   else
#   {
#   xlevels <- reg$xlevels 
#   }
# 
#    
# ## use length of xlevels to indicate wthere any factors in the model
#   hasfactors <- (length(xlevels)>0)
  ## check on weights model  for glm, lm,
  lw <- ncol(reg$model)
  lw <- which(colnames(reg$model)=="(weights)")
  weighted <- FALSE
  W <- NULL
  if(length(lw)==1 ){
    message("Replicate integer weights assumed")
    weighted <- TRUE 
    
    W <- floor(reg$model[,lw])
   ## browser()
    if(any(  W-reg$model[,lw] !=0) ){
      message("Note: non-integer weights have been floored")
    }
     }
## extract Y variable from formula 
  if(ols.rms | lrm.rms){
yvar <- names(attr(reg$terms,"dataClasses"))[1]
  }
  else
  {
  yvar <- names(reg$model)[1]
  }
  
  


## does it have cbind() syntax? eg. cbind(cases,controls)??
test <- grep(pattern="cbind\\(",x=yvar)
if(length(test > 0) ){
## later: think this works: needs checking  
 ##return(message(yvar," outcome syntax is not supported by regplot") )
}

  ##note Sreg bug detected here for lm model with
  ##  y`~  0 + x1:x2  no intercept no main effect. 
  ##  Sreg$coefficients[,1] returns no names()
  ## for safety use reg$coefficients 
  ## coefficients <- Sreg$coefficients[,1]
   
  Lcoefficients <- coefficients -1.96*SEbeta
  Ucoefficients <- coefficients +1.96*SEbeta
 
  ##coefficients <- reg$coefficients
  ## patch in possibility of zero beta.  Throws factor variables!

  coefficients <- ifelse(coefficients==0,0.0001,coefficients)
 

  offset <- !is.null(reg$offset) 
  if(rms){
    ## rms model , offset has value 1 , length 1
    if(length(reg$offset)==1){
      offset <- FALSE}
  }
  
  offsetvar <- NULL

  if(offset){
   ## how to get offset varaivle name?
   ## and:  o gives offset item number, which is extracted from as.character()! 
  
    o <- which(names(reg$call) == "offset")
    
    if(length(o) == 0){
      
      ##offset=.. not included in call. offset() must be specified in formula
      message(" \"offset()\" specified in formula. Please use  \"offset=\" to specify the offset")
    }
    else
    {
     offsetvar <- as.character(reg$call)[o]}

        
     ## create a redundant Pvalue
    Pval <- c(Pval,1)
  }
    
    #}
  ##  variable_names <- attr(reg$terms,"term.labels")
  ## variable_names <- gsub("=","",variable_names)
   ##   
    
    actualdata <- model.frame(reg)
    #  
    if(offset){
      ## last column of actual data is "(offset)".
      ## rename with offsetvar
      ## WHY?
      o <- which(names(actualdata)=="(offset)")
      names(actualdata)[o] <- offsetvar
}
   
# 
#   #3   x <- model.matrix( formula(reg), data=reg$model )
#     if(ols.rms){
#        x <- model.matrix(reg)}
#     else
#       {
#        x <- model.matrix( formula(reg), data=reg$model )}
#      
#   
     # 
     # if(rms & nointercept_term & hasfactors){
     #   
     #   ##  rms with n intercept behaves unlike glm().
     #   ## if the model has factor terms. model.matrix returns
     #   ## additional parameter in first columm, as is needed for the 
     #   ## glm() parameterisation. 
     #   
     #   ## try patching by removing first col of x
     #   x <- x[,-1]
     # } 
     
R <- list(xlevels,weighted,W,yvar,coefficients,
          offset,SEbeta,Lcoefficients,Ucoefficients,variable_names,
          actualdata,Pval,x,offsetvar,nointercept_term)
return(R)
}  #end glm_1 function

##############################################################################

## extract main items for coxph and survreg models 
## and rms equivalents cph, psm
coxsurv_1 <- function(reg,cox){
  rms       <- (class(reg)[2]=="rms")
 if(is.na(rms)) rms <- FALSE
 
   
variable_names <- attr(reg$terms,"term.labels")

  ##==================================================\\
  ## how to deal with possibility of strata() variables
  ## posibility of a strata(x) appearing in variable_names.
  ## need to remove


  lv <- length(variable_names)
  istrat <- NULL
  vstrat <- NULL
  nstrataterms <- 0
  for (i in 1:lv){

    gX <- getX(variable_names[i])
    if(gX[2] ==  "strata()" | gX[2] ==  "strat()"){
       nstrataterms <-  nstrataterms + 1
      strata_vars <- gX[1]
      strata_vars <- unlist(strsplit(strata_vars,", "))
  
     istrat <- c(i,istrat) 
     vstrat <- c(variable_names[i],vstrat)
    }}
  ## deAl with strata() in model by removing the strata() item 
  ##  from variable_names and xlevels


    actualdata <- model.frame(reg)
   ##  
  

   x <- model.matrix( reg,data=actualdata)
  coefficients <- reg$coefficients



if(rms){

  if(!is.null(vstrat) ){
    s <- which( substr(colnames(x),start=1,stop=nchar(vstrat))==vstrat)

    x <- x[,-s]

  }
  ## this is crucial for rms models to match varaible_names
  ## and have same behaviour as non-rms models.
  ## but craps out if cph() specified with no intercept
  ## needs a prior trap on this ? 
 
  names(coefficients) <- colnames(x)
  
  }

##  
     
  ## check on weights model for cox and survreg models
  weighted <- FALSE
  W <- NULL
  if(!is.null(reg$weights)){
  message("Replicate integer weights assumed")
  weighted <- TRUE
  W <- floor(reg$weights)
  


      if(any(  W-reg$weights !=0) ){
      message("Note: non-integer weights have been floored")
    }

  
  
  if(all(W==0)){
    message("all are zero weighted  - may well  crash!")}
  if(any(W <0)){
   message("there are negative weights - may well crash!")}
   }
 
  ## strip off last item "(weights)" 
   if(weighted)actualdata <- actualdata[-length(actualdata)]

## *********inside coxsurv_1 ************ 
   if(rms ) {
  ## patch for interactions in rms models have coefficients  "sex * age",
  ## rather than  sex:age.  Patch in ":
      names(coefficients) <- gsub(" \\* ",":",names(coefficients))
 
     ## if rms an = sign is added in  factor names
     ## age  "going=INTER" but
     ##  later names in model.matrix don't have =. Need to sptrip
     ##names(coefficients) <- sub(pattern= "=" , replacement="", x=names(coefficients) )
     ## need to add in term for intercept. But dont know SE of
     ## intercept term . Make arbitray 0?

      SEbeta <- sqrt(diag(vcov(reg)))
      names(SEbeta) <- names(coefficients)

      Pval <- 2*(1-pnorm(abs(coefficients/SEbeta)) )
 ## *********inside coxsurv_1 ************      
    nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")

   
   if(nointercept_term & !cox){message("Note:  model without an intercept")}
   ##note: nointercept is indicator, from hernceforth, of whether 
    ## nointercept in sense of glm() model with no intercept
   # if(rms) nointercept <- FALSE

       }
 
  else   ## of (if(rms)
    
  {  
  
  
  summry <- summary(reg)
   if(cox)      { Pval <- summry$coefficients[,5] }
  else          { Pval <- summry$table[,4] }  
  
 nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")

 if(nointercept_term & !cox){message("Note:  model without an intercept")}

  
  }
 
#   x <- model.matrix( reg )
#   
#   
# if(rms & class(reg)[1] == "psm"){
#   ## model matrix for rms psm models doesnt include "(intercept)" col
#   ## patch one in Note:  pms doesnt seem to do "no intercept models"=
#   ##  y  ~  0 + x + w  and y ~ x + w give same model
#   xrows <- nrow(x)
#   namex <- colnames(x)
#  x <- cbind(rep(1,times=xrows),x)
#  colnames(x) <- c("(Intercept)",namex)
# }
#   
# variable_names <- attr(reg$terms,"term.labels")
  
xlevels <- reg$xlevels  

## no xlevels for rms, use Design attribute
if(rms) {
  xlevels <- reg$Design$parms
  ##  
  ## there is a problem here with rms. If  formula has "as.factor(X)"
  ## names of xlevels is  "X". should be as.factor(X) for consistency.
  ## plug by adding "as.factor()" back into xlevel names
  
 ##   
 gXm  <- getXmulti(variable_names)
 asfact <- which(unlist(gXm[2])=="as.factor()" ) 
 if(length(asfact) > 0) {
  asf <- unlist(gXm[1])[asfact] 
   ## rEPLACE NAMES(XLEVELS) WITH  "AS.FACTOR()"" ADDED BACK IN
  ## loop this?? must be a better way??
   for(j in 1:length(asfact)  ) {
 
 names(xlevels)[which(names(xlevels)==asf[j])] <- paste0("as.factor(",asf[j],")")
   }
 }
 
 asstar <- which(unlist(gXm[2])=="strat()" ) 
 if(length(asstar) > 0) {
  asf <- unlist(gXm[1])[asstar] 
   
  ## loop this?? must be a better way??
   for(j in 1:length(asstar)  ) {
 
 names(xlevels)[which(names(xlevels)==asf[j])] <- paste0("strat(",asf[j],")")
   }
 }


}
  

     if(cox  & !rms ){
## check on  whether Surv() is non-interval
## (note survreg unnecessary as it doesn't support
##  stop-start Surv objects)
   
     Yatt <- attributes(reg$y)
     scurvy <- unlist(Yatt[[2]][2])

     ## this should be either ""time" "status" 
     ## or "start" "stop" "status"
     if(!is.element("time",scurvy)){
       if(is.element("start",scurvy)) {
         return(message("start-stop time type Surv() objects are not supported in regplot"))
         }
         else
          { return(message("mispecified Surv() in formula"))}
     }
      if(Yatt$type != "right"){
       return(message("must be a right censored Surv() object"))}
     
     }
     
    
  #   
  # ## check on weights model for cox and survreg models
  # weighted <- FALSE
  # W <- NULL
  # if(!is.null(reg$weights)){
  # message("Replicate integer weights assumed")
  # weighted <- TRUE
  # W <- floor(reg$weights)
  # 
  # if(all(W==0)){
  #   message("all are zero weighted  - may well  crash!")}
  # if(any(W <0)){
  #  message("there are negative weights - may well crash!")}
  #  }
  # 
##Sobject <- names(attr(reg$terms,"dataClasses")[1])
if(rms) {Sobject <- as.character(reg$sformula)[2]}
else
{
         Sobject <- names(attr(reg$terms,"dataClasses")[1])}
## extract time variable name  from  "Surv(time,fail)" object structure
## remove spaces
Sobject <- gsub(pattern= " ", replacement="",x=Sobject)
has_surv <- grep(pattern="Surv\\(",  x=Sobject )

if(length(has_surv)==0 ){
 return(message("Surv(..) not explicit in regression formula. Use Surv(..) in formula. "))
}
##  messy, should be better way??
 stripped  <- sub(pattern="Surv\\(", replacement="", x=Sobject )
 stripped  <- sub(pattern="\\)",     replacement="",x=stripped)
 stripped  <- sub(pattern=" ",       replacement="",x=stripped)
# 
#  if(Sobject == stripped) {
# ## Sobject must be a Surv() object specified outside of formula
#    message("Surv() object ",paste(Sobject)," should preferably be specified in formula")
#  }
 
 outcome_names <- strsplit(stripped,",")

 yvar <- unlist(outcome_names)[1]
 deadvar <-  unlist(outcome_names)[2]


 ##if(is.na(deadvar)){message("Note: no censoring in model")}
 
 
 ## deadvar may be a string with values eg  "status == 1"
 ## pick out first word using this little function
 deadvar <- string_fun(deadvar)
## coefficients <- reg$coefficients
## patch in possibility of zero beta.  Throws factor variables!
  coefficients <- ifelse(coefficients==0,0.0001,coefficients)
 

  ## cox/survreg
 ## if(interval=="coeff"){
    SEbeta <-  sqrt(diag(reg$var))
   ## if(survreg) last item is variance of log(scale) parameter,remove
    survreg <- !cox
# 
#     if(survreg){
#          if( reg$dist != "exponential" ){
# 
#   ##need to strip ot the SE parameter of the additional scale parameter
# 
#         lastp <- length(SEbeta)
#         ## assume it is last of the parameter vector
# 
#       SEbeta <-SEbeta[-lastp]}
#       }
# browser()
#       Lcoefficients <- coefficients -1.96*SEbeta
#       Ucoefficients <- coefficients +1.96*SEbeta
#     #}
# 

  offset <- !is.null(reg$offset) 
 ## variable_names <- attr(reg$terms,"term.labels")
  
  
  # ##==================================================\\
  # ## how to deal with possibility of strata() variables
  # ## posibility of a strata(x) appearing in variable_names.
  # ## need to remove
  # 
  # 
  # lv <- length(variable_names)
  # istrat <- NULL
  # vstrat <- NULL
  # nstrataterms <- 0
  # for (i in 1:lv){
  #   gX <- getX(variable_names[i])
  #   if(gX[2] ==  "strata()" | gX[2] ==  "strat()"){
  #      nstrataterms <-  nstrataterms + 1
  #     strata_vars <- gX[1]
  #     strata_vars <- unlist(strsplit(strata_vars,", "))
  # 
  #    istrat <- c(i,istrat) 
  #    vstrat <- c(variable_names[i],vstrat)
  #   }}
  # ## deAl with strata() in model by removing the strata() item 
  # ##  from variable_names and xlevels
  
  if(nstrataterms >1){
    return(message( "Error: regression formula has >1 \"strata()\" term. regplot allows only one"))
  }

strata <- FALSE
strata_levels <- NULL
slevels <- NULL
nstrata <- 0
  if(!is.null(vstrat)){
strata <- TRUE  
vstrat <- variable_names[istrat]
    variable_names <- variable_names[-istrat]
  ## iv <- which(names(xlevels) == vstrat)
  ## for cox model also need to remove  strata() from xlevels.
  ## this  seems not required for survreg 
    

    if(cox ) {
  ##remove strata variable levels from xlevels
    s <- is.element(names(xlevels),vstrat )
   iv <- which(s==TRUE)
   strata_levels <- unlist(xlevels[iv])
   nstrata <- length(strata_levels)
    xlevels <- xlevels[-iv]
    }
    
  if(survreg ){
  
   strata_levels <- names(reg$scale)
   nstrata <- length(strata_levels)
  }

    slevels <- strata_levels 
  ##  possibility if strata are factors that strata_levels 
  ##  not of required form  "sex=f, ascites=1". If both factors
  ##  eg sex and race it is returned as "f, maori" . Don't
  ##  know why there is this peculiarity.
  ##  for absence of "="
##  
    check <- grep("=",strata_levels)

    if(length(check)==0){
      ## pad out strata values to required form 
      

     L <- paste0(strata_vars,"=",unlist(strsplit(strata_levels, ", ") ))
     nvars <- length(strata_vars)
     test <- NULL
     for(i in 1:nstrata){
       i1 <- (i-1)*nvars +1
       i2 <- i*nvars
      test <- c(test,paste( L[i1:i2] , collapse=", ") )
     }
    strata_levels <- test
    }##if(length(check)==0){
 ##     

  message(vstrat,    " levels: ", paste(strata_levels, collapse=",  ") )
  
  }  ##if(!is.null(vstrat)){

  #  actualdata <- model.frame(reg)
  #  ##  
  # ## strip off last item "(weights)" 
  #  if(weighted)actualdata <- actualdata[-length(actualdata)]
 
## There is possibility of exp(coef)=Inf from coxph
    # if(any(is.infinite(exp(coefficients)))){
    # message("model with at least one exp(coef)=Inf. Probably a bad model!")
    # }
    #}




    if(survreg){
         if( reg$dist != "exponential" ){
## note there are a scale parameter for each stratum.
## these are included in SEbeta. Need to extract to leave just beta coefficient SE
## need to strip ot the SE parameter of the additional scale parameter(s)
        leaveout <- max(1,nstrata)
        lastp <- length(SEbeta)
        ## assume it is last of the parameter vector

      SEbeta <-SEbeta[1:(lastp-leaveout)] }
      }

      Lcoefficients <- coefficients -1.96*SEbeta
      Ucoefficients <- coefficients +1.96*SEbeta
   
 nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")
## nointercept  only used to  force reparameterisation. Not needed to be TRUE for Cox 
 ##models
 
 ##if(cox) nointercept <- FALSE
##browser()
 
R <- list(xlevels,weighted,W,yvar,coefficients,offset,SEbeta,
     Lcoefficients,Ucoefficients,variable_names, deadvar,actualdata,Pval,x,
     strata_levels,slevels,vstrat,nointercept_term)
return(R)
}  ##coxsurv_1 

##############################################################################

##  extract key quantities for  lme4  regression
lmer_1 <- function(reg){
fail <- FALSE
Sreg <- summary(reg)

  x <- model.matrix(reg)
 
## x is design matrix, excludes random effects vars 
## create predicted values  for all data
 
          d.f <- Sreg$devcomp$dims[1]- Sreg$devcomp$dims[3]
         tval <- abs(Sreg$coefficients[,3])
         Pval <- 2*(1-pt(tval,df=d.f) )
 coefficients <- Sreg$coefficients[,1]
       SEbeta <- Sreg$coefficients[,2]
Lcoefficients <- coefficients -1.96*SEbeta
Ucoefficients <- coefficients +1.96*SEbeta
    

## names of beta coefficients
names(coefficients) <- attributes(reg@pp$X)$dimnames[[2]]
##  alt-beta coefficients
##mxd@pp$delb
## two ways to get model frame
##mxd@frame %>% head
actualdata <- model.frame(reg)

#
nv <- ncol(actualdata)
yvar <- names(actualdata)[1]
##  need to create variable_names, which  must include interaction terms 
## need to mess about to do this.
## First get fixed variable names  (excludes interactions and random effect vars):
if(class(reg)[1]=="lmerMod")
  {
  fixed.varnames <- attr(attributes(reg@frame)$terms, "varnames.fixed")
  }
## or  use names of dataClasses?  No, it includes random effects
##fixed.varnamesX <- names(attr(attributes(reg@frame)$terms, "dataClasses"))
## or use :  
else
{

fixed <- as.character(attr(attributes(reg@frame)$terms, "predvars.fixed"))
## this fails to return  pseudo functions componemts bs(), rcs() 
## first item is "list", second is   y-variable
fixed.varnames <- fixed[-1]

}

##------------------------------------------
###  functions in formula problems:

## glmer models. cant seem to get to handle pseudofunctions.
## needs a local patch here to determine rcs(), bs() 
if(class(reg)[1]=="lmerMod"){
  cls <- "lmer"
  notallowed <- c("rcs()")
 ## rcs() nt allowed because predict() command for it craps out
  }
else
{
  ## cant get anything to work with glmer!!
  cls <- "glmer"
  notallowed <- c("rcs()","poly()","bs()","ns()")
}
## extract the kernels

##xxx <- getXmulti(fixed.varnames)[[2]]
## get out the outer function using getX
## in lapply
xxx <- lapply(fixed.varnames, function(x) getX(x,outer=TRUE))
##returns a list. Unlist gives all items, pich out even nos
xxx <- unlist(xxx)[seq(from=2,2*length(xxx),by=2)]
banned <- xxx %in% notallowed

if( any(banned) ){

message(paste(
    "Function(s) ",xxx[which(banned)]," not supported in ",cls," formula"))
 
  ## make a note why?
  if(xxx[which(banned)]=="rcs()" & cls=="lmer"){
    message("because rcs() fails in lmer generic predict() function. Try bs() instead")
  }
return() 
# fail <- TRUE
# #R <- list(rep(NA,times=17),fail)
# ## why doesn't rep work here? 
# ## Do it messy way to return 17 NAs!
# R <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,
#           NA,NA,NA,NA,NA,NA,NA,NA,fail)
# return(R)
}
##------------------------------
#### random vars (first 2 on list not needed)
randomv <- as.character(attr(attributes(reg@frame)$terms, "predvars.random"))[-(1:2)]
randomv <- paste(randomv,collapse=",")
lfv <- length(fixed.varnames)
## 

## Nest get names of all variables, including interaction and random effect terms
term.labels.varnames <- attr(attributes(reg@frame)$terms, "term.labels")

# extract only the interaction elements  with : 
ints <-   term.labels.varnames[grep(":", term.labels.varnames)] 
##append interaction terms to fixed.variables, excluding first 
## element of  fixed.variables which is  outcome Y

variable_names <- c(fixed.varnames[2:lfv],ints)
#

## offset force as reg$offset
offset <- !all(reg@resp$offset==0)

## get levels of factors, excluding possibility of factor outcome

##testing 
 ## this fails for glmer with bs() function but ok for lmer?
fix_frame <- reg@frame[fixed.varnames[2:lfv]]

#  
fs <- which(sapply(fix_frame,class)=="factor")

##xlevels <- list(length=0)
xlevels <- NULL

if(length(fs)>0) {
## need to establish  xlevels of factors 
lfs <- length(fs) 

xlevels <- list(length=lfs)

for(j in 1:lfs){
 

   xlevels[j] <-  list(levels(fix_frame[,fs[j] ]))
   names(xlevels)[j] <- names(fs)[j] 
    

}
}
## weights specified in  regression? 
   W <- reg@frame$`(weights)`
   weighted <- FALSE
   
  

 if(!is.null(W)){ 
   
   W <- floor(W)
     message("Replicate integer weights assumed ")
     
     

  
    if(any(  W-reg@frame$`(weights)` !=0) ){
      message("Note: non-integer weights have been floored")
    }

     
     
     
   
   if(all(W==0)){
    return(message("all are zero weight"))}
    if(any(W <0)){
    return(message("negative weight"))}
     weighted <- TRUE }
   
   ##  
nointercept_term <- !(names(coefficients)[1]=="Intercept" | names(coefficients)[1]=="(Intercept)")
##

R <- list(xlevels,weighted,W,yvar,coefficients,offset,SEbeta,
    Lcoefficients,Ucoefficients,variable_names, 
    randomv, fixed.varnames,actualdata,fix_frame,Pval,x,
    nointercept_term)
return(R)
   
}  #end lmer_1

##############################################################################

subticksf <- function(maintickpos, maintickvalue,npos,stick=0.05, func=NA,par, 
  decide=FALSE,axcol="black"){
l <- length(maintickvalue)

## possibility that  main axis has only 1 point?
# so no minor ticks possible, nothing to do

if(l >1){
  
## add 4 subtick marks between major ones, unless decide = TRUE.
##  Accounts for function if  func is not NA
## stick is short-tick length
## (ltick is long-tick length)
n4 <-  c(1,2,3,4)
mtv <- maintickvalue
if( !is.na(func) ) {
  if(func == "exp")      {mtv <- log( maintickvalue )}
  if(func == "expit")    {mtv <- log( maintickvalue / (1-maintickvalue) )}
  if(func == "cox")      {mtv <- log( log(maintickvalue) / log(par[1]) )}
  if(func == "loglogistic"){
    mtv <- -log(( ( (1-maintickvalue)/maintickvalue)^(1/par[1]) ) / par[2])}
  if(func == "log()" ){mtv <- exp( maintickvalue )}
  if(func == "sqrt()" ){mtv <-  maintickvalue*maintickvalue}
 
 
 if(func=="lognormal"){
 z <- qnorm(1-maintickvalue)
 mtv <- log(par[2]) -z/par[1]
 } 
 if(func == "gaussian"){
   z <- qnorm(1-maintickvalue)
  mtv <-  par[2] -z/par[1]  }
  
 
if(func == "weibull"| func == "exponential"){
 
 lam <- ((-log(maintickvalue))^(1/par[1]))/par[2]
mtv <- -log(lam)
 
 } 
 }

##  do it piecemeal, filling between major  ticks
for(i in 1:(l-1) ) {
  
  if(decide){
    stv <- pretty(maintickvalue[i:(i+1)])
            }
  else
  {
    gap <- ( maintickvalue[i+1] - maintickvalue[i] )/5
    stv <- maintickvalue[i] + n4*gap
  }
  

  
  if(!is.na(func)) {
  if(func == "exp")      {stv <- log(stv)}
  if(func == "log()")    {stv <- exp(stv)}
  if(func == "sqrt()")   {stv <- stv*stv}
  if(func == "expit")    {
    ## disallow possible 1 or 0 values

    stv <- stv[which(stv>0 & stv <1)]
    stv <- log(stv / (1-stv))}
    
  if(func == "cox")   {
    
    ## disallow possible 1 or 0 values
    stv <- stv[which(stv>0 & stv <1)]
    stv <- log( log(stv) / log(par[1]) )}        
  
  
  
  
   if(func == "loglogistic"){
  stv <- stv[which(stv>0 & stv <1)]
  stv <- -log(( ( (1-stv)/stv)^(1/par[1]) ) / par[2])
 } 
 
 
 if(func == "lognormal"){
  stv <- stv[which(stv>0 & stv <1)]
 z <- qnorm(1-stv)
 stv <- log(par[2]) -z/par[1]
 } 
  
 if(func == "gaussian"){
   stv <- stv[which(stv>0 & stv <1)]
   z <- qnorm(1-stv)
   stv <-  par[2] -z/par[1]  
}
  
 
if(func == "weibull"| func== "exponential" ){
 stv <- stv[which(stv>0)]
 lam <- ((-log(stv))^(1/par[1]))/par[2]
 stv <- -log(lam)
} 

  }  
  
  
  
    R <- (maintickpos[i+1] - maintickpos[i])/(mtv[i+1]-mtv[i])
    subtickpos <-  R*(stv - mtv[i]) + maintickpos[i] 
 
  segments( subtickpos,npos,subtickpos,npos-stick,col=axcol)

  }  ##for(i in 1:(l-1) )
  }  ##if(l>1) 
} 

##############################################################################
## pretty values, specifically of a probability scale

myprettyP <- function(P){ 
  
   
  prettyprobs <- pretty(P,n=10)
  xP <- max(P)
  mP <- min(P)
  prettyprobs <- prettyprobs[which(prettyprobs>0.001 & prettyprobs< 0.999)]
 
  
 nticks <- length(prettyprobs)
  it <- 1
  
  ## need to "fill-in" gaps, for eample in 0 - 0.1
  ## bt expanding and prettyfying the fill-in
while(prettyprobs[1] - mP > 0.0005 & it<5) {
   
   xpand <- pretty( c(mP, prettyprobs[1:2] ), n=6 )
   prettyprobs <- c(xpand,prettyprobs[3:nticks]) 
   prettyprobs <- prettyprobs[which(prettyprobs>0.001)]
   nticks <- length(prettyprobs)
   it <- it +1 
   }
## also for upper end  e.g 0.9 - 1
 it <- 1
while(xP -prettyprobs[nticks] > 0.0005 & it<5) {
   xpand <- pretty( c(prettyprobs[(nticks-1):nticks],xP),n=6)
   prettyprobs <- c(prettyprobs[1:(nticks-2)],xpand) 
   prettyprobs <- unique(prettyprobs)
   prettyprobs <- prettyprobs[which(prettyprobs<0.999)]
    nticks <- length(prettyprobs)
    it <- it +1 
   
}
  
   ## filter out  values witrh >=5 charachters 
  ##effectively 3 dps since minch=3 i.e  0.x
  nch <- nchar(as.character(prettyprobs))
  minch <- min(nch)
  
prettyprobs <- prettyprobs[which(nch <=minch+2)] 


 return(prettyprobs)
}

################################################################

## pretty  values of an Odds scale  (or any positive stretched scale)

myprettyO <- function(O){ 
  
  xP <- max(O)
  mP <- min(O)
  
  prettyodds <- pretty(O,min.n=20,n=20)
  ##  

  prettyodds <- prettyodds[which(prettyodds>0 )]
  nticks <- length(prettyodds)
  
  ## 0.00007 ?? 
   while(prettyodds[1] - mP > 0.0001) {
   
   xpand <- pretty( c(mP, prettyodds[1:2] ) )
   prettyodds <- c(xpand,prettyodds[3:nticks]) 
   prettyodds <- prettyodds[which(prettyodds>0)]
   nticks <- length(prettyodds)
   }


 return(prettyodds)
}

###############################################################

mypretty <- function(O){ 
  
  xP <- max(O)
  mP <- min(O)
  
  prettyodds <- pretty(O,n=8)
  ##  


  nticks <- length(prettyodds)
   while(abs(prettyodds[1] - mP) > 0.0007) {
   
   xpand <- pretty( c(mP, prettyodds[1:2] ) )
   prettyodds <- c(xpand,prettyodds[3:nticks]) 

   nticks <- length(prettyodds)
   }


 return(prettyodds)
}

##############################################################################

prettyalt <- function(x){
  
  ## function to  create pretty values of a log-scale
  
  ## get pretty values of underlying logged values
  prettyx <- pretty(x,n=4)
  e <- exp(prettyx)
  l <- length(prettyx)
  P <- NULL
  ##  
  ## fill in getting pretty values of exp(x) in gaps of x
  ## and concatenate
  for (i in 1:(l-1)){
    P <-  c(P,pretty( c(e[i],e[i+1]),n=4 ) )
  }
  P <- unique(P)
  return(P)
}

#################################################################

Total_distribution <- function(L,M,pointscale, tot_score,none, 
  PT,nrows,ltick, stick, center,delta,dencol,boxcol,spkcol,tickl,aspect,
  yref_pos,showsubticks,allfact,cexscales,lme4)
  {
 
# -------------------------------------------------------- 
# function for drawing total score  distribution
# -------------------------------------------------------- 

  if(lme4){
    {text(x=L-0.27*(M-L), adj=c(0,1), yref_pos,paste("Total fixed+random effects"),font=4)}
  }
  else
  {

  if(pointscale){
  text(x=L-0.27*(M-L),adj=c(0,1),yref_pos,paste("Total points"),font=4)}
  else
  {text(x=L-0.27*(M-L), adj=c(0,1),yref_pos,paste("Total score"),font=4)}
  }

## now force the position of the box plot to coincide with L, M at the end points

Max_tot_score <- max(tot_score)
Min_tot_score <- min(tot_score)
Range <- Max_tot_score-Min_tot_score



scale_score <- (M-L)* (tot_score -Min_tot_score)/Range + L

##position of distribution at npos+1=2
## position of Total scale at npos+0.5=1.5
##  make npos the centre line of each  box, violin plot 
npos <- yref_pos -0.5

#

if(PT != "no plot"  ){
   


  if(PT == "boxes"){
    
    myboxes(scale_score,npos,dencol,aspect,L,M)

  } ##if(boxes)  
  
  
  else 
    
  {  
    
  score_uniq <- unique(scale_score)
  luniq <- length(score_uniq)
 
  bandwidth <- 0.01*(max(scale_score)-min(scale_score))
    if(PT=="violin"){
    box <- vioplot(scale_score, add=TRUE,at=npos,horizontal=TRUE,  wex=0.7, h=bandwidth, 
                   colMed="black", pchMed=21, border="blue",col=dencol)
## NTM:  this vioplot call printing out "NULL". Why? Must be something to do w vioplot(), not me?
  }
   if(PT=="bean") {
     box <-  beanplot(scale_score, add=TRUE,log="", at=npos,horizontal=TRUE,wex=0.8, bw=bandwidth, 
                    what = c(0,1,0,1),maxwidth=0.5,ll=0.1,overallline="median",beanlinewd=0, method="overplot",
                     kernel="epanechnikov",  axes=FALSE, border="blue", col=dencol)
             pars = list(boxwex = 0.4, staplewex = 0.5, outwex = 0.5,cex=0.5) }                  
  
  if(PT=="boxplot"){
  box <-  boxplot(scale_score, add=TRUE,at=npos,horizontal=TRUE,  border="blue",col=dencol,
                  pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5,cex=0.5))
  }
  
if(PT=="density") { mydensity(scale_score,npos -0.5 ,dencol)}

    
if(PT=="bars") { myhist(scale_score,npos -0.5 ,dencol)}
    
if(PT=="ecdf") {  mycumul(scale_score,npos -0.5,tickl,dencol)  }

if(PT=="spikes"){    myspikes(scale_score,npos-0.5,spkcol,aspect,L,M)}  
  
 
  }  ## else of ##if(dplot=="boxes" )
    
  
  
}

#if pointscale adust scale to output pretty points
tickvalues <- pretty(tot_score,n=6)
##browser()
if(!pointscale){
tickv <- tickvalues
tickp <- (M-L)* (tickv -Min_tot_score)/Range + L
## fix positions of the scale relative to values of pretty values
## draw line to fit L-M boundary, ticks having  screen coordinates
}
else
{


  tickv <- tickvalues
  
  ## convert to points, but need to account for there being sum of nrows parameters
  tickv <- 100*(tickv -L*nrows)/(M-L)
  tickv <- pretty(tickv,n=8)
  ## convert back into actual total scores
  pts   <- ((M-L)*tickv)/100 +L*nrows
  tickp <- (M-L)* (pts -Min_tot_score)/Range + L 
  
  
} 

  Total_pos <- yref_pos -1
  limits <- which(tickp > L-0.15*(M-L) & tickp < M+0.15*(M-L) )
if(length(limits) > 2){
  tickp_lim <- tickp[limits]
  tickv_lim <- tickv[limits]
}
  ##browser()
segments(min(tickp_lim),Total_pos,max(tickp_lim),Total_pos)
segments(tickp_lim,Total_pos,tickp_lim,Total_pos - ltick)
if(showsubticks) subticksf(tickp_lim, tickv_lim,Total_pos,stick)


text(x=tickp_lim,y= Total_pos -delta ,paste(signif(tickv_lim,3)),cex=cexscales)

tickvalue <- tickv

## suppress  total axis label?

if(pointscale) {ex <- "Total points "}
else
{
if(center) 
{ex <- expression(italic(paste(Sigma, beta,"(X-m) ")))
     if(lme4){ex <-  expression(italic(paste(Sigma, beta,"(X-m)+RE ")))}
}

   else
   {ex <- expression(italic(paste(Sigma, beta,"X ")))
      if(lme4){ex <- expression(italic(paste(Sigma, beta,"X+RE ")))}
  }
}

}## end of function Total_distribution

##############################################################################

RE_distribution <- function(L,M,pointscale, RE, 
  PT,nrows,ltick, stick, center,delta,dencol,boxcol,spkcol,tickl,aspect,
  yref_pos,showsubticks,allfact,cexscales,lme4){
 
# -------------------------------------------------------- 
# function for drawing random effects RE distribution
# -------------------------------------------------------- 
 
npos <- yref_pos - 0.5

 # 
 ## use boxes of random effects 

  if(PT== "boxes" ){
     npos <- npos - 0.2 
     myboxes(RE,npos, boxcol, aspect,L,M)  
 
  } ##if(boxes)  
  
  
  else 
    
  {  
    #   random_effects plotting 
  score_uniq <- unique(RE)
  luniq <- length(score_uniq)
 
  bandwidth <- 0.01*(max(RE)-min(RE))
    if(PT== "violin" ){

  box <-  vioplot(RE, add=TRUE,at=npos,horizontal=TRUE,  wex=0.7, h=bandwidth, 
                   colMed="black", pchMed=21, border="blue",col=dencol)
  }
   if(PT== "bean" ) {
 
     box <-  beanplot(RE, add=TRUE,log="", at=npos,horizontal=TRUE,wex=0.8, bw=bandwidth, 
                    what = c(0,1,0,1),maxwidth=0.5,ll=0.1,overallline="median",beanlinewd=0, method="overplot",
                     kernel="epanechnikov",  axes=FALSE, border="blue", col=dencol)
             pars = list(boxwex = 0.4, staplewex = 0.5, outwex = 0.5,cex=0.5) }                  
  if(PT== "boxplot" ){

  box <-  boxplot(RE, add=TRUE,at=npos,horizontal=TRUE,  border="blue",col=dencol,
                  pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5,cex=0.5))
     
  }
  
if(PT== "density" ) { myd <-  mydensity(RE,npos-0.5,dencol) 
  if(myd) {
    message(paste("singular density for random effects?"))}
  }

  
    
if(PT== "bars" ) { myd <-  myhist(RE,npos-0.5,dencol) 
  if(myd) {
    message(paste("singular value for random effects?"))}
  }
    
if(PT== "ecdf" )   {  mycumul(RE,npos-0.5,tickl,dencol) }

if(PT=="spikes")  {  myspikes(RE,npos-0.5,spkcol,aspect,L,M)   } 
  
 
  }  ## else of ##if(dplot=="boxes" )
    


}## end of function RE_distribution

##############################################################################

clicked_action <- function(npanels,row_of_panel,nrows,L,M,S,isadummy,
  isinteraction_row, isa_factor,isa_pseudo,xlevels,row_names,vact,
  vact_names,nointercept,firstfactor,betas,m,person,nudist,irank,
  make_space,variable_names,lme4, selected,plot.type,plot.typef,
  haslikefactors,splineplot,X,rawdata, 
  kernel_varname,raw_variable_names){
##--------------------------------------------------------
## function clicked_action
## function to return action required for a mouse click
## if clicked on new  values adddata is returned
##------------------------------------------------------- 

     Rvar <- NA
     Rval <- NA
     clickedon_obs <- FALSE
     gap <- (npanels+2 + make_space)/100
     ## position just down from top
  ## create "menu bar" of graphic choices 
     ##  
     dialogpos <- npanels+2+ make_space + 1.5*gap
     
     xgap <- (M-L)/11
     ## rectangle around ESC
    # rect( L+11*xgap-xgap*0.4, dialogpos-gap, L+11*xgap+0.4*xgap, dialogpos+gap,border="blue",col="white")
     text( L+12*xgap,dialogpos,  paste("ESC"),cex=0.8,col="blue", font=2) 
     
     ## rectangle around PLOTS
 ##    rect( L , dialogpos-gap, L+xgap*10, dialogpos+gap,border="blue",col="white")
     text( L+0.5*xgap,dialogpos,  paste("      "),cex=0.7,col="blue")                          
    
     text( L+1.5*xgap,dialogpos,  paste("no plot"),cex=0.7,col="blue")
     text( L+2.5*xgap,dialogpos,  paste("density"),cex=0.7,col="blue")
     text( L+3.5*xgap,dialogpos,  paste("boxes")  ,cex=0.7,col="blue")
     text( L+4.5*xgap,dialogpos,  paste("spikes") ,cex=0.7,col="blue")
     text( L+5.5*xgap,dialogpos,  paste("boxplot"),cex=0.7,col="blue")
     text( L+6.5*xgap,dialogpos,  paste("ecdf")   ,cex=0.7,col="blue")                          
     text( L+7.5*xgap,dialogpos,  paste("bars")   ,cex=0.7,col="blue")                          
     text( L+8.5*xgap,dialogpos,  paste("violin") ,cex=0.7,col="blue")
     text( L+9.5*xgap,dialogpos,  paste("bean")   ,cex=0.7,col="blue")
    ## text( L+10.5*xgap,dialogpos,  paste("Default"),cex=0.7,col="blue")
## select dialog on same row 
     ## recangle aound SELECT
  ##   rect( L-3*xgap, dialogpos-gap, L-xgap, dialogpos+gap,border="blue",col="white")
     
     text(L-2.5*xgap,dialogpos +1.5*gap,  paste("Select"),cex=0.7,col="blue")
      text(L+5.5*xgap,dialogpos +1.5*gap,  paste("Plot type"),cex=0.7,col="blue")
      text( L-3*xgap,dialogpos,  paste("All"),cex=0.7,col="blue")
     text( L-2*xgap,dialogpos,  paste("    None")   ,cex=0.7,col="blue")
segments(L-(M-L)*0.3 , dialogpos-1.5*gap, M + (M-L)*0.2,dialogpos-1.5*gap, col="blue")    
     
    ##selected <- rep(FALSE, times=(npanels +2))
     checkpos <- 0.7
     totalcheckpos <- 0.7
     checkcol <- "blue"
     stay <- TRUE
##set up checkboxes of selected on entry
     for(i in 1:npanels){
        ypos <-  i+ make_space +checkpos
         points(L-0.3*(M-L),ypos,cex=1.5,col=checkcol, pch=0)
         if(selected[i]){
           ## colour  in box if selected
         points(L-0.3*(M-L),ypos,cex=1.45,col=checkcol, pch=15)}

     }
     
     ypos <- -totalcheckpos +make_space +checkpos
     points(L-0.3*(M-L),ypos,cex=1.5,col=checkcol, pch=0)
         if(selected[npanels+2]){
         points(L-0.3*(M-L),ypos,cex=1.45,col=checkcol, pch=15) }
       
##message(paste(selected))
    
     while(stay){
 
## how to use Esc keyboard to interrupt without 
## creating an error?? Dont know! Keyboard Esc gives
##  Error in if (panel > 0 & panel <= npanels) { : argument is of length zero
# 
    esc <- FALSE
    ## click on graphic to locate one (n=1) x,y coordinate
    
    XY <- locator(n=1)
  ## note the selected panel will depend on position of the checkbox
  ##    need to subtract 1 for checkbox just above the name of variable 

     panel <- floor(XY$y -  make_space ) 
     ##message(paste("panel",panel))
    ## browser()
##------------------------------------------------------------------------     
     ##  keyboard Esc press 
    if(length(panel)==0){
## keyboard Esc press  returns "numeric(0)" for panel, ie. length 0.      
      esc <- TRUE
      act <- list(esc,nudist,Rvar,Rval,selected,plot.type,plot.typef,splineplot)
## exiting, erase dialog menu bar with a white-over box slightly bigger
## uses same code as below for a mouse Esc 
      
## use as secret Esc keeping  diaalog
      
      if(FALSE){
  rect( L-0.3*(M-L), dialogpos-1.5*gap, L+13*xgap, dialogpos+2*gap,
  border="white",col="white")
## erase selected  points checkboxes
     for(panel in 1:npanels){
         rect( L-0.35*(M-L), panel+make_space +checkpos+0.2, L-0.27*(M-L), 
         panel+make_space +checkpos -0.2,
         border="white",col="white")
     }
        
      }  ##if(FALSE)
       return(act)
       }
##-------------------------------------------------------------------------
     
      if(panel >0 & panel <= npanels){
    ## if(panel >0){
       row <- row_of_panel[panel]
       row <- irank[row]
       
## clicked on a slpineplot thumbnail  on rhs of panel. 
##browser()
       if( kernel_varname[row] != "" &  XY$x > M ){
         splineplot <- !splineplot
         nudist <- TRUE
         ## list of items to return
       act <- list(esc,nudist,Rvar,Rval,selected,plot.type,plot.typef,splineplot)
       return(act)
       }
     }
       
     
      plot_select <- floor((XY$x-L)/xgap + 1 )
      
    ##message(paste("plotselect=",plot_select))
  ##---------------------------------------------------
 ## click on dialog PLOTS area
## if( plot_select <=13 & plot_select >= 0 & panel > npanels){ 
 ##browser()
 ## ensure clicked above points line     
      if( plot_select <=13 & plot_select >= 0 & XY$y > npanels+make_space+1.5){ 
        ##  cplot <- NA
  if(plot_select <= 2)  cplot <- "no plot" 
  if(plot_select == 3)  cplot <- "density"
  if(plot_select == 4)  cplot <- "boxes" 
  if(plot_select == 5)  cplot <- "spikes" 
  if(plot_select == 6)  cplot <- "boxplot"
  if(plot_select == 7)  cplot <- "ecdf"
  if(plot_select == 8)  cplot <- "bars"
  if(plot_select == 9)  {cplot <- "violin"
   ##                      require(vioplot)
                          }
  if(plot_select == 10)  {cplot <- "bean"
   ##                      require(beanplot)
                          }
  
  
  
  esc <- FALSE
  nudist <- TRUE
   if(plot_select >= 11 )   {  esc <- TRUE
    nudist <- FALSE }
  

  stay <- FALSE
  }  ##if(plot_select <=9 & plot_select >= 1 
 ##-------------------------------------------------
  ## click on SELECTed dialog All (-2) or None (-1)  
    
 ## if(  (plot_select == -2 | plot_select== -1)   & panel > npanels){ 
  if(  (plot_select <= -1)   & panel > npanels){ 
      
      ## clicked on select all or none 
      ## vectorise ypos and xpos and fill in selected[]
ypos <- c(1:(npanels+2) )+ make_space +checkpos
## ypos of total distribution at the bottom - fix up
  ypos[npanels+2] <- -totalcheckpos + make_space +checkpos
xpos <- rep(L-0.3*(M-L),times=npanels+2)

if(plot_select <= -2){selected <-  rep(TRUE, times=npanels+2)
points(xpos,ypos,cex=1.45,col=checkcol, pch=15)
        }
if(plot_select==-1 | plot_select==0 ){selected <-  rep(FALSE, times=npanels+2)}
  points(xpos,ypos,cex=1.45,col="white", pch=15)

 stay <- TRUE 
           
}  ##if(plot_select == -2 | plot_select== -1
##------------------------------------------------------    
    
##  fill in select boxes of current  selected.     
     for(i in 1:npanels){
        ypos <- i+ make_space +checkpos 
         points(L-0.3*(M-L),ypos,cex=1.5,col=checkcol, pch=0)
         if(selected[i]){
         points(L-0.3*(M-L),ypos,cex=1.45,col=checkcol, pch=15)}
         
     }
     ## dont forget the status of tot-distributionb
         ypos <-   -totalcheckpos+ make_space +checkpos 
         points(L-0.3*(M-L),ypos,cex=1.5,col=checkcol, pch=0)
         if(selected[npanels+2]){
         points(L-0.3*(M-L),ypos,cex=1.45,col=checkcol, pch=15) }
 
     
 ##--------------------------------------------------------- 
    
## is a checkbox clicked?  
## i.e. a click on far left, but below the top line 
         

        if(XY$x < L-(M-L)*.1 & panel <= npanels) {
         ypos <-  panel+ make_space +checkpos 
           if(panel<=0  ){
        ##selected  total distribution, default to making this npanels+2 slot
             ## positioned at the bottom
             panel <- npanels+2
             ypos <- -totalcheckpos+ make_space +checkpos 
           }
         
## change selected[panel] status 
         selected[panel] <- !selected[panel]
 ## change the fill of the checkbox       
         
         if(selected[panel]){
         points(L-0.3*(M-L),ypos,cex=1.45,col=checkcol, pch=15)
         }
         else
         { 
         points(L-0.3*(M-L),ypos,cex=1.45,col="white", pch=15)
         }
         
         stay <- TRUE

        }
 ##-------------------------------------------------------------   
   ## click within main body
  ## i.e not on far left and within clickable panels
  ##  indicating a change red-dot value 
         
    if(XY$x>L-0.1*(M-L) & person){
      

      
      OK <- TRUE
       if( panel <=0 | panel>npanels){
        
       OK <- FALSE }
     
    if(lme4 & npanels==panel){
      message("Cannot click random effect")
      OK <- FALSE
    }

      
    if( OK){ 
    nudist <- FALSE   
    stay <- FALSE
    ## clicked on a red dot person  
          clickedon_obs <- TRUE  
     {
         
       row <- row_of_panel[panel]
       row <- irank[row]
        if(row <= nrows ){

     #  
  if(!isinteraction_row[row] & !isa_pseudo[row] ){
  


    if(isa_factor[row]) {
      

     
    Xuniq <- unlist(xlevels[row_names[row]])
   ##  
    if(nointercept & firstfactor == row){
      beta_values <- betas[which(vact_names==row_names[row])]
      
###      eps <- abs(XY$x-beta_values )
  ##   eps <- XY$x-beta_values
      eps <- abs(XY$x-beta_values )
   ##  
      
    }
    else  ##if(nointercept & firstfactor == row
    {
    
    beta_values <- betas[which(vact_names==row_names[row])]
    ##  Need to order with 0 added
     
    ## find closest point clicked i.e.  snap to point
    eps <- abs(XY$x-c(0,beta_values) )
    
    
    } 
    
    
    oeps <- order(eps)
    
    cond <- Xuniq[oeps[1]]
    if(cond=="FALSE") {cond <- FALSE}
  
    if(cond=="TRUE"){cond <- TRUE}

  
    Rvar <- row_names[row]
    Rval <- cond

    }
    
    else  ##if(isa_factor[row])
    
      { nfact <- which(vact==row)
      value <- XY$x/betas[nfact]
   ##update adddata with clicked value   
       var <- row_names[row]
## if a functional value (e.g. log() get actaul value from a lookup. 
          if(kernel_varname[row] != ""){

##          yref <- lookup( X[,row], rawdata[,kernel_varname[row] ], value,reorder=TRUE)
      yref <- lookup( X[,row], rawdata[,raw_variable_names[row] ], value,reorder=TRUE)
       if(length(yref)>1){
      message( paste( "Ambiguous possibilities: ", paste0( signif(yref,3),collapse=", ")  ) ) 
  ## select minimum 
         message(paste("minimum used"))
              Rval <- signif(min(yref),4)
             ## print(Rval)
            }
    else
    {Rval <- signif(yref[1],4)}


        ##  Rval <- signif(yref[length(yref)],3)
          
          Rvar <-  raw_variable_names[row]
          if(is.na(Rval)) message( paste("inadmissible", Rvar) )
          }   ##if(kernel_varname[row] != ""
 
          else
          {
           Rvar <- var
           Rval <- value +m[nfact]
          }
       nudist <- FALSE
       esc <- FALSE
  

   } 
    
  
}    ##iif(!isinteraction_row[row] & !isa_pseudo[row] )

else
  
{
  
  
  if(isa_pseudo[row]){
   ## message(paste("Not clickable functional", kernel_varname[row]) )
  ## e.g for spline type function bs()
   ## get actual value, e.g Age by using lookup table
   
 yref <- lookup( X[,row], rawdata[,kernel_varname[row] ], XY$x,reorder=TRUE)
 
  if(length(yref)>1){
  message( paste( "Ambiguous possibilities: ", paste0( signif(yref,3),collapse=", ")  ) ) 
     message(paste("minimum used"))
     Rval <- signif(min(yref),4)
  }
    else
    {Rval <- signif(yref[1],4)}
      
##browser()
  
     Rvar <-  kernel_varname[row]
 ##    Rval <- signif(yref[length(yref)],3)
     if(is.na(Rval)) message(paste("inadmissible ", Rvar))
  }
 
  else
  {message("Cannot click on interaction") 
  }
}

}  ##if(row>=1 & <=npars
else
  
{
  
 ## esc <- TRUE
  ## do nothing  
  
}


} 
       
       
} # XY$x>L-0.1*(M-L) & panel >=1 & panel <= npanels
}  ## if(person)
 
} #while(TRUE) 
     
     ## need to prohibit density type plots on factors
     if(nudist){

      ## npanels is number of panels (which may include RE panel), but excludes
      ##  the total distribution panels , which is npanels+2 
       np <- npanels
      ## need to consider that  npanels item may be a RE, if lme4 model
      ## and no probition on density
       
       #WHY??  Cos isa_factor() not defined for the npanels_th panel which is RE
       
      
      if(lme4) np <- npanels -1 

       for( panel in 1:np){
         row <- row_of_panel[panel]
         row <- irank[row]
         
         
         {isaf <-  isa_factor[row]}
         
     
         if(selected[panel]){
         ##    
           if(isaf){
 ## can only do these plots if a factor
  if(is.element(cplot,c("no plot","boxes","bars","spikes"))){
           plot.typef[panel] <- cplot }
         
           }
           else
             {
               
           plot.type[panel] <- cplot
           }
        }  #if(selected[panel]
      }   #for( panel in 1:np
     
      ## need to consider total distribution panel??
      if(selected[npanels+2]) plot.type[npanels+2] <- cplot
       if(lme4 & selected[npanels]) plot.type[npanels] <- cplot
     

        
        
        
        
      if(is.na(cplot)){
        
        ##  clicked on non-respnse area of PLOTS menu. undo nudist
        nudist <- FALSE}
      
     }  ##if(nudist)
     
          
     if(esc){
## exiting, erase dialog menu bar with a white-over box slightly bigger
 
     rect( L-0.3*(M-L), dialogpos-1.5*gap, L+13*xgap, dialogpos+2*gap,
  border="white",col="white")
## erase selected  points checkboxes
     for(panel in 1:npanels){
         rect( L-0.35*(M-L), panel+make_space +checkpos+0.2, L-0.27*(M-L), 
           panel+make_space +checkpos -0.2,
           border="white",col="white")
     } 
       
       ## plus the total panel
            rect( L-0.35*(M-L), -totalcheckpos +make_space +checkpos+0.2,
              L-0.27*(M-L), -totalcheckpos +make_space +checkpos -0.2,
              border="white",col="white")
     }
## list of items to return
 act <- list(esc,nudist,Rvar,Rval,selected,plot.type,plot.typef,splineplot)


  
   if(!esc & !clickedon_obs & !any(selected) & plot_select != 11){
     ##forgot to SELECT  for a distribution selection
   message("No panels are selected")
 }

return(act)
} ##end of clicked_action function

##############################################################################

Poisson_scale  <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,ypos,yvar,negbin,showsubticks,cexscales){
 ## tickvalues are pretty values of total beta score
  ## corresponding   exp(betXa+b0) 
  
  
  
 ## Poisson mean <- exp(tot_score + intercept)
## make nice pretty correponding values  of the exp() scale
##  prettymean <- pretty(mean,n=20)
  
  prettymean <- prettyalt(tot_score + intercept)
  
  ## try filling in lower end of scale
   if(prettymean[1]==0){ 
   #  p2 <- pretty(c(0,prettymean[2]) )
   #  prettymean <- c(p2,prettymean[-1])
     prettymean <- prettymean[-1]
  ##   prettymean[1] <- signif(min(mean),1)
     
   }

  prettyscores <- log(prettymean) -intercept
  
    
  ## corresponding plotting positions on L,M
  tickpos <- (M-L)* (prettyscores  - Min_tot_score)/Range + L

  ## also limit by upper value of counts 
   mxcount <- exp(intercept+max(tot_score))
   tickpos <-  tickpos[which(prettymean< mxcount)]  
   prettymean <- prettymean[which(prettymean< mxcount)]

    
 ## tickpos <-  tickpos[limits]  
  ##prettymean <- prettymean[limits]
 ##   
sievePr <- sieve(tickpos,prettymean, 0.04*(M-L))

Xpos <- unlist(sievePr[1])
Pr <-   unlist(sievePr[2])

if(!showsubticks) tickpos <- Xpos

  
  limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.04*(M-L) )

  if(length(limits) > 2){
  tickpos <- tickpos[limits]
  
  Pr <- Pr[limits]
  Xpos <- Xpos[limits]
  }
  segments(min(tickpos),ypos,max(tickpos),ypos)
  segments(tickpos,ypos,tickpos,ypos-ltick)
  if(showsubticks) subticksf( tickpos,prettymean,ypos,stick,func="exp")


  
##sievePr <- sieve(tickpos,prettymean, 0.08*(M-L))


text(x=Xpos,y= ypos - delta,paste(signif(Pr,3)),cex=cexscales)

 ## text(x=tickpos,y= npos+0.6,paste(signif(prettymean,3)),cex=0.7)  
  
 if(negbin)
 {text(min(tickpos),ypos , adj=1, paste("Negbin ",yvar," "),cex=cexscales,font=3  )}
else
{text(min(tickpos),ypos , adj=1, paste("Poisson ",yvar," "),cex=cexscales,font=3  )}
  ## text((max(tickpos)+min(tickpos))/2,ypos -2.5*delta , paste("Poisson mean",yvar),cex=0.8,font=3  ) 
      
  
}  ## Poisson_scale

##############################################################################

Lm_scale <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,ypos,yvar,showsubticks,cexscales){
 
  tickmean <- tot_score+intercept
  prettyscores <- pretty(tickmean,n=8)
  
  ##browser()
  ## tick positions  correspond to scale of the boxplot and its position
  ## after subtracting out the  intercept
  
  tickpos <- (M-L)* (prettyscores -intercept - Min_tot_score)/Range + L
  
      
  
limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
if(length(limits) >2){tickpos <-  tickpos[limits]
prettyscores  <-  prettyscores[limits]}


  ## draw line to fit L-M boundary
   
  segments(min(tickpos),ypos,max(tickpos),ypos)

  segments(tickpos,ypos,tickpos,ypos-ltick)
  
 
  
  text(x=tickpos,y= ypos -delta,paste(signif(prettyscores,3)),cex=cexscales) 

  text(min(tickpos), adj=1,ypos, paste("mean",yvar," "),cex=cexscales ,font=3 ) 
  
   if(showsubticks) subticksf(tickpos, prettyscores,ypos,stick)
  
}  ## end Lm_scale

##############################################################################
### Lmer_scale  never used!!
# Lmer_scale <- function(reg,tot_score,intercept,L,M,Range,Min_tot_score,delta,
#   ltick,stick,ypos,yvar,showsubticks,cexscales){
# 
#   tickmean <- predict(reg)
#   
#   ## invokes predict.merMod {lme4}
#   ## The predict method for merMod objects, i.e. results of lmer(), glmer(), etc.
#    ## default is random.only=FALSE
#   
#   prettyscores <- pretty(tickmean,n=8)
#   diff <- tickmean -tot_score
#   ##  
#   
#   ## tick positions  correspond to scale of the boxplot and its position
#   ## after subtracting out the  intercept
#   
#   tickpos <- (M-L)* (prettyscores - Min_tot_score)/Range + L
#   
#       
#   
# limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
# if(length(limits) > 2){
#   tickpos <-  tickpos[limits]
# prettyscores  <-  prettyscores[limits]
# }
# 
#  
#   ## draw line to fit L-M boundary
#    
#   segments(min(tickpos),ypos,max(tickpos),ypos)
# 
#   segments(tickpos,ypos,tickpos,ypos-ltick)
#   
#  
#   
#   text(x=tickpos,y= ypos -delta,paste(signif(prettyscores,3)),cex=cexscales)                                     
#   text(min(tickpos), adj=1,ypos, paste("mean",yvar," "),cex=cexscales ,font=3 ) 
#   
#    if(showsubticks) subticksf(tickpos, prettyscores,ypos,stick)
#   
# }  ## end Lmer_scale
# 
##############################################################################

 Logistic_scale <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,ypos,yvar,odds,showsubticks,
   cexscales,polr){
   
   ## parameterisation of polr models requires switch
   ## sign (see Ekstrom page 98/99)
if(polr){ E <- exp(-tot_score+intercept)}
else
  {E <- exp(tot_score+intercept)}
if(odds){
 prettyprobs <- myprettyO( E ) 
 
 prettyscores <- log(prettyprobs) - intercept
 
 
}
else
{
prettyprobs <- myprettyP( E/(1+E))

prettyscores <- log(prettyprobs/(1-prettyprobs)) - intercept
}
if(polr) prettyscores <- -prettyscores
tickpos <- (M-L)* (prettyscores  - Min_tot_score)/Range + L
  
limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
if(length(limits) > 2){
  tickpos <-  tickpos[limits]
prettyprobs  <-  prettyprobs[limits]
}

 
sievePr <- sieve(tickpos,prettyprobs, 0.04*(M-L))

Xpos <- unlist(sievePr[1])
Pr <-   unlist(sievePr[2])

if(!showsubticks) tickpos <- Xpos

segments(min(tickpos),ypos,max(tickpos),ypos)
segments(tickpos,ypos,tickpos,ypos-ltick)

## use sieve function to avoid text overlay on axis

##sievePr <- sieve(tickpos,prettyprobs, 0.08*(M-L))
#logistic
text(x=Xpos,y= ypos - delta,paste(signif(Pr,3)),cex=cexscales)

if(odds){
text(min(tickpos), adj=1,ypos , paste("Odds(",yvar,") ") ,cex=cexscales,font=3)  
if(showsubticks) subticksf( tickpos,prettyprobs,ypos,stick, func="exp", decide=TRUE)

}
else
{  
text(min(tickpos), adj=1,ypos , paste("Pr(",yvar,") ") ,cex=cexscales ,font=3)
if(showsubticks) subticksf( tickpos,prettyprobs,ypos,stick, func="expit", decide=TRUE)

}

}  ## end Logistic_scale 

 ##############################################################################
 
probit_scale <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
                            ltick,stick,ypos,yvar,odds,showsubticks,
                            cexscales){
   
   ## parameterisation of polr models requires switch
   ## sign (see Ekstrom page 98/99)
 
   P <- pnorm(-tot_score+intercept)
   
   if(odds){
     prettyprobs <- myprettyO( P/(1-P) ) 
     
     prettyscores <- -qnorm(prettyprobs/(1+prettyprobs)) + intercept
     
     
   }
   else
   {
     prettyprobs <- myprettyP( P)
     
     prettyscores <- -qnorm(prettyprobs) + intercept
   }
##   if(polr) prettyscores <- -prettyscores
   tickpos <- (M-L)* (prettyscores  - Min_tot_score)/Range + L
   
   limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
   if(length(limits) > 2){
     tickpos <-  tickpos[limits]
     prettyprobs  <-  prettyprobs[limits]
   }
   
   
   sievePr <- sieve(tickpos,prettyprobs, 0.04*(M-L))
   
   Xpos <- unlist(sievePr[1])
   Pr <-   unlist(sievePr[2])
   
   if(!showsubticks) tickpos <- Xpos
   
   segments(min(tickpos),ypos,max(tickpos),ypos)
   segments(tickpos,ypos,tickpos,ypos-ltick)
   
   ## use sieve function to avoid text overlay on axis
   
   ##sievePr <- sieve(tickpos,prettyprobs, 0.08*(M-L))
   #logistic
   text(x=Xpos,y= ypos - delta,paste(signif(Pr,3)),cex=cexscales)
   
   if(odds){
     text(min(tickpos), adj=1,ypos , paste("Odds(",yvar,") ") ,cex=cexscales,font=3)  
     if(showsubticks) subticksf( tickpos,prettyprobs,ypos,stick, func="exp", decide=TRUE)
     
   }
   else
   {  
     text(min(tickpos), adj=1,ypos , paste("Pr(",yvar,") ") ,cex=cexscales ,font=3)
     if(showsubticks) subticksf( tickpos,prettyprobs,ypos,stick, func="expit", decide=TRUE)
     
   }
   
 }  ## end probit_scale 
 
 ##############################################################################

 Cox_scale <- function(reg, tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,pos_of_nomo,yvar,odds,baseS,h,s5,betas,center,m,bm,tcut,fail,
   showsubticks,cexscales,slevels,
   strata_levels,strata_gap,vstrat){

  
   nstrata <- 1
   
   if(is.null(baseS)) {
    hx <- h
   ## check for strata?
   if(ncol(h)==3 & names(h)[3] == "strata"){
  #strata_levels <- unique(h[,3])
  nstrata <- length(slevels)
  ##slevels <- strata_values(slevels)
  
  ## squeeze delta a bit, making text closer to axis
  delta <- delta/sqrt(nstrata)
   }
   }
## loop over the stara (if there are any) 
##  drawing a scale for each stratum baseline hazard
   #  
      
     for (istrata in 1:nstrata) {
     yposx <- pos_of_nomo 
  
  ## possibility that hazard not computable - NAs in betas? 
  if(is.null(baseS ) ){
    if(nstrata >1){
         ## extract   the hazards for this stratum. 3rd item is stratum. 
         hx <- h[which(h[,3]==slevels[istrata]),] 
          yposx <- pos_of_nomo + (istrata - 1)*strata_gap
         }
## m  =0 if not centred
   sumexp <- exp( sum(betas*m,na.rm=TRUE) )
    hz <- hx$hazard*sumexp 
 
# # ## user specified failtime. Select hazard "close" to tcut
#     st <- sort(h$time)
#     ibelow <- max(which(st<=tcut ))         
#     ## h5 <- hz[c(ibelow, ibelow+1)]
#     h5 <- hz[ibelow]
#      s5XX <- exp(-h5)
#     
    o <- order(hx$time)
    t <- hx$time[o]
    hz <- hz[o]
    ibelow <- max(which(t<=tcut )) 
    h5 <- hz[ibelow]
    s5 <- exp(-h5)
  

  }
  
  else
  {## input baseline survival  (baseS is non-null)
    ## need to scale if centred graphic
  if(center){
    s5 <- baseS ^exp(sum(bm))}
    else
    {s5 <- baseS}
}
 ## get pretty survival scores
 scut <- s5 ^ exp(tot_score)
 
 
   
 
 if(is.na(s5)) {
   
 message("Cannot make probability scale. Maybe too few data points. Small stratum?")
   
 }
 
 else
   
 {   
 if(odds){
   ##  surviavl odds odds
   sodds <- (scut)/(1-scut)
   prettyX <- myprettyO(sodds)
   
   ##retain "prettyprobs" even though odds
   probs <- prettyX/(1+prettyX) 
   
   exp_score <- log(probs)/log(s5)
  ## corresponding  betaX scores are 
    prettyscore <- log(exp_score)

 }
 else
 {
##  
   
   
 prettyX <- myprettyP( scut)

  ## use S(t)^exp(BX)=1-P to get scores corresponding to probabilities
 exp_score <- log(prettyX)/log(s5)
  ## corresponding  betaX scores are 
 prettyscore <- log(exp_score)
 }
 
 tickpos <- (M-L)* (prettyscore  - Min_tot_score)/Range + L
 
 
 limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
 ## allow bit more space for strata labels
 if(nstrata>1) limits <- which(tickpos > L-0.01*(M-L) & tickpos < M+0.1*(M-L) )
 if(length(limits) > 2){
 tickpos <-  tickpos[limits]
 
 prettyX  <-  prettyX[limits] 
}
sievePr <- sieve(tickpos,prettyX, 0.04*(M-L))

Xpos <- unlist(sievePr[1])
Pr <-   unlist(sievePr[2])

if(!showsubticks) tickpos <- Xpos
 

 segments(min(tickpos),yposx,max(tickpos),yposx)
 segments(tickpos,yposx,tickpos,yposx-ltick)
 
 
 ## subticks on odds scale not yet implemented. 
 if(!odds){
   if(showsubticks) subticksf(tickpos,prettyX,yposx,stick,func="cox",par=s5,decide=TRUE)
 }
 
## use sieve function to avoid text overlay on axis
# 
# sievePr <- sieve(tickpos,prettyX, 0.08*(M-L))
# 
# Xpos <- unlist(sievePr[1])
# Pr <-   unlist(sievePr[2])

 ## put values on the axis  Cox 
 if(fail){ chr <- "<" } else { chr <- ">" }
  if(!odds){
     if(fail) {
     text(x=Xpos,y= yposx - delta,paste(signif(1-Pr,3)),cex=cexscales)
     }
     else
     {
      text(x=Xpos,y= yposx -  delta,paste(signif(Pr,3)),cex=cexscales)
     }
    
  if(istrata==1)text(x=min(tickpos), yposx , adj=1, paste("Pr(",yvar,chr, signif(tcut,4),") " ), cex=cexscales,font=3) 

  }  
    else ##if(!odds
      
    {
      if(fail){
      text(x=Xpos,y= yposx -  delta,paste(signif(1/Pr,3)),cex=cexscales)}
      else
      {
      text(x=Xpos,y= yposx -  delta,paste(signif(Pr,3)),cex=cexscales)}
  
    if(istrata==1)text(x=min(tickpos), yposx , adj=1, paste("odds(",yvar,chr, signif(tcut,4),") " ), 
    cex=cexscales,font=3) 
#    
    }
 if(nstrata >1) {
   ## put a stratum labels
   clabel <- "blue"
   ## can sttum number be output in "red" of  "observation" stratum (this is held as 
   ## stratum). But this is messy. Needs to pass stratum via argument.
   ## not worth the bother??
   # if(slevels[istrata]==stratum ) clabel <- obscol 
   # ## cox scale 
   # browser()
 text(x=L-0.3*(M-L), yposx + delta, adj=0, paste(strata_levels[istrata]), 
    cex=cexscales,font=3,col=clabel )
   
  
 }
 
 
 }  # if(is.na(s5)....  
 
}  ##for(istrata...
   
    
   if(nstrata>1){
     text(x=L-0.3*(M-L), pos_of_nomo-delta, adj=0, vstrat, 
    cex=cexscales,font=3,col="blue")}
 
   ## note that if nstrata >  1,  the returned s5 must be the baseline survival of the
   ##  last listed stratum.
     
return(list(s5=s5))
}  ##end Cox_scale

##############################################################################

Survreg_scale <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,pos_of_nomo,yvar,betas,m,tcut,fail,p,dist,odds,showsubticks,cexscales,
  slevels,strata_gap,vstrat) 
  {
  nstrata <- 1
  px <- p
  
  ## are there strata?  indicated by non-null p names
  if(!is.null(names(p) ) ){
 ## strata_levels <- names(p)
   nstrata <- length( slevels)
     ## squeeze delta a bit, making text closer to axis
  delta <- delta/sqrt(nstrata)

  }
  ## wrap in a loop over strata levels
  yposx <- pos_of_nomo
  for(istrata in 1:nstrata){
    if(nstrata >1){
      ##pick out p parameter for this straum
     px <- p[istrata] }
    
    yposx <- pos_of_nomo + (istrata - 1)*strata_gap
 ##sumexp <- exp( sum(betas*m,na.rm=TRUE) )
   
## lp_lnorm <- predict(reg, type="linear") 
## grabbed formula  Klabfleisch & Prentice p25 (assuming p and lambda as for Weibull)


if(dist=="loglogistic"){
lambda <- exp(-tot_score-intercept)
scut <- 1/(1+(lambda*tcut)^px)}


if(dist=="lognormal"){

 scut <- 1- pnorm(px*(-tot_score - intercept +log(tcut)) )}

if(dist=="gaussian"){
scut <- 1- pnorm(px*(-tot_score-intercept+tcut))}


if(dist=="weibull" | dist=="exponential"){
 lambda <- exp(-tot_score-intercept)
 scut <-  exp(-(tcut*lambda)^px)}

 ## get pretty survival scores and surviaval odds
 if(odds){
   if(fail) {
   prettyX <- myprettyO((1-scut)/scut )
   probs <- 1/(1+prettyX)
   }
   else
   {
   prettyX <- myprettyO(scut/(1-scut) )
   probs <- prettyX/(1+prettyX)
   }
}
else
{prettyX <- myprettyP( scut)
  probs <- prettyX}
 
##  
 ## fix position of lower nomogram scale 
 ##npos <- 0.5
 
  if(dist=="loglogistic"){
 
 prettyscores <- -log(( ( (1-probs)/probs)^(1/px) ) / tcut) } 
 
 
 if(dist=="lognormal"){
 z <- qnorm(1-probs)
 prettyscores <- log(tcut) -z/px }

 if(dist=="gaussian"){
   z <- qnorm(1-probs)
   prettyscores <-  tcut -z/px  }
  
 
if(dist=="weibull"| dist=="exponential"){
 lam <- ((-log(probs))^(1/px))/tcut
 prettyscores <- -log(lam) } 
 
 

 
 
 tickpos <- (M-L)* (prettyscores -intercept  - Min_tot_score)/Range + L
 
 
  limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
if(length(limits)>2){
  tickpos <-  tickpos[limits]
 
 
 prettyX  <-  prettyX[limits]
}
  
sievePr <- sieve(tickpos,prettyX, 0.04*(M-L))

Xpos <- unlist(sievePr[1])
Pr <-   unlist(sievePr[2])

if(!showsubticks) tickpos <- Xpos



 ## main tick scale
 segments(min(tickpos),yposx,max(tickpos),yposx)
 segments(tickpos,yposx,tickpos,yposx-ltick)
 
 ##
 ## subticks on odds scale not yet implemented.  To be fixed. 
 ##

 if(!odds) {
   if(showsubticks) subticksf(tickpos,prettyX,yposx,stick,func=dist,par=c(p,tcut),decide=TRUE)
   }


## use sieve function to avoid text overlay on axis
# 
# sievePr <- sieve(tickpos,prettyX, 0.08*(M-L))
# 
# Xpos <- unlist(sievePr[1])
# Pr <-   unlist(sievePr[2])
# 
 
 # 
 if(fail){ chr <- "<" } else { chr <- ">" }


  if(!odds){
     if(fail) {
     text(x=Xpos,y= yposx - delta,paste(signif(1-Pr,3)),cex=cexscales)
     }
     else
     {
      text(x=Xpos,y= yposx -  delta,paste(signif(Pr,3)),cex=cexscales)
     }
   if(istrata==1) text(x=min(tickpos), yposx , adj=1, paste("Pr(",yvar,chr, signif(tcut,4),") " ), cex=cexscales,font=3) 

  }  
    else ##if(!odds
      
    {
      
      text(x=Xpos,y= yposx -  delta,paste(signif(Pr,3)),cex=cexscales)
      
      
 if(istrata==1) text(x=min(tickpos), yposx , 
    adj=1, paste("odds(",yvar,chr, signif(tcut,4),") " ), cex=cexscales,font=3) 
#    
  
    }
 
 #  
 if(nstrata>1){
    ## check whetrher full, form. If so strip awaY  "VAR="
    ## put a stratum labels
   ## check whetrher full, form. If so strip 
   # sl <- slevels
   # check <- grep("=",slevels)
   #  if(length(check) >0){ 
   #  sl <- strata_values(slevels)
   #  }

  ## label strata axes on the left. Write strata levels
 text(x=L-0.3*(M-L), yposx +delta, adj=0, paste(slevels[istrata]), cex=cexscales,
   font=3,col="blue") 
}# 
}  ##for(istrata in....
    
   if(nstrata>1){
     text(x=L-0.3*(M-L), pos_of_nomo-delta, adj=0, vstrat, 
    cex=cexscales,font=3,col="blue")}
 
}  ## end Survreg_scale 

##############################################################################

Survreg_scale_medsurv <- function(tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,pos_of_nomo,yvar,betas,m,p,dist,showsubticks,cexscales,
  slevels,strata_gap,Spercent,vstrat) 
 ##  quantile survival axes of survreg() and psm() models 
  {
  
  S <- Spercent/100
  nstrata <- 1
  px <- p
  
  ## are there strata?  indicated by non-null p names
  if(!is.null(names(p) ) ){
 ## strata_levels <- names(p)
  ##    
  nstrata <- length( slevels)
   ## squeeze delta a bit, making text closer to axis
  delta <- delta/sqrt(nstrata)

  }
  ## wrap in a loop over strata levels
  yposx <- pos_of_nomo
  for(istrata in 1:nstrata){
    if(nstrata >1){
      ##pick out p parameter for this straum
     px <- p[istrata] }
    
    yposx <- pos_of_nomo + (istrata - 1)*strata_gap
 ##sumexp <- exp( sum(betas*m,na.rm=TRUE) )
   
## lp_lnorm <- predict(reg, type="linear") 
## grabbed formula  Klabfleisch & Prentice p25 (assuming p and lambda as for Weibull)


if(dist=="loglogistic"){
  
  
 ##prettyscores <- -log(( ( (1-probs)/probs)^(1/px) ) / tcut) } 
 

  
lambda <- exp(-tot_score-intercept)
tsurvS <- ( ((1-S)/S) ^ (1/px) )/lambda }


if(dist=="lognormal"){

 ##scut <- 1- pnorm(px*(-tot_score - intercept +log(tcut)) )
  tsurvS <- exp(  (qnorm(1-S) +px*(tot_score + intercept) ) /px ) }

if(dist=="gaussian"){
##scut <- 1- pnorm(px*(-tot_score-intercept+tcut))}
tsurvS <- (qnorm(1-S) + px*(tot_score + intercept))/px }

if(dist=="weibull" | dist=="exponential"){
 lambda <- exp(-tot_score-intercept)

 tsurvS <- ( (-log(S) ) ^ (1/px) ) / lambda}

 ## get pretty survival scores and surviaval odds

prettyT <- pretty(tsurvS,n=10)
## expand out the lower end 
lp <- length(prettyT) 
prettyT <- c(pretty(prettyT[1:2]), prettyT[3:lp])
##.... and again! 
lp <- length(prettyT) 
prettyT <- c(pretty(prettyT[1:2]), prettyT[3:lp])

prettyT <- prettyT[which(prettyT>0)]

## need to get back to the graphic positions of the pretty values of median survival
## 



if(dist=="loglogistic"){

##prettyscores <- -log(( (S/(1-S)) ^ (1/px) )/prettyT) -intercept 
## or better
prettyscores <-  log(prettyT) - log((1-S)/S)/px  -intercept 
}



if(dist=="lognormal"){

 ##scut <- 1- pnorm(px*(-tot_score - intercept +log(tcut)) )
 ## tsurvS <- exp(  (qnorm(1-S) +px*(tot_score + intercept) ) /px ) 
  
  prettyscores <- log(prettyT) -qnorm(1-S)/px-intercept}

if(dist=="gaussian"){
##scut <- 1- pnorm(px*(-tot_score-intercept+tcut))}
##tsurvS <- (qnorm(1-S) + px*(tot_score + intercept))/px
prettyscores <- prettyT -qnorm(1-S)/px - intercept}

if(dist=="weibull" | dist=="exponential"){
 ##lambda <- exp(-tot_score-intercept)
 ##scut <-  exp(-(tcut*lambda)^px)
 
 ## tsurvS <- ((-log(S))^(1/px))/lambda
 prettyscores <- log(prettyT) -log(-log(S))/px -intercept
 }


 tickpos <- (M-L)* (prettyscores  - Min_tot_score)/Range + L
 
 
limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
if(length(limits) > 2){
 tickpos <-  tickpos[limits]
 prettyT<-  prettyT[limits]
}

sievemedsurv <- sieve(tickpos,prettyT, 0.04*(M-L))

Xpos <- unlist(sievemedsurv[1])
medsurv <-   unlist(sievemedsurv[2])




if(!showsubticks) tickpos <- Xpos



 ## main tick scale
 segments(min(tickpos),yposx,max(tickpos),yposx)
 segments(tickpos,yposx,tickpos,yposx-ltick)
 
 ##
 ## subticks on odds scale not yet implemented.  To be fixed. 
 
    text(x=Xpos,y= yposx - delta,paste(signif(medsurv,3)),cex=cexscales)
    
if(istrata==1){ text(x=min(tickpos), yposx , adj=1, 
paste0("fail ",100-Spercent,"%   "), cex=cexscales,font=3) 
 
 ## label time axis on rhs     
     text(x=max(tickpos),y=yposx + delta, paste(yvar),cex=cexscales)
}
    if(nstrata>1){
 
  ## label strata axes on the left
 text(x=L-0.3*(M-L), yposx +delta, adj=0, paste(slevels[istrata]), cex=cexscales,
   font=3,col="blue") 
}# 
}  ##do(istrata in....
    
   if(nstrata>1){
     text(x=L-0.3*(M-L), pos_of_nomo-delta, adj=0, vstrat, 
    cex=cexscales,font=3,col="blue")}
 
}  ## end Survreg_scalemedsurv 

##############################################################################
 
Cox_scalemedsurv <- function(reg, tot_score,intercept,L,M,Range,Min_tot_score,delta,
  ltick,stick,pos_of_nomo,yvar,odds,baseS,h,betas,center,m,bm,tcut,fail,
   showsubticks,cexscales,slevels,strata_levels,
  strata_gap,Spercent,vstrat){
## cox scale for quantile survival 
  
   nstrata <- 1
   
   S <- Spercent/100
   if(is.null(baseS)) {
    hx <- h
   ## check for strata?
   if(ncol(h)==3 & names(h)[3] == "strata"){
  #strata_levels <- unique(h[,3])
  nstrata <- length(slevels)
            ## squeeze delta a bit, making text closer to axis
  delta <- delta/sqrt(nstrata)

   }
   }
## loop over the strata (if there are any) 
##  drawing a scale for each stratum baseline hazard
      
     for (istrata in 1:nstrata) {
     yposx <- pos_of_nomo 
       
  ## possibility that hazard not computable - NAs in betas? 
  if(is.null(baseS ) ){
    if(nstrata >1){
         ## extract   the hazards for this stratum 
         hx <- h[which(h[,3]==slevels[istrata]),] 
          yposx <- pos_of_nomo + (istrata - 1)*strata_gap
  
         }
## m=0 if not centred
   sumexp <- exp( sum(betas*m,na.rm=TRUE) )
    hz <- hx$hazard*sumexp 
#     
    o <- order(hx$time)
    t <- hx$time[o]
    hx$hazard <- hx$hazard[o]
    hz <- hz[o]
    lp <- length(hz)
    
 ###  median (quantile) survival
 
 ## individual hazard for  0.5 survival are when
 ##  0.5=S(t)= exp(-hz(t)exp(tot_score))
 ## ie. when log 0.5 = hz(t)exp(tot_score)
 ## so need to identify  t  from hx, such that hz(t) = -log(0.5)/exp(tot_score)
 H   <- 1/(  exp(tot_score)  / (-log(S) )  )
# 
# 
# ## loop it???
 lp <- length(H)
  tS <- vector(length=lp)     
      isub <- 0
       for (i in 1:lp){
         ##  
 ##        tS[i] <- max(t[which( hz < H[i])])
         
         lessthanh <- which( hz < H[i])
         
         if(length(lessthanh) > 0 ) {
           isub <- isub + 1
         it <- max( lessthanh)
           ## tS[i] <- t[ it ]
            
             g <- 0
             
             ##interpolate to get time 
     if(it>1 & it < lp){
   g <- (t[it+1]-t[it]) / (hz[it+1]-hz[it])}
   tS[isub] <- t[it] + (H[i] -hz[it]) * g
  
         } 
            
        
   ##if(tot_score[i]<0.01 & tot_score[i] > -0.01)    
       }
## try filling in the lower time scale  with more points
 prettyT <- pretty(tS,n=20)
 prettyT <- prettyT[which(prettyT>0)]
 lp <- length(prettyT)
 prettyT <- c(pretty(c(0,prettyT[1]) ),prettyT[2:lp])

 ## trim off possible <=0 's
 ## patch in some larger values??
 # 
 # lp <- length(prettyT)
 # prettyT <- c(prettyT[1:(lp-1)], pretty( c(prettyT[lp],5*prettyT[lp])  )  )

 lt <- length(t)
 prettyT <- prettyT[which(prettyT >=t[1] & prettyT <= t[lt])]
  lp <- length(prettyT)

  prettyscores <- vector(length=lp)
  h0 <- vector(length=lp)
  ## need to establist where to position?
 for (i in 1:lp){
   lit <- which(t <= prettyT[i])
   if(length(lit) >0){
   it <- max(lit )
   g <- 0
   if(it>1 & it < lp){
   g <- (hz[it+1]-hz[it]) / (t[it+1]-t[it])}
   h0[i] <- hz[it] + (prettyT[i] -t[it]) * g
   
  ##h0[i] <- max( hz[ which(t < prettyT[i]) ] ) 
  prettyscores[i] <- log( -(log(S)) ) -log( h0[i] )
  }##  
 }   
 

 tickpos <- (M-L)* (prettyscores  - Min_tot_score)/Range + L
 
if(length(tickpos)==0) {
msg <- "Cannot compute quantile scale"

  if(nstrata>0){
    msg <- c(msg, paste(" for",strata_levels[istrata]) )
  }
message(msg)

}  #if(length(tickpos)==0)
 
 else  ## of if(length(tickpos)==0)
   
 {   
 

 limits <- which(tickpos > L-0.1*(M-L) & tickpos < M+0.1*(M-L) )
 if(length(limits) > 2){
   tickpos <-  tickpos[limits]
 prettyT <- prettyT[limits]
 }
 

sievemedsurv <- sieve(tickpos,prettyT, 0.04*(M-L))

Xpos <- unlist(sievemedsurv[1])
medsurv <- unlist(sievemedsurv[2])

tickpos <- Xpos
prettyT <- medsurv

 segments(min(tickpos),yposx,max(tickpos),yposx)
 segments(tickpos,yposx,tickpos,yposx-ltick)
 
 
 
     text(x=tickpos,y= yposx - delta,paste(signif(prettyT,3)),cex=cexscales)
 if(istrata==1)  {  text(x=min(tickpos), yposx , adj=1, paste0("fail ",signif(100*(1-S),4),"%  " ), 
    cex=cexscales,font=3) 
## label time axis on rhs     
     text(x=max(tickpos),y=yposx + delta, paste(yvar),cex=cexscales)}
#    
    }
 if(nstrata >1) {
   ## put a stratum labels
   
 text(x=L-0.3*(M-L), yposx + delta, adj=0, paste(strata_levels[istrata]), 
    cex=cexscales,font=3,col="blue")
 }  ##if(nstrata >1)
 
  }  ## else of if(length(tickpos)==0)
 
}  ##for(istrata...
   
   
   if(nstrata>1){
     text(x=L-0.3*(M-L), pos_of_nomo-delta, adj=0, vstrat, 
          cex=cexscales,font=3,col="blue")}
  
return()
}  ##end Cox_scalemedsurv

##############################################################################

 points_tables <- function(survreg,dist,p,cox,s5,fail,tcut,poisson,
   negbin,lm,ols,lmer,logistic,pts,intercept,
   npars,yvar,tickv,medsurv) 
{
      
 ## point_table to create last total points to outcome scoring table.   
   if(logistic){
        lprob <- exp(pts+intercept)/(1 + exp(pts+intercept))
        pointsframe <-  as.data.frame( cbind(tickv,signif(lprob,4) ) )
        colnames(pointsframe) <- c("Total Points",paste("Pr(",yvar,")")) 
   }
   
   if(lm|ols | lmer){
     
        pointsframe <-  as.data.frame( cbind(tickv,signif(pts+intercept,4) ) )
        colnames(pointsframe) <- c("Total Points", paste("mean",yvar)) 
   }
   
   if(poisson | negbin ){
     
        pred_mean <- exp(pts+intercept)
        pointsframe <-  as.data.frame( cbind(tickv,signif(pred_mean,4)) )
        if(poisson){
        colnames(pointsframe) <- c("Total Points",paste("Poisson mean",yvar)) 
        }
        else
        {colnames(pointsframe) <- c("Total Points",paste("Negbin mean",yvar))
        }
   }
   
   if(cox){
        if(fail){chr <- "<"} else {chr <- ">"}
  ##  s5 is here the baseline survival first time cutoff (passed s5[1]) 
  ##  and the last stratum, since  s5 is the last listed stratum retrn in cxp$s5
  ##  from Cox_scale 
  ## identifying first s5 and last stratum  not possible from passed item??
 
       if(!medsurv){
        pred_mean <- signif(s5^exp(pts),4)
  
        if(fail) { pred_mean <- 1-pred_mean }
        pointsframe <-  as.data.frame( cbind(tickv,signif(pred_mean,4) ) )
        
          
         ## browser()
        colnames(pointsframe) <- c("Total Points",paste("Pr(",yvar,chr, signif(tcut,4),")" )) 
       }
     
     else
     {
      pointsframe <-  as.data.frame( cbind(tickv,paste("not supported" )) )
      colnames(pointsframe) <- c("Total Points",paste(tcut, "quantile survival" )) 
  
     }
   }  ## if(cox)
   
   if(survreg){
    TP <- "Total points"
     if(length(p) >1){
       ##  possibility of strata, p multi-valued.
       ## fudge by taking  last stratum only
       ## need to fix up this limnitation? 
       p <- p[length(p)]
       TP <- paste("Total points",names(p))
     }
  if(!medsurv){
    
  if(fail){ chr <- "<" } else { chr <- ">" }      
  if(dist=="loglogistic"){
  pred_mean <- 1/ (1+(exp(-pts-intercept)*tcut)^p) }
   if(dist=="lognormal"){
  pred_mean <- 1- pnorm(p*(-pts - intercept +log(tcut)) )} 
  if(dist=="gaussian"){
  pred_mean <- 1- pnorm(p*(-pts-intercept + tcut) )}
  if(dist=="weibull"| dist=="exponential"){
  lam <- exp(-pts-intercept)

  pred_mean <- exp(  -(lam*tcut)^p )}

        if(fail) { pred_mean <- 1-pred_mean }
        pointsframe <-  as.data.frame( cbind(tickv,signif(pred_mean,4)) )
       
        
        colnames(pointsframe) <- c(TP,paste("Pr(",yvar,chr, signif(tcut,4),")" ))
  }
      else
     {
      pointsframe <-  as.data.frame( cbind(tickv,paste("not supported" )) )
      colnames(pointsframe) <- c("Total Points",paste(tcut, "quantile survival" )) 
  
     }
    
   }  ##if(survreg)
   return( pointsframe)
 }  #end points_table

##############################################################################

## function to  delete (sieve out) too many tick axis marks 
## usage sieve(tickpos,prettymean, 0.08*(M-L))
## separation must be > c
sieve <- function(x,v,cfract){
  
  if(length(x)==0 | length(x)==0){
    sievedv <- v
    sievedx <- x 
    kept <- NULL
    return(list(sievedx,sievedv,kept))
  }
  sievedv <- v
  sievedx <- x 
  n <- length(x)
  ## are points ordered? Only sieve ordered scales?
  ## no, doesn't work well
  # ord <- order(x)
  
  # kept <- seq(1:n)
  # if( TRUE | all(ord==seq(1:n))  |  all(ord==rev(seq(1:n)))  ){
 if(TRUE){
   i <- 2
  xp <- x[1]
 
  keep <- logical(length=n)
 # ## filter out  values witrh >=5 characters 
 #  nch <- nchar(as.character(v))
 #  minch <- min(nch)
 #  
 #  keep[which(nch <=minch+2)] <- TRUE
  kept <- numeric(length=n)

    
  while ( i <= n & i>1) {
    
    ##d <- min(abs(x[i] -x[i-1]),abs(x[i]-x[i+1]))
     d <- abs(x[i] -xp)
     if(d >= cfract) { xp <- x[i]
                 keep[i] <- TRUE
                 }
     
     else
     {keep[i] <- FALSE
       }
    
    
    i <- i+1
    
  }
 ##  
  ## ensure first and last is kept
  ##  NB: not sure about this- should return just one point?
  # 
  ##  
  # if(!any(keep)){
  # keep[1] <- TRUE
  # keep[n] <- TRUE}
  
  # kept <- which(keep==TRUE)
  # if(length(kept)==1) { keep[1] <- TRUE
  # kept <- which(keep==TRUE)}
  # 
  ## keep max or min?
  kmax <- which(x==max(x))
  kmin <- which(x==min(x))
  keep[kmax] <- TRUE
  keep[kmin] <- TRUE
  
 keep[1] <- TRUE
# keep[n] <- TRUE
 kept <- which(keep==TRUE)
 ## if only one point, ensure  last is kept.
 if(length(kept)==1) {
   kept[2] <- n
 }
   
  # lastkept <- max(kept)
  # 
  # if(!keep[n] & x[n]-x[lastkept] < c){
  #   keep[n] <- TRUE
  #   keep[lastkept] <- FALSE
  #   kept <- which(keep==TRUE)
  # }
  # 
 ##     
  sievedv <- v[kept]
  sievedx <- x[kept] 
}
##browser()
## overcrowded text  
maxchar <- max(nchar(paste(sievedv)))
## why XXX=4? or #  Trial and error.
XXX <- 1.5
## how many characters can be packed into scale?
## if allowing cfract space for each character
## this is max(sievedx)-min(sievedx)/cfract 
## use XXX to compensate for spaces??
##browser()
if(length(sievedv)*maxchar > XXX*( (max(sievedx)-min(sievedx))/cfract)  ) 
{ odd <- seq(1,length(sievedv),by=2)
## take out even numbered 
sievedv <- sievedv[odd]
sievedx <- sievedx[odd] 
}
  return (list(sievedx,sievedv,kept))
}

##############################################################################
## function to slot in parameter names as variable names
## to create a new variable_names.
expand_vars <- function(X, variable_names, 
  parameter_names,DF,isa_factor,nvars){
     new_variable_names <- NULL
     new_DF <- NULL
     new_isa_factor <- NULL

    i <- 0
    for(k in 1:nvars){
    
      if(X[k]){
## note: fixed parameter necessary 
      DF[k]  <- length(grep(variable_names[k],parameter_names,fixed=TRUE) )
      new_variable_names <- 
      c(new_variable_names, parameter_names[(i+1):(i+DF[k])]) 

      new_DF <- c(new_DF,rep(1,times=DF[k]) )
      new_isa_factor <- c(new_isa_factor,rep(FALSE,times=DF[k]))
      }
      else{
        new_variable_names <- 
         c(new_variable_names, variable_names[k])
        new_DF <- c(new_DF,DF[k])
        new_isa_factor <- c(new_isa_factor,isa_factor[k])
      }
      
      i <- i +DF[k]
    }
    variable_names <- new_variable_names
    nvars <- length(variable_names)
    isa_factor <- new_isa_factor
    DF <- new_DF

    return(expanded=list(nvars=nvars,isa_factor=isa_factor,
      DF=DF,variable_names=variable_names))
} 

##############################################################################

## use predict() as a check on regplot calculated outcomes
## against  calculated with predict()
## Also to add in an interval estimate on the  outcome scale as a 
## red (obscol) overline.

pcheck <- function(new_obs,reg,confI,predI, observation,intercept, 
   lm,ols,cox,logistic,poisson,negbin,survreg,pred_mean,odds,
   L,M,Range, Min_tot_score,delta,pos_of_nomo,obscol,
   dist,ftime,fail,p,s5,bm){
  ##------------------------------------------------------
  
  rtype <- class(reg)
  rms <- (rtype[2]=="rms")
  if(is.na(rms)) rms <- FALSE
pred_input <- pred_mean
    tick <- 0.7*delta
##NB: there is much code repetition here, but done in interest of clarity. 
warn <- FALSE

##--------------------------------------------------------
##  lm  models 


    if(lm){
  ## lm  includes lm and rms:ols 

 if(rms){
   if(confI) ctype <- "mean"
   if(predI) ctype <- "individual"
   
   pr <- unlist(predict(reg,type="lp",conf.type=ctype, conf.int=0.95,newdata=observation, se.fit=TRUE))
#   
 #   UL <- pr[1] + 1.96*pr[2]
 #   LL <- pr[1] - 1.96*pr[2]
      LL <- pr[3]
      UL <- pr[4]
  }
      else
      {
      if(confI) interv <- "confidence"
      if(predI) interv <- "prediction"
    pr <- predict(reg,newdata=observation,interval=interv)
    UL <- pr[3]
    LL <- pr[2]
     }
    if(abs((pred_mean-pr[1])/pred_mean) > 0.001){
      WARN <- TRUE
}
    else
    {
    if(confI | predI ){
       Int <- "CI: "
      if(predI) {Int <- "PI: "}
    
         ##  write out CI ? Used for checking
if(new_obs) message( paste(Int),signif(pred_mean,4),"(", paste0(signif(LL,4),",",signif(UL,4)),")" )

    ##  get confidence or prediction interval and  draw on the nomogram axis
    lwr <-  (M-L)* (LL-intercept -Min_tot_score)/Range + L
    upr <-  (M-L)* (UL-intercept -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)
    }
    }
    }  #if(lm)


##--------------------------------------------------------
## for ols (glm object)  predict returns a list.
  if(ols){
   
    pr <- unlist(predict(reg,newdata=observation,se.fit=TRUE))

    if(abs((pred_mean-pr[1])/pred_mean) > 0.001){
warn <- TRUE  }
    else
    {
    
    ##  get confidence interval and  draw on the nomogram axis
    if(predI | confI){
      if(confI){
        Int <- "CI: "
      SE <- pr[2]}
      else
      {Int<- "PI: "
      SE <- pr[3]}

    lower <- pr[1] -1.96*SE
    upper <- pr[1] +1.96*SE
    
    
    ##  write out CI ? Used for checking
if(new_obs) message(paste(Int),signif(pred_mean,4),"(", paste0(signif(lower,4),",",signif(upper,4)),")" )

    lwr <-  (M-L)* (lower-intercept -Min_tot_score)/Range + L
    upr <-  (M-L)* (upper-intercept -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)
    }
  }
}  ##if(ols)
##----------------------------------------------------------------
## poisson and negative binomial models
  if(poisson | negbin ){
    
 if(rms){
  ##  Glm rms command  
   pr <- unlist(predict(reg,type="lp",newdata=observation, se.fit=TRUE))
#   
## note this returns  centered linear predictor pr[1] It is not what I want. 
## pthe warn=TRUE contion resets pr[1] to log(pred_mean)
##   pr[2] is the correct standard error. 

  }
      else
      {
      
    pr <- unlist(predict(reg,newdata=observation,se.fit=TRUE))
    
      }
    
    
    if(abs((pred_mean-exp(pr[1]))/pred_mean) > 0.001){
    warn <- TRUE   
    pr[1] <- log(pred_mean)}
     ##  get confidence interval and  draw on the nomogram axis
   ##   
   if(confI){
      SE <- pr[2]
 
    low <- pr[1] -1.96*SE 
    upp <- pr[1] +1.96*SE 
    Su <- exp(upp)
    Sl <- exp(low)
    ##  write out CI ? Used for checking
if(new_obs) message("CI: ",signif(pred_mean,3),"(", paste0(signif(Sl,3),",",signif(Su,3)),")" )

    lower <- pr[1] -1.96*SE -intercept
    upper <- pr[1] +1.96*SE -intercept
    
    
    lwr <-  (M-L)* (lower  -Min_tot_score)/Range + L
    upr <-  (M-L)* (upper  -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)
    
  

}
}  ##if(poisson | negbin ){
##---------------------------------------------------------
## for logistic|polr   models
  if(logistic ){
    
    pr <- unlist(predict(reg,newdata=observation,se.fit=TRUE))
## 
        Q <- pr[1]
    pred <- exp(Q)/(1+exp(Q))
    if(odds){pred <- exp(Q)}
    if(abs((pred_mean-pred)/pred_mean) > 0.001){warn <- TRUE
    }
    
    else
    {
    ##  get confidence interval and  draw on the nomogram axis
   if(confI){
      SE <- pr[2]
## nwed - intercept for graphic position
    lower <- pr[1] -1.96*SE -intercept
    upper <- pr[1] +1.96*SE -intercept
    
    low <- pr[1] -1.96*SE 
    upp <- pr[1] +1.96*SE 
   
    if(odds){
    Su <- exp(upp)
    Sl <- exp(low)
    }
    else
    {
    Su <- exp(upp)/(1+exp(upp))
    Sl <- exp(low)/(1+exp(low))
    }
    ##  write out CI ? Used for checking
if(new_obs) message("CI: ",signif(pred,3),"(", paste0(signif(Sl,3),",",signif(Su,3)),")" )

    
    lwr <-  (M-L)* (lower  -Min_tot_score)/Range + L
    upr <-  (M-L)* (upper  -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)

}
}
}  ##if(logistic){
##------------------------------------------------------    
 ## for survreg survival models

if(survreg){

    pr <- unlist(predict(reg,newdata=observation,se.fit=TRUE,type="lp"))
    SE <- pr[2]   
    lower <- pr[1] -1.96*SE 
    upper <- pr[1] +1.96*SE 
  
    if(dist=="loglogistic"){
  pred <- 1/ (1+(exp(-pr[1])*ftime)^p) 
  Slower <- 1/ (1+(exp(-lower)*ftime)^p) 
  Supper <- 1/ (1+(exp(-upper)*ftime)^p) 
  
  }
 
  if(dist=="lognormal"){
  pred <-   1- pnorm(p*log( exp(-pr[1])*ftime) )
  Slower <- 1- pnorm(p*log( exp(-lower)*ftime) )
  Supper <- 1- pnorm(p*log( exp(-upper)*ftime) )
  }
  
  if(dist=="gaussian"){
  pred <- 1- pnorm(p*(-pr[1] + ftime) )
Slower <- 1- pnorm(p*(-lower + ftime) )
Supper <- 1- pnorm(p*(-upper + ftime) )
  }
  
  if(dist=="weibull"| dist=="exponential"){
  lam <- exp(-pr[1])
  lamL <- exp(-lower)
  lamU <- exp( -upper)
  pred <- exp(  -(lam*ftime)^p )
 Slower <- exp(  -(lamL*ftime)^p ) 
 Supper <- exp(  -(lamU*ftime)^p )
 }
  
  if(fail) { pred <- 1-pred 
  Slower <- 1- Slower
  Supper <- 1- Supper}

      if(odds){pred <- pred/(1-pred)
      Slower <- Slower/(1-Slower)
      Supper <- Supper/(1-Supper)
      }

    if(abs((pred_mean-pred)/pred_mean) > 0.001){
     warn <- TRUE  }
    
    else
      
    {
    
    ##  get confidence interval and  draw on the nomogram axis
   if(confI){
     

    lower <- pr[1] -1.96*SE -intercept
    upper <- pr[1] +1.96*SE -intercept
    lwr <-  (M-L)* (lower  -Min_tot_score)/Range + L
    upr <-  (M-L)* (upper  -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)
    
    
    if(new_obs) message("CI: ",signif(pred,3),"(", paste0(signif(Slower,3),",",signif(Supper,3)),")" )
 
 }
    }   
  
}  ## if(survreg
##---------------------------------------------------
## for cox models
if(cox ){
  

  if(rms ){
    ## use "lp", gives centered  linear predictor
    lp <- unlist(predict(reg,newdata=observation,type="lp",se.fit=TRUE))
   SElp <- lp[2]
   
   ## only interested in SElp ???
    lp <- lp[1]
   # ## with newdata=0, trick gives value for xb zeroes
   # ##  doesn't work for coxph predict. So need to use alt. way 
   # ##  for coxph.
   # ## this trick doesnt work either e.g if log(Age) in the formula!!
   # isnumeric <- which(sapply(observation,class)=="numeric")
   # o <- observation
   # o[isnumeric] <- 0
     lp0 <-unlist(predict(reg,newdata=0, type="lp",se.fit=TRUE))
   #  
     lp0 <- lp0[1]
   #  ## need also to account for centering in regplot
     xb <- lp-lp0 - sum(bm)
     pred <- s5^exp(xb)
   ## assume pred is input pred_mean

  }
  else  ##if(rms)
  {
    pr <- unlist(predict(reg,newdata=observation,type="expected",se.fit=TRUE))
   ##
    
    pred <- exp(-pr[1])
    SE <- pr[2]
  }
  

  if(fail) { pred <- 1-pred }
   

    if(odds){pred <- pred/(1-pred)}
  
  
  
    if(abs((pred_input-pred)/pred_input) > 0.001){
     warn <- TRUE 

## failure of calculated outcome to match  predict() calculation  
##message("warning: regplot calculated outcome ",paste(signif(pred_input,4)),
##" differs from predict() ",paste(signif(pred,4)))

     pred <- pred_mean
     }
  
  
  
     ##  get confidence interval and  draw on the nomogram axis
   if(confI){
     
     if(rms ){
       # Slower <- s5^exp(xb +1.96*SElp)
       # Supper <- s5^exp(xb -1.96*SElp)
       ##  
       S0 <- pred_mean
       if(fail) S0 <- 1- S0
       Slower <- S0^exp(1.96*SElp)
       Supper <-  S0^exp(-1.96*SElp)
     }
     else
     {
      SE <- pr[2]
      
    lower <- pr[1] -1.96*SE 
    upper <- pr[1] +1.96*SE 
    Slower <- exp(-upper)
    Supper <- exp(-lower)
     }
    if(Supper > 1) {
      message( "Warning: probability estimate > 1. Default fix 0.9999")
      Supper <- 0.9999}


##write out checking    
    if(fail){
       Sl <- 1- Supper
       Su<- 1- Slower}
    else
    {Sl <- Slower
    Su <- Supper}
    
    
    if(odds){
      Sl <- Sl/(1-Sl)
      Su <- Su /(1-Su)
    }

 ##  write out CI ? Used for checking
if(new_obs) message("CI: ",signif(pred,3),"(", paste0(signif(Sl,3),",",signif(Su,3)),")" )
    
  if(TRUE | ( log(Slower)/log(s5) >0 & log(Supper)/log(s5)>0 )){
    PL <- log(log(Slower)/log(s5))
    PS <- log(log(Supper)/log(s5))


    lwr <-  (M-L)* (PL  -Min_tot_score)/Range + L
    upr <-  (M-L)* (PS  -Min_tot_score)/Range + L

    segments(lwr,pos_of_nomo, upr, pos_of_nomo, col=obscol,cex=2.,lwd=2)
    segments(lwr,pos_of_nomo+tick,lwr,pos_of_nomo-tick,col=obscol,lwd=2)
    segments(upr,pos_of_nomo+tick,upr,pos_of_nomo-tick,col=obscol,lwd=2)
   }
    else
    {
    
  message("Warning: interval  out of bounds")
    }
   }
 
    
  
}  ## cox model
##----------------------------------------------------------- 

# if(warn){
# ## failure of calculated outcome to match  predict() calculation  
# message("warning: regplot calculated outcome ",paste(signif(pred_input,4)),
# " differs from predict() ",paste(signif(pred,4)))
#   
# }     
  return(warn)  
}  ##end of pcheck

##############################################################################

inverse <- function(expression, say=TRUE){
## to invert a function of a regression formula 
  ## return [1] the vraiible of the function
  ## [2] the inverted function in terms of "x"
  
  if(is.na(expression)){
    inv <- c(NA,NA)
    return(inv)
  }
  inv.expression <- NA
## split out the function parts eg. log(x+1) into
## x+1 and log()
  ## remove spaces from any expression
  
  expression <- gsub(" ", "", expression, fixed = TRUE)
  
  
  ##browser()
  ## inner or outer?? 
  ## dunno, but testing with cut() function
  ## getting myself in mess. Revert to previous
  gX <- getX(expression,outer=FALSE)
  if(is.na(gX[1])) return(NA)
 ##browser()
  kernel <- gX[1]
  fun <- gX[2]
  if(is.na(fun)){
    message("Unknown expression ", expression)
     inv <- c(NA,NA)
    return(inv)
  }

  possiblefun <- c( "","abs()", "()","as.factor()","factor()", "as.numeric()","numeric()",
    "log()","sqrt()", "poly()","bs()","ns()","exp()","I()","strata()", "strat()","rcs()","cut()")
gXouter <- getX(expression,outer=TRUE)
fun <- gXouter[2]
kernel <- gXouter[1]
    ## remove anything to right of a comma from the kernel eg. "Age,knots=3"
kernel <- unlist(strsplit(kernel,","))[1]

  if( !is.element(fun,possiblefun)){
 
 message(paste0("Warning: unsupported function ", expression,". Uncertain consequences"))
   ##?
   inv <- c(expression,NULL)
   #@# assume the kernel  is a variable name. 
   inv <- c(gX[1],NA)
    return(inv)}

  ## empty string or coercer  function <- "none, indicates inv.expression=expression

  if( fun=="" | fun=="as.factor()" | fun=="as.numeric()" | fun=="factor()"
    | fun=="numeric()" | fun=="strata()" | fun=="strat()" | fun =="abs()")  {
    
      inv <- c(kernel,"x")
   
    return(inv)
  }
  
 #---------------------------------------------------------- 
  if(fun=="cut()" | fun=="abs()" | fun=="poly()"| fun=="rcs()" | fun=="bs()" | fun == "ns()" ){
  ## return NA functions that cannot be inverted
    ## strip away anything in kernel to right of a comma eg, poly(x,degree=3)
    
    
    ## remove anything to right of a comma from the kernel eg. "Age,knots=3"
    kernel <- unlist(strsplit(kernel,","))[1]
   inv <- c(kernel,NA)
    return(inv) 
  }
#-----------------------------------------------------------------
  if( fun=="log()" | fun=="sqrt()" | fun=="exp()" | fun=="as.numeric()" ){
   plus  <-    (length(grep(kernel, pattern="\\+")) >0)
   minus <-    (length(grep(kernel, pattern="\\-")) >0)
   multiply <- (length(grep(kernel, pattern="\\*")) >0)
   power <-    (length(grep(kernel, pattern="\\^")) >0)
   divide <-   (length(grep(kernel, pattern="\\/")) >0)
   
   if( multiply | power | divide){
     ## only + and - operators allowed e.g. log(x +1)
     ## return with expression as inverse
      inv <- c(expression,NA)
      return(inv)
   }
     
  if(plus | minus  ){
    s <- "\\+"
    if(minus) s <- "\\-"
    x <- unlist(strsplit(kernel,split=s))
    
    ## both  non-mumeric like log(age + bmi)
    if(!is.numeric.like(x[1]) & !is.numeric.like(x[2])){
      message("function ",fun," kernel may have 2 variables")
    inv <- c(expression,NA)
      return(inv)
      }
    
    var <- x[1]
    constant <- x[2]
    
          ## possibility of const + var 
      ## patch by interchange
      if(is.numeric.like(x[1]) ){
        xxx <- var
        var <- constant
       constant <- xxx}
    ## possibility of an algebraic constant.?
   if(!ImpIsExp(constant)){
       return(c(expression,NA))} 
    constant <- as.character(eval(parse(text=constant)))

    operator <- "+"
    if(minus) operator <- "-"
    
    ## possibility that second item is not a constant

    if(is.na(as.numeric(constant)) ){
      inv <- c(expression,NA)
      return(inv)
    }
    else
    {
     
     if(fun=="log()") {
     inv.expression <- paste0("exp(x)-",operator,constant)}
     if(fun=="exp()") {
    inv.expression <- paste0("log(x)-",operator,constant)}
     if(fun=="sqrt()") {
    inv.expression <- paste0("x^2-",operator,constant)}
     if(fun=="as.numeric()") {
    inv.expression <- paste0("x-",operator,constant)}
    }
    
    }
  ## else of if(plus|minus)
  else
    
    ## assume no constant has been added
  {var <- kernel
  if(fun=="log()") {
    inv.expression <- paste0("exp(x)")}
  if(fun=="exp()") {
    inv.expression <- paste0("log(x)")}
  if(fun=="sqrt()") {
    inv.expression <- paste0("x^2")}
  if(fun=="as.numeric()") {
    inv.expression <- paste0("x")}
  }
    
   
   ## remove possible spaces from var 
  var <- unlist(var)
  var <- gsub(" ", "", var, fixed = TRUE)
  ## return with variable name in [1] and  inverted expression in [2]
  ##  inv.expression=NA returned for  non-invertable, not meeting rules. 
  inv <- c(var,inv.expression)

  return(inv)

   }  
  
  #-------------------------------------------------------------- 
## I() functions. At present only accommodates  power transformations
  if(fun == "I()"){
# 
#     gt <- length(unlist(strsplit(kernel,"\\>"))) >0
#     lt <- length(unlist(strsplit(kernel,"\\<"))) >0
#     if(gt | lt) {
#       message(" \"<\" or \">\" in I() expression. \"I\" unnecessary for logicals")
#     }
  power <- FALSE
  
  inv.expression <- NA
##--------possibility of I(x^power)---------------------------  
 ##    if(unlist(gregexpr("\\^",kernel)) == 1){
     x <-  unlist(strsplit(kernel,"\\^"))
     if( length(x) == 2){

      var   <- x[1]
## need a check here that var is a singl4e variable
##  eg  with I(age+age^2)  var <- age + age  
      
        
      
      p <- x[2]
      
    if(!is.numeric.like(p)){
 ##     message(paste("number expected, not",p)) 
      inv.expression <- NA
      return(c(expression,NA))
    }
  ##  possibility that power is a constant represent a number
      ## eg.g could have I(x^A]  where A <- 0.5 outside the formula
      ## use evcal(parse())   trick to convert to a number 
if(!ImpIsExp(p)){
       return(c(expression,NA))} 
     
     p <- as.character(eval(parse(text=p)))
    
    inv.expression <- paste0("x^","(1/",p,")")
 
    
       
   plus  <-    (length(grep(var, pattern="\\+")) >0)
   minus <-    (length(grep(var, pattern="\\-")) >0)
   multiply <- (length(grep(var, pattern="\\*")) >0)
   divide <-   (length(grep(var, pattern="\\/")) >0)
   if(minus | multiply | plus | divide){
   ##  if(say)(message(paste( var," should be a variable name, but includes operators?")))
   inv.expression <- NA
   }

    if(!is.numeric.like(p)){

   ## if(say)  message(paste("power in ",expression," not a number"))
      inv.expression <- NA}
    power <- !is.na(inv.expression)
   ##   
    } ##if(grep.."^"
 ##--------possibility of I(x+const)  if  not a power ?-------------------------- 
## only do this if not a power treansformation.  eg avoid crapping out with age^-1
## This is all rather  messy!!
if(!power){
  
      x <- unlist(strsplit(kernel,split="\\+"))
     if( length(x) == 2){
    
      var   <- x[1]
#
      
      const <- x[2]

      ## possibility of "const + var" 
      ## patch assuming var is second item
      if(!is.numeric.like(const)){
        xxx <- var
        var <- const
       const <- xxx}
  ## still not fixed??      
      if(!is.numeric.like(const)){
        return(c(expression,NA))
      }
      if(!ImpIsExp(const)){
       return(c(expression,NA))} 
  ##  possibility that power is a constant represent a number
      ## eg.g could have I(x^A]  where A <- 0.5 outside the formula
      ## use evcal(parse())   trick to convert to a number 
     const <- as.character(eval(parse(text=const)))
    
    inv.expression <- paste0("x-",const)
 
    ##  
       
   plus  <-    (length(grep(var, pattern="\\+")) >0)
   minus <-    (length(grep(var, pattern="\\-")) >0)
   multiply <- (length(grep(var, pattern="\\*")) >0)
   divide <-   (length(grep(var, pattern="\\/")) >0)
   if(minus | multiply | plus | divide){
   ##  if(say)(message(paste( var," should be a variable name, but includes operators?")))
   inv.expression <- NA
   }

    if(!is.numeric.like(const)){

   ## if(say)  message(paste("power in ",expression," not a number"))
      inv.expression <- NA}
    
   ##   
    } ##if(grep.."+"

 ##--------possibility of I(x - const)---------------------------  

      x <- unlist(strsplit(kernel,split="\\-"))
     if( length(x) == 2){
        var   <- x[1]
## need a check here that var is a single variable
##  eg  with I(age+age^2)  var <- age + age  
      
        
      
      const <- x[2]
           
      ## possibility of "const - var "
      ## patch assuming var is second 
      if(!is.numeric.like(const)){
        xxx <- var
        var <- const
       const <- xxx}
if(!ImpIsExp(p)){
       return(c(expression,NA))} 
  ##  possibility that power is a constant represent a number
      ## eg.g could have I(x^A]  where A <- 0.5 outside the formula
      ## use evcal(parse())   trick to convert to a number 
      ##  
     const <- as.character(eval(parse(text=const)))
    
    inv.expression <- paste0("x+",const)
 
    
       
   plus  <-    (length(grep(var, pattern="\\+")) >0)
   minus <-    (length(grep(var, pattern="\\-")) >0)
   multiply <- (length(grep(var, pattern="\\*")) >0)
   divide <-   (length(grep(var, pattern="\\/")) >0)
   if(minus | multiply | plus | divide){
   ##  if(say)(message(paste( var," should be a variable name, but includes operators?")))
   inv.expression <- NA
   }

    if(!is.numeric.like(const)){

   ## if(say)  message(paste("power in ",expression," not a number"))
      inv.expression <- NA}
    
   ##   
    } ##if(grep.."+"

} ##if(!power)

    ##unrecognised I()

      if(is.na(inv.expression)){
      var <- kernel 
      message(expression, " in formula not supported.")  
      return(c(var,""))
     }

   
    
    ## remove possible spaces from var 
  var <- unlist(var)
  var <- gsub(" ", "", var, fixed = TRUE)
  ## return with variable name in [1] and  inverted expression in [2]
  ##  inv.expression=NA returned for  non-invertable, not meeting rules. 
  inv <- c(var,inv.expression)

return(inv)

  }  ##if(fun()== "I()"
  
  
#--------------------------------------------------------------  
  ## type "()" transformations are logical.
  ## allows  >, >=, <, <=, == and !=  expressions.
  
  ## NOTE to myself: what about compound logical expression
  ## eg. (age <40 & sex==1) ??

  if(fun=="()"){
   ok <- FALSE
        if( length(grep(kernel,pattern=">")) > 0){
         
           if( length(grep(kernel,pattern=">=")) > 0  ){
              x <- unlist(strsplit(kernel,split=">="))
              var <- x[1]
              const <- x[2] 
              inv.expression <- paste0(const,"-1+x") 
              ok <- TRUE
              } else {   ## assume of for "X > const"
              x <- unlist(strsplit(kernel,split=">"))
              var <- x[1]
              const <- x[2]
             ok <- TRUE
            inv.expression <- paste0(const,"+x")
              }
         
         }

    if( length(grep(kernel,pattern="<")) > 0){
           if( length(grep(kernel,pattern="<=")) > 0  ){
              x <- unlist(strsplit(kernel,split="<="))
              var <- x[1]
              const <- x[2] 
               ok <- TRUE
              inv.expression <- paste0(const,"+1-x") 
           }else{
         ## assume of for "X > const"
      x <- unlist(strsplit(kernel,split="<"))
      var <- x[1]
      const <- x[2]
    
    inv.expression <- paste0(const,"-x")
     ok <- TRUE
       }
    
    }
    
  ## logical  (x==const)  
    if( length(grep(kernel,pattern="==")) > 0){
 
              x <- unlist(strsplit(kernel,split="=="))
              var <- x[1]
              const <- x[2] 
    
              if(is.numeric.like(const)){
               ok <- TRUE
              inv.expression <- paste0(const,"+1-x") 
              }
              
              else
              {
                inv.expression <- paste0( "lchar(x," , paste0(const), ")" )
              ok <- TRUE
              }
              
             ##   

    }
    
    
       if( length(grep(kernel,pattern="!=")) > 0){
 
              x <- unlist(strsplit(kernel,split="!="))
              var <- x[1]
              const <- x[2] 
               if(is.numeric.like(const)){

               ok <- TRUE
              inv.expression <- paste0(const,"+x") 
               }
              else
              {
              inv.expression <- paste0( "lchar(1-x," , paste0(const), ")" )
              ok <- TRUE
            
              }
    }
# ## check on var possibly being a number  age written   in formula  as (50<age)
## ie. wrong way round, should be age>50
#   use grabbed  is.numeric.like routine from StackOverflow

   if( !is.numeric.like(const)) {

##     message( paste("unsupported logical expression", expression))
    ## inv.expression <- NA
   }
    
   if( is.numeric.like(var) ){ 
   ##  message(paste("unsupported logical in formula:",expression))
  ##   inv.expression <- NA
   ## return( message("must be of form \"var > value\" (or connectives  <, >=,<=,== or !=)"))
   }
   

 ## possible compound logical with  &  | symbol
   if(length(grep(kernel,pattern="\\&"))>0 | length(grep(kernel,pattern="\\|"))>0 ){
    ##  message(paste("unsupported logical in formula:",expression))
     inv.expression <- NA
   }
    inv <- c(var,inv.expression)
    return(inv)
  }
  ##----end of if(fun="()"---------------------
 
  ## should not reach here?  Shouldve return()ed for all function possibilities
  ## leave anyway? 
  var <- unlist(var)
  var <- gsub(" ", "", var, fixed = TRUE)
  ## return with variable name in [1] and  inverted expression in [2]
  ##  inv.expression=NA returned for  non-invertable, not meeting rules. 
  inv <- c(var,inv.expression)

return(inv)
  
}   ## end of inverse()

##############################################################################

lchar <- function(x,c){
  
  if(x==1){
    f <- c}
  else
  { f <- paste0("!",c)}
  return(f)
  
}
  
##############################################################################

## Utility functions to  extract variable from  function e.g  "age" from "log(age)"
##  instr grabbed from a web page and adapted  find positions of "(" and ")"
instr <- function(str1, str2, startpos=1, n=1){
  
  aa <- unlist(strsplit(substring(str1,startpos),str2))
 
  if(length(aa) < n ) return(0);
  ##  
  return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}

##############################################################################

getXmulti <-  function(X){
  
  l <- length(X)
  f <- NULL
  v <- NULL
  vf <- NULL
  for(i in 1:l){
    gX <- getX(X[i])
    kernel <- gX[1]
    
## remove anything to right of a comma from the kernel eg. "Age,knots=3"
##    browser()
    if(!is.na(kernel)){
    kernel <- unlist(strsplit(kernel,","))[1]
    
    v <- c(v,kernel)
  ## strip away anything to right  of a comma  
    
    f <- c(f,gX[2])
    }
    
  }

  
  return(list(v,f))
}

#######################################################################

getX <- function(in_str, outer=FALSE){
  
  ## extract  variable name from function e.eg "age" from "log(age)"
  ## need double backslash for special "(" and ")" characters
  ## str is a string of  characters
  ## special case "(xxx)" excluded (with left >1 condition)

  if(is.na(in_str)){
    outgetx <- NA
    return(outgetx)
  }
  
  
  str <- in_str
  
   
    func <- ""
    ncha <- nchar(str)
  
  
    # use max and min to pick up kernel i.e. to deal with log(abs(x))
     ## extract x or abs(x) as kernel?  use inner pair
   if(!outer){
   l <- max(unlist(gregexpr("\\(",str)))
   u <- min(unlist(gregexpr("\\)",str)))
   }
    else
    {
   l <- min(unlist(gregexpr("\\(",str)))
   u <- max(unlist(gregexpr("\\)",str)))
   }
      

  ## -1's returned if no  ( or ) i.e. not a function
   # print(str)
   # print(u)
   # 
   if( u== -1   &  l== -1 ) {
     outgetX <- c(str,func)
  ##   return(outgetX)
     func <- ""
 ##    func <- NA  ## or NA?
   }
     
  
  if( u < l){
    
    ##browser()
 ## strange expression eg I((x+1)*(y+2)) 
 ## also craps out with interactions
   func <- ""
   outgetX <- c(str,"")

   message("function ", str, " is not recognised")

   return(outgetX) 
  }

   if(l >= 1 & u <= ncha){
     func <- paste0( substring(in_str,1,l) , substring(in_str,u,ncha) )
     str <- substr(str,l+1,u-1)
   }

   ## consider possibility that it is logical?
   ## assume  < or > in string indicate logical  e.g   "age > 60"
   ## Brackets are removed in regression objects. 
   ## need to patch back in  func="()"
   ## assume any =, > , <, or !  signals logical
   ##browser()
   if(func == ""){
   if(length(grep(str,pattern=">") >0 )) {func <- "()"}
   if(length(grep(str,pattern="<") >0 )) {func <- "()"}
   if(length(grep(str,pattern="=") >0 )) {func <- "()"}
   if(length(grep(str,pattern="!") >0 )) {func <- "()"}
   }
   
   
  possiblefun <- c( "","abs()", "()","as.factor()","factor()", "as.numeric()","numeric()",
    "log()","sqrt()", "poly()","bs()","ns()","exp()","I()","strata()", "strat()","rcs()")
##if(!is.element(func,possiblefun) ) str <- NA
 ## str is the "kernel" . Is it ? WHY???    
 ## outgetX <- c(str,func)
  outgetX <- c(str,func)
  
  return(outgetX)
}
#############################################

checkvarsnew <- function(vars,clickable){
## check on whether functions in  formula can be inverted 
    
  inv.vars <- as.character(vector(length=length(vars))) 
  raw.vars <- as.character(vector(length=length(vars))) 

   for( i in 1: length(vars)){
 
    ## only do for main effects 
     if(length(grep(":",vars[i]))==0) {
     ##  
  X  <- inverse(vars[i],clickable)
  
  if(is.na(X[1])) return(NA)

   inv.vars[i] <- X[2] 
   raw.vars[i] <- X[1]

  if(is.na(inv.vars[i]) & clickable ){

##message("Note: functional variable ",vars[i] )
  
 funct <-   getX(vars[i])[2]  

 # if( person & funct == "poly()"  ) {
#   message("poly() need poly() defined in \"observation\"!!")
# }
    
  }
 
   } 
   else
   {
     ## interaction
     raw.vars[i] <- vars[i]
     inv.vars[i] <- vars[i]
   }
     
     }

  X <- list(raw.vars,inv.vars)
  return(X)   
 }

##############################################################################

mydensity <- function( X,npos,dencol) {
    bandwidth <- 0.01*(max(X)-min(X))
    singular <- FALSE
    if(bandwidth < 1.0e-6 & FALSE){
    bandwidth <- 0.001
    singular <- TRUE
    }

   dens <- density(X,kernel="epanechnikov", bw=bandwidth)
   ## dens <- density(X,kernel="rectangular", bw=bandwidth)
  
    
    dens$y <- 0.6* dens$y/max(dens$y) +npos
 ##   lines(dens$x,dens$y,col="blue")
    zero <- c(rep(npos,times=length(dens$x)) ) 
    
  ## fill in with handy bit of code 
   
    tcol <- dencol
    polygon(c(dens$x, rev(dens$x)), c(dens$y, zero),
            col = tcol, border = NA)
    lines(dens$x,dens$y,col="blue")
    return(singular)
  }

##############################################################################

myhist <- function( X,npos,dencol) {
    bandwidth <- 0.01*(max(X)-min(X))
    singular <- FALSE
    if(bandwidth < 1.0e-6){
    bandwidth <- 0.001
    singular <- TRUE
    }
## generate a histogram  "Sturges" is default breaks
    h <- hist(X, plot=FALSE, breaks="FD")

    y <- 0.6* h$counts/max(h$counts) +npos
    ## interspesed vector : later not needed
 ##   lines(dens$x,dens$y,col="blue")
    nb <- length(h$breaks)
    y0 <- c(rep(npos,times=nb-1) ) 
   ## Y <- c(rbind(y0, y)) 
    nb <- length(h$breaks)
    x <- h$breaks[1:(nb-1)]
    xn <- h$breaks[2:nb]
    ## blocks of rectangles 
    rect(x,y0,xn,y,border="blue",col=dencol)
     

    return(singular)
  }

##############################################################################

myspikes <- function(X,npos,spkcol,aspect,L,M) {
    tX <- table(X)
    frq <- as.numeric(tX)
    score_uniq <- as.numeric(names(tX))
    
    luniq <- length(score_uniq)
    if(luniq>500){
      
      ## many spikes?  Too slow. But No need to over-draw each spike
      ## only need draw maximum spike at an approximate value
      ## by grouping up close values
      ## divide scale M-L into 1000 small intervals
    w <- (M-L)/1000
    
   score_uniqw <- round(score_uniq/w) * w
   A <- aggregate(frq, FUN=max,by=list(score_uniqw))
   score_uniq <- A[[1]]
   frq <- A[[2]]
   
    }

    maxfreq <- max(frq)
   ## if(maxfreq==1){message("Note: total scores all unique, value frequency=1")}
    
    dy <- 0.6*frq/maxfreq 
 ##   dx <- dy * aspect

    luniq <- length(score_uniq)
    
  
    ybottom <- c(rep(npos,times=luniq)) 
    ytop <-    c(rep(npos,times=luniq)) + dy
    
     segments( score_uniq, ybottom, score_uniq,
               ytop, lwd=3,col=spkcol)

    }

##############################################################################

mycumul <- function( X,npos,tickl,dencol) {          
          
  tX <- table(X)
  n.uniq <- length(tX)
  frq <- as.numeric(tX)
  cfrq <- cumsum(frq)
  n.obs <- cfrq[n.uniq]
  ##ordered values returned by  table() and value names
  values <- as.numeric(names(tX))
  ##norderS <- length(orderS)
  ecdy <- (cfrq)/n.obs
  
  ## fudge as a step function, doubling up  x and y coordinates
  values <- sort(c(values,values))
  ecdy <- sort(c(ecdy,ecdy))
  
  l <- length(ecdy) 
 ## values <- values[1:l], offset first point, ecdf value zero
  ecdy <- c(0, ecdy[1:(l -1)],1 )
  ##ecdy <- c(0, ecdy ), extend  plot a little way to the right at 1 level
  values <-c(values,values[l] + 0.05*(values[l]-values[1]))
  l <- l+1 
  
      
   ## position vertical scale at about 20th (or 50th?) percentile 
  
  n20 <- min(which(ecdy>0.5))
  ypos <- values[n20]
 
  yspace <- 0.65
  ecdy <- yspace* ecdy +npos
 
 
    tcol <- dencol
    zero <- c(rep(npos,times=l) )
    polygon(c(values, rev(values)), c(ecdy, zero),
            col = tcol, border = NA)
 
  #lines(values,ecdy,type="s", col="blue")
  
  segments(ypos,npos,ypos,npos+yspace)
  text(ypos - tickl,npos+yspace, paste("1"),cex=0.6,adj=0)
  text(ypos - tickl,npos+0.5*yspace, paste("0.5"),cex=0.6,adj=0)
  segments(ypos, npos+yspace, ypos +tickl,npos+yspace)
  segments(ypos, npos+0.5*yspace, ypos +tickl,npos+0.5*yspace)
  segments(ypos, npos+yspace, values[l],npos+yspace,col="gray")
  segments(ypos, npos+yspace*0.5, values[l],npos+yspace*0.5,col="gray")
## put in  blue trace over the ecdf
   lines(values,ecdy,col="blue")
  }

##############################################################################

myboxes <- function(X,npos,boxcol,aspect,L,M){
      tX <- table(X)
      frq <- as.numeric(tX)
      score_uniq <- as.numeric(names(tX))
     
      luniq <- length(score_uniq)
      
    if(luniq>500){
      
  ## too many  boxes?  Use same trick as in myspikes() to not over-draw
    w <- (M-L)/1000
   score_uniqw <- round(score_uniq/w) * w
   A <- aggregate(frq, FUN=max,by=list(score_uniqw))
   score_uniq <- A[[1]]
   frq <- A[[2]]
   
    }

     luniq <- length(score_uniq)
                   
  ## order so that smallest on top 
    o <- order(frq,decreasing=TRUE)
    frq <- frq[o]
    score_uniq <- score_uniq[o]

     
    
    tot <- sum(frq)
    frqx <- max(frq)
    sqx <- sqrt(tot/frqx)
    
 ## drop box position down a little bit 
    
    
    dy <- 0.30*sqrt(frq/tot)*sqx 
    #dx <- dy*scale
    dx <- dy * aspect
    
    xleft <-  score_uniq - dx
    xright <- score_uniq + dx
    ybottom <- c(rep(npos,times=luniq)) - dy
    ytop <- c(rep(npos,times=luniq)) + dy

    # also add a faint axis ??
    ipos <- (ytop+ybottom)/2
    segments( min(xleft),ipos,max(xleft),ipos,col="gray") 

    rect(xleft, ybottom, xleft+2*dx, ytop,border="blue",col=boxcol)
    
    
    points(0.5*(xleft+xright),(ybottom+ytop)*0.5,cex=0.3, col="black")
  ## return value is frequency array, to avoid re-calculating  
  return(frq) 
   

    }

##########################################################

remo <- function(v,w,xlevels){
  ## utility to remove variable names from interaction
  ##  eg  v="sex:ethnic", w="sexf:ethnicM"
  ## to leave interaction  level combination "f & M" 
  ##  also reference category  represented with ORed combination of refenence
  ##  categories
  ##  First split v (which is passed as variable_names) into elements
  ## giving the variables of the interaction
  vars <- unlist(strsplit(v,":") )
  nv <- length(vars)
  xl <- xlevels[vars]
  ref <- NULL
  
  ## pick out the reference levels of each var, 
  ## fistr item of xlevels.  The reference category is union 
  ##  | of these, so use "|" connector 
  ## does it need to be in loop???
  ## coolect all reference levels as "ORed" together 
  for (i in 1:nv){
  ref <- c(ref, xl[[i]][1] ) }
  
  ref <- paste(ref, collapse=" | ")

 
## sequentially  replace name by blank ""
  for(i in 1:nv){  
  w <- gsub(pattern=vars[i], replacement="",x=w, fixed = TRUE)
  
  ##  
  
  ### this needs patching for rms models with logical varaible
  ##  for which "TRUE" and "FALSE" values are omitted from
  ## vars[i].  eg. input w may have something like
  ##  maori:pacific. With non-rms this may be maoriTRUE:pacificTRUE
  ## so when "maori" is stripped out left with  ":pacific"
  ## rather than  TRUE:pacific. And on last cylce need to post-append
  ## later:  this should never be required since 
  ##  patching of parameter coefficient names done earlier
  ## frst <- substr(w,start=1,stop=1)
  ## possible that first iten is empty.  Replace by "TRUE"
 ##  if(frst==":") w <- paste0("TRUE",w)
 ## last <- substr(w,start=nchar(w),stop=nchar(w))
  ## if(last==":") w <- paste0(w,"TRUE")
  ## use  & rather than  : connector ??? 
  
 ##  
  }
  
  ## & or : sign?  Use : I think. June 2020
#   w <- gsub(pattern=":", replacement=" & ",x=w, fixed = TRUE)
   w <- c(ref,w) 
    return(w)
}

###########################################################################  

revo <- function(v, variable_names){
  ## utility to take a factor-by-numeric interaction name 
  ## and rewrite it in for that the  factor in parentheses.
  ## e.g  factor variable sex and v= "sexF:Age" "is rewritten as 
  ## as  "Age(sexF)".
  ## It assumes main effects Age and sex are in variable_names
 ## v <- gsub(":", " * ", v, fixed = TRUE)
  
  ##?? I think v always unary?? Leave as is. 
  nv <- length(v)
  for (i in 1:nv){
  sv <- unlist(strsplit(v[i],":")) 
 
 ism <- is.element(sv, variable_names)
 ## relies on  elements of interaction being main effects 
 ## in variable_names.  So if there are numeric-by-factor interactions with
 ## no main effect, wont work
 

 if(any(ism)){
   
## which is the first factor ? i.e first FALSE
   
 fact <- which(ism==FALSE)
 cont <- which(ism==TRUE)

  
 mult <- paste(sv[cont], collapse=":")



 v[i] <- paste0(  mult,"(", paste0(sv[fact],collapse=" ") ,collapse=" ",")")

 }
  

  }  

    return(v)
}

##################################################################

plot_caxisX <- function(X,raw_row_names,row_names,L,M,
  ipos,ltick,delta,cexcats,axcol,rawX, 
  splineplot,FIRSTRUN,BXrawX){
  
  ##  
## creates axis for functional type covariates bs(), ns(), poly()
## by a look-up between raw data values and functionla forms 
if(FIRSTRUN ){
  ## rawX is actual data eg. Age in log(Age)
  ## X is not log(Age) but scaled position on the plot
  
  # if(length(X) != length(rawX)){
  #   message("raw data and model data rows differ. Possible NaNs?")
  #   rawX <- rawX[ which(!is.nan(X)) ]
  #   X <- X[which(!is.nan(X)) ]
  # }

BXrawX <- cbind(X,rawX)

BXrawX <- unique(BXrawX)
##print(B)
orawX <- order(BXrawX[,2])

BXrawX <- BXrawX[orawX,]

}

 X <-    BXrawX[,1]
 rawX <- BXrawX[,2]
 ##  
## experiment with inserting X v rawX plot on rhs.
## if(splineplot){
 len <- length(X)
minx <- min(X)
maxx <- max(X)
range <- maxx - minx 
width <- (M-L)*0.1
## left hand edge of box 
Xscale <- (M- 0*width ) +(X-minx)*(width/range)
minrawx <- rawX[1]
maxrawx <- rawX[len]
rawrange <- maxrawx - minrawx 
rawXscale <- ipos + 0.1 + 0.65*(rawX-minrawx)/rawrange
col <- "#333333" 
 if(splineplot){
##show spline plot thumbnail on righr

rect(min(Xscale),min(rawXscale ),max(Xscale),max(rawXscale ),col=NA,border=col)

points(Xscale,rawXscale, pch=21,cex=0.35,col="blue")

lines(Xscale,rawXscale, pch=21,cex=0.35,col="blue")
## axis labels horizontal
text((max(Xscale)+2*min(Xscale))/3,ipos + 0.05 ,row_names,adj=0.5,col=col, cex=cexcats)
## vertical
text(min(Xscale),ipos+0.6,raw_row_names,adj=1,col=col,cex=cexcats)
} 

else
  {
    ## show a greyed out mark
    points((min(Xscale)+max(Xscale))/2,(max(rawXscale )+min(rawXscale))/2, pch=25,cex=1,col="gray")
  } 
    ##if(splineplot)
##  
 ## which is smallest? X
 
 dX <- diff(X)
 
##  
if(FIRSTRUN){
 if(  !( all(dX <= 0) | all(dX >= 0) ) ){ 

   message(paste("non-monotonic ",row_names, "v" , raw_row_names))}
}  
 #   
 
## use myprettyO  to get pretty values that "fill in" around zero
 ## but only for positive  rawX. Otherwise use basic pretty()

 if(min(rawX)>=0){
  ticks     <- myprettyO(rawX) 
 }
 else
 {ticks <- pretty(rawX,n=20,min.n=20)
 

 
 }
 ## this is shutdown. Why?
  if(ticks[1] < min(rawX) & FALSE ){
    
    
  w <- which(ticks >= min(rawX))
  
   
   ticks <- ticks[w]
   }
  
## determine which values of X correspond to 
## get tick positions corresponding to ticks.
## these may be unknown, if for example, lowest tick is 
  ## less than observable value of rawX.
  
  # ## if this is the case re-pretty?
  # if(ticks[1] < rawX[1]){
  #   
  #   expand <- pretty(c(rawX[1],ticks[2]), n=10)
  #   ticks <- unique(c(expand,ticks))
  # }
  #   
## trim off the pretty values that are beyond raw values.
##  why??
  
  
 ticks_pos <- vector(length=length(ticks))  
 ticks_pos2 <- vector(length=length(ticks))  
trim <- 0

l <- length(ticks)


##print(B)

  for (i in 1:l){
    
    ## get plotting positions for given value of the raw variable
    ## eg.  rawX is Age and ticks[i] a particular Age
poss_positions <- lookup(rawX,X,ticks[i],reorder=FALSE)
# if(ticks[i]==90){
#   
#   
# print(cbind(rawX,X))
#     
# }
  ticks_pos[i] <-  poss_positions[1] 
  
  ##  
  }


notNA <- which( !is.na(ticks_pos) )
  ticks_pos <- ticks_pos[notNA]
  ticks <- ticks[notNA]
   
  tickval <- ticks
## are positions monotonic?
  l <- length(ticks)

 
  ## ensure within window L,M
w <- which(ticks_pos > L-0.3*(M-L) & ticks_pos < M+0.1*(M-L) )

ticks_pos  <-  ticks_pos[w]
ticks      <-  ticks[w]
tickval    <-  tickval[w]
 

if(TRUE){
 sv  <- sieve(ticks_pos,ticks, 0.04*(M-L))

ticks_pos <- unlist(sv[1])
tickval <-   unlist(sv[2])


## add the scale for this variable panel
}

## above or below axis? i.e to reverse direction of scale
dt <- diff(ticks_pos)
dt <- c(dt[1],dt)
##  
above <- (dt<0)
pm <- 2*above -1 


##browser()
##  extend axis to match data 
segments(min(X),ipos,max(X),ipos,col=axcol)

##segments(min(ticks_pos),ipos,max(ticks_pos),ipos,col=axcol)
segments(ticks_pos,ipos,ticks_pos,ipos - ltick,col=axcol)
## after sieving, not establish new wrap point
# if(wrap){
#   xpos <- which(ticks_pos < wrappoint) }

## axis  values  
##pos <- ipos +delta*(2*above -1)
pos <- ipos + pm*delta
## possibility that pretty values output as eg. 0.3999999999 when value is 0.4
## dunno why. use signif() to fix
text(x=ticks_pos,y= pos,signif(tickval,3),cex=cexcats,col=axcol)
## return input X rawX  now ordered. 

 return(list(ticks_pos,tickval,ticks, X, rawX, BXrawX))
}  ## end of plot_caxisX

##############################################################################

plot_caxis <- function(X,i,mean,nmax,Beta,isint,row_names,
  raw_row_names,inv_row_names,gX, subt,L,M,
  ipos,ltick,stick,delta,cexcats,axcol){
  
  ##NB if a function X[] are transformed data, 
  ## e.g pretty values of log(age)
  
  ## nmax passed ??  add a little
  
  nmax <- max(8,nmax)
  
  ticks     <- pretty(X[,i]+mean,n=nmax) 
  
 Xmax <- max(X[,i]+mean)
 Xmin <- min(X[,i]+mean)
 ## on graphing scale
 
 Xmax <- Xmax*Beta -mean*Beta 
 Xmin <- Xmin*Beta -mean*Beta 
 
 tickval <- ticks
  func <- NA
 ## v <- row_names[i]

  v <- raw_row_names[i]

  
 
  ## is a function and not interaction try inversion 
##browser()
 if(!isint &  !is.na(inv_row_names[i]) ){

     assign("x",X[,i]+mean)
##  
    ## these are  back-transfored values e.g. of age
    
    
     invticks  <- eval(parse(text=inv_row_names[i]) )


## bodge in extra  precision on pretty scale with + 2
   tickval <- pretty(invticks,n=nmax + 2)
## e.g pretty values of age 
   

   
   if(gX[2]=="log()" | gX[2]=="sqrt()" | gX[2]=="I()"){
 ## avoid lower   possible pretty zero    
     if(tickval[1]==0) tickval[1] <- tickval[2]*0.1 
   }
 

   ## tickval are pretty values of age  e.g  20 40 60 80 
   assign(v,tickval)

##   ticks <- eval(parse(text=row_names[i]) )
  ticks <- eval(parse(text=row_names[i]) )
  
  ## ticks are values of transformed tickvals
  ## eg.  2.995732 3.688879 4.094345 4.382027 4.605170
 
     ## are here vlaues on transformed scale
   if(subt) func <- gX[2]

   }

  
 ## ??ticks_pos <- ticks*Beta -mean*Beta 
  ## these are positions of   transformed scale
  ## corressponding pretty values of for example log(age)

ticks_pos <- ticks*Beta -mean*Beta 

sv  <- sieve(ticks_pos,tickval, 0.04*(M-L))

ticks_pos <- unlist(sv[1])
tickval <-   unlist(sv[2])
kept <- unlist(sv[3])
ticks <- ticks[kept]


## ensure within window L,M
w <- which(ticks_pos > L-0.3*(M-L) & ticks_pos < M+0.1*(M-L) )

ticks_pos  <-  ticks_pos[w]
ticks      <-  ticks[w]
tickval    <-  tickval[w]
## add the scale for this variable panel
## last tick end of axis of max/min X? 
## ans both?? 
segments(Xmin,ipos,Xmax,ipos,col=axcol)
##browser()

segments(min(ticks_pos),ipos,max(ticks_pos),ipos,col=axcol)
segments(ticks_pos,ipos,ticks_pos,ipos - ltick,col=axcol)

## suppress subticks if  function translated axis 
if(subt ) {
  subticksf(ticks_pos,ticks,ipos,stick,func=func,axcol=axcol)}
  
## add axis  values  
text(x=ticks_pos,y= ipos-delta,paste(tickval),cex=cexcats,col=axcol)

return(list(ticks_pos,tickval,ticks))
}  ## end of plot_caxis

##############################################################################

## function to extract first word [1:1} of a string 
string_fun <- function(x) {
  ul = unlist(strsplit(x, split = "\\s+"))[1:1]
  paste(ul,collapse=" ")
}

##############################################################################

## grabbed from web code  for testing whether "like a number"
# source  https://stackoverflow.com/a/21154566/2292993
is.numeric.like <- function(x,naAsTrue=TRUE,na.strings=c('','.','NA','na','N/A','n/a','NaN','nan')){
    x = trimws(x,'both')
    x[x %in% na.strings] = NA
    
    result = grepl("^[\\-\\+]?[0-9]+[\\.]?[0-9]*$|^[\\-\\+]?[0-9]+[L]?$|^[\\-\\+]?[0-9]+[\\.]?[0-9]*[eE][0-9]+$",x,perl=TRUE)
    if (naAsTrue) result = result | is.na(x)
    return((result))
} 

##############################################################################

baseSt <- function(tcut,h,stratum, betas,m) {
  ## baseline  for time t,  hazard matrix h, statum s
  
  nstrata <- 1
  if(ncol(h)==3 & names(h)[3] == "strata"){
  strata_levels <- unique(h[,3])
  nstrata <- length(strata_levels)
   }
## loop over the stara (if there are any) 
##  drawing a scale for each stratum baseline hazard
     
 hx <- h 
   
       if(nstrata >1){
         ## extract   the hazards for this stratum 
         hx <- h[which(h[,3]==strata_levels[stratum]),] 

         }
  ## possibility that hazard not computable - NAs in betas? 


    sumexp <- exp( sum(betas*m,na.rm=TRUE) )
    hz <- hx$hazard*sumexp 
#     
    o <- order(hx$time)
    t <- hx$time[o]
    hz <- hz[o]
    ibelow <- max(which(t<=tcut )) 
    h5 <- hz[ibelow]
    s5 <- exp(-h5)
 
  return(s5)
}

##############################################################################

baseht <- function(H,h,stratum, betas,m) {
  ## baseline  for time t,  hazard matrix h, statum 
  
  nstrata <- 1
  if(ncol(h)==3 & names(h)[3] == "strata"){
  strata_levels <- unique(h[,3])
  nstrata <- length(strata_levels)
   }
## loop over the strata (if there are any) 
##  drawing a scale for each stratum baseline hazard
     
 hx <- h 
   
       if(nstrata >1){
         ## extract   the hazards for this stratum 
         hx <- h[which(h[,3]==strata_levels[stratum]),] 

         }
  ## possibility that hazard not computable - NAs in betas? 


    sumexp <- exp( sum(betas*m,na.rm=TRUE) )
    hz <- hx$hazard*sumexp 
#     
    o <- order(hx$time)
    t <- hx$time[o]
    hz <- hz[o]
    lp <- length(hz)
    tmax <- t[lp]
    if( length(which( hz < H)) == 0)return(NA)
    it <- max(which( hz < H))
    ##print(c(it,hz[it],H) ) 

    g<-0
     if(it>1 & it < lp){
       g <- 0 
      if(hz[it] != hz[it+1]){g <- (t[it+1]-t[it]) / (hz[it+1]-hz[it])}
   }
    else
    { if(it ==lp){
      g <- 0 
      if(hz[it] != hz[it-1]){g <- (t[it]-t[it-1]) / (hz[it]-hz[it-1])}
      }
      else
      {g <- (t[2]-t[1]) / (hz[2]-hz[1])
      }
    }
   tS <- t[it] + (H -hz[it]) * g
  
   if(is.infinite(tS)) tS <- NA 
    
  ## tS <- t[ it ]

  tS <- paste(signif(tS,3))
  if(it==lp){
    tS <- paste0(">",signif(tmax,3))}
  
   return(tS)
}   ##baseht()

####################################################################

which_stratum <- function(strata_levels,vstrat,obs){
## extract the kernel of vstrat e.g of  vstrat= strat(sex,age)  
## split into separate variable
  ## v is value of the varaible 
  v <- obs[unlist(strsplit(getX(vstrat)[1],", "))]
  v <- as.matrix(v)
  
## browser()
cstratum <-  paste0(colnames(v),"=",v,collapse=", ")
 
 ##cstratum <- paste(v,collapse=", ")
  stratum <- which(strata_levels==cstratum)
  ##  

if(length(stratum)==0){
message("Observation does seem to not match one of the strata. First stratum is assumed")
stratum <- 1
  }
  
  return(stratum)
} 

##############################################################################

strata_values <- function(strata_levels){
## determine which  statum an observation is in.   
sl <- as.character(strata_levels)
sl <-   gsub("="," == ",strata_levels)
sl  <-  gsub(","," &",sl)
nstrata <- length(sl)
## this is messy code. Problem is that character 
## stata levels of not in quotes. e.g may have
##  sex == m &  spiders == 0.
## need to replace  m  with "m" in parse(eval)
## determine which items are  values of strata
words <- unlist(strsplit(sl[1]," "))
inx <- 3
## how many  conjuctions?
lampersands <- length(which(words=="&"))
if(lampersands>=1){
for(l in 1:lampersands){
  
  ## the value elements are at positions 3,7,11,....
  inx <- c(inx,(l+1)*3 + l)}
}
slevels <- NULL
for (s in 1:nstrata){
words <- unlist(strsplit(sl[s]," "))
values <- words[inx]
slevels <- c(slevels,paste(values,collapse= ","))
}
return(slevels)
}  ## end  strata_values

###########################################################################

reorderxy <- function(x,y){

 
B <- cbind(x,y)
B <- unique(B)
##print(B)

oy <- order(B[,2])

B <- B[oy,]

 x <-    B[,1]
 y <-    B[,2]
 ##  
 
 return(list(x,y))
}

##########################################################################

lookup <- function(x,y,xref,reorder){
 ## look-up  table of x and y returning  value of y (yref) for given xref 

  
B <- cbind(x,y)
B <- unique(B)

##  
##print(B)
if(reorder){
oy <- order(B[,2])

B <- B[oy,]
 x <-    B[,1]
 y <-    B[,2]
 ##  
}
   dx <- c(x[1],diff(x))
   dy <- c(y[1],diff(y))
 
  
   ##print(cbind(x,y)) 
 ## x- dx  is x[i] -(x[i]- x[i-1])= x[i-1]
   
   ## first dx =x1,x2-x1,x3-x2.....
   ##      x-dx = 0,x1,x2,.....
  crossings <- which( (x  >= xref  & x - dx < xref) | (x  <= xref  & x - dx > xref))
  

  
  yref <- NULL
##if(length(crossings)>0){
   for (j in 1:length(crossings) ){
     
   i <- crossings[j]
##  gives NA if crossing empty "integer(0)"
      yinterpol <- y[i] - ( (x[i]-xref)/(dx[i])  )*( y[i]-y[i-1] )
      yref <- c(yref,yinterpol)
      
   }
  
     
  ## }
  return(yref=yref)
  
} ## end lookup()

################################################################

added.obs <- function(reg,observation,rawdata,actualdata ,kernel_varname,FORMULA){
  ##  creates model.frame structure to an "obervation" of raw data.
   ## if there are pseudo variables (e.g rcs(), bs()
    ## utilises the "bases" of the splines for the whole data
    ## used in the 
    if(class(reg)[1]=="lmerMod" | class(reg)[1]=="glmerMod"){
      #  
       app <- rbind(observation,rawdata)
      adddata <- model.frame(FORMULA,data=app)[1,]
 ## NTM  model.frame(reg,data=..) construction doesn't work
 ## for  lme4 regressions. The argument data= is ignored and the model
 ## frame for reg returned.  So cannot be used to update maodel.frame structure
 ## of a new observation. The workaround is  with model.frame(FORMULA
 
    }
    else
    {
      
## append observation to raw data. Need to do this for certain functions 
##  eg. poly(x) which wont work on one observation      
           app <- rbind(observation,rawdata)
           
           if(class(reg)[1]=="polr"){
             

             ## polr models requires formula specification
            adddata <- model.frame(formula=formula(reg), data=app)[1,]  
             
           }
           else
           {
           
          adddata <- model.frame(reg, data=app)[1,]
           }
      
     # }
        ## adddata <- model.frame(reg,data=observation)
          
           ## annoying patch for weighted polr models
           ## model.frame returns actual name of weighted variable, 
           ## rather than  "(weights)" which is otherwise returned
           ##  but names(reg$model) does have "(weights)"
           ##  use to change to "(weights)" assuming positions the same!!
           if(class(reg)[1]=="polr"){
       
             w <- colnames(reg$model) %in% "(weights)"
             lw <- which(w==TRUE)
             ## if weighted data 
             ## adddata from above call wont have a "(weight)" col.
             ##  so need to add one

             if(length(lw)==1){
             adddata <- cbind(adddata,1)
             names(adddata)[lw] <- "(weights)"
             }
             ##browser()
           }
    }
  
  ##browser()
           w <- which(kernel_varname!="")
           wnms <- kernel_varname[w]
           
           ## wnms should be vraiuables in rawdata?\?
           xxx <- is.element(wnms, colnames(rawdata) )

   if(!all(xxx))
     {return(   message(" variable(s) ",  paste(  wnms[which(xxx==FALSE)] )," not in data" )  )}
           
## for any function with a kernel  (eg, log(x)) need to 
## establish  untransformed value           


## if this is a spline or similar, need to pick up the 
##  raw spline bases, because the above model.frame( data=app)
##  may respond with updated basis with rms models
## Want to overide the updated basis  and 
## identify closest rawdata value to Rval
            if(length(w) > 0 ) {
             ## print(w)
              for(i in 1:length(w) ){
     ##  
             rawdata_Rvar <- rawdata[,wnms[i]]
             Rval <- as.numeric(observation[wnms[i]])

             ## (there may be many "closest", only need one. 
             closest <- which.min( abs(rawdata_Rvar - Rval) )[1]
             snapped_Rval <- rawdata_Rvar[closest]
              
             rang <- range(rawdata_Rvar)
             snapped_pc <- (snapped_Rval -Rval)/(rang[2]-rang[1])
             snapped_pc <- (snapped_Rval -Rval)
             observation[wnms[i]] <- snapped_Rval
             
             ##browser()
             if( abs(snapped_pc ) > 0.05*abs(Rval)){ 
               message("Note: snapped to nearest datum ", paste(signif(snapped_Rval)))}
             nms <- unlist(getXmulti(colnames(actualdata))[1])
             nm <- which(nms==wnms[i])

             basis <- as.numeric( actualdata[closest,nm] )
             ## replace the basis with the raw basis. 
           #   
            adddata[,nm ] <-   replace(adddata[,nm ], seq_along(basis), basis)

            ##message(paste("snapped on value ", signif(snapped_Rval,3) ) )
 } ## for(i in
           }

           return(list(adddata,observation))
  }  ##end added.obs

######################################

## function by Billy Wu to check for parse-ability
ImpIsExp <- function(x){
  tryCatch(
    expr = {
      is.expression(parse(text = x))
    },
    error = function(e){ 
      FALSE
    }
  )
}
###############################################
inter_coeff <- function(nms_arefact,nms_coefficients){
  
  ## function to determine the number of regression coefficinets 
  ## that  refer to an interaction between elements of "arefact"
  ## eg nms_arefact= c("age","sex") and \
  ## coefficients  c("age10", "sexF"  "sexF:age10", "sexF:age20"))
  na <- length(nms_arefact)
  nc <- length(nms_coefficients)
  nterms <- 0

  for(i in 1:nc){
    XXX <- unlist(strsplit(nms_coefficients[i],split=":"))
##browser()
    if(length(XXX)==na){
      incl <- TRUE

      for (j in 1:na){

        # hasv <- grep(nms_arefact[j] , XXX)
        # if(length(hasv) == 0) incl <- FALSE
        ## better to check first position, ordering is ensured 
  ## browser()
        
        hasv <- nms_arefact[j]== substr( XXX[j],start=1,stop=nchar(nms_arefact[j]) )
        if(!hasv) incl <- FALSE
      }
      nterms <- nterms + incl
    }
    
  }
  return(nterms)
}
