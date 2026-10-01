#' aMNLFA simultaneous model fitting function
#'
#' This function generates the simultaneous aMNLFA model from all the initial inputs.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @param keepmean Boolean. If intercept or loading DIF are present, should the corresponding mean impact effect be retained? Defaults to FALSE.
#' @return No return value. Generates a file entitled "round3calibration.inp", to be run in \emph{Mplus}, in the directory specified in the aMNLFA.object. 
#' @keywords MNLFA
#' @export
#' @examples
#' \dontrun{
#'  wd  <-  tempdir()
#'  first <- paste0(system.file(package='aMNLFA'),"/extdata")
#'  the.list  <-  list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'    
#'  ob  <-  aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("bin_", 1:12),
#'  catindicators = paste0("bin_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  
#'  aMNLFA.simultaneous(ob)
#'  }


aMNLFA.simultaneous <- function(input.object, keepmean = FALSE){

  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mycatindicators = input.object$catindicators
  mycountindicators = input.object$countindicators
  myMeanImpact = input.object$meanimpact
  myVarImpact = input.object$varimpact
  myMeasInvar = input.object$measinvar
  mytime = input.object$time
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  thresholds = input.object$thresholds
  #keepmean is a new argument as of 8/18 -- can keep mean impact if corresponding loading or intercept DIF is present
  
  if (thresholds == TRUE) {
    stop("thresholds == TRUE is disabled in this version of aMNLFA. Reset thresholds to FALSE to run this function.")
  }
  
  varlist <- c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist <- unique(varlist)

  USEVARIABLES <- paste("USEVARIABLES=")
  semicolon <- paste(";")
  AUXILIARY <- ifelse(length(myauxiliary)>0,paste("AUXILIARY="),paste("!"))
  AUXILIARY <- append(AUXILIARY,myauxiliary)
  AUXILIARY <- noquote(append(AUXILIARY,semicolon))
  AUXILIARY <- utils::capture.output(cat(AUXILIARY))
  CATEGORICAL <- paste("CATEGORICAL=")
  CATEGORICAL <- append(CATEGORICAL,mycatindicators)
  CATEGORICAL <- noquote(append(CATEGORICAL,semicolon))
  CATEGORICAL <- utils::capture.output(cat(CATEGORICAL))
  COUNT <- paste("COUNT=")
  COUNT <- append(COUNT,mycountindicators)
  COUNT <- noquote(append(COUNT,semicolon))
  COUNT <- utils::capture.output(cat(COUNT))
  ANALYSIS <- noquote("ANALYSIS: ESTIMATOR=ML;ALGORITHM=INTEGRATION;INTEGRATION=MONTECARLO;PROCESSORS=4;")
  ETA <- paste("ETA BY ")
  l <- length(myindicators)
  loadings <- list()
  for (i in 1:l){
    loadings[i] <- paste(ETA,myindicators[i],"*(l_",i,");",sep="")
  }
  loadings <- noquote(loadings)
  loadings <- unlist(loadings)
  tech1 <- paste("OUTPUT: tech1;")
  MODEL <- paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT <- paste("CONSTRAINT=")
  varMODEL <- paste("MODEL: [ETA@0];ETA*(veta);")
  MODELCONSTRAINT <- paste("MODEL CONSTRAINT: new(")
  ####ROUND 1 USES p<.05 AS GATE TO GET TO ROUND 2 FOR MEAS INV and p<.1 for IMPACT####################

  ##Read in mean impact script and test for impact at p<.1
  meanimpact <- MplusAutomation::readModels(fixPath(file.path(dir,"meanimpactscript.out",sep="")))
  meanimpact <- as.data.frame(meanimpact$parameters$unstandardized)
  meanimpact <- meanimpact[which(meanimpact$paramHeader=="ETA.ON"),]
  meanimpact <- meanimpact[which(meanimpact$pval<.1),]
  keepmeanimpact <- meanimpact$param
  for (j in 1:length(keepmeanimpact)){
    if(length(grep("_",keepmeanimpact[j]))>0) keepmeanimpact <- append(keepmeanimpact,substr(keepmeanimpact[j],1,3))
    if(length(grep("_",keepmeanimpact[j]))>0) keepmeanimpact <- append(keepmeanimpact,substr(keepmeanimpact[j],5,7))
  }
  keepmeanimpact <- keepmeanimpact[which(keepmeanimpact!=2)]
  keepmeanimpact <- unique(keepmeanimpact)
  threeletterlist <- substr(myMeanImpact,1,3)
  test <- (keepmeanimpact[match(threeletterlist,keepmeanimpact)])
  pos <- match(threeletterlist,test)
  pos <- pos[!is.na(pos)]
  pos <- unique(pos)
  add <- myMeanImpact[pos]
  keepmeanimpact <- unique(append(keepmeanimpact,add))
  keepmeanimpact2 <- match(keepmeanimpact,myMeanImpact)
  keepmeanimpact2 <- keepmeanimpact2[!is.na(keepmeanimpact2)]
  keepmeanimpact <- myMeanImpact[keepmeanimpact2]

  ##Read in var impact script and test for impact at p<.1
  varimpact <- MplusAutomation::readModels(fixPath(file.path(dir,"varimpactscript.out",sep="")))
  varimpact <- as.data.frame(varimpact$parameters$unstandardized)

  varimpact <- varimpact[which(varimpact$paramHeader=="New.Additional.Parameters"&varimpact$pval<.1),]  ######alpha <.1 to trim###########
  varimpact <- noquote(substr(varimpact$param,2,3))
  myVarImpact2 <- as.data.frame(myVarImpact)
  myVarImpact3 <- as.data.frame(NULL)

  #if (!is.null(dim(varimpact))){ #added by IS to accommodate no var impact
  if (length(varimpact) > 0) { #Changed by VC on 10/07 - because varimpact is a single number, not a dataframe, need to use length rather than dim
    for (i in 1:length(varimpact)){
      myVarImpact3[i,1] <- myVarImpact2[varimpact[i],]
    }
  
  keepvarimpact <- noquote(t(myVarImpact3))

  for (j in 1:length(keepvarimpact)){
    if(length(grep("_",keepvarimpact[j]))>0) keepvarimpact <- append(keepvarimpact,substr(keepvarimpact[j],1,3))
    if(length(grep("_",keepvarimpact[j]))>0) keepvarimpact <- append(keepvarimpact,substr(keepvarimpact[j],5,7))
  }
  }else { #added by IS to accommodate no var impact
    keepvarimpact=NULL
  }
  
  keepvarimpact <- keepvarimpact[which(keepvarimpact!=2)]
  keepvarimpact <- unique(keepvarimpact)
  threeletterlist <-  stringr::str_sub(myVarImpact,1,3)
  test <- (keepvarimpact[match(threeletterlist,keepvarimpact)])
  pos <- match(threeletterlist,test)
  pos <- pos[!is.na(pos)]
  pos <- unique(pos)
  add <- myVarImpact[pos]
  keepvarimpact <- unique(append(keepvarimpact,add))
  keepvarimpact2 <- match(keepvarimpact,myVarImpact)
  keepvarimpact2 <- keepvarimpact2[!is.na(keepvarimpact2)]
  keepvarimpact <- myVarImpact[keepvarimpact2]

  alllambdadf <- data.frame(matrix(c(NA),nrow=length(myindicators),ncol=1+length(myMeasInvar)))
  colnames(alllambdadf)=c("items",myMeasInvar)
  alllambdadf$items=myindicators
  for (i in 1:length(myindicators)){
    dif <- MplusAutomation::readModels(fixPath(file.path(dir,base::tolower(paste("measinvarscript_",myindicators[i],".out",sep="")))))
    dif <- dif$parameters$unstandardized
    lambdadif <- dif[which(dif$paramHeader=="New.Additional.Parameters"),]
    lambdadif <- lambdadif[(grep("L",lambdadif$param)),] #Addition, 6/15: This gets rid of threshold parameters if they are present
    lambdadif$param <- noquote(stringr::str_sub(lambdadif$param,-2,-1))
    lambdadif <- lambdadif[which(lambdadif$param!="00"),]
    #New as of 6/5
    #lambdadif$param <- noquote(stringr::str_sub(lambdadif$param,2,length(lambdadif$param))) 
    lambdadif$param <- noquote(stringr::str_sub(lambdadif$param,2,nchar(lambdadif$param))) #VTC fix on 11/2 -- issue was that we were using length instead of nchar 
    for (q in 1:length(lambdadif$param)) {
      lambdadif$param[q] <- myMeasInvar[as.numeric(lambdadif$param[q])]
      }
    lambda_min <- rep(NA,length(myMeasInvar))
    #VTC fix on 11/2/2021
    #lambdadif <- noquote(t(lambdadif[keep]))
    #colnames(lambdadif)=lambdadif[1,]
    #lambdadif=t(as.matrix(lambdadif[-1,])) #The "as.matrix" part was causing trouble because it deleted colnames
    #the.names <- lambdadif[1,]
    #lambdadif <- lambdadif[-1,]
    #lambda_min <- rep(NA,length(myMeasInvar))
    #for (v in 1:length(myMeasInvar)) {
    #  lambda_min[v] <- (as.numeric(min(subset(lambdadif,colnames(lambdadif)==myMeasInvar[v]))))}
    for (v in 1:length(myMeasInvar)) {
      lambda_min[v] <- (as.numeric(min(subset(lambdadif$pval,lambdadif$param==myMeasInvar[v]))))
      }
    #Fix indexing issue with threshold DIF
    lambda_index <- length(myMeasInvar) #Gets the number of covariates there are, regardless of whether we're using thresholds or not.
    alllambdadf[i,2:(lambda_index+1)] <- lambda_min
    for (j in 1:length(myMeasInvar)){
      alllambdadf[i,(j+1)] <- ifelse(alllambdadf[i,(j+1)]<.05,myMeasInvar[j],NA)
    }
    keeplambda <- unlist(as.list(alllambdadf[i,2:(lambda_index+1)]))
    keeplambda <- keeplambda[!is.na(keeplambda)]
    names(keeplambda) <- NULL
    for (j in 1:length(keeplambda)){
      if(length(grep("_",keeplambda[j]))>0) keeplambda <- append(keeplambda,stringr::str_sub(keeplambda[j],1,3))
      if(length(grep("_",keeplambda[j]))>0) keeplambda <- append(keeplambda,stringr::str_sub(keeplambda[j],5,7))
    }
    keeplambda <- unique(keeplambda)
    threeletterlist <- stringr::str_sub(myMeasInvar,1,3)
    test <- (keeplambda[match(threeletterlist,keeplambda)])
    pos <- match(threeletterlist,test)
    pos <- pos[!is.na(pos)]
    pos <- unique(pos)
    add <- myMeasInvar[pos]
    keeplambda <- unique(append(keeplambda,add))
    keeplambda2 <- match(keeplambda,myMeasInvar)
    keeplambda2 <- keeplambda2[!is.na(keeplambda2)]
    keeplambda <- myMeasInvar[keeplambda2]
    for (j in 2:(lambda_index+1)){
      if (length(keeplambda)>0){
      for (k in 1:length(keeplambda)){
        if(length(grep(keeplambda[k],myMeasInvar[j-1]))>0)  alllambdadf[i,j] <- myMeasInvar[j-1]
        }
      }
    }
  }

  colnames(alllambdadf) <- NULL
  
  allinterceptdf <- data.frame(matrix(c(NA),nrow=length(myindicators),ncol=1+length(myMeasInvar)))
  
  if (thresholds==FALSE) {
    colnames(allinterceptdf)=c("items",myMeasInvar)
    allinterceptdf$items=myindicators
    for (i in 1:length(myindicators)){
      dif <- MplusAutomation::readModels(fixPath(file.path(dir,base::tolower(paste("measinvarscript_",myindicators[i],".out",sep="")))))
      dif <- dif$parameters$unstandardized
      keep <- c("param","pval") #headers we'll keep -- change for threshold 
      intdif <- dif[grep(".ON",dif$paramHeader),] #subset to just mean effects for each item (don't have to worry about ETA here)
      intdif <- noquote(t(intdif[keep])) #gives dataframe with number of rows = 2 ("param" and "pval") and number of columns = number of DIF covariates
      colnames(intdif)=intdif[1,] #set names of DF columns to the names of the covariates
      intdif=intdif[-1,] #get rid of that first row -- so now all we have is a list of p values for each covariate
      allinterceptdf[i,2:(length(intdif)+1)] <- intdif #now add the p. values to allinterceptdf dataframe
      for (j in 1:length(myMeasInvar)){ #now set the i,j^th value of the matrix to the name of the covariate if the corresponding p. value is < .05, NA otherwise
        allinterceptdf[i,j+1] <- ifelse(allinterceptdf[i,j+1]<.05,myMeasInvar[j],NA)
      }
      #keepintercept <- unlist(as.list(allinterceptdf[i,2:length(myMeasInvar)+1]))
      keepintercept <- unlist(as.list(allinterceptdf[i,2:(length(myMeasInvar)+1)])) #Fixed on 11/2 -- missed parenthesis was causing indexing issue
      keepintercept <- keepintercept[!is.na(keepintercept)]
      names(keepintercept) <- NULL
      for (j in 1:length(keepintercept)){
        if(length(grep("_",keepintercept[j]))>0) keepintercept <- append(keepintercept,substr(keepintercept[j],1,3))
        if(length(grep("_",keepintercept[j]))>0) keepintercept <- append(keepintercept,substr(keepintercept[j],5,7))
      }
      keepintercept <- unique(keepintercept)
      threeletterlist <- substr(myMeasInvar,1,3)
      test <- (keepintercept[match(threeletterlist,keepintercept)])
      pos <- match(threeletterlist,test)
      pos <- pos[!is.na(pos)]
      pos <- unique(pos)
      add <- myMeasInvar[pos]
      keepintercept <- unique(append(keepintercept,add))
      keepintercept2 <- match(keepintercept,myMeasInvar)
      keepintercept2 <- keepintercept2[!is.na(keepintercept2)]
      keepintercept <- myMeasInvar[keepintercept2]
      if (length(keepintercept)>0)
        for (j in 2:(length(myMeasInvar)+1)){ #11/2 -- fixed issue with indexing caused by lack of parentheses :-|
          for (k in 1:length(keepintercept)){
            if(length(grep(keepintercept[k],myMeasInvar[j-1]))>0)  allinterceptdf[i,j] <- myMeasInvar[j-1]
          }
        }
      }
  } else {
    colnames(allinterceptdf)=c("items",myMeasInvar)
    allinterceptdf$items=myindicators
    for (i in 1:length(myindicators)){
      dif <- MplusAutomation::readModels(fixPath(file.path(dir,base::tolower(paste("measinvarscript_",myindicators[i],".out",sep="")))))
      dif <- dif$parameters$unstandardized
      keep <- c("param","pval") #headers we'll keep -- change for threshold 
      thrdif <-  dif[-grep("L|00",dif$param),] #combine the param condition (don't contain L or 00) to get thresholds
      thrdif <-  subset(thrdif, thrdif$paramHeader == "New.Additional.Parameters")      #...with the paramHeader condition ("New.Additional.Parameters") 
      thrdif <- noquote(t(thrdif[keep])) #gives dataframe with number of rows = 2 ("param" and "pval") and number of columns = number of DIF covariates
      how.many.thresholds  <-  sub("\\_.*", "", thrdif[1,]) #Two-step process to get just the number of threshold -- step 1: extract portion before "_"
      how.many.thresholds  <-  max(as.numeric(gsub("T", "", how.many.thresholds))) #Step 2: just get the numeric portion of it, and get the maximum
      colnames(thrdif)=thrdif[1,] #set names of DF columns to the names of the covariates
      thrdif=thrdif[-1,] #get rid of that first row -- so now all we have is a list of p values for each covariate
      allinterceptdf[i,2:(how.many.thresholds+1)] <- thrdif #now add the p. values to allinterceptdf dataframe
      for (j in 1:length(myMeasInvar)){ #now set the i,j^th value of the matrix to the name of the covariate if ANY thresholds have p < .05, NA otherwise
        single.cov  <-  thrdif[grep(paste0("_",j),names(thrdif))]
        allinterceptdf[i,(j+1)] <- ifelse(sum(single.cov<.05)>0,myMeasInvar[j],NA) #11/2 - added parentheses in to fix indexing issue 
      }
      keepintercept <- unlist(as.list(allinterceptdf[i,2:(length(myMeasInvar)+1)]))
      keepintercept <- keepintercept[!is.na(keepintercept)]
      names(keepintercept) <- NULL
      for (j in 1:length(keepintercept)){
        if(length(grep("_",keepintercept[j]))>0) keepintercept <- append(keepintercept,substr(keepintercept[j],1,3))
        if(length(grep("_",keepintercept[j]))>0) keepintercept <- append(keepintercept,substr(keepintercept[j],5,7))
      }
      keepintercept <- unique(keepintercept)
      threeletterlist <- substr(myMeasInvar,1,3)
      test <- (keepintercept[match(threeletterlist,keepintercept)])
      pos <- match(threeletterlist,test)
      pos <- pos[!is.na(pos)]
      pos <- unique(pos)
      add <- myMeasInvar[pos]
      keepintercept <- unique(append(keepintercept,add))
      keepintercept2 <- match(keepintercept,myMeasInvar)
      keepintercept2 <- keepintercept2[!is.na(keepintercept2)]
      keepintercept <- myMeasInvar[keepintercept2]
      if (length(keepintercept)>0)
        for (j in 2:(length(myMeasInvar)+1)){
          for (k in 1:length(keepintercept)){
            if(length(grep(keepintercept[k],myMeasInvar[j-1]))>0)  allinterceptdf[i,j] <- myMeasInvar[j-1]
          }
        }
    }
  }
  
  for (i in 1:dim(allinterceptdf)[1]){
    for (j in 2:dim(allinterceptdf)[2]){
      allinterceptdf[i,j] <- ifelse(is.na(allinterceptdf[i,j]),alllambdadf[i,j],allinterceptdf[i,j])
    }
  }
  

  colnames(allinterceptdf) <- NULL
  #allmeasdif <- cbind(alllambdadf,allinterceptdf)
  #diflist <- allmeasdif[1,]
  #intdiflist <- allinterceptdf[1,]
  #for (i in 2:length(myindicators)){
  #  intdiflist <- append(intdiflist,allinterceptdf[i,])
  #  diflist <- append(diflist,allmeasdif[i,])
  #}
  #intdiflist <- noquote(unique(unlist(intdiflist)))
  #intdiflist <- diflist[!is.na(intdiflist)]
  #diflist <- noquote(unique(unlist(diflist)))
  #diflist <- diflist[!is.na(diflist)]

  intdiflist <- list()
  for (q in 1:length(myMeasInvar)) {
    if ((myMeasInvar[q] %in% allinterceptdf[,(q+1)]) == TRUE) #If there was intercept DIF for covariate q, it will appear in the q+1 column
    {intdiflist <- c(intdiflist, myMeasInvar[q])}
  }
  intdiflist <- unlist(intdiflist) #VC changed this on 8/18
  
  #VC changed after email conversations, 8/15-18, about including mean impact if corresponding DIF effect is there
  #Also, not sure why we were using "append" in here -- changed it to "c" because that can take multiple arguments
  if (keepmean == TRUE) {
    keepmeanimpact <- unique(c(keepmeanimpact, intdiflist, keeplambda, keepvarimpact))
  } else {
    keepmeanimpact <- unique(c(keepmeanimpact, keepvarimpact))
  }
  
  
  

  
  ########################
  #constraints section
  ########################
  
  new.constraint.section <- data.frame(NULL)
  new.constraint.row <- 1
  
  constraint.section <- data.frame(NULL)
  constraint.row <- 1
  
  vstart <- data.frame(NULL)
  if (length(keepvarimpact)>0) {
    varindices <- which(myVarImpact %in% keepvarimpact) #6/15 change to replace variance parameter names with indices associated with variable order in myVarImpact
    for (v in 1:length(keepvarimpact)){
      vstart[v,1] <- paste("v",varindices[v],"*0",sep="")
    }
  }
  vstart <- ifelse(length(keepvarimpact)>0,paste(utils::capture.output(cat(noquote(unlist(vstart)))),sep=""),"!")
  new.constraint.section[new.constraint.row,1] <- utils::capture.output(cat(append(MODELCONSTRAINT,vstart)) )
  
  
  constraint.section[constraint.row,1] <- "veta=1*exp("
  if (length(keepvarimpact)>0)
    for (v in 1:length(keepvarimpact)){
      constraint.section[(constraint.row+1),1] <- paste("v",varindices[v],"*",keepvarimpact[v],"+",sep="")
      constraint.row <- constraint.row + 1
    }
  constraint.section[(constraint.row+1),1] <- paste("0);")
  constraint.row <- constraint.row + 1
  
  
  m <- length(myMeasInvar)
  ind <- length(myindicators)
  for (i in 1:ind){
    # predlist2 <- unlist(alllambdadf[i,2:m+1])
    # predlist2 <- unlist(alllambdadf[i,1:(m+1)]) #IS changed to 1:m+1....was omitting first column of lambda DIF before! :(
    predlist2 <- unlist(alllambdadf[i,2:(m+1)]) #VTC changed -- issue wasn't that indexing started at wrong place but that the index needed to be inside parentheses. To see this, type "2:3+2" and "2:(3+2)" -- they give different results
    predlist2 <- predlist2[!is.na(predlist2)]
    eq <- as.data.frame(NULL)
    start <- as.data.frame(NULL)
    eq[1,1] <- ifelse(length(predlist2)>0,paste("l_",i,"=l",i,"_0",sep=""),"!")
    start[1,1] <- ifelse(length(predlist2)>0,paste("l",i,"_0*1 ",sep=""),"!")
    if (length(predlist2)>0)
      predlist2indices <- which(myMeasInvar %in% predlist2) #6/15 -- Here too, like other parts, give the index that corresponds to its order in myMeasInvar 
    for (w in 1:length(predlist2)){
      eq[(1+w),1] <- ifelse(length(predlist2)>0,paste("+l",i,"_",predlist2indices[w],"*",predlist2[w],sep="") ,"!")
      start[(1+w),1] <- ifelse(length(predlist2)>0,paste(" l",i,"_",predlist2indices[w],"*0",sep="") ,"!")
    }
    constraint.section[(constraint.row+1),1] <- paste(utils::capture.output(cat(noquote(unlist(eq)))),semicolon,sep="")
    new.constraint.section[(new.constraint.row+1),1] <- paste(utils::capture.output(cat(noquote(unlist(start)))),sep="")
    constraint.row <- nrow(constraint.section)
    new.constraint.row <- nrow(new.constraint.section)
  }
  
  if (thresholds == FALSE){
    new.constraint.section[(new.constraint.row+1),1] <- paste(");")
  } else {
    for (i in 1:l) {
      th <-length(unique(mrdata[stats::complete.cases(mrdata), myindicators[l]]))-1
      sig.covs.thr <- allinterceptdf[i,]
      cov.index <- seq(0, th)
      if (sum(is.na(sig.covs.thr)) > 0) {
        cov.index <- cov.index[is.na(sig.covs.thr) == FALSE] #only choose values of cov.index for which the corresponding value of sig.covs.thr is significant
        cov.index <- cov.index[2:length(cov.index)]
        sig.covs.thr <- sig.covs.thr[is.na(sig.covs.thr) == FALSE]
        sig.covs.thr <- sig.covs.thr[2:length(sig.covs.thr)]
        for (k in 1:th) {
          constraint.section[(constraint.row + 1), 1] <- paste0("T",i,"_",k,"=")
          constraint.section[(constraint.row + 2), 1] <- paste0("T",i,"_",k,"_0 + ")
          new.constraint.section[(new.constraint.row + 1), 1] <- paste0("T",i,"_",k,"_0")
          new.constraint.row <- nrow(new.constraint.section)
          for (q in 1:length(sig.covs.thr)) {
            new.constraint.section[(new.constraint.row + q), 1] <- paste0("T",i,"_",k,"_",cov.index[q])
            constraint.section[(constraint.row + q + 2), 1] <- paste0("T",i,"_",k,"_",cov.index[q], "*", sig.covs.thr[q], ifelse(q == length(sig.covs.thr), ";", "+"))
          }
          constraint.row <- nrow(constraint.section)
          new.constraint.row <- nrow(new.constraint.section)
        }
      }
      new.constraint.section[(new.constraint.row + 1),1] <- ");"
    }
  }
  
  
    
  ##Writing script with all parameters with p<.05
  ##Take any variables that's only in constraints (and not in the mean model) out of the usevariables section
  if (thresholds == FALSE) {
    useround2 <- c(myindicators,intdiflist,keepmeanimpact)
  } else {
    useround2 <- c(myindicators,keepmeanimpact)
  }
  useround2 <- unlist(unique(useround2))
  useround2 <- utils::capture.output(cat(useround2))
  
  
  
  ETAON2 <- paste("ETA ON ",keepmeanimpact,semicolon,sep="")
  uniquelambda <- unique(unlist(alllambdadf[,-1]))
  uniquelambda <- uniquelambda[!is.na(uniquelambda)]
  con <- unique(append(keepvarimpact,uniquelambda))
  CONSTRAINT <- noquote(append(CONSTRAINT,con))
  CONSTRAINT <- append(CONSTRAINT,semicolon)
  CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))

  header <- readLines(fixPath(file.path(dir,"header.txt")))

  round2input <- as.data.frame(NULL)
  round2input[1,1] <- paste("TITLE: Round 2 Calibration Model")
  round2input[2,1] <- header[2]
  round2input[3,1] <- header[3]
  round2input[4,1] <- header[4]
  round2input[5,1] <- header[5]
  round2input[6,1] <- ifelse(length(header)>5,header[6],"!")
  round2input[7,1] <- ifelse(length(header)>6,header[7],"!")
  round2input[8,1] <- ifelse(length(header)>7,header[8],"!")
  round2input[9,1] <- ifelse(length(header)>8,header[9],"!")
  round2input[10,1] <- paste("USEVARIABLES= ",useround2,semicolon,sep="")
  round2input[11,1] <- AUXILIARY
  round2input[12,1] <- ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  round2input[13,1] <- ifelse(length(mycountindicators)>0,COUNT,"!")
  round2input[14,1] <- CONSTRAINT
  round2input[15,1] <- ANALYSIS
  round2input[16,1] <- varMODEL
  l <- length(loadings)
  for (i in 1:l){
    round2input[(16+i),1] <- loadings[i]
  }
  round2input[(17+l),1] <- ifelse(length(keepmeanimpact)>0,utils::capture.output(cat(ETAON2)),"!")

  the.row <- nrow(round2input)
  
  if (thresholds == FALSE) {
    for (i in 1:l){
      #predlist <- unlist(allinterceptdf[i,2:l])
      #predlist <- unlist(allinterceptdf[i,1:(length(myMeasInvar)+1)])  #changed by IS to get all possible intercept DIF --should go thru length(myMeasInvar)+1
      predlist <- unlist(allinterceptdf[i,2:(length(myMeasInvar)+1)])  #VC changed again -- should start at 2 because first element of predlist is a string
      predlist <- predlist[!is.na(predlist)]
      predlist <- utils::capture.output(cat(predlist))
      round2input[(the.row+i),1] <- ifelse(length(predlist)>0,paste(myindicators[i]," on ",predlist,";",sep=""),"!") #VC: check this
      the.row <- nrow(round2input)
    }
  } else {
    for (i in 1:l) { #IS not sure if this needs to be or what to do for this section when thresholds=TRUE?? 
      th <-length(unique(mrdata[stats::complete.cases(mrdata), myindicators[l]]))-1
      if (sum(is.na(allinterceptdf[i,])) > 0) {
        for (k in 1:th) {
          round2input[(the.row + 1),1] <- paste0("[",myindicators[i], "$", k, "](T", i, "_", k, ");")
          the.row <- nrow(round2input)
        }
      }
    }
  }
  
  round2input <- rbind(round2input, new.constraint.section, constraint.section)
  the.row <- nrow(round2input)  
  
  round2input[(the.row + 1),1] <- tech1


  #write.table(round2input,file.path(dir,"round2calibration.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(round2input,fixPath(file.path(dir,"round2calibration.inp",sep="")))
  message("COMPLETE. Check '", dir, "/' for Mplus inp file for round 2 calibration model (run this manually).")
  message("\n\nNOTE: The generated Mplus inputs are templates, which will likely need to be altered by the user. \nPlease read each input, alter it if necessary, and run it manually; similarly, please interpret all outputs manually. \n\nThis message will appear after all subsequent code-generating steps.")
}
