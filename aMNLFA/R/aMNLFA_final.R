#' aMNLFA simultaneous model fitting function
#'
#' This function generates the simultaneous aMNLFA model from all the initial inputs.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @param mchoice String representing the method of determining the number of tests, denoted m. Options include "actual", which uses the number of effects actually tested in the round 2 model as m, and "ibc", which uses the maximum number of all possible tests -- i.e., the number of items times the number of covariates. Defaults to "actual".
#' @param method String representing the method of adjusting for multiple comparisons. Options include "bh", which invokes Benjamini-Hochberg correction with m defined using the mchoice parameter, and "bonferroni", which invokes a Bonferroni correction with m defined using the mchoice parameter.  Defaults to "bh".
#' @param highest.category Boolean. If threshold DIF is tested, should only the category with the highest value of the test statistic be used when adjusting p. values? Defaults to TRUE, which corresponds to the results from "threshold.highest" in the aMNLFA.prune() step. If FALSE, all threshold effects will be considered, even those below the maximum value for a given item, which corresponds to the "thresholds.all" option in the aMNLFA.prune() step.
#' @param keepmean Boolean. If intercept or loading DIF are present, should the corresponding mean impact effect be retained? Defaults to FALSE.
#' @return No return value. Generates a file entitled "round3calibration.inp", to be run in \emph{Mplus}, in the directory specified in the aMNLFA.object. 
#' @keywords MNLFA
#' @export
#' @examples
#'  \dontrun{
#'  wd  <-  tempdir()
#'  first <- paste0(system.file(package='aMNLFA'),"/extdata")
#'  the.list  <-  list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'    
#'  ob  <-  aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("BIN_", 1:12),
#'  catindicators = paste0("BIN_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  
#'  aMNLFA.simultaneous(ob)
#'  }


aMNLFA.final <- function(input.object, mchoice = "actual", method = "BH", highest.category = TRUE, keepmean = FALSE){

  method <- base::toupper(method) #Put this in to accommodate people using lowercase "bh" on 10/7
  
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
  
  #Get some strings we will use in the code no matter what
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
  
  the.prune <- aMNLFA.prune(input.object)
  
  
  lambdadif.mat.BH.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.BH.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.bon.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.bon.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  
  intdif.mat.BH.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.BH.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.bon.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.bon.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  
  # lambdadif <- the.prune$`Summary of Effects`$`Loading DIF`
  lambdadif <- the.prune$summary$loadingDIF #IS changed to match the.prune data structure....
  
  if (thresholds == TRUE) {
    if (highest.category == TRUE) {
      # intdif <- the.prune$`Summary of Effects`$`Threshold DIF - Highest Category Used`
      intdif <- the.prune$summary$`Threshold DIF - Highest Category Used` #IS changed partially but this will likely not run as I have not set THRESHOLDS=TRUE and thus do not know the actual structure
      
    } else {
      # intdif <- the.prune$`Summary of Effects`$`Threshold DIF - All Categories Used`
      intdif <- the.prune$summary$`Threshold DIF - All Categories Used` #IS changed partially but this will likely not run as I have not set THRESHOLDS=TRUE and thus do not know the actual structure
      
    }
  } else {
    # intdif <- the.prune$`Summary of Effects`$`Intercept DIF`
    intdif <- the.prune$summary$interceptDIF #IS changed to match the.prune data structure
    
  }
  
  for (m in 1:length(myindicators)) {
    for (p in 1:length(myMeasInvar)) {
      if (nrow(subset(lambdadif, ((lambdadif$item.label == m) & (lambdadif$covariate.label == p) & (lambdadif$pexact < lambdadif$BH.actual)))) > 0) {lambdadif.mat.BH.actual[m, p] <- 1}
      if (nrow(subset(lambdadif, ((lambdadif$item.label == m) & (lambdadif$covariate.label == p) & (lambdadif$pexact < lambdadif$BH.ibc)))) > 0) {lambdadif.mat.BH.ibc[m, p] <- 1}
      if (nrow(subset(lambdadif, ((lambdadif$item.label == m) & (lambdadif$covariate.label == p) & (lambdadif$pexact < lambdadif$bon.actual)))) > 0) {lambdadif.mat.bon.actual[m, p] <- 1}
      if (nrow(subset(lambdadif, ((lambdadif$item.label == m) & (lambdadif$covariate.label == p) & (lambdadif$pexact < lambdadif$bon.ibc)))) > 0) {lambdadif.mat.bon.ibc[m, p] <- 1}
      
      if (nrow(subset(intdif, ((intdif$item.label == m) & (intdif$covariate.label == p) & (intdif$pexact < intdif$BH.actual)))) > 0) {intdif.mat.BH.actual[m, p] <- 1}
      if (nrow(subset(intdif, ((intdif$item.label == m) & (intdif$covariate.label == p) & (intdif$pexact < intdif$BH.ibc)))) > 0) {intdif.mat.BH.ibc[m, p] <- 1}
      if (nrow(subset(intdif, ((intdif$item.label == m) & (intdif$covariate.label == p) & (intdif$pexact < intdif$bon.actual)))) > 0) {intdif.mat.bon.actual[m, p] <- 1}
      if (nrow(subset(intdif, ((intdif$item.label == m) & (intdif$covariate.label == p) & (intdif$pexact < intdif$bon.ibc)))) > 0) {intdif.mat.bon.ibc[m, p] <- 1}
    }
  }
  
  if (mchoice == "actual") {
    if (method == "BH") {
      the.intdif <- intdif.mat.BH.actual
      the.lambdadif <- lambdadif.mat.BH.actual
    } else {
      the.intdif <- intdif.mat.bon.actual
      the.lambdadif <- lambdadif.mat.bon.actual
    }
  } else {
    if (method == "BH") {
      the.intdif <- intdif.mat.BH.ibc
      the.lambdadif <- lambdadif.mat.BH.ibc
    } else {
      the.intdif <- intdif.mat.bon.ibc
      the.lambdadif <- lambdadif.mat.bon.ibc
    }
  }
  
  the.intdif[the.lambdadif == 1] <- 1 #Any covariate that has loading DIF needs to have intercept DIF too
  
  #added by IS for sanity checking
  #.GlobalEnv$the.intdif <- the.intdif
  #.GlobalEnv$the.lambdadif <- the.lambdadif
  #.GlobalEnv$myMeasInvar<-myMeasInvar
  #.GlobalEnv$myindicators<-myindicators
  lambda_dif_from_aMNLFA_final=the.lambdadif
  colnames(lambda_dif_from_aMNLFA_final)=myMeasInvar
  rownames(lambda_dif_from_aMNLFA_final)=myindicators
  #VC replacing "homedir" with the working directory specified by the user
  utils::write.csv(lambda_dif_from_aMNLFA_final, paste0(dir, "/lambda_dif_from_aMNLFA_final.csv")) #saves out lambda DIF
  
  intercept_dif_from_aMNLFA_final=the.intdif
  colnames(intercept_dif_from_aMNLFA_final)=myMeasInvar
  rownames(intercept_dif_from_aMNLFA_final)=myindicators
  utils::write.csv(intercept_dif_from_aMNLFA_final, paste0(dir, "/intercept_dif_from_aMNLFA_final.csv")) #saves out lambda DIF
  
  
  
  
  ####Start writing input
  model.section <- data.frame(NULL)
  model.row <- 1
  
  constraint.section <- data.frame(NULL)
  constraint.row <- 1
  
  
  new.constraint.section <- data.frame(NULL)
  new.constraint.section[1,1] <- paste("MODEL CONSTRAINT: new(")
  new.constraint.row <- 2
  
  ####Impact
  
  #Mean
  # mean.prune <- the.prune$`Summary of Effects`$`Mean Impact`
  mean.prune <- the.prune$summary$meanimpact #IS changed to match the.prune data structure
  
  mean.prune <- subset(mean.prune, mean.prune$pval < .05)
  
  for (a in 1:nrow(mean.prune)) {
    model.section[model.row,1] <- paste0("eta ON ", mean.prune$param[a], ";")
    model.row <- model.row + 1
  }
  
  #Variance
  # var.prune <- the.prune$`Summary of Effects`$`Variance Impact`
  var.prune <- the.prune$summary$varimpact #IS changed to match the.prune data structure
  
  var.prune <- subset(var.prune, var.prune$pval < .05)
  
  varindices <- as.numeric(sub("V", "", var.prune$param))

  
  constraint.section[constraint.row,1] <- "veta=1*exp("
  constraint.row <- constraint.row + 1
  
  if (nrow(var.prune)>0)
    for (v in 1:length(varindices)){
      constraint.section[constraint.row,1] <- paste("v",varindices[v],"*",myMeasInvar[varindices[v]],"+",sep="")
      constraint.row <- constraint.row + 1
      new.constraint.section[new.constraint.row, 1] <- paste0(var.prune$param[v], "*0")
      new.constraint.row <- new.constraint.row + 1
    }
  constraint.section[constraint.row,1] <- paste("0);")
  new.constraint.section[new.constraint.row,1] <- "!" #Need to put this in just get the indexing right
  
  ####DIF
  
  #loadings
  
  M <- length(myMeasInvar)
  ind <- length(myindicators)
  for (i in 1:ind){
    predlist2 <- myMeasInvar[which(the.lambdadif[i,] == 1)] #list of covars with sig lambda DIF for each indicator
    predlist2 <- predlist2[!is.na(predlist2)]
    eq <- as.data.frame(NULL)
    start <- as.data.frame(NULL)
    eq[1,1] <- ifelse(length(predlist2)>0,paste("l_",i,"=l",i,"_0",sep=""),"!")
    start[1,1] <- ifelse(length(predlist2)>0,paste("l",i,"_0*1 ",sep=""),"!")
    if (length(predlist2)>0)
      predlist2indices <- which(myMeasInvar %in% predlist2) #6/15 -- Here too, like other parts, give the index that corresponds to its order in myMeasInvar 
    for (w in 1:length(predlist2)){
      eq[1+w,1] <- ifelse(length(predlist2)>0,paste("+l",i,"_",predlist2indices[w],"*",predlist2[w],sep="") ,"!")
      start[1+w,1] <- ifelse(length(predlist2)>0,paste(" l",i,"_",predlist2indices[w],"*0",sep="") ,"!")
    }
    constraint.section[constraint.row+1,1] <- paste(utils::capture.output(cat(noquote(unlist(eq)))),";",sep="")
    new.constraint.section[new.constraint.row+1,1] <- paste(utils::capture.output(cat(noquote(unlist(start)))),sep="")
    constraint.row <- nrow(constraint.section)
    new.constraint.row <- nrow(new.constraint.section)
  }
  
  #intercept/threshold
  if (thresholds == FALSE){
    new.constraint.section[new.constraint.row+1,1] <- paste(");")
  } else {
    for (i in 1:l) {
      th <-length(unique(mrdata[stats::complete.cases(mrdata), myindicators[l]]))-1
      sig.covs.thr <- myMeasInvar[which(the.intdif[i,] == 1)]
      cov.index <- seq(1, th) #Used to be 0, not 1 -- that's in other code, because we coded covariates differently
      if (length(sig.covs.thr) > 0) { #Have to index this differently than in the simultaneous function, for the same reason -- we're doing covariates differently
        cov.index <- cov.index[which(myMeasInvar %in% sig.covs.thr)] #only choose values of cov.index for which the corresponding value of sig.covs.thr is significant
        #cov.index <- cov.index[2:length(cov.index)] 
        sig.covs.thr <- sig.covs.thr[is.na(sig.covs.thr) == FALSE]
        #sig.covs.thr <- sig.covs.thr[2:length(sig.covs.thr)]
        for (k in 1:th) {
          constraint.section[constraint.row + 1, 1] <- paste0("T",i,"_",k,"=")
          constraint.section[constraint.row + 2, 1] <- paste0("T",i,"_",k,"_0 + ")
          new.constraint.section[new.constraint.row + 1, 1] <- paste0("T",i,"_",k,"_0")
          new.constraint.row <- nrow(new.constraint.section)
          for (q in 1:length(sig.covs.thr)) {
            new.constraint.section[new.constraint.row + q, 1] <- paste0("T",i,"_",k,"_",cov.index[q])
            constraint.section[constraint.row + q + 2, 1] <- paste0("T",i,"_",k,"_",cov.index[q], "*", sig.covs.thr[q], ifelse(q == length(sig.covs.thr), ";", "+"))
          }
          constraint.row <- nrow(constraint.section)
          new.constraint.row <- nrow(new.constraint.section)
        }
      }
      new.constraint.section[new.constraint.row + 1,1] <- ");"
    }
  }
  
  ########################
  
  any.intercept <- apply(the.intdif, 2, sum)
  uniqueint <- myMeasInvar[which(any.intercept > 0)]
  any.lambda <- apply(the.lambdadif, 2, sum)
  # uniquelambda <- myMeasInvar[which(any.intercept > 0)]
  uniquelambda <- myMeasInvar[which(any.lambda > 0)] #IS changed to get unique LAMBDA
  
  
  # keepmeanimpact <- unique(the.prune$`Summary of Effects`$`Mean Impact`$param)
  #keepmeanimpact <- unique(the.prune$summary$meanimpact$param) #IS changed to reflect the.prune data structure
  #keepmeanimpact <- unique(c(mean.prune$param, uniquelambda,uniqueint)) #IS changed again to pull only the mean impact for covariates that have sig mean impact OR sig lambda/int dif
  
  #VC changed after email conversations, 8/15-18, about including mean impact if corresponding DIF effect is there
  if (keepmean == TRUE) {
    keepmeanimpact <- unique(c(mean.prune$param, uniquelambda,uniqueint)) 
  } else {
    keepmeanimpact <- unique(c(mean.prune$param)) 
    }
  # keepvarimpact <- unique(the.prune$`Summary of Effects`$`Mean Impact`$param)
  #keepvarimpact <- unique(the.prune$summary$meanimpact$param) #IS changed to reflect the.prune data structure
 #keepvarimpact <- unique(the.prune$summary$varimpact$covariate.name) #IS changed again to reflect the.prune data structure
 #keepvarimpact <- unique(var.prune$param) #IS changed again to pull only var impact with p<0.05
  keepvarimpact <- unique(var.prune$covariate.name) #Changed on 10/7 by VTC; the thing we should be taking from var.prune is covariate.name, not param
  
  if (thresholds == FALSE) {
    usefinal <- utils::capture.output(cat(unique(c(myindicators, uniqueint, keepmeanimpact))))
  } else {
    usefinal <- utils::capture.output(cat(unique(c(myindicators, keepmeanimpact))))
  }
  
  ETAON2 <- paste("ETA ON ",keepmeanimpact,";",sep="")
  con <- unique(append(keepvarimpact,uniquelambda))
  CONSTRAINT <- noquote(append(CONSTRAINT,con))
  CONSTRAINT <- append(CONSTRAINT,";")
  CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))
  
  header <- readLines(fixPath(file.path(dir,"header.txt")))
  
  finalinput <- as.data.frame(NULL)
  finalinput[1,1] <- paste("TITLE: Final Model to Get Scoring Parameters")
  finalinput[2,1] <- header[2]
  finalinput[3,1] <- header[3]
  finalinput[4,1] <- header[4]
  finalinput[5,1] <- header[5]
  finalinput[6,1] <- ifelse(length(header)>5,header[6],"!")
  finalinput[7,1] <- ifelse(length(header)>6,header[7],"!")
  finalinput[8,1] <- ifelse(length(header)>7,header[8],"!")
  finalinput[9,1] <- ifelse(length(header)>8,header[9],"!")
  finalinput[10,1] <- paste("USEVARIABLES= ",usefinal,";",sep="")
  finalinput[11,1] <- AUXILIARY
  finalinput[12,1] <- ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  finalinput[13,1] <- ifelse(length(mycountindicators)>0,COUNT,"!")
  finalinput[14,1] <- CONSTRAINT
  finalinput[15,1] <- ANALYSIS
  finalinput[16,1] <- varMODEL
  l <- length(loadings)
  for (i in 1:l){
    finalinput[16+i,1] <- loadings[i]
  }
  finalinput[17+l,1] <- ifelse(length(keepmeanimpact)>0,utils::capture.output(cat(ETAON2)),"!")
  
  the.row <- nrow(finalinput)
  
  
  if (thresholds == FALSE) {
    for (i in 1:l){
      predlist <- unlist(myMeasInvar[which(the.intdif[i,] == 1)])
      predlist <- predlist[!is.na(predlist)]
      predlist <- utils::capture.output(cat(predlist))
      finalinput[the.row+i,1] <- ifelse(length(predlist)>0,paste(myindicators[i]," on ",predlist,";",sep=""),"!") #VC: check this
      the.row <- nrow(finalinput)
    }
  } else {
    for (i in 1:l) {
      th <-length(unique(mrdata[stats::complete.cases(mrdata), myindicators[l]]))-1
      if (sum(the.intdif[i,]) > 0) {
        for (k in 1:th) {
          finalinput[the.row + 1,1] <- paste0("[",myindicators[i], "$", k, "](T", i, "_", k, ");")
          the.row <- nrow(finalinput)
        }
      }
    }
  }
  
  finalinput <- rbind(finalinput, new.constraint.section, constraint.section)
  
  #write.table(finalinput,file.path(dir,"finalcalibration.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(finalinput,fixPath(file.path(dir,"round3calibration.inp",sep="")))
  message("COMPLETE. Check '", dir, "/' for Mplus inp file for round 3 calibration model (run this manually). \n\nNOTE: The generated Mplus inputs are templates, which will likely need to be altered by the user. \nPlease read each inputm, alter it if necessary, and run it manually; similarly, please interpret all outputs manually. \n\nThis message will appear after all subsequent code-generating steps.")
}
