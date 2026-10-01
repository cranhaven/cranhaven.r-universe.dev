#' aMNLFA score generating function
#'
#' This function creates scores generated using aMNLFA. Can only be run after the final model has been fit -- i.e., after the aMNLFA.final function.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @return No return value. Generates an INP file to be run in \emph{Mplus} to generate scores in the directory specified in the aMNLFA.object. 
#' @keywords MNLFA
#' @export
#' @examples
#' 
#'  \dontrun{
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/extdata")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'  
#'  ob <- aMNLFA::aMNLFA.object(dir = wd, 
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
#'  aMNLFA.scores(ob)
#'  }


aMNLFA.scores<-function(input.object){
  
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
  
  if (thresholds == TRUE) {
    stop("thresholds == TRUE is disabled in this version of aMNLFA. Reset thresholds to FALSE to run this function.")
  }
  
  
  
  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist<-unique(varlist)
  
  header<-readLines(fixPath(file.path(dir,"header.txt")))
  header2<-readLines(fixPath(file.path(dir,"header2.txt")))
  
  USEVARIABLES<-paste("USEVARIABLES=")
  semicolon<-paste(";")
  AUXILIARY<-paste("AUXILIARY=")
  AUXILIARY<-append(AUXILIARY,myauxiliary)
  AUXILIARY<-noquote(append(AUXILIARY,semicolon))
  AUXILIARY<-utils::capture.output(cat(AUXILIARY))
  CATEGORICAL<-paste("CATEGORICAL=")
  CATEGORICAL<-append(CATEGORICAL,mycatindicators)
  CATEGORICAL<-noquote(append(CATEGORICAL,semicolon))
  CATEGORICAL<-utils::capture.output(cat(CATEGORICAL))
  COUNT<-paste("COUNT=")
  COUNT<-append(COUNT,mycountindicators)
  COUNT<-noquote(append(COUNT,semicolon))
  COUNT<-utils::capture.output(cat(COUNT))
  ANALYSIS<-noquote("ANALYSIS: ESTIMATOR=ML; ALGORITHM=INTEGRATION; INTEGRATION=MONTECARLO; PROCESSORS=4;") #IS added spaces to accommodate new write.inp.file
  ETA<-paste("ETA BY ")
  l<-length(myindicators)
  loadings<-list()
  for (i in 1:l){
    loadings[i]<-paste(ETA,myindicators[i],"*(l",i,");",sep="")
  }
  loadings<-noquote(loadings)
  loadings<-unlist(loadings)
  tech1<-paste("OUTPUT: tech1;")
  MODEL<-paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT<-paste("CONSTRAINT=")
  varMODEL<-paste("MODEL: [ETA@0];ETA*(veta);")
  fixvarMODEL<-paste("MODEL: [ETA@0];ETA@1;")
  MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(")
  
  
  #############################################################################
  ###Read in final calibration model to obtain final parameter estimates#######
  ##############Output scoring model input#####################################
  #############################################################################
  
  round3output<-MplusAutomation::readModels(fixPath(file.path(dir,"round3calibration.out",sep="")))
  # round3output<-MplusAutomation::readModels(fixPath(file.path(substr(dir, 1, nchar(dir)-1),"round3calibration.out",sep=""))) #IS changed
  
  round3input<-round3output$input$model.constraint
  
  keepvarimpact<-list()
  for (i in 1:length(round3input)){
    if (length(grep("+",round3input[i],value=TRUE,fixed=TRUE))>0) keepvarimpact<-append(keepvarimpact,round3input[i])
  }
  
  foo<-strsplit(as.character(keepvarimpact),"+",fixed=TRUE)
  foo<-strsplit(as.character(unlist(foo)),"*",fixed=TRUE)
  foo<-unlist(foo)
  foo<-foo[!is.na(match(foo,myVarImpact))] #IS: this makes it NA
  keepvarimpact<-utils::capture.output(cat(foo))
  round3model <- round3output$input$model
  
  threshold.string <- round3model[which(grepl("\\$", round3model))]
  
  round3constraints <- round3output$input$model.constraint
  #slightly hackish way to only get the portion after the declaration of new constraints
  end.new <- grep(");", round3constraints)
  end.new <- min(end.new)
  round3constraints <- round3constraints[(end.new+1):length(round3constraints)]
  
  round3output<-as.data.frame(round3output$parameters$unstandardized)
  
  
  ETAPREDLIST<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  
  
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  lambdaconstraints<-constraints[which(substr(constraints$param,1,1)=="L"),]
  lambdaconstraints$itemnum<-substr(lambdaconstraints$param,2,2)
  lambdaconstraints$item<-myindicators[as.numeric(lambdaconstraints$itemnum)]
  #lambdaconstraints$predictornum<-substr(lambdaconstraints$param,3,4)
  lambdaconstraints$predictornum<-stringi::stri_extract_last_regex(lambdaconstraints$param, "\\d{1}") #IS changed to index last digit (to accommodate different char lengths)
  
  #IS edited this section below to unscramble covariate assignment to lambda DIF
  # lambdaconstraints$predictor<-ifelse(lambdaconstraints$predictornum=="_0","intercept",myMeasInvar[as.numeric(sub("_","",lambdaconstraints$predictornum))])
  if (nrow(lambdaconstraints)>0){ #only if there is lambda DIF
    lambdaconstraints$predictor=NA
    lambdaconstraints$predictor=ifelse(lambdaconstraints$predictornum=="_0","intercept", lambdaconstraints$predictor)
    for (q in 1:length(lambdaconstraints$predictornum)) {lambdaconstraints$predictor[q] <- ifelse(is.na(lambdaconstraints$predictor[q]),
                                                                                                  myMeasInvar[as.numeric(sub("_","",lambdaconstraints$predictornum[q]))],
                                                                                                  lambdaconstraints$predictor[q])} #added by IS, adapted from aMNLFA_simultaneous
  }
  
  
  lambdainvlist<-unique(unlist(lambdaconstraints$item))
  
  lambda<-round3output[which(grep(".BY",round3output$paramHeader)>0),]
  
  round4<-unlist(round3output$param)
  keepround4<-myindicators
  for (i in 1:length(round4))
  {
    if (length(grep(round4[i],myMeasInvar))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myMeanImpact))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myVarImpact))>0) keepround4<-append(keepround4,round4[i])
  }
  keepround4<-unique(keepround4)
  keepround4<-keepround4[!is.na(keepround4)]
  keepround4<-utils::capture.output(cat(keepround4))
  
  lc<-lambdaconstraints$predictor[which(lambdaconstraints$predictor!="intercept")]
  lc<-unique(lc)
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  varimpact<-constraints[which(substr(constraints$param,1,1)=="V"),]
  varimpact<-substr(varimpact$param,2,3)
  keepvarimpact<-myVarImpact[as.numeric(varimpact)]
  con<-unique(append(keepvarimpact,lc))
  CONSTRAINT<-noquote(append(CONSTRAINT,con))
  CONSTRAINT<-append(CONSTRAINT,semicolon)
  CONSTRAINT<-utils::capture.output(cat(CONSTRAINT))
  CONSTRAINT<-ifelse(length(con)>0,CONSTRAINT,"!")
  
  meanimpact<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  keepmeanimpact<-meanimpact$param
  ETAON3<-"!"
  if(length(keepmeanimpact)>0) ETAON3<-paste("ETA ON ",keepmeanimpact,"@",sep="")
  #writing scoring model
  scoringinput<-as.data.frame(NULL)
  
  scoringinput[1,1]<-paste("TITLE: Scoring Model")
  scoringinput[2,1]<-header2[2]
  scoringinput[3,1]<-header2[3]
  scoringinput[4,1]<-header2[4]
  scoringinput[5,1]<-header2[5]
  scoringinput[6,1]<-ifelse(length(header2)>5,header[6],"!")
  scoringinput[7,1]<-ifelse(length(header2)>6,header[7],"!")
  scoringinput[8,1]<-ifelse(length(header2)>7,header[8],"!")
  scoringinput[9,1]<-ifelse(length(header2)>8,header[9],"!")
  scoringinput[10,1]<-paste("USEVARIABLES= ",keepround4,semicolon,sep="")
  clus<-ifelse(is.null(mytime),"!",paste("cluster=",myID,semicolon,sep=""))
  aux<-append(myauxiliary,myID)
  aux<-unique(aux)
  aux<-ifelse(length(aux)>0,paste("AUXILIARY=",aux,semicolon,sep=""),"!")
  scoringinput[11,1]<-aux
  scoringinput[12,1]<-clus
  scoringinput[13,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  scoringinput[14,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  scoringinput[15,1]<-CONSTRAINT
  scAN<-paste(ANALYSIS," type=complex;",sep="") #IS added space to accommodate new write.inp.file
  scoringinput[16,1]<-ifelse(is.null(mytime),ANALYSIS,scAN)
  scoringinput[17,1]<-varMODEL
  
  the.row <- nrow(scoringinput) + 1
  
  l<-length(myindicators)
  scoreloadings<-list()
  for (i in 1:l){
    scoreloadings[i]<-paste(ETA,myindicators[i],sep="")
    scoreloadings[i]<-ifelse(lambda$pval[i] !=999,paste(" @",lambda$est[i],semicolon,sep=""),paste("*(l_",i,")",semicolon,sep=""))
    scoreloadings[i]<-paste(ETA,myindicators[i],scoreloadings[i],sep="")
  }
  
  for (i in 1:l){
    scoringinput[the.row,1]<-scoreloadings[i]
    the.row <- the.row + 1
  }
  for (i in 1:length(ETAON3)){
    scoringinput[the.row,1]<-paste(ETAON3[i],ETAPREDLIST$est[i],semicolon,sep="")
    the.row <- the.row + 1
  }
  
  
  intdif<-round3output[grep(".ON",round3output$paramHeader),]
  # intdif<-intdif[which(intdif$item!="ETA"),]
  intdif<-intdif[which(intdif$paramHeader!="ETA.ON"),] #IS edited to get intercept DIF
  
  intcode<-character(0)
  if (nrow(intdif)>0) {
    intdif$item<-utils::read.table(text = intdif$paramHeader, sep = ".", as.is = TRUE)$V1
    keepcols<-c("param","est","item")
    intdif<-intdif[,keepcols]
    intcode<-paste(intdif$item," ON ",intdif$param,"@",intdif$est,semicolon,sep="")
    
    for (i in 1:length(intcode)){
      scoringinput[the.row,1]<-intcode[i]
      the.row <- the.row + 1
    }
  }
  
  thresh<-round3output[which(round3output$paramHeader=="Thresholds"|round3output$paramHeader=="Intercepts"&round3output$param!="ETA"),] 
  thresh <- subset(thresh, thresh$est != 999) 
  for (i in 1:nrow(thresh)){
    scoringinput[the.row,1]<-paste("[",thresh$param[i],"@",thresh$est[i],"];",sep="")
    the.row <- the.row + 1
  }
  
  if (length(threshold.string > 0)) {
    thresh.dataframe <- as.data.frame(NULL)
    for (t in 1:length(threshold.string)) {
      thresh.dataframe[t,1] <- threshold.string[t]
    }
    scoringinput <- rbind(scoringinput, thresh.dataframe)
  }
  
  
  the.row <- nrow(scoringinput)
  
  constraint.section <- as.data.frame(NULL)
  MODELCON<-paste("MODEL CONSTRAINT:")
  constraint.section[1,1] <- MODELCON
  constraint.row <- 2
  constraint.replaced <- round3constraints
  
  if (nrow(constraints)>0){ #added by IS to accommodate no constraints
    for (c in 1:length(round3constraints)) {
      for (d in 1:nrow(constraints)) {
        if (grepl(constraints$param[d], round3constraints[c], ignore.case = TRUE) == TRUE) {
          constraint.replaced[c] <- sub(constraints$param[d], constraints$est[d], constraint.replaced[c], ignore.case = TRUE)
          constraint.replaced[c] <- sub("\\+\\-", "\\-", constraint.replaced[c])
        }
      }
      constraint.section[constraint.row,1] <- constraint.replaced[c]
      constraint.row <- constraint.row + 1
    }
  } else { #added by IS to accommodate no constraints
    for (c in 1:length(round3constraints)) {
      constraint.section[constraint.row,1] <- constraint.replaced[c]
      constraint.row <- constraint.row + 1
    }
    # constraint.section=constraint.section[1:3,] #added by IS to accommodate no constraints (removes excess stuff??)
  }
  
  scoringinput <- rbind(scoringinput, constraint.section)
  
  the.row <- nrow(scoringinput)
  scoringinput[(the.row + 1),1]<-tech1
  scoringinput[(the.row + 2),1]<-paste("SAVEDATA: SAVE=FSCORES; FILE=scores.dat;")
  
  #utils::write.table(scoringinput,paste(dir,"/scoring.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(scoringinput,fixPath(file.path(dir,"scoring.inp",sep="")))
  message("Check '", dir, "/' for Mplus inp file for scoring model (run this manually).")
  message("\n\nNOTE: The generated Mplus inputs are templates, which will likely need to be altered by the user. \nPlease read each inputm, alter it if necessary, and run it manually; similarly, please interpret all outputs manually. \n\nThis message will appear after all subsequent code-generating steps.")}