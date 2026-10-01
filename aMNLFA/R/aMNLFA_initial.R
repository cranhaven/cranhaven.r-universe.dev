#' aMNLFA initial model fitting function
#'
#' This function generates the initial itemwise aMNLFA models.
#' @name aMNLFA.initial
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' 
#'  \dontrun{
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
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
#'  aMNLFA.initial(ob)
#'  }
#'  

aMNLFA.initial<-function(input.object){
  

  
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
  
  varlist<-c(myID, myauxiliary, myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist<-unique(varlist)
  
  #Create input text
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
  ANALYSIS<-noquote("ANALYSIS: ESTIMATOR=ML;ALGORITHM=INTEGRATION;INTEGRATION=MONTECARLO;PROCESSORS=4;")
  
  ETA<-paste("ETA BY ")
  l<-length(myindicators)
  loadings<-list()
  for (i in 1:l){
    loadings[i]<-paste(ETA,myindicators[i],"*(l",i,");",sep="")
  }
  loadings<-noquote(loadings)
  loadings<-unlist(loadings)
  
  tech1<-paste("OUTPUT: tech1;")
  
  #Mean Impact
  MODEL<-paste("MODEL: [ETA@0]; ETA@1;")
  allmean<-append(myindicators,myMeanImpact)
  USEVARMeanImpact<-append(USEVARIABLES,allmean)
  USEVARMeanImpact<-append(USEVARMeanImpact,semicolon)
  USEVARMeanImpact<-noquote(USEVARMeanImpact)
  usemean<-utils::capture.output(cat(USEVARMeanImpact))
  ETAON<-paste("ETA ON")
  ETAON<-append(ETAON,myMeanImpact)
  ETAON<-noquote(append(ETAON,semicolon))
  ETAON<-utils::capture.output(cat(ETAON))
  ETAON4VAR<-append(ETAON,myVarImpact)
  ETAON4VAR<-noquote(append(ETAON4VAR,semicolon))
  ETAON4VAR<-utils::capture.output(cat(ETAON4VAR))
  
  
  meaninput<-as.data.frame(NULL)
  
  #header<-readLines(fixPath(file.path(dir,"header.txt")))
  header<-readLines(file.path(dir,"header.txt"))
  
  meaninput[1,1]<-paste("TITLE: Mean Impact Model")
  meaninput[2,1]<-header[2]
  meaninput[3,1]<-header[3]
  meaninput[4,1]<-header[4]
  meaninput[5,1]<-header[5]
  meaninput[6,1]<-ifelse(length(header)>5,header[6],"!")
  meaninput[7,1]<-ifelse(length(header)>6,header[7],"!")
  meaninput[8,1]<-ifelse(length(header)>7,header[8],"!")
  meaninput[9,1]<-ifelse(length(header)>8,header[9],"!")
  meaninput[10,1]<-usemean
  meaninput[11,1]<-AUXILIARY
  meaninput[12,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  meaninput[13,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  meaninput[14,1]<-ANALYSIS
  meaninput[15,1]<-MODEL
  l<-length(loadings)
  for (i in 1:l){
    meaninput[15+i,1]<-loadings[i]
  }
  meaninput[16+l,1]<-ETAON
  meaninput[17+l,1]<-tech1
  #write.table(meaninput,file.path(dir,"meanimpactscript.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  #need to load the write.inp.file function (found in dropbox folder)
  write.inp.file(meaninput,fixPath(file.path(dir,"meanimpactscript.inp",sep="")))
  message("Check '", dir, "/' for Mplus inp file for mean impact model (run this manually)")
  

  
  ##Variance impact script
  if (length(myVarImpact) > 0) {
    allvariance<-append(myindicators,myVarImpact)
    USEVARImpact<-append(USEVARIABLES,allvariance)
    USEVARImpact<-append(USEVARImpact,semicolon)
    USEVARImpact<-noquote(USEVARImpact)
    usevariance<-utils::capture.output(cat(USEVARImpact))
    CONSTRAINT<-paste("CONSTRAINT=")
    CONSTRAINT<-append(CONSTRAINT,myVarImpact)
    CONSTRAINT<-append(CONSTRAINT,semicolon)
    CONSTRAINT<-utils::capture.output(cat(CONSTRAINT))
    MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(") #This was moved up here on 8/3
  } else { 
    #Adding this in to accommodate case of no variance impact
    allvariance <- myindicators
    USEVARImpact<-append(USEVARIABLES,allvariance)
    USEVARImpact<-append(USEVARImpact,semicolon)
    USEVARImpact<-noquote(USEVARImpact)
    usevariance<-utils::capture.output(cat(USEVARImpact))
    CONSTRAINT = "!"
    MODELCONSTRAINT = "MODEL CONSTRAINT: "
  }
  
  varMODEL<-paste("MODEL: [ETA@0];ETA*(veta);")
  
  if (length(myVarImpact > 0)) { 
    #Adding in new condition to account for no variance impact
    ETAON2<-paste("ETA ON")
    ETAON2<-append(ETAON2,myVarImpact)
  } else {
    #Adding in new condition to account for no variance impact
    ETAON2 <- "[ETA@0]"
  }
  ETAON2<-noquote(append(ETAON2,semicolon))
  ETAON2<-utils::capture.output(cat(ETAON2))
  varMODELwETAON<-paste("MODEL: ",ETAON2," ETA*(veta);",sep="")
  
  varinput<-as.data.frame(NULL)
  
  varinput[1,1]<-paste("TITLE: Variance Impact Model")
  varinput[2,1]<-header[2]
  varinput[3,1]<-header[3]
  varinput[4,1]<-header[4]
  varinput[5,1]<-header[5]
  varinput[6,1]<-ifelse(length(header)>5,header[6],"!")
  varinput[7,1]<-ifelse(length(header)>6,header[7],"!")
  varinput[8,1]<-ifelse(length(header)>7,header[8],"!")
  varinput[9,1]<-ifelse(length(header)>8,header[9],"!")
  varinput[10,1]<-usevariance
  varinput[11,1]<-AUXILIARY
  varinput[12,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  varinput[13,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  varinput[14,1]<-CONSTRAINT
  varinput[15,1]<-ANALYSIS
  varinput[16,1]<-varMODELwETAON
  l<-length(loadings)
  #Let's set a counter variable just because it helps us deal with different number strings -- new as of 8/3
  #We'll call it "var.counter" and we'll increment it every time we add a new line
  #This looks unnatural in some places (i.e., outside a for loop), but it gets the job done
  #-VTC, 8/3/2021
  var.counter <- nrow(varinput) + 1
  for (i in 1:l){
    varinput[var.counter,1]<-loadings[i]
    var.counter <- var.counter + 1
  }
  varinput[var.counter,1]<-MODELCONSTRAINT
  var.counter <- var.counter + 1
  v<-length(myVarImpact)
  #Let's add in the following for no variance impact -- now conditional on length(myVarImpact) > 0
  if (v > 0) {
    for (i in 1:v){
      varinput[var.counter,1]<-paste("v",i,"*0",sep="")
      var.counter <- var.counter + 1
    }
  varinput[var.counter,1]<-paste(");")
  var.counter <- var.counter + 1
  varinput[var.counter,1]<-paste("veta=1*exp(")
  var.counter <- var.counter + 1
  #OK, so this needs to be (i in 1:v-1) because the last line after this does the vth covariate
  #for (i in 1:(v-1)){ #This had been (i in 2:v-1) but it works now -- check why?
    #VTC, 11/2/2021 -- OK, so we know why now: because the first element of this yields v0
    #but it was causing messed up output when you only had one variance variable
    if (v > 1)  {
        for (i in 1:(v-1))  {
        varinput[var.counter,1]<-paste("v",i,"*",myVarImpact[i],"+",sep="")
        var.counter <- var.counter + 1
      }
    }
  varinput[var.counter,1]<-paste("v",v,"*",myVarImpact[v],");",sep="")
  } else {
    varinput[var.counter, 1] <- "veta = 1;"
  }
  var.counter <- var.counter + 1
  varinput[var.counter,1]<-tech1
  
  
  #write.table(varinput,file.path(dir,"varimpactscript.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(varinput,fixPath(file.path(dir,"varimpactscript.inp",sep="")))
  message("Check '", dir, "/' for Mplus inp file for variance impact model (run this manually). If you trim the variance model manually, do not change the new parameter labels (e.g., v2 is still v2 if you drop the v1 parameter).")
  
  
  
  # Creating Measurement invariance scripts
  
  #Preliminary step: have to make sure MODELCONSTRAINT contains "new( " even if it was omitted for null variance impact -- VTC, 8/5/2021
  MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(") #This was moved up here on 8/3
  
  
  # First are a series of logical gates to make sure that the user has properly defined
  # their categorical indicators and if they want or do not want threshold DIF
  if (exists("mycatindicators")==FALSE) {
    stop("User must define mycatindicators as NULL or NA if there are no categorical indicators")
  }
  if (thresholds==TRUE) {
    if (identical(mycatindicators, NA)|identical(mycatindicators, NULL)) {
      stop("mycatindicators defined as NA or NULL; need categorical indicators to
           perform threshold invariance testing. Otherwise change thresholds argument
           to FALSE")
    } else {
      #Perform threshold invariance testing
      ## Noninvariance with thresholds (intercepts below)
      mycontindicators <- myindicators[!(myindicators %in% mycatindicators)]
      
      allmi <- append(myindicators, myMeasInvar)
      USEMI <- append(USEVARIABLES, allmi)
      USEMI <- append(USEMI, semicolon)
      USEMI <- noquote(USEMI)
      usemi <- utils::capture.output(cat(USEMI))
      CONSTRAINT <- paste("CONSTRAINT=")
      CONSTRAINT <- append(CONSTRAINT, myMeasInvar)
      CONSTRAINT <- append(CONSTRAINT, semicolon)
      CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))
      reg <- utils::capture.output(cat(myMeasInvar))
      
      indlength <- length(myindicators)
      for (w in 1:indlength) {
        miinput <- as.data.frame(NULL)
        miinput[1, 1] <- paste("TITLE: Measurement Invariance Model for",
                               myindicators[w], sep = " ")
        miinput[2, 1] <- header[2]
        miinput[3, 1] <- header[3]
        miinput[4, 1] <- header[4]
        miinput[5, 1] <- header[5]
        miinput[6, 1] <- ifelse(length(header) > 5, header[6], "!")
        miinput[7, 1] <- ifelse(length(header) > 6, header[7], "!")
        miinput[8, 1] <- ifelse(length(header) > 7, header[8], "!")
        miinput[9, 1] <- ifelse(length(header) > 8, header[9], "!")
        miinput[10, 1] <- usemi
        miinput[11, 1] <- AUXILIARY
        miinput[12, 1] <- ifelse(length(mycatindicators) > 0, CATEGORICAL, "!")
        miinput[13, 1] <- ifelse(length(mycountindicators) > 0, COUNT, "!")
        miinput[14, 1] <- CONSTRAINT
        miinput[15, 1] <- ANALYSIS
        miinput[16, 1] <- MODEL
        
        # print the loadings
        l <- length(loadings)
        for (i in 1:l) {
          miinput[16 + i, 1] <- loadings[i]
        }
        # Define the # of thresholds
        # Print the thresholds
        #th <-length(unique( mrdata[stats::complete.cases(mrdata), myindicators[w]] ))-1 RDS commented out temporarily for meriah
        th  = length( unique(stats::na.omit(mrdata[, myindicators[w]])) )-1
        
        for (i in seq(th)) {
          miinput[16 + l +i, 1] <- paste("[", myindicators[w], "$",i,
                                         "](T",i,");", sep = "" )
        }
        # Print the model constraints
        miinput[17 + l + th , 1] <- MODELCONSTRAINT
        miinput[18 + l + th, 1] <- paste("l", w, "_00*1", sep = "")
        # new loading parameters
        h <- length(myMeasInvar)
        for (i in seq(h)) {
          miinput[18 + l + th + i, 1] <- paste("l", w, "_", i, "*0", sep = "")
        }
        row <- 18 + l + th + h +1
        # new threshold parameters
        for (i in seq(th)) {
          miinput[row, 1] <- paste("T", i, "_00*",i, sep = "")
          row <- row+1
        }
        s <- 0
        for (i in seq(th)) {
          for (ii in seq(h)) {
            miinput[row, 1] <-
              paste("T", i, "_", ii, "*0", sep = "")
            row <- row +1
          }
          s <- s+1
        }
        
        miinput[row, 1] <- ");"
        # Define Loadings
        miinput[row+1, 1] <- paste("l", w, " = ", "l", w, "_00", sep = "")
        for (i in seq(h)) {
          miinput[row + 2, 1] <- paste("+ l", w, "_", i, "*", myMeasInvar[i], sep = "")
          row <- row + 1
        }
        row <- row+2
        miinput[row, 1] <- ";"
        # Define Thresholds
        # pick up here
        for (i in seq(th)) {
          miinput[row+1, 1] <- paste("T",i," = ", "T", i, "_00", sep = "" )
          row <- row+1
          for (ii in seq(myMeasInvar)) {
            miinput[row+1,1] <-  paste("+ T", i, "_", ii, "*", myMeasInvar[ii], sep = "")
            row <- row + 1
          }
          miinput[row+1,1] <- ";"
          row <- row+1
        }
        
        
        
        
        
        # write.table(miinput,paste(dir,'/measinvarscript_',myindicators[w],'.inp',sep=''),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
        #write.inp.file(miinput, fixPath(file.path(dir,"measinvarscript_", myindicators[w], ".inp", sep = "")))
        write.inp.file(miinput, fixPath(file.path(dir, paste("measinvarscript_",myindicators[w], ".inp",sep=''), sep = "")))
        
        message("COMPLETE. Check '", dir, "/' for Mplus inp file for measurement invariance model for ", myindicators[w], " (run this manually).")  
      }
    }
  } else {
    #Noninvariance with intercepts
    allmi <- append(myindicators, myMeasInvar)
    USEMI <- append(USEVARIABLES, allmi)
    USEMI <- append(USEMI, semicolon)
    USEMI <- noquote(USEMI)
    usemi <- utils::capture.output(cat(USEMI))
    CONSTRAINT <- paste("CONSTRAINT=")
    CONSTRAINT <- append(CONSTRAINT, myMeasInvar)
    CONSTRAINT <- append(CONSTRAINT, semicolon)
    CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))
    reg <- utils::capture.output(cat(myMeasInvar))
    
    indlength <- length(myindicators)
    for (w in 1:indlength) {
      miinput <- as.data.frame(NULL)
      miinput[1, 1] <- paste("TITLE: Measurement Invariance Model for",
                             myindicators[w], sep = " ")
      miinput[2, 1] <- header[2]
      miinput[3, 1] <- header[3]
      miinput[4, 1] <- header[4]
      miinput[5, 1] <- header[5]
      miinput[6, 1] <- ifelse(length(header) > 5, header[6], "!")
      miinput[7, 1] <- ifelse(length(header) > 6, header[7], "!")
      miinput[8, 1] <- ifelse(length(header) > 7, header[8], "!")
      miinput[9, 1] <- ifelse(length(header) > 8, header[9], "!")
      miinput[10, 1] <- usemi
      miinput[11, 1] <- AUXILIARY
      miinput[12, 1] <- ifelse(length(mycatindicators) > 0, CATEGORICAL, "!")
      miinput[13, 1] <- ifelse(length(mycountindicators) > 0, COUNT, "!")
      miinput[14, 1] <- CONSTRAINT
      miinput[15, 1] <- ANALYSIS
      miinput[16, 1] <- MODEL
      l <- length(loadings)
      for (i in 1:l) {
        miinput[16 + i, 1] <- loadings[i]
      }
      miinput[17 + l, 1] <- paste(myindicators[w], " on ", reg, ";", sep = "")
      miinput[18 + l, 1] <- MODELCONSTRAINT
      miinput[19 + l, 1] <- paste("l", w, "_00*1", sep = "")
      h <- length(myMeasInvar)
      for (i in 1:h) {
        miinput[19 + l + i, 1] <- paste("l", w,"_", i, "*0", sep = "")
      }
      miinput[20 + l + h, 1] <- paste(");")
      miinput[21 + l + h, 1] <- paste("l", w, "=l", w, "_00", sep = "")
      for (i in 1:h) {
        miinput[21 + l + h + i, 1] <- paste("+l", w, "_",i, "*", myMeasInvar[i], sep = "")
      }
      miinput[22 + l + h + h, 1] <- semicolon
      miinput[23 + l + h + h, 1] <- tech1
      
      
      # write.table(miinput,paste(dir,'/measinvarscript_',myindicators[w],'.inp',sep=''),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
      write.inp.file(miinput, fixPath(file.path(dir,paste("measinvarscript_", myindicators[w], ".inp", sep = ""))))
      message("COMPLETE. Check '", dir, "/' for Mplus inp file for measurement invariance model for ", myindicators[w], " (run this manually).")  
    }
  }
  message("\n\nNOTE: The generated Mplus inputs are templates, which will likely need to be altered by the user. \nPlease read each inputm, alter it if necessary, and run it manually; similarly, please interpret all outputs manually. \n\nThis message will appear after all subsequent code-generating steps.")
}
