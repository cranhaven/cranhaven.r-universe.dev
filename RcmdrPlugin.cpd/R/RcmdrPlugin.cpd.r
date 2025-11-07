#' 'cpd' Plug-In Utility Functions
#'
#' @name RcmdrPlugin.Utility
NULL
#> NULL
#'@import RcmdrMisc



#Hook function .onAttach is called when attach package.
#' @importFrom Rcmdr putRcmdr getRcmdr closeCommander Commander
.onAttach <- function(libname, pkgname){
   if (!interactive()) return()
   Rcmdr <- options()$Rcmdr
   plugins <- Rcmdr$plugins
   if (!pkgname %in% plugins) {
      Rcmdr$plugins <- c(plugins, pkgname)
      options(Rcmdr=Rcmdr)
      if("package:Rcmdr" %in% search()) {
         if(!getRcmdr("autoRestart")) {
            closeCommander(ask=FALSE, ask.save=TRUE)
            Commander()
         }
      }
      else {
         Commander()
      }
   }
}

#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition checkBoxes activeDataSet
#' @importFrom cpd pcbp
#' @return Not return value, the effects are shown in 'Rcmdr'  GUI

cbpSamples<-function (){
   checksFrame <- meanVariable <- sumVariable <- sdVariable <- NULL
   initial <- getDialog("cbpSamples",
                        defaults = list(
                           initialValues=c(1,4),
                           sName = "cbpSamples",
                           nObse = 100,
                           nSamp = 1,
                           sMean = "1",
                           sSum = "0",
                           sSd="0"))
   initializeDialog(title = "CBP Distribution")
   frame <- tkframe(top)
   sNVar     <-tclVar(initial$sName)
   sNEnt     <- ttkentry(frame, width = "30",textvariable = sNVar)
   bVar     <-tclVar(initial$initialValues[1])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[2])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   nSVar <-tclVar(initial$nSamp)
   nSEnt <- ttkentry(frame, width = "6",textvariable = nSVar)
   nOVar <-tclVar(initial$nObse)
   nOEnt <- ttkentry(frame, width = "6",textvariable = nOVar)
   checkBoxes(frame = "checksFrame", boxes = c("mean", 
             "sum", "sd"), initialValues = c(initial$sMean, 
            initial$sSum, initial$sSd), labels = gettextRcmdr(c("Sample means", 
            "Sample sums", "Sample standard deviations")))
   
   tkgrid(labelRcmdr(frame, text = "Sample name"), sNEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Number of Obs."), nOEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Number of Samples"), nSEnt, sticky = "w", padx = 6)
   tkgrid(frame, sticky = "w")
   tkgrid(checksFrame, sticky = "w")
   tkgrid.configure(sNEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   tkgrid.configure(nOEnt, sticky = "w")
   tkgrid.configure(nSEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      sN    <- gsub(" ","", tclvalue(sNVar))
      nO    <- as.numeric(tclvalue(nOVar))
      nS    <- as.numeric(tclvalue(nSVar))
      
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpSamples, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpSamples, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpSamples, message ="gamma not positive" )
         return()
      }
      if (sN==""){
         errorCondition(recall = cbpSamples, message = "no sample name")
         return()
      }
      if (trunc(nO)!=nO || nO < 1){
         errorCondition(recall = cbpSamples, message = "observations not interger greater than 0")
         return()
      }
      if (trunc(nS)!=nS || nS < 1){
         errorCondition(recall = cbpSamples, message = "samples not interger greater than 0")
         return()
      }
      
      ins <- paste(sN,"<-as.data.frame(matrix(rcbp(",nO*nS,",",b,",",gamma,"), ncol=",nO,"))\n",sep="")
      ins <- paste(ins,"rownames(",sN,") <- paste('sample', 1:", nS,", sep='') \n",sep="")
      ins <- paste(ins,"colnames(",sN,") <- paste('obs', 1:",nO,", sep='') \n",sep="")
      doItAndPrint(ins)
      additional <- ""
      if (tclvalue(meanVariable) == "1") {
         additional <- paste(additional,"mean <- rowMeans(",sN,"[,1:",nO,"])\n",sep="")
      }
      if (tclvalue(sumVariable) == "1") {
         additional <- paste(additional,"sum <- rowSums(",sN,"[,1:",nO,"])\n",sep="")
      }
      if (tclvalue(sdVariable) == "1") {
         additional <- paste(additional,"sd <- apply(",sN,"[,1:",nO,"],1,sd)\n",sep="")
      }
      if (additional!=""){
         doItAndPrint(paste(sN," <- within(",sN,", {\n",additional,"})",sep=""))
      }
      activeDataSet(sN)
      putDialog("cbpSamples", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), 
                                 sName = tclvalue(sNVar), nObse = tclvalue(nOVar),
                                 nSamp = tclvalue(nSVar), sMean = tclvalue(meanVariable),
                                 sSum = tclvalue(sumVariable), sSd=tclvalue(sdVariable)  ) , resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "rcbp", reset = "cbpSamples", apply = "cbpSamples")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = sNEnt) 
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition checkBoxes activeDataSet
#' @importFrom cpd pcbp
#' @export

ctpSamples<-function (){
   checksFrame <- meanVariable <- sumVariable <- sdVariable <- NULL
   initial <- getDialog("ctpSamples",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           sName = "ctpSamples",
                           nObse = 100,
                           nSamp = 1,
                           sMean = "1",
                           sSum = "0",
                           sSd="0"))
   initializeDialog(title = "CTP Distribution")
   frame <- tkframe(top)
   sNVar     <-tclVar(initial$sName)
   sNEnt     <- ttkentry(frame, width = "30",textvariable = sNVar)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   nSVar <-tclVar(initial$nSamp)
   nSEnt <- ttkentry(frame, width = "6",textvariable = nSVar)
   nOVar <-tclVar(initial$nObse)
   nOEnt <- ttkentry(frame, width = "6",textvariable = nOVar)
   checkBoxes(frame = "checksFrame", boxes = c("mean", 
                                               "sum", "sd"), initialValues = c(initial$sMean, 
                                                                               initial$sSum, initial$sSd), labels = gettextRcmdr(c("Sample means", 
                                                                                                                                   "Sample sums", "Sample standard deviations")))
   
   tkgrid(labelRcmdr(frame, text = "Sample name"), sNEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Number of Obs."), nOEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Number of Samples"), nSEnt, sticky = "w", padx = 6)
   tkgrid(frame, sticky = "w")
   tkgrid(checksFrame, sticky = "w")
   tkgrid.configure(sNEnt, sticky = "w")
   tkgrid.configure(aEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   tkgrid.configure(nOEnt, sticky = "w")
   tkgrid.configure(nSEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      sN    <- gsub(" ","", tclvalue(sNVar))
      nO    <- as.numeric(tclvalue(nOVar))
      nS    <- as.numeric(tclvalue(nSVar))
      
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpSamples, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpSamples, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpSamples, message ="gamma not specified" )
         return()
      }
      if (gamma<=2*a){
         errorCondition(recall = ctpSamples, message ="gamma less than 2a" )
         return()
      }
      if (sN==""){
         errorCondition(recall = ctpSamples, message = "no sample name")
         return()
      }
      if (trunc(nO)!=nO || nO < 1){
         errorCondition(recall = ctpSamples, message = "observations not interger greater than 0")
         return()
      }
      if (trunc(nS)!=nS || nS < 1){
         errorCondition(recall = ctpSamples, message = "samples not interger greater than 0")
         return()
      }
      
      ins <- paste(sN,"<-as.data.frame(matrix(rctp(",nO*nS,",",a,",",b,",",gamma,"), ncol=",nO,"))\n",sep="")
      ins <- paste(ins,"rownames(",sN,") <- paste('sample', 1:", nS,", sep='') \n",sep="")
      ins <- paste(ins,"colnames(",sN,") <- paste('obs', 1:",nO,", sep='') \n",sep="")
      doItAndPrint(ins)
      additional <- ""
      if (tclvalue(meanVariable) == "1") {
         additional <- paste(additional,"mean <- rowMeans(",sN,"[,1:",nO,"])\n",sep="")
      }
      if (tclvalue(sumVariable) == "1") {
         additional <- paste(additional,"sum <- rowSums(",sN,"[,1:",nO,"])\n",sep="")
      }
      if (tclvalue(sdVariable) == "1") {
         additional <- paste(additional,"sd <- apply(",sN,"[,1:",nO,"],1,sd)\n",sep="")
      }
      if (additional!=""){
         doItAndPrint(paste(sN," <- within(",sN,", {\n",additional,"})",sep=""))
      }
      
      activeDataSet(sN)
      putDialog("ctpSamples", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), 
                                   sName = tclvalue(sNVar), nObse = tclvalue(nOVar),
                                   nSamp = tclvalue(nSVar), sMean = tclvalue(meanVariable),
                                   sSum = tclvalue(sumVariable), sSd=tclvalue(sdVariable)) , resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "rctp", reset = "ctpSamples", apply = "ctpSamples")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = sNEnt) 
}

#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition checkBoxes activeDataSet radioButtons tkconfigure
#' @importFrom cpd pebw
#' @export

ebwSamples<-function (){
  checksFrame <- meanVariable <- sumVariable <- sdVariable <- typeEBWFrame <- typeEBWVariable <-NULL
  initial <- getDialog("ebwSamples",
                       defaults = list(
                         initialValues=c(0.5,1,3.5),
                         sName = "ebwSamples",
                         nObse = 100,
                         nSamp = 1,
                         sMean = "1",
                         sSum = "0",
                         sSd="0"))
  initializeDialog(title = "EBW Distribution")
  frame <- tkframe(top)
  
  radioButtons(frame, name="typeEBW",
               buttons=c("alphaNegative", "alphaPositive"),
               labels=gettextRcmdr(c("alpha < 0 and gamma > 0 ",
                                     "alpha > 0 and rho > 0")),
               initialValue="alphaPositive",
               title=gettextRcmdr("Parametrization"),
               command=function(){
                  if ( tclvalue(typeEBWVariable) == "alphaPositive"){
                    tkconfigure(gammaEnt, state = "disabled")
                    tkconfigure(rhoEnt, state = "enabled")
                  }else{
                    tkconfigure(gammaEnt, state = "enabled")
                    tkconfigure(rhoEnt, state = "disabled")
                  }
               })
  
  sNVar     <-tclVar(initial$sName)
  sNEnt     <- ttkentry(frame, width = "30",textvariable = sNVar)
  alphaVar     <-tclVar(initial$initialValues[1])
  alphaEnt     <- ttkentry(frame, width = "6",textvariable = alphaVar)
  rhoVar     <-tclVar(initial$initialValues[2])
  rhoEnt     <- ttkentry(frame, width = "6",textvariable = rhoVar)
  gammaVar <-tclVar(initial$initialValues[3])
  gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
  nSVar <-tclVar(initial$nSamp)
  nSEnt <- ttkentry(frame, width = "6",textvariable = nSVar)
  nOVar <-tclVar(initial$nObse)
  nOEnt <- ttkentry(frame, width = "6",textvariable = nOVar)
  checkBoxes(frame = "checksFrame", boxes = c("mean", 
                                              "sum", "sd"), initialValues = c(initial$sMean, 
                                                                              initial$sSum, initial$sSd), labels = gettextRcmdr(c("Sample means", 
                                                                                                                                  "Sample sums", "Sample standard deviations")))
  tkgrid(labelRcmdr(frame, text = "Sample name"), sNEnt, sticky = "w", padx = 6)
  tkgrid(typeEBWFrame, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "alpha"), alphaEnt, sticky = "w", padx = 30)
  tkgrid(labelRcmdr(frame, text = "rho"), rhoEnt, sticky = "w", padx = 30)
  tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 30)
  tkconfigure(gammaEnt, state = "disabled")
  tkgrid(labelRcmdr(frame, text = "Number of Obs."), nOEnt, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "Number of Samples"), nSEnt, sticky = "w", padx = 6)
  tkgrid(frame, sticky = "w")
  tkgrid(checksFrame, sticky = "w")
  tkgrid.configure(sNEnt, sticky = "w")
  tkgrid.configure(alphaEnt, sticky = "w")
  tkgrid.configure(rhoEnt, sticky = "w")
  tkgrid.configure(gammaEnt, sticky = "w")
  tkgrid.configure(nOEnt, sticky = "w")
  tkgrid.configure(nSEnt, sticky = "w")
  
  onOK <- function(){
    closeDialog()
    
    alpha     <- as.numeric(tclvalue(alphaVar))
    rho     <- as.numeric(tclvalue(rhoVar))
    gamma <- as.numeric(tclvalue(gammaVar))
    sN    <- gsub(" ","", tclvalue(sNVar))
    nO    <- as.numeric(tclvalue(nOVar))
    nS    <- as.numeric(tclvalue(nSVar))
    
    #validattions
    if (is.na(alpha)){
      errorCondition(recall = ebwSamples, message ="alpha not specified" )
      return()
    }
    
    if (alpha > 0){
      if (is.na(rho)){
        errorCondition(recall = ebwSamples, message ="rho not specified" )
        return()
      }
      if (rho < 0){
        errorCondition(recall = ebwSamples, message ="rho must be positive" )
        return()
      }
    }
    else{
      if (is.na(gamma)){
        errorCondition(recall = ebwSamples, message ="gamma not specified" )
        return()
      }
      if (gamma < 0){
        errorCondition(recall = ebwSamples, message ="gamma must be positive" )
        return()
      }
    }
    if (sN==""){
      errorCondition(recall = ebwSamples, message = "no sample name")
      return()
    }
    if (trunc(nO)!=nO || nO < 1){
      errorCondition(recall = ebwSamples, message = "observations not interger greater than 0")
      return()
    }
    if (trunc(nS)!=nS || nS < 1){
      errorCondition(recall = ebwSamples, message = "samples not interger greater than 0")
      return()
    }
      
    if (alpha < 0)
      ins <- paste(sN,"<-as.data.frame(matrix(rebw(",nO*nS,",alpha=",alpha,",gamma=",gamma,"), ncol=",nO,"))\n",sep="")
    else
      ins <- paste(sN,"<-as.data.frame(matrix(rebw(",nO*nS,",alpha=",alpha,",rho=",rho,"), ncol=",nO,"))\n",sep="")
    
    ins <- paste(ins,"rownames(",sN,") <- paste('sample', 1:", nS,", sep='') \n",sep="")
    ins <- paste(ins,"colnames(",sN,") <- paste('obs', 1:",nO,", sep='') \n",sep="")
    doItAndPrint(ins)
    additional <- ""
    if (tclvalue(meanVariable) == "1") {
      additional <- paste(additional,"mean <- rowMeans(",sN,"[,1:",nO,"])\n",sep="")
    }
    if (tclvalue(sumVariable) == "1") {
      additional <- paste(additional,"sum <- rowSums(",sN,"[,1:",nO,"])\n",sep="")
    }
    if (tclvalue(sdVariable) == "1") {
      additional <- paste(additional,"sd <- apply(",sN,"[,1:",nO,"],1,sd)\n",sep="")
    }
    if (additional!=""){
      doItAndPrint(paste(sN," <- within(",sN,", {\n",additional,"})",sep=""))
    }
    
    activeDataSet(sN)
    putDialog("ctpSamples", list(initialValues = c( tclvalue(alphaVar), tclvalue(rhoVar), tclvalue(gammaVar)), 
                                 sName = tclvalue(sNVar), nObse = tclvalue(nOVar),
                                 nSamp = tclvalue(nSVar), sMean = tclvalue(meanVariable),
                                 sSum = tclvalue(sumVariable), sSd=tclvalue(sdVariable)) , resettable = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "rebw", reset = "ebwSamples", apply = "ebwSamples")
  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(focus = sNEnt) 
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition tclVar ttkradiobutton closeDialog tclvalue ttkradiobutton closeDialog
#' @importFrom cpd pcbp
#' @export

cbpPlot<-function (){
   initial <- getDialog("cbpPlot",
                        defaults = list(
                           initialValues=c(1,4),
                           typePlot = "mass"))
   initializeDialog(title = "CBP Distribution")
   frame <- tkframe(top)
   bVar     <-tclVar(initial$initialValues[1])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[2])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   typeVar  <- tclVar(initial$typePlot)
   buttonFrame <- tkframe(top)
   massBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "mass")
   distBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "distribution")
   
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(massBut, labelRcmdr(buttonFrame, text = "Probability mass"), sticky = "w")
   tkgrid(distBut, labelRcmdr(buttonFrame, text = "Distribution function"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      typePlot<- tclvalue(typeVar)
      
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpPlot, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpPlot, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpPlot, message ="gamma not positive" )
         return()
      }
      #action
      er=1e-4
      firstValue=qcbp(er/2,b,gamma)
      lastValue=qcbp(er/2,b,gamma,lower.tail = FALSE)
      if (typePlot=="mass"){
         ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
         ins=paste(ins, "plotDistr(x,dcbp(x, ",b,", ",gamma, "), xlab='x', \n", sep = "")
         ins=paste(ins, "ylab='Probability Mass', main='CBP distribution b=",b," gamma=",gamma,"',\n", sep = "")
         ins=paste(ins, "discrete=TRUE)})",sep="")
      }else{
         ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
         ins=paste(ins, "plotDistr(x,pcbp(x, ",b,", ",gamma, "), xlab='x', \n", sep = "")
         ins=paste(ins, "ylab='Cumulative Probability', main='CBP distribution b=",b," gamma=",gamma,"',\n", sep = "")
         ins=paste(ins, "discrete=TRUE, cdf=TRUE)})",sep="")
      }
      
      doItAndPrint(ins)
      putDialog("cbpPlot", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), 
                                        typePlot = tclvalue(typeVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "plotDistr", reset = "cbpPlot", apply = "cbpPlot")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = bEnt)
}






#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pcbp dctp
#' @export

ctpPlot<-function (){
   initial <- getDialog("ctpPlot",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           typePlot = "mass"))
   initializeDialog(title = "CTP Distribution")
   frame <- tkframe(top)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   typeVar  <- tclVar(initial$typePlot)
   buttonFrame <- tkframe(top)
   massBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "mass")
   distBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "distribution")
   
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(massBut, labelRcmdr(buttonFrame, text = "Probability mass"), sticky = "w")
   tkgrid(distBut, labelRcmdr(buttonFrame, text = "Distribution function"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(aEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      typePlot<- tclvalue(typeVar)
      
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpPlot, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpPlot, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpPlot, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpPlot, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a){
         errorCondition(recall = ctpMass, message ="gamma not greater than 2a" )
         return()
      }
      #action
      er=1e-4
      firstValue=qctp(er/2,a,b,gamma)
      lastValue=qctp(er/2,a,b,gamma,lower.tail = FALSE)
      if (typePlot=="mass"){
         ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
         ins=paste(ins, "plotDistr(x,dctp(x, ",a,", ",b,", ",gamma, "), xlab='x', \n", sep = "")
         ins=paste(ins, "ylab='Probability Mass', main='CTP distribution a=",a," b=",b," gamma=",gamma,"',\n", sep = "")
         ins=paste(ins, "discrete=TRUE)})",sep="")
      }else{
         ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
         ins=paste(ins, "plotDistr(x,pctp(x, ",a,", ",b,", ",gamma, "), xlab='x', \n", sep = "")
         ins=paste(ins, "ylab='Cumulative Probability', main='CTP distribution a=",a," b=",b," gamma=",gamma,"',\n", sep = "")
         ins=paste(ins, "discrete=TRUE, cdf=TRUE)})",sep="")
      }
      
      doItAndPrint(ins)
      putDialog("cbpPlot", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), 
                                typePlot = tclvalue(typeVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "plotDistr", reset = "ctpPlot", apply = "ctpPlot")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = aEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition checkBoxes activeDataSet
#' @importFrom cpd pebw debw
#' @export

ebwPlot<-function (){
  typeEBWFrame <- typeEBWVariable <-NULL
  initial <- getDialog("ebwPlot",
                       defaults = list(
                         initialValues=c(0.5,1,3.5),
                         typePlot="mass"))
  initializeDialog(title = "EBW Distribution")
  frame <- tkframe(top)

  radioButtons(frame, name="typeEBW",
               buttons=c("alphaNegative", "alphaPositive"),
               labels=gettextRcmdr(c("alpha < 0 and gamma > 0 ",
                                     "alpha > 0 and rho > 0")),
               initialValue="alphaPositive",
               title=gettextRcmdr("Parametrization"),
               command=function(){
                 if ( tclvalue(typeEBWVariable) == "alphaPositive"){
                   tkconfigure(gammaEnt, state = "disabled")
                   tkconfigure(rhoEnt, state = "enabled")
                 }else{
                   tkconfigure(gammaEnt, state = "enabled")
                   tkconfigure(rhoEnt, state = "disabled")
                 }
               })

  alphaVar     <-tclVar(initial$initialValues[1])
  alphaEnt     <- ttkentry(frame, width = "6",textvariable = alphaVar)
  rhoVar     <-tclVar(initial$initialValues[2])
  rhoEnt     <- ttkentry(frame, width = "6",textvariable = rhoVar)
  gammaVar <-tclVar(initial$initialValues[3])
  gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
  typeVar  <- tclVar(initial$typePlot)
  buttonFrame <- tkframe(top)
  massBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "mass")
  distBut <- ttkradiobutton(buttonFrame, variable = typeVar, value = "distribution")
  
  tkgrid(typeEBWFrame, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "alpha"), alphaEnt, sticky = "w", padx = 30)
  tkgrid(labelRcmdr(frame, text = "rho"), rhoEnt, sticky = "w", padx = 30)
  tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 30)
  tkconfigure(gammaEnt, state = "disabled")
  tkgrid(massBut, labelRcmdr(buttonFrame, text = "Probability mass"), sticky = "w")
  tkgrid(distBut, labelRcmdr(buttonFrame, text = "Distribution function"), sticky = "w")
  tkgrid(frame, sticky = "w")
  tkgrid(buttonFrame, sticky = "w")
  tkgrid.configure(alphaEnt, sticky = "w")
  tkgrid.configure(rhoEnt, sticky = "w")
  tkgrid.configure(gammaEnt, sticky = "w")


  onOK <- function(){
    closeDialog()
    
    alpha     <- as.numeric(tclvalue(alphaVar))
    rho     <- as.numeric(tclvalue(rhoVar))
    gamma <- as.numeric(tclvalue(gammaVar))
    typePlot<- tclvalue(typeVar)
    
    #validattions
    if (is.na(alpha)){
      errorCondition(recall = ebwPlot, message ="alpha not specified" )
      return()
    }

    if (alpha > 0){
      if (is.na(rho)){
        errorCondition(recall = ebwPlot, message ="rho not specified" )
        return()
      }
      if (rho < 0){
        errorCondition(recall = ebwPlot, message ="rho must be positive" )
        return()
      }
    }
    else{
      if (is.na(gamma)){
        errorCondition(recall = ebwPlot, message ="gamma not specified" )
        return()
      }
      if (gamma < 0){
        errorCondition(recall = ebwPlot, message ="gamma must be positive" )
        return()
      }
    }

    er=1e-4
    if (alpha < 0){
      firstValue=qebw(er/2,alpha=alpha,gamma=gamma)
      lastValue=qebw(er/2,alpha=alpha,gamma=gamma,lower.tail = FALSE)
      if (typePlot=="mass"){
        ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
        ins=paste(ins, "plotDistr(x,debw(x, alpha=",alpha,", gamma=" ,gamma, "), xlab='x', \n", sep = "")
        ins=paste(ins, "ylab='Probability Mass', main='EBW distribution alpha=",alpha," gamma=",gamma,"',\n", sep = "")

      }else{
        ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
        ins=paste(ins, "plotDistr(x,pebw(x, alpha=",alpha,", gamma=" ,gamma, "), xlab='x', \n", sep = "")
        ins=paste(ins, "ylab='Cumulative Probability', main='EBW distribution alpha=",alpha," gamma=",gamma,"',\n", sep = "")
      }
    }else{
      firstValue=qebw(er/2,alpha=alpha,rho = rho)
      lastValue=qebw(er/2,alpha=alpha,rho = rho,lower.tail = FALSE)
      if (typePlot=="mass"){
        ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
        ins=paste(ins, "plotDistr(x,debw(x, alpha=",alpha,", rho=" ,rho, "), xlab='x', \n", sep = "")
        ins=paste(ins, "ylab='Probability Mass', main='EBW distribution alpha=",alpha," rho=",rho,"',\n", sep = "")
        
      }else{
        ins=paste("local({x <- ",firstValue,":",lastValue, "\n", sep = "")
        ins=paste(ins, "plotDistr(x,pebw(x, alpha=",alpha,", rho=" ,rho, "), xlab='x', \n", sep = "")
        ins=paste(ins, "ylab='Cumulative Probability', main='EBW distribution alpha=",alpha," rho=",rho,"',\n", sep = "")
      }
    }
    ins=paste(ins, "discrete=TRUE)})",sep="")
    doItAndPrint(ins)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "plotDistr", reset = "ebwPlot", apply = "ebwPlot")
  tkgrid(buttonsFrame, sticky = "ew")
  putDialog("ebwPlot", list(initialValues = c( tclvalue(alphaVar), tclvalue(rhoVar), tclvalue(gammaVar)), 
                            typePlot = tclvalue(typeVar)), resettable = FALSE)
  
  dialogSuffix(focus = alphaEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition getDialog initializeDialog tkframe ttkentry tkgrid labelRcmdr tkgrid.configure doItAndPrint putDialog tkfocus CommanderWindow OKCancelHelp dialogSuffix tclVar closeDialog tclvalue gettextRcmdr 
#' @importFrom cpd pcbp
#' @export

cbpMass<-function (){
   initial <- getDialog("cbpMass",
                        defaults = list(
                           initialValues=c(1,4),
                           error = 1e-4))
   initializeDialog(title = "CBP Mass")
   frame <- tkframe(top)
   bVar     <-tclVar(initial$initialValues[1])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[2])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   eVar     <- tclVar(initial$error)
   eEnt     <- ttkentry(frame, width = "30",textvariable = eVar)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Tail probability"), eEnt, sticky = "w", padx = 6)
   tkgrid(frame, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   tkgrid.configure(eEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      er     <- as.numeric(tclvalue(eVar))
      
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpMass, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpMass, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpMass, message ="gamma not positive" )
         return()
      }
      if ( er >= 1 || er <= 0){
        errorCondition(recall = ctpMass, message = gettextRcmdr("Error must be greater than 0 and less than 1."))
        return()
      }
      #action
      firstValue=qcbp(er/2,b,gamma)
      lastValue=qcbp(er/2,b,gamma,lower.tail = FALSE)
      ins=paste("local({data <- data.frame(Probability=dcbp(",firstValue,":",lastValue,", ",b,", ",gamma,")) \n", sep = "")
      ins=paste(ins, "rownames(data) <- ",firstValue,":",lastValue,"\n", sep = "")
      ins=paste(ins, "print(data)})", sep = "")
      
      doItAndPrint(ins)
      putDialog("cbpMass", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), 
                                     error = tclvalue(eVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "dcbp", reset = "cbpMass", apply = "cbpMass")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = bEnt)


}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pcbp
#' @export

ctpMass<-function (){
   initial <- getDialog("ctpMass",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           error = 1e-4))
   initializeDialog(title = "CTP Mass")
   frame <- tkframe(top)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   eVar     <- tclVar(initial$error)
   eEnt     <- ttkentry(frame, width = "30",textvariable = eVar)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "Error"), eEnt, sticky = "w", padx = 6)
   tkgrid(frame, sticky = "w")
   tkgrid.configure(aEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")
   tkgrid.configure(eEnt, sticky = "w")
   
   onOK <- function(){
      closeDialog()
      
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      er     <- as.numeric(tclvalue(eVar))
      
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpMass, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpMass, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpMass, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpMass, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a){
         errorCondition(recall = ctpMass, message ="gamma not greater than 2a" )
         return()
      }
      if ( er >= 1 || er <= 0){
        errorCondition(recall = ctpMass, message = gettextRcmdr("Error must be greater than 0 and less than 1."))
        return()
      }
      #action
      firstValue=qctp(er,a,b,gamma)
      lastValue=qctp(er,a,b,gamma,lower.tail = FALSE)
      ins=paste("local({data <- data.frame(Probability=dctp(",firstValue,":",lastValue,", ",a,", ",b,", ",gamma,")) \n", sep = "")
      ins=paste(ins, "rownames(data) <- ",firstValue,":",lastValue,"\n", sep = "")
      ins=paste(ins, "print(data)})", sep = "")
      print(ins)
      doItAndPrint(ins)
      putDialog("ctpMass", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), 
                                error = tclvalue(eVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "dctp", reset = "ctpMass", apply = "ctpMass")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = aEnt)
   
   
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pebw
#' @export

ebwMass<-function (){
  typeEBWFrame <- typeEBWVariable <-NULL
  initial <- getDialog("ebwMass",
                       defaults = list(
                         initialValues=c(0.5,1,3.5),
                         error = 1e-4))
  initializeDialog(title = "EBW Mass")
  frame <- tkframe(top)
  radioButtons(frame, name="typeEBW",
               buttons=c("alphaNegative", "alphaPositive"),
               labels=gettextRcmdr(c("alpha < 0 and gamma > 0 ",
                                     "alpha > 0 and rho > 0")),
               initialValue="alphaPositive",
               title=gettextRcmdr("Parametrization"),
               command=function(){
                 if ( tclvalue(typeEBWVariable) == "alphaPositive"){
                   tkconfigure(gammaEnt, state = "disabled")
                   tkconfigure(rhoEnt, state = "enabled")
                 }else{
                   tkconfigure(gammaEnt, state = "enabled")
                   tkconfigure(rhoEnt, state = "disabled")
                 }
               })
  
  alphaVar     <-tclVar(initial$initialValues[1])
  alphaEnt     <- ttkentry(frame, width = "10",textvariable = alphaVar)
  rhoVar     <-tclVar(initial$initialValues[2])
  rhoEnt     <- ttkentry(frame, width = "10",textvariable = rhoVar)
  gammaVar <-tclVar(initial$initialValues[3])
  gammaEnt <- ttkentry(frame, width = "10",textvariable = gammaVar)
  eVar     <- tclVar(initial$error)
  eEnt     <- ttkentry(frame, width = "10",textvariable = eVar)
  tkgrid(typeEBWFrame, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "alpha"), alphaEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "rho"), rhoEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "Error"), eEnt, sticky = "w", padx = 10)
  tkconfigure(gammaEnt, state = "disabled")

  tkgrid(frame, sticky = "w")
  tkgrid.configure(alphaEnt, sticky = "w")
  tkgrid.configure(rhoEnt, sticky = "w")
  tkgrid.configure(gammaEnt, sticky = "w")
  tkgrid.configure(eEnt, sticky = "w")
  
  onOK <- function(){
    closeDialog()
    
    alpha     <- as.numeric(tclvalue(alphaVar))
    rho     <- as.numeric(tclvalue(rhoVar))
    gamma <- as.numeric(tclvalue(gammaVar))
    er     <- as.numeric(tclvalue(eVar))
    
    #validattions
    if (is.na(alpha)){
      errorCondition(recall = ebwPlot, message ="alpha not specified" )
      return()
    }
    
    if (alpha > 0){
      if (is.na(rho)){
        errorCondition(recall = ebwPlot, message ="rho not specified" )
        return()
      }
      if (rho < 0){
        errorCondition(recall = ebwPlot, message ="rho must be positive" )
        return()
      }
    }
    else{
      if (is.na(gamma)){
        errorCondition(recall = ebwPlot, message ="gamma not specified" )
        return()
      }
      if (gamma < 0){
        errorCondition(recall = ebwPlot, message ="gamma must be positive" )
        return()
      }
    }
    if ( er >= 1 || er <= 0){
      errorCondition(recall = ctpMass, message = gettextRcmdr("Error must be greater than 0 and less than 1."))
      return()
    }
    #action
    if (alpha < 0){
      firstValue=qebw(er/2,alpha=alpha,gamma=gamma)
      lastValue=qebw(er/2,alpha=alpha,gamma=gamma,lower.tail = FALSE)
      ins=paste("local({data <- data.frame(Probability=debw(",firstValue,":",lastValue,", alpha= ",alpha, ", gamma=",gamma,")) \n", sep = "")
    }
    else{
      firstValue=qebw(er/2,alpha=alpha,rho = rho)
      lastValue=qebw(er/2,alpha=alpha,rho = rho,lower.tail = FALSE)
      ins=paste("local({data <- data.frame(Probability=debw(",firstValue,":",lastValue,", alpha= ",alpha, ", rho=",rho,")) \n", sep = "")
      
    }

    ins=paste(ins, "rownames(data) <- ",firstValue,":",lastValue,"\n", sep = "")
    ins=paste(ins, "print(data)})", sep = "")
    print(ins)
    doItAndPrint(ins)
    putDialog("ebwMass", list(initialValues = c( tclvalue(alphaVar), tclvalue(rhoVar), tclvalue(gammaVar)), 
                              error = tclvalue(eVar)), resettable = FALSE)
    
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "debw", reset = "ebwMass", apply = "ebwMass")
  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(focus = alphaEnt)
  
  
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pcbp
#' @export

cbpProbabilities<-function (){
   initial <- getDialog("cbpProbabilities",
                        defaults = list(
                           initialValues=c(1,4),
                           tail = "lower",
                           values = ""))
   initializeDialog(title = "CBP Probabilities")
   frame <- tkframe(top)
   vVar     <- tclVar(initial$values)
   vEnt     <- ttkentry(frame, width = "30",textvariable = vVar)
   bVar     <-tclVar(initial$initialValues[1])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[2])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Variable value(s)"), vEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(vEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      values <- gsub(",+", ",", gsub(" ",",", tclvalue(vVar)))
      if (values==""){
         errorCondition(recall = cbpProbabilities, message = gettextRcmdr("No values specified."))
         return()
      }
      
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      options()
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpProbabilities, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpProbabilities, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpProbabilities, message ="gamma not positive" )
         return()
      }
      #action
      doItAndPrint(paste("pcbp(c(",values, "), ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("cbpProbabilities", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     values = tclvalue(vVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qcbp", reset = "cbpProbabilities", apply = "cbpProbabilities")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = vEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pctp
#' @export

ctpProbabilities<-function (){
   initial <- getDialog("ctpProbabilities",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           tail = "lower",
                           values = ""))
   initializeDialog(title = "CTP Probabilities")
   frame <- tkframe(top)
   vVar     <- tclVar(initial$values)
   vEnt     <- ttkentry(frame, width = "30",textvariable = vVar)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Probabilities"), vEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(vEnt, sticky = "w")
   tkgrid.configure(aEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      values <- gsub(",+", ",", gsub(" ",",", tclvalue(vVar)))
      if (values==""){
         errorCondition(recall = ctpProbabilities, message = gettextRcmdr("No values specified."))
         return()
      }
      
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpProbabilities, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpProbabilities, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpProbabilities, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpProbabilities, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a){
         errorCondition(recall = ctpProbabilities, message ="gamma not greater than 2a" )
         return()
      }
      #action
      doItAndPrint(paste("pctp(c(",values, "), ", a,", ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("ctpProbabilities", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     values = tclvalue(vVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qctp", reset = "ctpProbabilities", apply = "ctpProbabilities")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = vEnt)
}

#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd pebw
#' @export

ebwProbabilities<-function (){
  typeEBWFrame <- typeEBWVariable <-NULL
  initial <- getDialog("ebwProbabilities",
                       defaults = list(
                         initialValues=c(0.5,1,3.5),
                         tail = "lower",
                         values = ""))
  initializeDialog(title = "EBW Probabilities")
  frame <- tkframe(top)
  vVar     <- tclVar(initial$values)
  vEnt     <- ttkentry(frame, width = "30",textvariable = vVar)
  radioButtons(frame, name="typeEBW",
               buttons=c("alphaNegative", "alphaPositive"),
               labels=gettextRcmdr(c("alpha < 0 and gamma > 0 ",
                                     "alpha > 0 and rho > 0")),
               initialValue="alphaPositive",
               title=gettextRcmdr("Parametrization"),
               command=function(){
                 if ( tclvalue(typeEBWVariable) == "alphaPositive"){
                   tkconfigure(gammaEnt, state = "disabled")
                   tkconfigure(rhoEnt, state = "enabled")
                 }else{
                   tkconfigure(gammaEnt, state = "enabled")
                   tkconfigure(rhoEnt, state = "disabled")
                 }
               })
  
  alphaVar     <-tclVar(initial$initialValues[1])
  alphaEnt     <- ttkentry(frame, width = "10",textvariable = alphaVar)
  rhoVar     <-tclVar(initial$initialValues[2])
  rhoEnt     <- ttkentry(frame, width = "10",textvariable = rhoVar)
  gammaVar <-tclVar(initial$initialValues[3])
  gammaEnt <- ttkentry(frame, width = "10",textvariable = gammaVar)
  tailVar  <- tclVar(initial$tail)
  buttonFrame <- tkframe(top)
  lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
  upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")
  
  tkgrid(labelRcmdr(frame, text = "Probabilities"), vEnt, sticky = "w", padx = 6)
  tkgrid(typeEBWFrame, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "alpha"), alphaEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "rho"), rhoEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 10)
  tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
  tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
  tkconfigure(gammaEnt, state = "disabled")
  tkgrid(frame, sticky = "w")
  tkgrid(buttonFrame, sticky = "w")
  tkgrid.configure(vEnt, sticky = "w")
  tkgrid.configure(alphaEnt, sticky = "w")
  tkgrid.configure(rhoEnt, sticky = "w")
  tkgrid.configure(gammaEnt, sticky = "w")
  
  onOK <- function(){
    closeDialog()
    values <- gsub(",+", ",", gsub(" ",",", tclvalue(vVar)))
    if (values==""){
      errorCondition(recall = ctpProbabilities, message = gettextRcmdr("No values specified."))
      return()
    }
    
    alpha     <- as.numeric(tclvalue(alphaVar))
    rho     <- as.numeric(tclvalue(rhoVar))
    gamma <- as.numeric(tclvalue(gammaVar))
    tail  <- tclvalue(tailVar)
    
    #validattions
    if (is.na(alpha)){
      errorCondition(recall = ebwPlot, message ="alpha not specified" )
      return()
    }
    
    if (alpha > 0){
      if (is.na(rho)){
        errorCondition(recall = ebwPlot, message ="rho not specified" )
        return()
      }
      if (rho < 0){
        errorCondition(recall = ebwPlot, message ="rho must be positive" )
        return()
      }
    }
    else{
      if (is.na(gamma)){
        errorCondition(recall = ebwPlot, message ="gamma not specified" )
        return()
      }
      if (gamma < 0){
        errorCondition(recall = ebwPlot, message ="gamma must be positive" )
        return()
      }
    }
    #action
    if (alpha < 0){
      doItAndPrint(paste("pebw(c(",values, "), alpha=", alpha,", gamma= ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
    }
    else{
      doItAndPrint(paste("pebw(c(",values, "), alpha=", alpha,", rho= ",rho, ", lower.tail=", tail == "lower", ")", sep = ""))
    }
      
    putDialog("ebwProbabilities", list(initialValues = c( tclvalue(alphaVar), tclvalue(rhoVar), tclvalue(gammaVar)),tail = tclvalue(tailVar),
                                       values = tclvalue(vVar)), resettable = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "qebw", reset = "ebwProbabilities", apply = "ebwProbabilities")
  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(focus = vEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd qcbp
#' @export

cbpQuantiles<-function (){
   initial <- getDialog("cbpQuantiles",
                  defaults = list(
                     initialValues=c(1,4),
                     tail = "lower",
                     quantiles = ""))
   initializeDialog(title = "CBP Quantiles")
   frame <- tkframe(top)
      qVar     <- tclVar(initial$quantiles)
      qEnt     <- ttkentry(frame, width = "30",textvariable = qVar)
      bVar     <-tclVar(initial$initialValues[1])
      bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
      gammaVar <-tclVar(initial$initialValues[2])
      gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
      tailVar  <- tclVar(initial$tail)
      buttonFrame <- tkframe(top)
         lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
         upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

      tkgrid(labelRcmdr(frame, text = "Probabilities"), qEnt, sticky = "w", padx = 6)
      tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
      tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
      tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
      tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
      tkgrid(frame, sticky = "w")
      tkgrid(buttonFrame, sticky = "w")
      tkgrid.configure(qEnt, sticky = "w")
      tkgrid.configure(bEnt, sticky = "w")
      tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      quantiles <- gsub(",+", ",", gsub(" ",",", tclvalue(qVar)))
      if (quantiles==""){
          errorCondition(recall = cbpQuantiles, message = gettextRcmdr("No probabilities specified."))
          return()
      }
      
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpQuantiles, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpQuantiles, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpQuantiles, message ="gamma not positive" )
         return()
      }
      #action
      doItAndPrint(paste("qcbp(c(",quantiles, "), ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("cbpQuantiles", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                            quantiles = tclvalue(qVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qcbp", reset = "cbpQuantiles", apply = "cbpQuantiles")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = qEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd qctp
#' @export

ctpQuantiles<-function (){
   initial <- getDialog("ctpQuantiles",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           tail = "lower",
                           quantiles = ""))
   initializeDialog(title = "CTP Quantiles")
   frame <- tkframe(top)
   qVar     <- tclVar(initial$quantiles)
   qEnt     <- ttkentry(frame, width = "30",textvariable = qVar)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Probabilities"), qEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(qEnt, sticky = "w")
   tkgrid.configure(aEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      quantiles <- gsub(",+", ",", gsub(" ",",", tclvalue(qVar)))
      if (quantiles==""){
         errorCondition(recall = ctpQuantiles, message = gettextRcmdr("No probabilities specified."))
         return()
      }
      
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpQuantiles, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpQuantiles, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpQuantiles, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpQuantiles, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a){
         errorCondition(recall = ctpQuantiles, message ="gamma not greater than 2a" )
         return()
      }
      #action
      doItAndPrint(paste("qctp(c(",quantiles, "), ", a,", ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("ctpQuantiles", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     quantiles = tclvalue(qVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qctp", reset = "ctpQuantiles", apply = "ctpQuantiles")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = qEnt)
}

#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl errorCondition
#' @importFrom cpd qebw
#' @export

ebwQuantiles<-function (){
  typeEBWFrame <- typeEBWVariable <-NULL
  initial <- getDialog("ebwQuantiles",
                       defaults = list(
                         initialValues=c(0.5,1,3.5),
                         tail = "lower",
                         quantiles = ""))
  initializeDialog(title = "EBW Quantiles")
  frame <- tkframe(top)
  qVar     <- tclVar(initial$quantiles)
  qEnt     <- ttkentry(frame, width = "30",textvariable = qVar)
  radioButtons(frame, name="typeEBW",
               buttons=c("alphaNegative", "alphaPositive"),
               labels=gettextRcmdr(c("alpha < 0 and gamma > 0 ",
                                     "alpha > 0 and rho > 0")),
               initialValue="alphaPositive",
               title=gettextRcmdr("Parametrization"),
               command=function(){
                 if ( tclvalue(typeEBWVariable) == "alphaPositive"){
                   tkconfigure(gammaEnt, state = "disabled")
                   tkconfigure(rhoEnt, state = "enabled")
                 }else{
                   tkconfigure(gammaEnt, state = "enabled")
                   tkconfigure(rhoEnt, state = "disabled")
                 }
               })
  
  alphaVar     <-tclVar(initial$initialValues[1])
  alphaEnt     <- ttkentry(frame, width = "10",textvariable = alphaVar)
  rhoVar     <-tclVar(initial$initialValues[2])
  rhoEnt     <- ttkentry(frame, width = "10",textvariable = rhoVar)
  gammaVar <-tclVar(initial$initialValues[3])
  gammaEnt <- ttkentry(frame, width = "10",textvariable = gammaVar)
  tailVar  <- tclVar(initial$tail)
  buttonFrame <- tkframe(top)
  lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
  upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")
  
  tkgrid(labelRcmdr(frame, text = "Probabilities"), qEnt, sticky = "w", padx = 6)
  tkgrid(typeEBWFrame, sticky = "w", padx = 6)
  tkgrid(labelRcmdr(frame, text = "alpha"), alphaEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "rho"), rhoEnt, sticky = "w", padx = 10)
  tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 10)
  tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
  tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
  tkconfigure(gammaEnt, state = "disabled")
  
  tkgrid(frame, sticky = "w")
  tkgrid(buttonFrame, sticky = "w")
  tkgrid.configure(qEnt, sticky = "w")
  tkgrid.configure(alphaEnt, sticky = "w")
  tkgrid.configure(rhoEnt, sticky = "w")
  tkgrid.configure(gammaEnt, sticky = "w")
  
  onOK <- function(){
    closeDialog()
    quantiles <- gsub(",+", ",", gsub(" ",",", tclvalue(qVar)))
    if (quantiles==""){
      errorCondition(recall = ctpQuantiles, message = gettextRcmdr("No probabilities specified."))
      return()
    }
    
    alpha     <- as.numeric(tclvalue(alphaVar))
    rho     <- as.numeric(tclvalue(rhoVar))
    gamma <- as.numeric(tclvalue(gammaVar))
    tail  <- tclvalue(tailVar)
    
    #validattions
    if (is.na(alpha)){
      errorCondition(recall = ebwPlot, message ="alpha not specified" )
      return()
    }
    
    if (alpha > 0){
      if (is.na(rho)){
        errorCondition(recall = ebwPlot, message ="rho not specified" )
        return()
      }
      if (rho < 0){
        errorCondition(recall = ebwPlot, message ="rho must be positive" )
        return()
      }
    }
    else{
      if (is.na(gamma)){
        errorCondition(recall = ebwPlot, message ="gamma not specified" )
        return()
      }
      if (gamma < 0){
        errorCondition(recall = ebwPlot, message ="gamma must be positive" )
        return()
      }
    }
    #action
    if (alpha < 0){
      doItAndPrint(paste("qebw(c(",quantiles, "), alpha= ", alpha,", gamma= ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
    }
    else{
      doItAndPrint(paste("qebw(c(",quantiles, "), alpha= ", alpha,", rho= ",rho, ", lower.tail=", tail == "lower", ")", sep = ""))
    }
    putDialog("ebwQuantiles", list(initialValues = c( tclvalue(alphaVar), tclvalue(rhoVar), tclvalue(gammaVar)),tail = tclvalue(tailVar),
                                   quantiles = tclvalue(qVar)), resettable = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "qebw", reset = "ebwQuantiles", apply = "ebwQuantiles")
  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(focus = qEnt)
}

