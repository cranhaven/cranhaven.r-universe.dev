#' aMNLFA simultaneous model fitting function
#'
#' This function generates the simultaneous aMNLFA model from all the initial inputs.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @return A list (entitled summary) with the following elements:
#' \itemize{
#' \item {indicators} {a list of indicators as specified by the user in the aMNLFA.object()}
#' \item {measinvar} {a list of measurement invariance variables as specified by the user in the aMNLFA.object()}
#' \item {meanimpact} {parameter values, standard errors, test statistics, and p. values for all mean impact effects tested in the simultaneous model}
#' \item {varimpact} {parameter values, standard errors, test statistics, and p. values for all variance impact effects tested in the simultaneous model}
#' \item {loadingDIF} {parameter values, standard errors, test statistics, and p. values for all loading DIF effects tested in the simultaneous model. Also includes critical values for different corrections according to the number of tests, \emph{m}: Benjamini-Hochberg or Bonferroni with \emph{m} defined as the actual number of tests included in the model (BH.actual and bon.actual, respectively); Benjamini-Hochberg or Bonferroni with \emph{m} defined as the number of items times the number of covariates (BH.ibc and bon.ibc, respectively).}  
#' \item {interceptDIF} {If thresholds = FALSE in the corresponding aMNLFA.object: parameter values, standard errors, test statistics, and p. values for all intercept DIF effects tested in the simultaneous model. Also includes critical values for different corrections according to the number of tests, \emph{m}: Benjamini-Hochberg or Bonferroni with \emph{m} defined as the actual number of tests included in the model (BH.actual and bon.actual, respectively); Benjamini-Hochberg or Bonferroni with \emph{m} defined as the number of items times the number of covariates (BH.ibc and bon.ibc, respectively).}  
#' \item {tDIF_highest} {If thresholds = TRUE in the corresponding aMNLFA.object: parameter values, standard errors, test statistics, and p. values for all threshold DIF effects tested in the simultaneous model, with tests performed only on the category with the largest test statistic for each item. Also includes critical values for different corrections according to the number of tests, \emph{m}: Benjamini-Hochberg or Bonferroni with \emph{m} defined as the actual number of tests included in the model (BH.actual and bon.actual, respectively); Benjamini-Hochberg or Bonferroni with \emph{m} defined as the number of items times the number of covariates (BH.ibc and bon.ibc, respectively).}  
#' \item {tDIF_all} {If thresholds = TRUE in the corresponding aMNLFA.object: parameter values, standard errors, test statistics, and p. values for all threshold DIF effects tested in the simultaneous model, with tests performed on all categories for each item. Also includes critical values for different corrections according to the number of tests, \emph{m}: Benjamini-Hochberg or Bonferroni with \emph{m} defined as the actual number of tests included in the model (BH.actual and bon.actual, respectively); Benjamini-Hochberg or Bonferroni with \emph{m} defined as the number of items times the number of covariates (BH.ibc and bon.ibc, respectively).}  
#' }
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
#'  aMNLFA.prune(ob)
#'  }


aMNLFA.prune<-function(input.object){
  
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
  
  #VC got rid of fixPath here - shouldn't be redefined within the function
  #VC note: Ask IS why it was there - was it not working when loaded from the other f'n? 
  
  round2 <- MplusAutomation::readModels(fixPath(file.path(dir,"round2calibration.out",sep="")))
  round2estimates <- round2$parameters$unstandardized
  round2estimates$pexact <- 2*(1 - stats::pnorm(abs(round2estimates$est_se)))
  

  
  #################################################################################################
  #################Use p<.1 criterion for impact###################################################
  #################Meas Invar: Lambdas: Use FDR with 5% error rate, # items * # predictors#########
  ########Meas Invar: Lambdas: Use FDR with 5% error rate, # items w/0 lambda * # predictors ######
  #################################################################################################
  ##Read in mean impact script and test for impact at p<.1
  meanimpact <- as.data.frame(round2estimates)
  meanimpact <- meanimpact[which(meanimpact$paramHeader=="ETA.ON"),]
  meanimpact <- subset(meanimpact, meanimpact$pexact < .1) #VC added this on 8/13 to filter exclusively significant DIF

  varimpact <- as.data.frame(round2estimates)
  
  varimpact <- varimpact[which(varimpact$paramHeader=="New.Additional.Parameters" & varimpact$pval<.1),]  ######alpha <.1 to trim###########
  varimpact <- varimpact[grep("V", varimpact$param),] #Now subset it to just variance parameters -- i.e., eliminate lambdas

  #Added in the following to get covariates associated with variance impact
  #VC, 8/4/2021
  v.names <- varimpact$param
  varimpact.covariate.label <- sub(".*\\V", "", v.names)
  varimpact.covariate.label <- as.numeric(varimpact.covariate.label)
  varimpact$covariate.label <- varimpact.covariate.label
  varimpact$covariate.name <- myVarImpact[varimpact$covariate.label]
  
  
  ##lambda DIF
  lambdadif <- round2estimates
  lambdadif <- subset(lambdadif, lambdadif$paramHeader == "New.Additional.Parameters")
  lambdadif <- lambdadif[(grepl("L", lambdadif$param) == TRUE) & (grepl("_0", lambdadif$param) == FALSE),]
  l.names <- lambdadif$param
  
  item.label <- sub("\\_.*", "", l.names)
  item.label <- sub("L", "", item.label)
  item.label <- as.numeric(item.label)  
  
  covariate.label <- sub(".*\\_", "", l.names)
  covariate.label <- as.numeric(covariate.label)
  
  lambdadif$item.label <- item.label
  lambdadif$covariate.label <- covariate.label
  
  lambdadif$covariate.name <- myMeasInvar[lambdadif$covariate.label]
  
  ##intercept and threshold DIF
  if (thresholds == TRUE) {
    tdif <- round2estimates
    tdif <- subset(tdif, tdif$paramHeader == "New.Additional.Parameters")
    tdif <- tdif[(grepl("T", tdif$param) == TRUE),]
    
    t.names <- tdif$param
    
    item.label <- sub("\\_.*", "", t.names)
    item.label <- sub("T", "", item.label)
    item.label <- as.numeric(item.label)
      
    category.label <- rep(NA, length(item.label))
    for (a in 1:length(t.names)) {
      category.label[a] <- sub(paste0(item.label[a], "_"), "", t.names[a])
      category.label[a] <- sub("\\_.*", "", category.label[a])
      category.label[a] <- sub("T", "", category.label[a])
      }
    category.label <- as.numeric(category.label)
    
    covariate.label <- sub(".*\\_", "", t.names)
    covariate.label <- as.numeric(covariate.label)
  
    tdif <- data.frame(tdif, item.label, category.label, covariate.label)
    tdif <- subset(tdif, tdif$covariate.label != 0 )
    
    intdif <- tdif[0,]
    for (q in 1:length(myindicators)) {
        if (q %in% tdif$item.label) {
          the.set <- subset(tdif, tdif$item.label == q)
          for (p in 1:length(myMeasInvar)) {
            if (p %in% the.set$covariate.label) {
              the.new.row <- subset(the.set, (the.set$item.label == q & the.set$covariate.label == p))
              the.new.row <- subset(the.new.row, the.new.row$pval == min(the.new.row$pval))
              if (nrow(the.new.row) > 1) {
                the.new.row <- the.new.row[sample(nrow(the.new.row), 1),]
              }
              intdif <- rbind(intdif,the.new.row)
            }
          }
        }
      }
    } else {
    intdif <- round2estimates[(grepl(".ON", round2estimates$paramHeader) == TRUE) & (grepl("ETA", round2estimates$paramHeader) == FALSE),]
    covariate.label <- match(intdif$param,myMeasInvar)
    
    which.indicators <- sub(".ON", "", intdif$paramHeader)
    item.label <- match(which.indicators, myindicators)
    
    intdif <- data.frame(intdif, item.label, covariate.label)
    }
  
  
  ##########################################
  #########Tabulate DIF effects#############
  ##########################################
  
  if (nrow(lambdadif) > 0) {
    lambdadif <- lambdadif[order(lambdadif$pexact) , ]
    lambdadif$BH.actual <- .05/(nrow(lambdadif):1)
    l.BH.ibc <- .05/((length(myMeasInvar)*length(myindicators)):1)
    lambdadif$BH.ibc <- l.BH.ibc[1:nrow(lambdadif)]
    lambdadif$bon.actual <- .05/nrow(lambdadif)
    lambdadif$bon.ibc <- .05/(length(myMeasInvar)*length(myindicators))
  }
  
  lambdadif.mat.BH.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.BH.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.bon.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  lambdadif.mat.bon.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  
  
  if (nrow(intdif) > 0) {
    intdif <- intdif[order(intdif$pexact) , ]
    intdif$BH.actual <- .05/(nrow(intdif):1)
    i.BH.ibc <- .05/((length(myMeasInvar)*length(myindicators)):1)
    intdif$BH.ibc <- i.BH.ibc[1:nrow(intdif)]
    intdif$bon.actual <- .05/nrow(intdif)
    intdif$bon.ibc <- .05/(length(myMeasInvar)*length(myindicators))
  }
  
  intdif.mat.BH.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.BH.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.bon.actual <- matrix(0, length(myindicators), length(myMeasInvar))
  intdif.mat.bon.ibc <- matrix(0, length(myindicators), length(myMeasInvar))
  
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
  
  if (thresholds == TRUE) {
    tdif <- tdif[order(tdif$pexact) , ]
    tdif$BH.actual <- .05/(nrow(tdif):1)
    t.BH.ibc <- .05/(sum(category.label):1)
    tdif$BH.ibc <- t.BH.ibc[1:nrow(tdif)]
    tdif$bon.actual <- .05/nrow(tdif)
    tdif$bon.ibc <- .05/sum(category.label)
  }

  
  ##########################################
  #########Plot DIF effects#################
  ##########################################
  
  if (thresholds == TRUE) {
    prune.summary <- list("indicators" = myindicators, "Measurement Invariance Variables" = myMeasInvar, "Mean Impact" = meanimpact, "Variance Impact" = varimpact, "Loading DIF" = lambdadif, "tDIF_highest" = intdif, "tDIF_all" = tdif)
  } else {
    prune.summary <- list("indicators" = myindicators, "measinvar" = myMeasInvar, "meanimpact" = meanimpact, "varimpact" = varimpact, "loadingDIF" = lambdadif, "interceptDIF" = intdif)
  }
  
  list("summary" = prune.summary)
  
}
  
  #If the "highest.category" option is invoked when "thresholds = TRUE", for each combination of item and covariate the decision about whether to retain threshold effects going from the simultaneous model to the penultimate model will be based on the largest effect across all thresholds. 
  #So, for instance, suppose we have 16 items, each with 3 categories, and 2 covariates that affect them. 
  #If we have invoked "items.by.covariates" as the way to choose the number of tests, we will consider a maximum of 16 x 2 = 32 effects, rather than 16 x 2 x 3 = 96 effects in the pruning stage.
  #If we have invoked "actual.tests" as the way to choose the number of tests, we will consider the number of item-covariate pairs that are actually included in the model. So if 6 items have effects of covariate 1 and 7 items have effects of covariate 2, we will have 13 tests.
  #Suppose for item 1 the test statistics are 1.45, 1.69, and 1.90 for the effect of covariate 1 on thresholds for endorsing response options 1, 2, and 3, respectively. In this case, only the covariate's effect on threshold 3 will be used in calculating the number of tests. 
  #Note that this does NOT mean only the covariate effect on threshold 3 will be included for item 1, just that this is the only value that is used for testing; if the effect of covariate 1 on the threshold for a response of 3 on item 1 is significant, we retain the effects of covariate 1 on all thresholds for item 1.
  #If "highest.category" is not true, all threshold effects will be considered, even those below the maximum value for a given item.
  
