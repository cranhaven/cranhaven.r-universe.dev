#' catmap: Case-Control and TDT Meta-Analysis Package
#'
#' This package conducts fixed-effects (with inverse variance weighting) and
#'  random-effects [DerSimonian and Laird (1986)] meta-analyses of case-control
#'  or family-based (TDT) genetic data. In addition, catmap performs
#'  meta-analyses which combine these two types of study designs. Specifically,
#'  this package implements a fixed-effects model [Kazeem and Farrall (2005)]
#'  and a random-effects model [Nicodemus (2008)] for combined studies.
#'
#' Use the output of the \code{catmap} function to generate figures using
#'  secondary functions. These secondary functions produce output as either
#'  a txt file, a pdf plot, or both.
#'
#' A standard forest plot is available via \code{\link{catmap.forest}}.
#'  A funnel plot is available via \code{\link{catmap.funnel}}. However,
#'  no formal test of publication bias is available (see
#'  [Ioannidis and Trikalinos (2007)]).
#'
#' In addition, cumulative meta-analyses over time (\code{\link{catmap.cumulative}})
#'  and leave-one-out sensitivity analyses (\code{\link{catmap.sense}}) are
#'  available for the fixed-effects estimates and random-effects estimates.
#'
#' @param dataset A \code{data.frame}, \code{matrix}, or file location.
#'  The input should have the following column names: name, study, t, nt,
#'  caserisk, controlrisk, casenotrisk, controlnotrisk. For a file location,
#'  provide the data in a tab-delimited format. Note that the header must have
#'  these exact columns and all cells in the table must have an entry, even
#'  if the entry is 0 or NA. See \code{\link{catmapdata}} for an example.
#' @param ci A numeric value. The confidence level for confidence intervals;
#'  0 < ci < 1.
#' @param printout A boolean. Toggles whether a text file of the models
#'  and Q statistic results should get saved to the working directory.
#'
#' @author Algorithm designed and implemented by Kristin K. Nicodemus.
#'  Code modified and updated by Thom Quinn.
#' @seealso \code{\link{catmap}}, \code{\link{catmap.forest}},
#'  \code{\link{catmap.sense}}, \code{\link{catmap.cumulative}},
#'  \code{\link{catmap.funnel}}
#'
#' @examples
#' data(catmapdata)
#' catmapobject <- catmap(catmapdata, 0.95, TRUE)
#' @export
catmap <- function(dataset, ci = 0.95, printout = FALSE){

  options(warn = -1)

  # Input data as data.frame, matrix, or file
  if(class(dataset) == "data.frame" | class(dataset) == "matrix"){
    a1 <- dataset
  }else{
    if(file.exists(dataset)){
      a1 <- read.table(dataset, header = TRUE)
    }else{
      stop("Input dataset not recognized.")
    }
  }

  #split data into tdt and case-control studies
  tdt<-a1[a1$study==1,]
  cc<-a1[a1$study==2,]

  #fixed-effects estimates
  #calculate tdt-specific log ORs, variances and weights
  tdtlogOR<-log(tdt$t/tdt$nt)
  tdtvar<-((1/tdt$t)+(1/tdt$nt))
  tdtweight<-(1/tdtvar)
  tdtse<-sqrt(tdtvar)

  #calculate case-control specific log ORs, variances and weights
  cclogOR<-log((cc$caserisk*cc$controlnotrisk)/(cc$casenotrisk*cc$controlrisk))
  ccvar<-((1/cc$caserisk)+(1/cc$controlrisk)+(1/cc$casenotrisk)+(1/cc$controlnotrisk))
  ccse<-sqrt(ccvar)
  ccweight<-(1/ccvar)

  #calculate combined log OR, variance, confidence interval and p-value
  a1$lev<-as.factor(a1$study)
  if(nlevels(a1$lev)==1 & a1$study[1]==1){
    weight<-tdtweight
    logOR<-tdtlogOR
    seLogOR<-tdtse
    comvarlogOR<-tdtvar
  }else if(nlevels(a1$lev)==1 & a1$study[1]==2){
    weight<-ccweight
    logOR<-cclogOR
    seLogOR<-ccse
    comvarlogOR<-ccvar
  }else if(nlevels(a1$lev)==2){
    studyorder<-cbind(1:nrow(a1), a1$study)
    studyorder1<-studyorder[order(studyorder[,2]),]
    weight1<-c(tdtweight, ccweight)
    logOR1<-c(tdtlogOR, cclogOR)
    seLogOR1<-c(tdtse, ccse)
    comvarlogOR1<-c(tdtvar, ccvar)
    studyorder2<-cbind(studyorder1, weight1, logOR1, seLogOR1, comvarlogOR1)
    studyorder3<-studyorder2[order(studyorder2[,1]),]
    weight<-studyorder3[,3]
    logOR<-studyorder3[,4]
    seLogOR<-studyorder3[,5]
    comvarlogOR<-studyorder3[,6]
  }else{
    stop("Input dataset not correctly formatted.")
  }

  #get qnorm values
  alpha<-(1-((1-ci)/2))
  quantilenorm<-qnorm(alpha, 0, 1)
  OR1<-exp(logOR)
  lbci1<-exp(logOR-(quantilenorm*seLogOR))
  ubci1<-exp(logOR+(quantilenorm*seLogOR))

  #calculate combined log OR, variance, confidence interval and p-value
  combinedLogOR<-((sum(weight*logOR))/sum(weight))
  combinedOR<-exp(combinedLogOR)
  combinedSeLogOR<-(sqrt(1/sum(weight)))
  combinedVarLogOR<-(1/sum(weight))
  combinedChisq<-(((combinedLogOR-0)^2)/combinedVarLogOR)
  combinedValue<-pchisq(combinedChisq, df=1)
  combinedPvalue<-(1-combinedValue)

  #calculate combined log OR, variance, confidence interval and p-value
  lbci<-exp(combinedLogOR-(quantilenorm*combinedSeLogOR))
  ubci<-exp(combinedLogOR+(quantilenorm*combinedSeLogOR))
  combinedCI<-c(lbci, ubci)
  SeLogOR<-sqrt(comvarlogOR)
  lbci.fe<-exp(logOR-(quantilenorm*SeLogOR))
  ubci.fe<-exp(logOR+(quantilenorm*SeLogOR))

  #calculate heterogeneity
  het.df<-(nrow(a1)-1)
  chisqHet<-(sum(weight*(((logOR-combinedLogOR)^2))))
  combinedHetValue<-pchisq(chisqHet, df=het.df)
  heterogeneityPvalue<-(1-combinedHetValue)

  #DerSimonian and Laird random-effects estimates
  tau2<-((chisqHet-het.df)/(sum(weight)-(sum(weight^2)/(sum(weight)))))
  if(tau2 <= 0){

    cat("NOTICE: tau2 is less than or equal to 0;",
        "\n no random effects estimates will be calculated\n")
    table.header<-c("Inverse Variance Fixed-Effects OR",
                    "Inverse Variance Fixed-Effects Lower Bound CI",
                    "Inverse Variance Fixed-Effects Upper Bound CI",
                    "Inverse Variance Fixed-Effects Chi-Square",
                    "Inverse Variance Fixed-Effects p-value",
                    "Q Statistic (Heterogeneity) Chi-Square",
                    "Q Statistic (Heterogeneity) p-value")
    table.fill<-c(combinedOR, combinedCI, combinedChisq, combinedPvalue,
                  chisqHet, heterogeneityPvalue)
    results<-rbind(table.header, round(table.fill, digits=5))

  }else if(tau2 > 0){

    weight.dsl<-(1/(comvarlogOR+tau2))
    logOR.dsl<-((sum(weight.dsl*logOR))/(sum(weight.dsl)))
    OR.dsl<-exp(logOR.dsl)
    seLogOR.dsl<-(1/(sqrt(sum(weight.dsl))))
    varLogOR.dsl<-(1/sum(weight.dsl))
    lbci.dsl<-exp(logOR.dsl-(quantilenorm*seLogOR.dsl))
    ubci.dsl<-exp(logOR.dsl+(quantilenorm*seLogOR.dsl))
    ci.dsl<-c(lbci.dsl, ubci.dsl)
    chisq.dsl<-(((logOR.dsl-0)^2)/varLogOR.dsl)
    value.dsl<-pchisq(chisq.dsl, df=1)
    pvalue.dsl<-(1-value.dsl)

    table.header<-c("Inverse Variance Fixed-Effects OR",
                    "Inverse Variance Fixed-Effects Lower Bound CI",
                    "Inverse Variance Fixed-Effects Upper Bound CI",
                    "Inverse Variance Fixed-Effects Chi-Square",
                    "Inverse Variance Fixed-Effects p-value",
                    "Q Statistic (Heterogeneity) Chi-Square",
                    "Q Statistic (Heterogeneity) p-value",
                    "DerSimonian & Laird Random-Effects OR",
                    "DerSimonian & Laird Random-Effects Lower Bound CI",
                    "DerSiminian & Laird Random-Effects Upper Bound CI",
                    "DerSimonian & Laird Random-Effects Chi-Square",
                    "DerSimonian & Laird Random-Effects p-value")
    table.fill<-c(combinedOR, combinedCI, combinedChisq, combinedPvalue,
                  chisqHet, heterogeneityPvalue, OR.dsl, lbci.dsl,
                  ubci.dsl, chisq.dsl, pvalue.dsl)
    results<-rbind(table.header, round(table.fill, digits=5))
  }

  # Print results to console
  cat("# Pooled Estimates\n")
  for(i in 1:ncol(results)) cat(cat(results[1, i], results[2, i], sep = ": "), "\n")
  cat("\n")
  ind.header<-c("Study", "Fixed-Effects ORs", "Lower Bound CIs",
                "Upper Bound CIs", "Study Weights")
  dataname<-as.vector(a1$name)
  ind.fill<-data.frame(cbind(dataname, as.list(exp(logOR)), as.list(lbci1), as.list(ubci1), as.list(weight)))
  names(ind.fill)<-ind.header
  cat("# Individual Study Estimates\n")
  print(ind.fill)
  cat("\n")

  # Print results to file dataset.output.txt
  dataset <- as.character(match.call()$dataset)
  if(printout == TRUE){
    sink(paste(dataset, "output.txt", sep="."))
    cat("# Pooled Estimates\n")
    for(i in 1:ncol(results)) cat(cat(results[1, i], results[2, i], sep = ": "), "\n")
    cat("\n")
    cat("# Individual Study Estimates\n")
    print(ind.fill)
    sink()
  }

  if(tau2 <= 0){

    output <- list(comvarlogOR, combinedLogOR, combinedOR, combinedSeLogOR,
                   weight, logOR, combinedVarLogOR, combinedChisq, combinedValue,
                   combinedPvalue, lbci, ubci, combinedCI, SeLogOR, lbci.fe,
                   ubci.fe, het.df, chisqHet, combinedHetValue, heterogeneityPvalue,
                   tau2, a1$name, a1, quantilenorm, ci, dataset)

    names(output) <- c("comvarlogOR", "combinedLogOR", "combinedOR", "combinedSeLogOR",
                       "weight", "logOR", "combinedVarLogOR", "combinedChisq", "combinedValue",
                       "combinedPvalue", "lbci", "ubci", "combinedCI", "SeLogOR", "lbci.fe",
                       "ubci.fe", "het.df", "chisqHet", "combinedHetValue", "heterogeneityPvalue",
                       "tau2", "studyname", "a1", "quantilenorm", "ci", "dataset")

  }else if(tau2 > 0){

    output <- list(comvarlogOR, combinedLogOR, combinedOR, combinedSeLogOR,
                   weight, logOR, combinedVarLogOR, combinedChisq, combinedValue,
                   combinedPvalue, lbci, ubci, combinedCI, SeLogOR, lbci.fe,
                   ubci.fe, het.df, chisqHet, combinedHetValue, heterogeneityPvalue,
                   tau2, a1$name, a1, quantilenorm, ci, dataset, weight.dsl,
                   logOR.dsl, OR.dsl, seLogOR.dsl, varLogOR.dsl, lbci.dsl, ubci.dsl,
                   ci.dsl, chisq.dsl, value.dsl, pvalue.dsl)

    names(output) <- c("comvarlogOR", "combinedLogOR", "combinedOR", "combinedSeLogOR",
                       "weight", "logOR", "combinedVarLogOR", "combinedChisq", "combinedValue",
                       "combinedPvalue", "lbci", "ubci", "combinedCI", "SeLogOR", "lbci.fe",
                       "ubci.fe", "het.df", "chisqHet", "combinedHetValue", "heterogeneityPvalue",
                       "tau2", "studyname", "a1", "quantilenorm", "ci", "dataset", "weight.dsl",
                       "logOR.dsl", "OR.dsl", "seLogOR.dsl", "varLogOR.dsl", "lbci.dsl", "ubci.dsl",
                       "ci.dsl", "chisq.dsl", "value.dsl", "pvalue.dsl")
  }

  return(output)
}
