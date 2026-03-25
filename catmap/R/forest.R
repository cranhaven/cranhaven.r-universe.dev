#' catmap: Forest Plot
#'
#' The \code{catmap.forest} creates forest plots of the individual study
#'  Odds Ratios (OR) and Confidence Intervals (CI). It then summarizes the
#'  data using a fixed-effects or random-effects pooled OR and CI.
#'
#' @inheritParams catmap.funnel
#' @param fe.forest A boolean. Toggles whether the forest plot should get saved
#'  to the current working directory.
#' @param re.forest A boolean. Toggles whether the forest plot should get saved
#'  to the current working directory.
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
#' catmap.forest(catmapobject, TRUE, TRUE)
#' @export
catmap.forest <- function(catmapobject, fe.forest = FALSE, re.forest = FALSE){

  if(!fe.forest & !re.forest) par(ask = TRUE)

  # FE
  grid::grid.newpage()
  if(fe.forest) pdf(file = paste0(catmapobject$dataset, ".fixed.effects.plot.pdf"))
  f1 <- makeForest(catmapobject, summary = "fixed")
  if(fe.forest) graphics.off()

  # RE
  if(catmapobject$tau2 <= 0){
    message("NOTICE: tau2 is less than or equal to 0.\n",
            " No random effects estimates calculated.\n")
  }else{
    grid::grid.newpage()
    if(re.forest) pdf(file = paste0(catmapobject$dataset, ".random.effects.plot.pdf"))
    f2 <- makeForest(catmapobject, summary = "random")
    if(re.forest) graphics.off()
    f1 <- list(f1, f2)
  }

  if(!fe.forest & !re.forest) par(ask = FALSE)
  return(f1)
}

#' catmap: Leave-One-Out Sensitivity Analysis
#'
#' The \code{catmap.sense} conducts leave-one-out sensitivity analyses
#'  and creates plots of Odds Ratios (OR) and Confidence Intervals (CI)
#'  using a fixed-effects or random-effects model.
#'
#' @inheritParams catmap.forest
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
#' catmap.sense(catmapobject, FALSE, FALSE, FALSE)
#' @export
catmap.sense <- function(catmapobject, fe.forest = FALSE, re.forest = FALSE, printout = FALSE){

  if(!fe.forest & !re.forest) par(ask = TRUE)

  # Fixed-Effects Sensitivity Analysis
  sfplot<-matrix(0,nrow(catmapobject$a1),3)
  for(f in 1:nrow(catmapobject$a1)){

    sf.weight<-catmapobject$weight[-f]
    sf.logOR<-catmapobject$logOR[-f]

    sf.combinedLogOR<-((sum(sf.weight*sf.logOR))/sum(sf.weight))
    sf.combinedOR<-exp(sf.combinedLogOR)
    sf.combinedSeLogOR<-(sqrt(1/sum(sf.weight)))
    sf.combinedVarLogOR<-(1/sum(sf.weight))
    sf.combinedChisq<-(((sf.combinedLogOR-0)^2)/sf.combinedVarLogOR)
    sf.combinedValue<-pchisq(sf.combinedChisq, df=1)
    sf.combinedPvalue<-(1-sf.combinedValue)

    #get qnorm values
    alpha<-(1-((1-catmapobject$ci)/2))
    quantilenorm<-qnorm(alpha, 0, 1)

    sf.lbci<-exp(sf.combinedLogOR-(quantilenorm*sf.combinedSeLogOR))
    sf.ubci<-exp(sf.combinedLogOR+(quantilenorm*sf.combinedSeLogOR))
    sf.combinedCI<-c(sf.lbci, sf.ubci)
    sf.SeLogOR<-sqrt(sf.combinedVarLogOR)
    sf.lbci<-exp(sf.logOR-(quantilenorm*sf.SeLogOR))
    sf.ubci<-exp(sf.logOR+(quantilenorm*sf.SeLogOR))

    #calculate heterogeneity
    sf.chisqHet<-(sum(sf.weight*(((sf.logOR-sf.combinedLogOR)^2))))
    sf.df<-(nrow(catmapobject$a1)-2)
    sf.combinedHetValue<-pchisq(sf.chisqHet, df=sf.df)
    sf.heterogeneityPvalue<-(1-sf.combinedHetValue)

    study.removed<-paste("Study Removed =", catmapobject$studyname[f], sep=" ")
    sftable.header<-c("Inverse Variance Fixed-Effects OR",
                      "Inverse Variance Fixed-Effects Lower Bound CI",
                      "Inverse Variance Fixed-Effects Upper Bound CI",
                      "Inverse Variance Fixed-Effects Chi-Square",
                      "Inverse Variance Fixed-Effects p-value",
                      "Q Statistic (Heterogeneity) Chi-Square",
                      "Q Statistic (Heterogeneity) p-value")
    sftable.fill<-c(sf.combinedOR, sf.combinedCI, sf.combinedChisq, sf.combinedPvalue,
                    sf.chisqHet, sf.heterogeneityPvalue)
    sf.results<-rbind(sftable.header, round(sftable.fill, digits=5))
    sfvalues<-c(sf.combinedOR, sf.combinedCI)
    sfplot[f,]<-sfvalues
    cat("# Fixed-Effects Sensitivity Analysis\n")
    cleanSink(study.removed, sf.results, sep="\n")
    cat("\n")

    # Optional print-out of results
    if(printout){
      sink(paste0(catmapobject$dataset, ".fixed.effects.sensitivity.txt"), append = TRUE)
      cat("# Fixed-Effects Sensitivity Analysis\n")
      cleanSink(study.removed, sf.results, sep="\n")
      cat("\n")
      sink()
    }
  }

  # Optional output of plot
  grid::grid.newpage()
  if(fe.forest) pdf(file = paste0(catmapobject$dataset, ".fixed.effects.sensitivity.plot.pdf"))
  makeForest(catmapobject, main = "Sensitivity Analysis:\nInverse Variance (Fixed-Effects) ORs",
             mean = sfplot[, 1], lower = sfplot[, 2], upper = sfplot[, 3], summary = "fixed",
             study = c("Study Removed", sub(",", " ", catmapobject$studyname)))
  if(fe.forest) graphics.off()

  # Random-Effects Sensitivity Analysis
  if(catmapobject$tau2 <= 0){

    message("NOTICE: tau2 is less than or equal to 0.\n",
            " No random effects estimates calculated.\n")

  }else{

    srplot<-matrix(0,nrow(catmapobject$a1),3)
    for(r in 1:nrow(catmapobject$a1)){
      sr.weight<-catmapobject$weight[-r]
      sr.logOR<-catmapobject$logOR[-r]
      sr.comvarlogOR<-catmapobject$comvarlogOR[-r]

      sr.combinedLogOR<-((sum(sr.weight*sr.logOR))/sum(sr.weight))
      sr.combinedOR<-exp(sr.combinedLogOR)
      sr.combinedSeLogOR<-(sqrt(1/sum(sr.weight)))
      sr.combinedVarLogOR<-(1/sum(sr.weight))
      sr.combinedChisq<-(((sr.combinedLogOR-0)^2)/sr.combinedVarLogOR)
      sr.combinedValue<-pchisq(sr.combinedChisq, df=1)
      sr.combinedPvalue<-(1-sr.combinedValue)

      #get qnorm values
      alpha<-(1-((1-catmapobject$ci)/2))
      quantilenorm<-qnorm(alpha, 0, 1)

      sr.lbci<-exp(sr.combinedLogOR-(quantilenorm*sr.combinedSeLogOR))
      sr.ubci<-exp(sr.combinedLogOR+(quantilenorm*sr.combinedSeLogOR))
      sr.combinedCI<-c(sr.lbci, sr.ubci)
      sr.SeLogOR<-sqrt(sr.combinedVarLogOR)
      sr.lbci<-exp(sr.logOR-(quantilenorm*sr.SeLogOR))
      sr.ubci<-exp(sr.logOR+(quantilenorm*sr.SeLogOR))

      #calculate heterogeneity
      sr.df<-(nrow(catmapobject$a1)-2)
      sr.chisqHet<-(sum(sr.weight*(((sr.logOR-sr.combinedLogOR)^2))))
      sr.combinedHetValue<-pchisq(sr.chisqHet, df=sr.df)
      sr.heterogeneityPvalue<-(1-sr.combinedHetValue)

      #DerSimonian and Laird random-effects sensitivity analysis
      sr.tau2<-((sr.chisqHet-sr.df)/(sum(sr.weight)-(sum(sr.weight^2)/(sum(sr.weight)))))
      if (sr.tau2 <=0){
        sr.tau2<-0
      }

      srweight.dsl<-(1/(sr.comvarlogOR+sr.tau2))
      srlogOR.dsl<-((sum(srweight.dsl*sr.logOR))/(sum(srweight.dsl)))
      srOR.dsl<-exp(srlogOR.dsl)
      srseLogOR.dsl<-(1/(sqrt(sum(srweight.dsl))))
      srvarLogOR.dsl<-(1/sum(srweight.dsl))
      srlbci.dsl<-exp(srlogOR.dsl-(quantilenorm*srseLogOR.dsl))
      srubci.dsl<-exp(srlogOR.dsl+(quantilenorm*srseLogOR.dsl))
      srci.dsl<-c(srlbci.dsl, srubci.dsl)
      srchisq.dsl<-(((srlogOR.dsl-0)^2)/srvarLogOR.dsl)
      srvalue.dsl<-pchisq(srchisq.dsl, df=1)
      srpvalue.dsl<-(1-srvalue.dsl)

      srstudy.removed<-paste("Study Removed =", catmapobject$studyname[r], sep=" ")
      srtable.header<-c("Q Statistic (Heterogeneity) Chi-Square",
                        "Q Statistic (Heterogeneity) p-value",
                        "DerSimonian & Laird Random-Effects OR",
                        "DerSimonian & Laird Random-Effects Lower Bound CI",
                        "DerSimonian & Laird Random-Effects Upper Bound CI",
                        "DerSimonian & Laird Random-Effects Chi-Square",
                        "DerSimonian & Laird Random-Effects p-value")
      srtable.fill<-c(sr.chisqHet, sr.heterogeneityPvalue, srOR.dsl, srlbci.dsl,
                      srubci.dsl, srchisq.dsl, srpvalue.dsl)
      sr.results<-rbind(srtable.header, round(srtable.fill, digits=5))
      srvalues<-c(srOR.dsl, srci.dsl)
      srplot[r,]<-srvalues
      cat("# Random-Effects Sensitivity Analysis\n")
      cleanSink(srstudy.removed, sr.results, sep="\n")
      cat("\n")

      # Optional print-out of results
      if(printout){
        sink(paste0(catmapobject$dataset, ".random.effects.sensitivity.txt"), append = TRUE)
        cat("# Random-Effects Sensitivity Analysis\n")
        cleanSink(srstudy.removed, sr.results, sep="\n")
        cat("\n")
        sink()
      }
    }

    # Optional output of plot
    grid::grid.newpage()
    if(re.forest) pdf(file = paste0(catmapobject$dataset, ".random.effects.sensitivity.plot.pdf"))
    makeForest(catmapobject, main = "Sensitivity Analysis:\nDerSimonian & Laird (Random-Effects) ORs",
               mean = srplot[, 1], lower = srplot[, 2], upper = srplot[, 3], summary = "random",
               study = c("Study Removed", sub(",", " ", catmapobject$studyname)))
    if(re.forest) graphics.off()
  }

  if(!fe.forest & !re.forest) par(ask = FALSE)
  return(TRUE)
}

#' catmap: Cumulative Meta-Analysis
#'
#' The \code{catmap.cumulative} conducts cumulative meta-analyses
#'  and creates plots of Odds Ratios (OR) and Confidence Intervals (CI)
#'  using a fixed-effects or random-effects model. Note that studies
#'  should be listed in chronological order in the input file! This
#'  function does not re-order studies by publication year! Also
#'  note that random-effects estimates are not defined for a single
#'  (i.e., the first) study.
#'
#' @inheritParams catmap.forest
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
#' catmap.cumulative(catmapobject, FALSE, FALSE, FALSE)
#' @export
catmap.cumulative <- function(catmapobject, fe.forest = FALSE, re.forest = FALSE, printout = FALSE){

  if(!fe.forest & !re.forest) par(ask = TRUE)

  # Fixed-Effects Sensitivity Analysis
  ci100 <- catmapobject$ci*100
  cfplot<-matrix(0, nrow(catmapobject$a1),3)
  for(c in 1:nrow(catmapobject$a1)){
    cf.weight<- catmapobject$weight[1:c]
    cf.logOR<- catmapobject$logOR[1:c]

    cf.combinedLogOR<-((sum(cf.weight*cf.logOR))/sum(cf.weight))
    cf.combinedOR<-exp(cf.combinedLogOR)
    cf.combinedSeLogOR<-(sqrt(1/sum(cf.weight)))
    cf.combinedVarLogOR<-(1/sum(cf.weight))
    cf.combinedChisq<-(((cf.combinedLogOR-0)^2)/cf.combinedVarLogOR)
    cf.combinedValue<-pchisq(cf.combinedChisq, df=1)
    cf.combinedPvalue<-(1-cf.combinedValue)

    #get qnorm values
    alpha<-(1-((1-catmapobject$ci)/2))
    quantilenorm<-qnorm(alpha, 0, 1)

    cf.lbci<-exp(cf.combinedLogOR-(quantilenorm*cf.combinedSeLogOR))
    cf.ubci<-exp(cf.combinedLogOR+(quantilenorm*cf.combinedSeLogOR))
    cf.combinedCI<-c(cf.lbci, cf.ubci)
    cf.SeLogOR<-sqrt(cf.combinedVarLogOR)
    cf.lbci<-exp(cf.logOR-(quantilenorm*cf.SeLogOR))
    cf.ubci<-exp(cf.logOR+(quantilenorm*cf.SeLogOR))

    #calculate heterogeneity
    cf.df<-(c-1)
    cf.chisqHet<-(sum(cf.weight*(((cf.logOR-cf.combinedLogOR)^2))))
    cf.combinedHetValue<-pchisq(cf.chisqHet, df=cf.df)
    cf.heterogeneityPvalue<-(1-cf.combinedHetValue)

    study.added<-paste("Study Added =", catmapobject$studyname[c], sep=" ")
    cftable.header<-c("Inverse Variance Fixed-Effects OR",
                      "Inverse Variance Fixed-Effects Lower Bound CI",
                      "Inverse Variance Fixed-Effects Upper Bound CI",
                      "Inverse Variance Fixed-Effects Chi-Square",
                      "Inverse Variance Fixed-Effects p-value",
                      "Q Statistic (Heterogeneity) Chi-Square",
                      "Q Statistic (Heterogeneity) p-value")
    cftable.fill<-c(cf.combinedOR, cf.combinedCI, cf.combinedChisq, cf.combinedPvalue,
                    cf.chisqHet, cf.heterogeneityPvalue)
    cf.results<-rbind(cftable.header, round(cftable.fill, digits=5))
    cfvalues<-c(cf.combinedOR, cf.combinedCI)
    cfplot[c,]<-cfvalues
    cat("# Fixed-Effects Cumulative Meta-Analysis\n")
    cleanSink(study.added, cf.results, sep="\n")
    cat("\n")

    # Optional print-out of results
    if(printout){
      sink(paste0(catmapobject$dataset, ".fixed.effects.cumulative.txt"), append = TRUE)
      cat("# Fixed-Effects Cumulative Meta-Analysis\n")
      cleanSink(study.added, cf.results, sep="\n")
      cat("\n")
      sink()
    }
  }

  # Optional output of plot
  grid::grid.newpage()
  if(fe.forest) pdf(file = paste0(catmapobject$dataset, ".fixed.effects.cumulative.plot.pdf"))
  makeForest(catmapobject, main = "Cumulative Meta-Analysis:\nInverse Variance (Fixed-Effects) ORs",
             mean = cfplot[, 1], lower = cfplot[, 2], upper = cfplot[, 3], summary = "fixed",
             study = c("Study Added", sub(",", " ", catmapobject$studyname)))
  if(fe.forest) graphics.off()

  # Random-Effects Sensitivity Analysis
  if(catmapobject$tau2 <= 0){

    message("NOTICE: tau2 is less than or equal to 0.\n",
            " No random effects estimates calculated.\n")

  }else{
    crplot<-matrix(0,nrow(catmapobject$a1),3)
    for(u in 1:nrow(catmapobject$a1)){
      #v<-u-1
      cr.weight<- catmapobject$weight[1:u]
      cr.logOR<- catmapobject$logOR[1:u]
      cr.comvarlogOR<- catmapobject$comvarlogOR[1:u]

      cr.combinedLogOR<-((sum(cr.weight*cr.logOR))/sum(cr.weight))
      cr.combinedOR<-exp(cr.combinedLogOR)
      cr.combinedSeLogOR<-(sqrt(1/sum(cr.weight)))
      cr.combinedVarLogOR<-(1/sum(cr.weight))
      cr.combinedChisq<-(((cr.combinedLogOR-0)^2)/cr.combinedVarLogOR)
      cr.combinedValue<-pchisq(cr.combinedChisq, df=1)
      cr.combinedPvalue<-(1-cr.combinedValue)

      #get qnorm values
      alpha<-(1-((1- catmapobject$ci)/2))
      quantilenorm<-qnorm(alpha, 0, 1)

      cr.lbci<-exp(cr.combinedLogOR-(quantilenorm*cr.combinedSeLogOR))
      cr.ubci<-exp(cr.combinedLogOR+(quantilenorm*cr.combinedSeLogOR))
      cr.combinedCI<-c(cr.lbci, cr.ubci)
      cr.SeLogOR<-sqrt(cr.combinedVarLogOR)

      #calculate heterogeneity
      cr.df<-(u-1)
      cr.chisqHet<-(sum(cr.weight*(((cr.logOR-cr.combinedLogOR)^2))))
      cr.combinedHetValue<-pchisq(cr.chisqHet, df=cr.df)
      cr.heterogeneityPvalue<-(1-cr.combinedHetValue)

      #DerSimonian and Laird random-effects cumulative analysis
      cr.tau2c<-(cr.chisqHet-cr.df)/(sum(cr.weight)-(sum(cr.weight^2)/(sum(cr.weight))))
      cr.tau2<-max(0,cr.tau2c)
      #if (cr.tau2 <=0){
      #cr.tau2<-0
      #}

      crweight.dsl<-(1/(cr.comvarlogOR+cr.tau2))
      crlogOR.dsl<-((sum(crweight.dsl*cr.logOR))/(sum(crweight.dsl)))
      crOR.dsl<-exp(crlogOR.dsl)
      crseLogOR.dsl<-(1/(sqrt(sum(crweight.dsl))))
      crvarLogOR.dsl<-(1/sum(crweight.dsl))
      crlbci.dsl<-exp(crlogOR.dsl-(quantilenorm*crseLogOR.dsl))
      crubci.dsl<-exp(crlogOR.dsl+(quantilenorm*crseLogOR.dsl))
      crci.dsl<-c(crlbci.dsl, crubci.dsl)
      crchisq.dsl<-(((crlogOR.dsl-0)^2)/crvarLogOR.dsl)
      crvalue.dsl<-pchisq(crchisq.dsl, df=1)
      crpvalue.dsl<-(1-crvalue.dsl)
      #added } here

      #crOR.dsl[1]<-exp(catmapobject$logOR[1])
      #crlbci.dsl[1]<-catmapobject$lbci.fe[1]
      #crubci.dsl[1]<-catmapobject$ubci.fe[1]

      crstudy.added<-paste("Study Added =", catmapobject$studyname[u], sep=" ")
      crtable.header<-c("Q Statistic (Heterogeneity) Chi-Square",
                        "Q Statistic (Heterogeneity) p-value",
                        "DerSimonian & Laird Random-Effects OR",
                        "DerSimonian & Laird Random-Effects Lower Bound CI",
                        "DerSimonian & Laird Random-Effects Upper Bound CI",
                        "DerSimonian & Laird Random-Effects Chi-Square",
                        "DerSimonian & Laird Random-Effects p-value")
      crtable.fill<-c(cr.chisqHet, cr.heterogeneityPvalue, crOR.dsl, crlbci.dsl,
                      crubci.dsl, crchisq.dsl, crpvalue.dsl)
      cr.results<-rbind(crtable.header, round(crtable.fill, digits=5))
      crvalues<-c(crOR.dsl, crci.dsl)
      crplot[u,]<-crvalues
      crplot[1,1]<-exp(catmapobject$logOR[1])
      crplot[1,2]<-catmapobject$lbci.fe[1]
      crplot[1,3]<-catmapobject$ubci.fe[1]
      cat("# Random-Effects Cumulative Meta-Analysis\n")
      cleanSink(crstudy.added, cr.results, sep="\n")
      cat("\n")

      # Optional print-out of results
      if(printout){
        sink(paste0(catmapobject$dataset, ".random.effects.cumulative.txt"), append = TRUE)
        cat("# Random-Effects Cumulative Meta-Analysis\n")
        cleanSink(crstudy.added, cr.results, sep="\n")
        cat("\n")
        sink()
      }
    }

    # Optional output of plot
    grid::grid.newpage()
    if(re.forest) pdf(file = paste0(catmapobject$dataset, ".random.effects.cumulative.plot.pdf"))
    makeForest(catmapobject, main = "Cumulative Meta-Analysis:\nDerSimonian & Laird (Random-Effects) ORs",
               mean = crplot[, 1], lower = crplot[, 2], upper = crplot[, 3], summary = "random",
               study = c("Study Added", sub(",", " ", catmapobject$studyname)))
    if(re.forest) graphics.off()
  }

  if(!fe.forest & !re.forest) par(ask = FALSE)
  return(TRUE)
}
