#' Analysis: generalized linear models for factor qualitative
#'
#' Performs the deviance analysis for the generalized linear model using binomial or quasibinomial family. The function also returns multiple comparison test with tukey adjustment
#' @param trat Numerical or complex vector with treatments
#' @param resp Numerical vector containing the response in percentage of the experiment.
#' @param n Number of seeds per repetition
#' @param method method for analysis (analysis of variance - aov or analysis by generalized linear model - glm)
#' @param family a description of the error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function.
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab treatments name (Accepts the \emph{expression}() function)
#' @param theme ggplot2 theme (\emph{default} is theme_bw())
#' @param sup Number of units above the standard deviation or average bar on the graph
#' @param reversed Letter order (\emph{default} is FALSE)
#' @param angle x-axis scale text rotation
#' @param font.family Font family (\emph{default} is sans)
#' @param geom type of graph ("bar" or "point")
#' @return The function returns analysis by glm (binomial or quasibinomial family), post-hoc and column graph
#' @export
#' @importFrom emmeans emmeans
#' @importFrom emmeans regrid
#' @importFrom multcomp cld
#' @importFrom stringr str_trim
#' @importFrom stats shapiro.test
#' @importFrom stats aov
#' @importFrom stats bartlett.test
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#' quali_model(trat, germ, n=25, family="quasibinomial")

quali_model=function(trat,
                     resp,
                     method="glm",
                     n=50,
                     family="binomial",
                     ylab="Germination (%)",
                     xlab=expression("Temperature ("^"o"*"C)"),
                     reversed=TRUE,
                     angle=0,
                     sup=NA,
                     theme=theme_classic(),
                     font.family="sans",
                     geom="bar"){
  if(is.na(sup==TRUE)){sup=0.1*mean(resp)}
  requireNamespace("ggplot2")
  requireNamespace("drc")
  requireNamespace("multcomp")
  requireNamespace("emmeans")
  requireNamespace("stringr")

  if(method=="aov"){
  nseeds=resp*n/100
  trat=as.factor(trat)
  mod=aov(resp~trat,family = family)
  hnp::hnp(mod, print.on=TRUE)
  anova=anova(mod)
  aaa=cld(emmeans(mod,~trat),Letters = letters,
          reversed = reversed,
          sort = FALSE)
  graph=ggplot(aaa,aes(y=aaa$emmean,
                       x=as.factor(trat)))+
  geom_col(color="black",fill="gray")+
  theme+
  geom_errorbar(aes(ymin=aaa$lower.CL,
                    ymax=aaa$upper.CL),width=0.2,size=0.8)+
  geom_label(aes(y=aaa$upper.CL+sup,
                 label=paste(round(aaa$emmean,1),
                             str_trim(aaa$.group))))+
  theme(axis.text = element_text(size=12,color="black"),
        axis.text.x=element_text(hjust = 1.01,angle=angle))+
  labs(x=xlab, y=ylab)
  cat("\n=======================================================\n")
  cat("Analysis of variance")
  cat("\n=======================================================\n")
  anova=data.frame(anova)
  colnames(anova)=c("SQ","MS","Df","F-value","P-value")
  print(as.matrix(anova),na.print = "")
  cat("\n=======================================================\n")
  cat("Assumptions of the analysis")
  cat("\n=======================================================\n")
  norm=shapiro.test(mod$residuals)
  homog=bartlett.test(mod$residuals~trat)
  press=rbind("Shapiro-Wilk (W)"=norm$p.value,
        "Bartlett " = homog$p.value)
  colnames(press)=""
  print(press)
  cat("\n=======================================================\n")
  print(aaa)
  print(graph)
  cat("\n=======================================================\n")
  list(graph)[[1]]}

  if(method=="glm"){
    nseeds=resp*n/100
    trat=as.factor(trat)
    mod=glm(cbind(nseeds,n-nseeds)~trat,
            family = family)
    hnp::hnp(mod, print.on=TRUE)
    anova=car::Anova(mod)
    aaa=cld(regrid(emmeans(mod,~trat)),Letters = letters,
            reversed = reversed,
            sort = FALSE)
    if(geom=="bar"){
    graph=ggplot(aaa,aes(y=aaa$prob*100,x=as.factor(trat)))+
      geom_col(color="black",fill="gray")+
      theme+
      geom_errorbar(aes(ymin=aaa$asymp.LCL*100,
                        ymax=aaa$asymp.UCL*100),width=0.2,size=0.8)+
      geom_label(aes(y=aaa$asymp.UCL*100+sup,
                     label=paste(round(aaa$prob*100,1),
                                 str_trim(aaa$.group))),family = font.family)+
      theme(axis.text = element_text(size=12,color="black",family = font.family),
            axis.text.x=element_text(hjust = 1.01,angle=angle,family = font.family))+
      labs(x=xlab, y=ylab)}

    if(geom=="point"){
      graph=ggplot(aaa,aes(y=aaa$prob*100,x=as.factor(trat)))+
        geom_point(color="black",fill="gray",size=5)+
        theme+
        geom_errorbar(aes(ymin=aaa$asymp.LCL*100,
                          ymax=aaa$asymp.UCL*100),width=0.2,size=0.8)+
        geom_label(aes(y=aaa$asymp.UCL*100+sup,
                       label=paste(round(aaa$prob*100,1),
                                   str_trim(aaa$.group))),family = font.family)+
        theme(axis.text = element_text(size=12,color="black",family = font.family),
              axis.text.x=element_text(hjust = 1.01,angle=angle,family = font.family))+
        labs(x=xlab, y=ylab)}
    cat("\n=======================================================\n")
    print(anova)
    cat("\n=======================================================\n")
    print(aaa)
    print(graph)
    cat("\n=======================================================\n")
    list(graph)[[1]]}
  }
