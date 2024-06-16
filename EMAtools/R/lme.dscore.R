#' Calculate d scores from an lme4 or nlme object
#'
#' This will calculate Cohen's D for each effect in an lme4 object.
#' @param mod An lme4 or nlme object
#' @param data The dataset the lme4 or nlme object was drawn from
#' @param type Either "lme4" or "nlme"
#' @return A table of d-scores.
#' @keywords Cohen's D
#' @note lme4 and nlme models will produce slightly different estimates. This is because when using type="lme4", the numerator DF will be calculated using the Satterthwaite approximations to degrees of freedom (via the lmerTest package), whereas nlme includes Kenward-Roger numerator degress of freedom. If you have sufficent level-1 samples, the difference between models will be miniscule.
#' @examples
#' \dontrun{model1<-lmer(DV~IV1+IV2+IV3+(1|subject),data=DATA_1)}
#'  \dontrun{lme.dscore(model1,data=DATA_1,type="lme4")}



lme.dscore<-function(mod,data,type){
  if (type=="lme4") {
    mod1<-lmerTest::lmer(mod,data=data)
    eff<-cbind(summary(mod1)$coefficients[,4],summary(mod1)$coefficients[,3])
  }

  if (type=="nlme") {
    eff=cbind(summary(mod)$tTable[,4],summary(mod)$fixDF$terms)
  }

  colnames(eff)<-c("t","df")
  eff<-as.data.frame(eff)
  eff$d<-(2*eff$t)/sqrt(eff$df)
  eff<-eff[-1,]
  return(eff)
}




