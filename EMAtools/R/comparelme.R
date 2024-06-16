#' Compare the slopes of two lme models
#'
#' This allows you to compare two lme4 models that have the same fixed predictors but differ in other ways (e.g., from different datasets, different random effects). It will produce a Z score a p-value for each effect.
#' @param VAR1 An lme4 object
#' @param VAR2 An lme4 object that has the same variables, in the same order as VAR1.
#' @return Z-tests comapring slopes.
#' @keywords Compare slopes
#' @examples
#' \dontrun{model1<-lmer(DV~IV1+IV2+IV3+(1|subject),data=DATA_1)}
#' \dontrun{model2<-lmer(DV~IV1+IV2+IV3+(1|subject),data=DATA_2)}
#'  \dontrun{lm_slopes_compare(model1,model2)}



lm_slopes_compare=function(VAR1,VAR2){

  if (!identical((NROW(summary(VAR1)$coefficients)),(NROW(summary(VAR2)$coefficients)))) {stop("Models do not have same number of effects")}




  compare<-as.data.frame(cbind((cbind(summary(VAR1)$coefficients[,1],summary(VAR1)$coefficients[,2])),
                               (cbind(summary(VAR2)$coefficients[,1],summary(VAR2)$coefficients[,2]))))
  colnames(compare)<-c("B1","SE1","B2","SE2")

  results<-as.data.frame(cbind(rownames(compare),as.numeric((compare$B1-compare$B2)/(sqrt(((compare$SE1^2)+compare$SE2^2))))))
  colnames(results)<-c("Variable","Z-Score")
  results$`Z-Score`<-as.numeric(as.character(results$`Z-Score`))

  results<-cbind(results,(2*pnorm(-abs(results$`Z-Score`))))
  colnames(results)<-c("Variable","Z-Score","P-value")
  return(results)
}
