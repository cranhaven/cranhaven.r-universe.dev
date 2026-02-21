#' @title Perform the univariate Cox regression analysis.
#' @description The function `get_univarCox_result` uses to perform the univariate Cox regression analysis.
#' @param DE_path_sur A binary metadata table containing  survival status and survival time of each sample.Note that the column names of survival time and survival status must be "survival" and "event"
#' @importFrom clusterProfiler bitr
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @importFrom survival coxph
#' @importFrom survival Surv
#' @importFrom stats as.formula
#' @return Return a data frame, the univariate Cox regression analysis results.
#' @export
#' @examples
#' #get path of the mutation annotation file.
#' data(path_cox_data)
#' #perform function `get_univarCox_result`.
#' res<-get_univarCox_result(path_cox_data)


get_univarCox_result<-function(DE_path_sur){
  path_name<-cbind(colnames(DE_path_sur),paste0("a",1:length(colnames(DE_path_sur))))
  colnames(DE_path_sur)[1:(dim(DE_path_sur)[2]-2)]<-path_name[1:(dim(path_name)[1]-2),2]
  covariates<-colnames(DE_path_sur)[1:(length(DE_path_sur[1,])-2)]
  univ_formulas <- sapply(covariates,function(x) as.formula(paste('Surv(survival, event) ~', x)))
  univ_models <- lapply( univ_formulas, function(x){coxph(x, data =DE_path_sur)})
  univ_results <- lapply(univ_models,
                         function(x){
                           x <- summary(x)
                           p.value<-signif(x$wald["pvalue"], digits=2)
                           wald.test<-signif(x$wald["test"], digits=2)
                           beta<-signif(x$coef[1], digits=2);
                           HR <-signif(x$coef[2], digits=2);
                           HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                           HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)

                           res<-c(beta,HR,HR.confint.lower,HR.confint.upper, p.value)
                           names(res)<-c("beta","HR", "HR.95L", "HR.95H","p.value")
                           return(res)
                         })
  res <- t(as.data.frame(univ_results, check.names = FALSE))
  result<-as.data.frame(res)
  rownames(result)<-path_name[match(rownames(result),path_name[,2]),1]
  return(result)
}
