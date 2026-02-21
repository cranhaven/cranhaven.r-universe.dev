#' @title Perform the multivariate Cox regression
#' @description The function `get_MultivariateCox_result` uses to perform multivariate Cox regression analysis on the cancer-specific dysregulated signaling pathways.
#' @param DE_path_sur A binary metadata table containing sample survival status and survival time.Note that the column names of survival time and survival status must be "survival" and "event".
#' @importFrom survival coxph
#' @importFrom survival Surv
#' @return Return the multivariate Cox regression results of cancer-specific dysregulated signaling pathways.
#' @export
#' @examples
#' #Load the data.
#' data(path_cox_data)
#' #perform function `get_MultivariateCox_result`.
#' res<-get_MultivariateCox_result(path_cox_data)




get_MultivariateCox_result<-function(DE_path_sur){
  path_name<-cbind(colnames(DE_path_sur),paste0("a",1:length(colnames(DE_path_sur))))
  colnames(DE_path_sur)[1:(dim(DE_path_sur)[2]-2)]<-path_name[1:(dim(path_name)[1]-2),2]
  result<-coxph(Surv(survival,event)~ .,
                data=DE_path_sur)
  x<-summary(result)
  beta<-as.matrix(x$coef[,1])
  pvalue<-as.matrix(x$coefficients[,5])
  HR<-as.matrix(x$coefficients[,2])
  HR.95L<-x$conf.int[,3]
  HR.95H<-x$conf.int[,4]
  Ucox_mul_res<-cbind(beta,pvalue,HR,HR.95L,HR.95H)
  colnames(Ucox_mul_res)<-c("beta","pvalue", "HR", "HR.95L","HR.95H")
  rownames(Ucox_mul_res)<-path_name[match(rownames(Ucox_mul_res),path_name[,2]),1]
  return(Ucox_mul_res)}
