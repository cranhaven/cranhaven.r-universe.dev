#' Calculate Probabilities
#' @description Use Survival() function from 'rms' pacakge to calculate probabilities after lrm(), cph() or psm() regression. If you want to calculate lrm() probabilities, please leave linear.predictors be TRUE and times be missing. If you want to calculate cph() probabilites, please leave both linear.predictors and surv be TRUE.
#' @param reg regression results after lrm(), cph() or psm() in 'rms' package.
#' @param times if you want to calculate probabilities for lrm() function, please left times missing.
#' @param q quantile, for example 0.5
#' @param lp linear predictors
#' @return lieaner predictors and probabilities as a dataframe
#' @export
#'
#' @examples
#' set.seed(2018)
#' n <-2019
#' age <- rnorm(n,60,20)
#' sex <- factor(sample(c('female','male'),n,TRUE))
#' sex <- as.numeric(sex)
#' weight <- sample(50:100,n,replace = TRUE)
#' time <- sample(50:800,n,replace = TRUE)
#' units(time)="day"
#' death <- sample(c(1,0,0),n,replace = TRUE)
#' df <- data.frame(time,death,age,sex,weight)
#' 
#' library(rms) #needed for lrm(), cph() and psm()
#' ddist <- datadist(df)
#' oldoption <- options(datadist='ddist')
#' 
#' # lrm() function
#' f <- lrm(death~sex+age+weight,data=df,
#'          linear.predictors = TRUE)
#' head(prob_cal(reg = f))
#' 
#' # cph() function
#' f <- cph(Surv(time,death)~sex+age+weight,data=df,
#'          linear.predictors=TRUE,surv=TRUE)
#' head(prob_cal(reg = f,times = c(365,365*2)))
#' 
#' # psm() function
#' f <- psm(Surv(time,death)~sex+age+weight,data=df)
#' head(prob_cal(reg = f,times = c(365,365*2)))
prob_cal <- function(reg,times,q,lp){
    if (!'linear.predictors' %in% names(reg)){
        stop('linear preditors must be included in reg') 
    }
    if (missing(lp)){
        linear.predictors=reg$linear.predictors
    }else{
        linear.predictors=lp
    }
    if (all(missing(times),missing(q))){
        #for logistic ratio
        result=1/(1+exp(-linear.predictors))
        result=data.frame(result)
        colnames(result)='Prob'
    }else if (all(!missing(times),missing(q))){
        #for survival regression times
        surv <- rms::Survival(reg)
        for (i in 1:length(times)) {
            if (i==1){
                result=surv(times[i],linear.predictors)
                result=data.frame(result)
                colnames(result)=paste0('P',times[i])
            }else{
                result.i=surv(times[i],linear.predictors)
                result.i=data.frame(result.i)
                colnames(result.i)=paste0('P',times[i])
                result=cbind(result,result.i)
            }
        }
    }else if (all(missing(times),!missing(q))){
        #for survival regression quantile
        qtl <- Hmisc::Quantile(reg)
        for (i in 1:length(q)) {
            if (i==1){
                result=qtl(q[i],linear.predictors)
                result=data.frame(result)
                colnames(result)=paste0('P',q[i])
            }else{
                result.i=qtl(q[i],linear.predictors)
                result.i=data.frame(result.i)
                colnames(result.i)=paste0('P',q[i])
                result=cbind(result,result.i)
            }
        }
    }
    result=cbind(linear.predictors,result)
    return(result)
}
