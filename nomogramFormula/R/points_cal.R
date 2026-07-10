#' Calculate Total Points
#' @description Calculate total points.
#' @param formula the formula of total points with raw data or linear predictors
#' @param rd raw data, which cannot have missing values
#' @param lp linear predictors
#' @param digits default is 6
#'
#' @return total Points
#' @export
#'
#' @examples
#' library(rms)  # needed for nomogram
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
#' ddist <- datadist(df)
#' oldoption <- options(datadist='ddist')
#' f <- cph(formula(Surv(time,death)~sex+age+weight),data=df,
#'          x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
#' surv <- Survival(f)
#' nomo <- nomogram(f,
#'                  lp=TRUE,
#'                  fun=list(function(x) surv(365,x),
#'                           function(x) surv(365*2,x)),
#'                  funlabel=c("1-Year Survival Prob",
#'                             "2-Year Survival Prob"))
#' options(oldoption)
#' #get the formula by the best power using formula_lp
#' results <- formula_lp(nomo)
#' points_cal(formula = results$formula,lp=f$linear.predictors)
#' 
#' #get the formula by the best power using formula_rd
#' results <- formula_rd(nomogram = nomo)
#' points_cal(formula = results$formula,rd=df)
points_cal <- function(formula,rd,lp,digits=6){
    #data
    if (!missing(rd)){
        if (any(is.na(rd))) stop("data must not have NAs.")
        nomoF.matrix=as.matrix(formula)
        data1=rd[,rownames(nomoF.matrix)]
        for (i in 1:ncol(data1)) {
            if (i==1) score=rep(0,nrow(data1))
            beta_for_x=nomoF.matrix[i,]
            x_for_beta=data1[,i]
            if (is.factor(x_for_beta)){
                x_for_beta=as.numeric(as.character(x_for_beta))
            }
            for (j in 1:length(nomoF.matrix[i,])) {
                if (is.na(beta_for_x[j])) next(j)
                if (j==1){
                    each.var.score=(beta_for_x[j])*(x_for_beta^(j-1))
                }else{
                    each.var.score=each.var.score+(beta_for_x[j])*(x_for_beta^(j-1))
                }
            }
            score=score+each.var.score
        }
        
        return(score)
    #lp
    }else if (!missing(lp)){
        for (j in 1:ncol(formula)) {
            if (is.na(formula[1,j])) next(j)
            if (j==1){
                score=(formula[1,j])*(lp^(j-1))
            }else{
                score=score+(formula[1,j])*(lp^(j-1))
            }
        }
        score=round(score,digits)
        return(score)
    }
}
