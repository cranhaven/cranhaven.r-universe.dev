#' @title Explore the Formula of Total Points and Linear Predictors
#' @description Explore the formula of total points and linear predictors by the best power.
#' @param nomogram results of nomogram() function in 'rms' package
#' @param power power can be automatically selected based on all R2 equal 1
#' @param digits default is 6
#' @importFrom stats as.formula lm predict
#' @return formula is the formula of total points and linear predictors. test is the R2 and RMSE which are used to test the fitted points. diff is difference between nomogram points and fitted points
#' @export
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
#' formula_lp(nomogram = nomo)
#' formula_lp(nomogram = nomo,power = 1)
#' formula_lp(nomogram = nomo,power = 3,digits=6)
formula_lp <- function(nomogram,power,digits=6){
    #total.points always appears in names of nomogram
    #total.points can only be changed in plot using points.label
    variable_part=nomogram["lp" == names(nomogram)]
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (test$R2<1) {
            power=power+1
            ######get 2 variables
            #1
            points=as.numeric(unlist(variable_part$lp[1]))
            #2
            lp=as.numeric(unlist(variable_part$lp[2]))
            ######calculate
            formu=as.formula(paste0('points~',
                                    inner_Add_Symbol(paste0("I(lp^",1:power,")"))))
            #regressiong
            reg=lm(formu)
            #formula
            lm.result=data.frame(t(reg$coefficients))
            rownames(lm.result)="linear predictor"
            colnames(lm.result)=c("b0",paste0("x^",1:power))
            #real,fit,diff
            fit=predict(reg)
            diff=points-fit
            real_fit=t(data.frame(nomogram=points,fit,diff))
            colnames(real_fit)=lp
            # test
            R2=suppressWarnings(summary(reg)$r.squared)
            RMSE=(mean(predict(reg)-points)^2)^(1/2)
            test=data.frame(R2,RMSE)
        }
    }else{
        #exist power
        if (power<1) stop("power must not be less 1")
        ######get 2 variables
        #1
        points=as.numeric(unlist(variable_part$lp[1]))
        #2
        lp=as.numeric(unlist(variable_part$lp[2]))
        ######calculate
        formu=as.formula(paste0('points~',
                                inner_Add_Symbol(paste0("I(lp^",1:power,")"))))
        #regressiong
        reg=lm(formu)
        #formula
        lm.result=data.frame(t(reg$coefficients))
        rownames(lm.result)="linear predictor"
        colnames(lm.result)=c("b0",paste0("x^",1:power))
        #real,fit,diff
        fit=predict(reg)
        diff=points-fit
        real_fit=t(data.frame(nomogram=points,fit,diff))
        colnames(real_fit)=lp
        # test
        R2=suppressWarnings(summary(reg)$r.squared)
        RMSE=(mean(predict(reg)-points)^2)^(1/2)
        test=data.frame(R2,RMSE)
    }
    rownames(test)="linear predictor"
    result=list(formula=round(lm.result,digits),
                test=round(test,digits),
                diff=round(real_fit,digits))
    return(result)
}

