#' @title Explore the Formula of Total Points and Raw Data
#' @description Explore the formula of total points and raw data by the best power.
#' @param nomogram results of nomogram() function in 'rms' package
#' @param power power can be automatically selected based on all R2 equal 1
#' @param digits default is 6
#' @return formula is the formula of total points and raw data. test is the R2 and RMSE which are used to test the fitted points. diff is difference between nomogram points and fitted points
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
#' formula_rd(nomogram = nomo)
#' formula_rd(nomogram = nomo,power = 1)
#' formula_rd(nomogram = nomo,power = 3,digits=6)
formula_rd <- function(nomogram,power,digits=6){
    #total.points always appears in names of nomogram
    #the positon of total.points is always above lp
    #total.points can only be changed in plot using points.label
    variable_part=nomogram[1:("total.points" %==% names(nomogram)-1)]
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (any(test$R2<1)) {
            power=power+1
            for (i in 1:length(variable_part)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_fit_list=list()
                }
                ######get each 3 variables
                nomo.i=nomogram[i]
                var.i=names(nomo.i)
                #change name of nomo.i to a, to get list points
                names(nomo.i)="a"
                points.i=nomo.i$a$points
                names(points.i)=NULL
                value.i=as.numeric(unlist(nomo.i$a[1]))
                if (any(is.na(value.i))) stop("please make sure that the variable",var.i," is a number in the regression")
                #reg
                formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
                reg=lm(as.formula(formu))
                fit.i=predict(reg)
                #diff
                diff=points.i-fit.i
                real_fit=t(data.frame(nomogram=points.i,fit=fit.i,diff))
                colnames(real_fit)=value.i
                real_fit=round(real_fit,digits)
                real_fit.i=list(real_fit)
                names(real_fit.i)=var.i
                real_fit_list=c(real_fit_list,real_fit.i)
                #test
                R2=suppressWarnings(summary(reg)$r.squared)
                RMSE=(mean(predict(reg)-points.i)^2)^(1/2)
                test.i=data.frame(R2,RMSE)
                test=rbind(test,test.i)
                #formula
                coef=reg$coefficients
                lm.result=data.frame(t(coef))
                rownames(lm.result)=var.i
                colnames(lm.result)=c("b0",paste0("x^",1:power))
                nomo.reslut=rbind(nomo.reslut,lm.result)
            }
        }
    }else{
        #exist power
        if (power<1) stop("power must not be less 1")
        for (i in 1:length(variable_part)) {
            if (i==1){
                nomo.reslut=data.frame()
                test=data.frame()
                real_fit_list=list()
            }
            ##get each 3 variables
            nomo.i=nomogram[i]
            var.i=names(nomo.i)
            #change name of nomo.i to a, to get list points
            names(nomo.i)="a"
            points.i=nomo.i$a$points
            names(points.i)=NULL
            value.i=as.numeric(unlist(nomo.i$a[1]))
            if (any(is.na(value.i))) stop("please make sure that the variable",var.i," is a number in the regression")
            #reg
            formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
            reg=lm(as.formula(formu))
            #diff
            fit.i=predict(reg)
            diff=points.i-fit.i
            real_fit=t(data.frame(nomogram=points.i,fit=fit.i,diff))
            colnames(real_fit)=value.i
            #because we can not change digits of list, we change list units
            real_fit=round(real_fit,digits)
            real_fit.i=list(real_fit)
            names(real_fit.i)=var.i
            real_fit_list=c(real_fit_list,real_fit.i)
            #test
            R2=suppressWarnings(summary(reg)$r.squared)
            RMSE=(mean(predict(reg)-points.i)^2)^(1/2)
            test.i=data.frame(R2,RMSE)
            test=rbind(test,test.i)
            #formula
            coef=reg$coefficients
            lm.result=data.frame(t(coef))
            rownames(lm.result)=var.i
            colnames(lm.result)=c("b0",paste0("x^",1:power))
            nomo.reslut=rbind(nomo.reslut,lm.result)
        }
    }
    rownames(test)=rownames(nomo.reslut)
    result=list(formula=round(nomo.reslut,digits),
                test=round(test,digits),
                diff=real_fit_list)
    return(result)
    }
