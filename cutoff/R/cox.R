#' @title Significant Cutoff Value for Cox Regression
#'
#' @param data data
#' @param time name for time variable
#' @param y name for y, must be coded as 1 and 0. The outcome must be 1
#' @param x name for x
#' @param cut.numb number of cutoff points
#' @param n.per the least percentage of the smaller group comprised in all patients
#' @param y.per the least percentage of the smaller outcome patients comprised in each group
#' @param p.cut cutoff of p value, default is 0.05
#' @param strict logical. TRUE means significant differences for each group
#'     combination were considered. FALSE means considering for any combination
#' @param include direction of cutoff point. Any left letter of lower or upper
#' @param round digital. Default is 2
#' @param adjust numeric value, adjust methord for p value. 1, defaulted, represents Bonferroni. 2 represent formula given by Douglas G in 1994
#' @return a dataframe contains cutoff points value, subject numbers in each group,
#'     dumb variable, beta of regression and p value.
#' @export
#'
#' @examples
#' cox(data=mtcars,
#'     time = 'disp', y='am', x='wt',
#'     cut.numb=2,
#'     n.per=0.25,
#'     y.per=0.10)
#'
#' cox(data=mtcars,
#'     time = 'disp', y='am', x='wt',
#'     cut.numb=2,
#'     n.per=0.25,
#'     y.per=0.10,
#'     p.cut=0.05,
#'     strict=TRUE,
#'     include='low',
#'     round=2)
cox <- function(data,time,y,x,
                  cut.numb,
                  n.per,
                  y.per,
                  p.cut=0.05,
                  strict=TRUE,
                  include='low',
                  round=2,adjust=1){
    data=delet_na_df(data)
    if (length(unique(data[,y])) != 2){
        message('y must be 2 levels')
        return(NULL)
    }
    if (!any(min(unique(data[,y]))==0,max(unique(data[,y]))==1)){
        message('y must code 0 or 1')
        return(NULL)
    }
    res.cut.1=get_cutoff(regress = 'logit',
                         data,x,cut.numb,n.per,include,round,
                         y,y.per)
    for (i in 1:nrow(res.cut.1)) {
        if (i==1){
            res.cut=res.cut.1
            pair.filt=NULL
        }
        res.cut.i=res.cut[i,1:cut.numb]
        bt=cutit(data[,x],res.cut.i,include)
        #all pairs
        for (j in 1:cut.numb){
            if (j==1) df=NULL
            b=to.refer(bt,j)
            j.coef=suppressWarnings(coef(summary(survival::coxph(survival::Surv(data[,time],data[,y])~b)))[,-1])
            if (!is.matrix(j.coef)){
                j.coef=data.frame(t(j.coef),check.names = F)
                rownames(j.coef)='b2'
            }
            name.j=paste0(rownames(j.coef),collapse = '/')
            beta.j=paste0(digital(j.coef[,1],round),collapse = '/')
            p.j=paste0(digital(j.coef[,4],round),collapse = '/')
            p.value.coef=j.coef[,4]
            if (adjust==1){
                #Bonferroni correction
                p.adjust.j=paste0(digital(p.value.coef*nrow(res.cut.1),round),collapse = '/')
            }else if(adjust==2){
                #P_douglas1994
                p.adjust.j=paste0(digital(P_douglas1994(p.value.coef,n.per),round),collapse = '/')
            }
            sum.j=sum(j.coef[,4] <= p.cut)
            df=rbind(df,data.frame(dump=name.j,hr=beta.j,
                                   pvalue=p.j,sum=sum.j,
                                   p.adjust=p.adjust.j))
        }
        #judge
        if (strict){
            if (max(df[,4])==cut.numb){
                pair.filt=rbind(pair.filt,df[df[,4]==cut.numb,][1,])
            }else{
                res.cut[i,]=NA
            }
        }else{
            if (any(df[,4] >0 )){
                pair.filt=rbind(pair.filt,df[df[,4]==max(df[,4]),][1,])
            }else{
                res.cut[i,]=NA
            }
        }
    }
    res=na.omit(res.cut)
    if (nrow(res)==0){
        message('No results. Please lower n.per, y.per or cut.numb')
        return(NULL)
    }
    r=cbind(na.omit(res.cut),pair.filt[,-4])
    rownames(r)=NULL
    message('\n4: last combination: ',nrow(r),'\n')
    return(r)
}
