#' @title Significant Cutoff Value for Logrank Analysis
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
#'
#' @return a dataframe contains cutoff points value, subject numbers in each group,
#'     dumb variable, beta of regression and p value.
#' @export
#'
#' @examples
#' logrank(data=mtcars,
#'         time = 'disp',y='am', x='wt',
#'         cut.numb=2,
#'         n.per=0.25,
#'         y.per=0.10)
#'
#' logrank(data=mtcars,
#'         time = 'disp',y='am', x='wt',
#'         cut.numb=2,
#'         n.per=0.25,
#'         y.per=0.10,
#'         p.cut=0.05,
#'         strict=TRUE,
#'         include='low',
#'         round=2)
logrank <- function(data,time,y,x,
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
    res.cut=get_cutoff(regress = 'logit',
                         data,x,cut.numb,n.per,include,round,
                         y,y.per)
    for (i in 1:nrow(res.cut)) {
        if (i==1){
            pvalue=NULL
            p.adjust=NULL
        }
        res.cut.i=res.cut[i,1:cut.numb]
        bt=cutit(data[,x],res.cut.i,include)
        p.i=survdiff_p.value(survival::survdiff(survival::Surv(data[,time],data[,y])~bt))
        pvalue=c(pvalue,p.i)
        if (adjust==1){
            #Bonferroni correction
            p.adjust.j=paste0(digital(p.i*nrow(res.cut),round),collapse = '/')
            p.adjust=c(p.adjust,p.adjust.j)
        }else if(adjust==2){
            #P_douglas1994
            p.adjust.j=paste0(digital(P_douglas1994(p.i,n.per),round),collapse = '/')
            p.adjust=c(p.adjust,p.adjust.j)
        }
    }
    res.cut=cbind(res.cut,pvalue=digital(pvalue,round),p.adjust=digital(p.adjust,round))
    res=res.cut[pvalue<=p.cut,]
    if (nrow(res)==0){
        message('No results. Please lower n.per, y.per or cut.numb')
        return(NULL)
    }
    rownames(res)=NULL
    message('\n4: last combination: ',nrow(res),'\n')
    return(res)
}
