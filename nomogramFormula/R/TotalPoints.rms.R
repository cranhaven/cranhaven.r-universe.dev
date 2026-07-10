#' Caculate Total Points for nomogram Picture
#' @description Compared with points_cal() command, TotalPoints.rms() is suit 
#'     for more complexed condition. Since this command is based on formula 
#'     from 'rms' package, it may be also more accurate. However, formula for 
#'     each variable can not be caculated.
#' @param rd raw data
#' @param fit regression result in 'rma' package
#' @param nom nomoram() command result
#' @param kint number of intercept. Default is to use fit$interceptRef if it 
#'     exists, or 1.
#' 
#' @return a dataframe contains rawdata and total points
#' @export
#'
#' @examples
#' library(rms)
#' n <- 1000    
#' set.seed(17) 
#' d <- data.frame(age = rnorm(n, 50, 10),
#'                 blood.pressure = rnorm(n, 120, 15),
#'                 cholesterol = rnorm(n, 200, 25),
#'                 sex = factor(sample(c('female','male'), n,TRUE)))
#' 
#' d <- upData(d,
#'             L = .4*(sex=='male') + .045*(age-50) +
#'               (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male')),
#'             y = ifelse(runif(n) < plogis(L), 1, 0))
#' 
#' ddist <- datadist(d); options(datadist='ddist')
#' 
#' f <- lrm(y ~ lsp(age,50) + sex * rcs(cholesterol, 4) + blood.pressure,
#'          data=d)
#' nom <- nomogram(f)
#' TotalPoints.rms(rd = d,fit = f,nom = nom)
TotalPoints.rms<-function(rd,fit,nom,kint=NULL){
  if (is.null(kint)){
    kint=fit$interceptRef
  }
  #xbeta
  xx=predict(object = fit, 
             newdata = rd, 
             type = "terms", 
             center.terms = FALSE, 
             se.fit = FALSE, 
             kint = kint)
  nom.list=unclass(nom)
  nom.attr=attr(nom.list,'info')
  sc=nom.attr$sc
  R=nom.attr$R
  k=do::`%==%`("total.points",names(nom))-1
  for (i in 1:k) {
    if (i==1) {
      df=NULL
      coln=NULL
    }
    nom.efname=attr(nom.list[[i]],'info')$effect.name
    xx.ii=xx[,nom.efname]
    xt=if(length(nom.efname)>1){
      rowSums(xx.ii)
    }else{
      xx.ii
    }
    #(xt-R[1,nom])*sc is the formula
    score=(xt-R[1,attr(nom.list[[i]],'info')$predictor])*sc
    if (i==1){
      df=score
    }else{
      df=cbind(df,score)
    }
    coln=c(coln,names(nom.list[[i]])[[1]])
  }
  df2=df[,!duplicated(coln)]
  class(df2)
  if (is.data.frame(df2) || is.matrix(df2)){
    ts=rowSums(df2)
  }else{
    if (nrow(rd)==1){
      ts=sum(df2)
    }else{
      ts=df2
    }
  }
  cbind(rd[,names(fit$Design$units)],`total points`=ts)
}

