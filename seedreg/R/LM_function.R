#' Analysis: Linear regression graph
#'
#' @author Gabriel Danilo Shimizu
#' @author Leandro Simoes Azeredo Goncalves
#'
#' @description Linear regression analysis of an experiment with a quantitative factor or isolated effect of a quantitative factor
#' @param trat Numerical vector with treatments (Declare as numeric)
#' @param resp Numerical vector containing the response of the experiment.
#' @param ylab Dependent variable name (Accepts the \emph{expression}() function)
#' @param xlab Independent variable name (Accepts the \emph{expression}() function)
#' @param grau Degree of the polynomial (1,2 or 3)
#' @param theme ggplot2 theme (\emph{default} is theme_classic())
#' @param error Error bar (It can be SE - \emph{default}, SD or FALSE)
#' @param legend.position Legend position (\emph{default} is "top")
#' @param cardinal Defines the value of y considered extreme (\emph{default} considers 0 germination)
#' @param scale Sets x scale (\emph{default} is none, can be "log")
#' @param width.bar Bar width
#' @param textsize Font size
#' @param pointsize shape size
#' @param linesize line size
#' @param pointshape format point (\emph{default} is 21)
#' @param font.family Font family (\emph{default} is sans)
#' @keywords regression linear
#' @note If the maximum predicted value is equal to the maximum x, the curve does not have a maximum point within the studied range. If the minimum value is less than the lowest point studied, disregard the value.
#' @return
#'   \describe{
#'   \item{\code{Coefficients}}{Coefficients and their p values}
#'   \item{\code{Optimum temperature}}{Optimum temperature (equivalent to the maximum point)}
#'   \item{\code{Optimum temperature response}}{Response at the optimal temperature (equivalent to the maximum point)}
#'   \item{\code{Minimal temperature}}{Temperature that has the lowest response}
#'   \item{\code{Minimal temperature response}}{Lowest predicted response}
#'   \item{\code{Predicted maximum basal value}}{Lower basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{Predicted minimum basal value}}{Upper basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{AIC}}{Akaike information criterion}
#'   \item{\code{BIC}}{Bayesian Inference Criterion}
#'   \item{\code{VIF}}{Variance inflation factor (multicollinearity)}
#'   \item{\code{r-squared}}{Determination coefficient}
#'   \item{\code{RMSE}}{Root mean square error}
#'   \item{\code{grafico}}{Graph in ggplot2 with equation}
#'   }
#' @export
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#'
#' #================================
#' # Germination
#' #================================
#' LM_model(trat,germ, grau=3)
#'
#' #================================
#' # Germination speed
#' #================================
#' LM_model(trat, vel, grau=3,
#' ylab=expression("v"~(dias^-1)))


LM_model=function(trat,
                  resp,
                  ylab="Germination (%)",
                  error="SE",
                  xlab=expression("Temperature ("^"o"*"C)"),
                  grau=NA,
                  theme=theme_classic(),
                  cardinal=0,
                  legend.position="top",
                  width.bar=NA,
                  scale="none",
                  textsize=12,
                  pointsize=4.5,
                  linesize=0.8,
                  pointshape=21,
                  font.family="sans"){
  requireNamespace("ggplot2")
  requireNamespace("crayon")
  if(is.na(width.bar)==TRUE){width.bar=0.01*mean(trat)}
  if(is.na(grau)==TRUE){grau=1}
  dados=data.frame(trat,resp)
  medias=c()
  dose=tapply(trat, trat, mean)
  mod=c()
  mod1=c()
  mod2=c()
  mod05=c()

  modm=c()
  mod1m=c()
  mod2m=c()
  mod05m=c()

  text1=c()
  text2=c()
  text3=c()
  text05=c()

  mods=c()
  mod1s=c()
  mod2s=c()
  mod05s=c()

  media=tapply(resp, trat, mean)
  if(error=="SE"){desvio=tapply(resp,trat,sd)/sqrt(tapply(resp,trat,length))}
  if(error=="SD"){desvio=tapply(resp,trat,sd)}
  if(error=="FALSE"){desvio=0}
  dose=tapply(trat, trat, mean)
  moda=lm(resp~trat)
  mod1a=lm(resp~trat+I(trat^2))
  mod2a=lm(resp~trat+I(trat^2)+I(trat^3))
  mod05a=lm(resp~trat+I(trat^0.5))

  mods=summary(moda)$coefficients
  mod1s=summary(mod1a)$coefficients
  mod2s=summary(mod2a)$coefficients
  mod05s=summary(mod05a)$coefficients

  modm=lm(media~dose)
  mod1m=lm(media~dose+I(dose^2))
  mod2m=lm(media~dose+I(dose^2)+I(dose^3))
  mod05m=lm(media~dose+I(dose^0.5))

  if(grau=="1"){r2=round(summary(modm)$r.squared,2)}
  if(grau=="2"){r2=round(summary(mod1m)$r.squared,2)}
  if(grau=="3"){r2=round(summary(mod2m)$r.squared,2)}
  if(grau=="0.5"){r2=round(summary(mod05m)$r.squared,2)}
  if(grau=="1"){s1=s <- sprintf("~~~y == %e %s %e*x ~~~~~ italic(R^2) == %0.2f",
                                coef(moda)[1],
                                ifelse(coef(moda)[2] >= 0, "+", "-"),
                                abs(coef(moda)[2]),
                                r2)}
  if(grau=="2"){s2=s <- sprintf("~~~y == %e %s %e * x %s %e * x^2 ~~~~~ italic(R^2) ==  %0.2f",
                                coef(mod1a)[1],
                                ifelse(coef(mod1a)[2] >= 0, "+", "-"),
                                abs(coef(mod1a)[2]),
                                ifelse(coef(mod1a)[3] >= 0, "+", "-"),
                                abs(coef(mod1a)[3]),
                                r2)}
  if(grau=="3"){s3=s <- sprintf("~~~y == %e %s %e * x %s %e * x^2 %s %0.e * x^3 ~~~~~ italic(R^2) == %0.2f",
                                coef(mod2a)[1],
                                ifelse(coef(mod2a)[2] >= 0, "+", "-"),
                                abs(coef(mod2a)[2]),
                                ifelse(coef(mod2a)[3] >= 0, "+", "-"),
                                abs(coef(mod2a)[3]),
                                ifelse(coef(mod2a)[4] >= 0, "+", "-"),
                                abs(coef(mod2a)[4]),
                                r2)}
  if(grau=="0.5"){s05=s <- sprintf("~~~y == %e %s %e * x %s %e * x^0.5 ~~~~~ italic(R^2) ==  %0.2f",
                                coef(mod05a)[1],
                                ifelse(coef(mod05a)[2] >= 0, "+", "-"),
                                abs(coef(mod05a)[2]),
                                ifelse(coef(mod05a)[3] >= 0, "+", "-"),
                                abs(coef(mod05a)[3]),
                                r2)}
  data1=data.frame(trat,resp)
  data1=data.frame(trat=as.numeric(names(media)),
                   resp=media,
                   desvio)
  grafico=ggplot(data1,aes(x=trat,y=resp))+
    geom_errorbar(aes(ymin=resp-desvio, ymax=resp+desvio),width=width.bar,size=linesize)+
    geom_point(aes(fill=as.factor(rep(1,length(resp)))),na.rm=T,
               size=pointsize,color="black",shape=pointshape)+
    theme+ylab(ylab)+xlab(xlab)
  if(grau=="1"){grafico=grafico+geom_smooth(method = "lm",se=F, na.rm=T, formula = y~x,size=linesize,color="black")}
  if(grau=="2"){grafico=grafico+geom_smooth(method = "lm",se=F, na.rm=T, formula = y~x+I(x^2),size=linesize,color="black")}
  if(grau=="3"){grafico=grafico+geom_smooth(method = "lm",se=F, na.rm=T, formula = y~x+I(x^2)+I(x^3),size=linesize,color="black")}
  if(grau=="0.5"){grafico=grafico+geom_smooth(method = "lm",se=F, na.rm=T, formula = y~x+I(x^0.5),size=linesize,color="black")}
  if(grau=="1"){grafico=grafico+
    scale_fill_manual(values="gray",label=c(parse(text=s1)),name="")}
  if(grau=="2"){grafico=grafico+
      scale_fill_manual(values="gray",label=c(parse(text=s2)),name="")}
  if(grau=="3"){grafico=grafico+
      scale_fill_manual(values="gray",label=c(parse(text=s3)),name="")}
  if(grau=="0.5"){grafico=grafico+
    scale_fill_manual(values="gray",label=c(parse(text=s05)),name="")}

  grafico=grafico+
    theme(text = element_text(size=textsize,color="black",family = font.family),
          axis.text = element_text(size=textsize,color="black",family = font.family),
          axis.title = element_text(size=textsize,color="black",family = font.family),
          legend.position = legend.position,
          legend.text=element_text(size=textsize,family = font.family),
          legend.direction = "vertical",
          legend.text.align = 0,
          legend.justification = 0)
  if(scale=="log"){grafico=grafico+scale_x_log10()}
  moda=lm(resp~trat)
  mod1a=lm(resp~trat+I(trat^2))
  mod2a=lm(resp~trat+I(trat^2)+I(trat^3))
  mod05a=lm(resp~trat+I(trat^0.5))

  if(grau=="1"){
  models=mods
  r2=summary(modm)$r.squared
  aic=AIC(moda)
  bic=BIC(moda)
  vif=NA
  predesp=predict(moda)
  predobs=resp
  rmse=sqrt(mean((predesp-predobs)^2))

  temp1=seq(min(trat),max(trat),length.out=10000)
  result=predict(moda,newdata = data.frame(trat=temp1),type="response")
  maximo=temp1[which.max(result)]
  respmax=result[which.max(result)]
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>maximo]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<maximo]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}}

  if(grau=="2"){
  models=mod1s
  r2=summary(mod1m)$r.squared
  aic=AIC(mod1a)
  bic=BIC(mod1a)
  vif=car::vif(mod1a)
  predesp=predict(mod1a)
  predobs=resp
  rmse=sqrt(mean((predesp-predobs)^2))

  temp1=seq(min(trat),max(trat),length.out=10000)
  result=predict(mod1a,newdata = data.frame(trat=temp1),type="response")
  maximo=temp1[which.max(result)]
  respmax=result[which.max(result)]
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>maximo]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<maximo]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}}


  if(grau=="3"){
  models=mod2s
  r2=summary(mod2m)$r.squared
  aic=AIC(mod2a)
  bic=BIC(mod2a)
  vif=car::vif(mod2a)
  predesp=predict(mod2a)
  predobs=resp
  rmse=sqrt(mean((predesp-predobs)^2))

  temp1=seq(min(trat),max(trat),length.out=10000)
  result=predict(mod2a,newdata = data.frame(trat=temp1),type="response")
  maximo=temp1[which.max(result)]
  respmax=result[which.max(result)]
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>maximo]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<maximo]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}}

  if(grau=="0.5"){
    models=mod05s
    r2=summary(mod05m)$r.squared
    aic=AIC(mod05a)
    bic=BIC(mod05a)
    vif=car::vif(mod05a)
    predesp=predict(mod05a)
    predobs=resp
    rmse=sqrt(mean((predesp-predobs)^2))
    temp1=seq(min(trat),max(trat),length.out=10000)
    result=predict(mod05a,newdata = data.frame(trat=temp1),type="response")
    maximo=temp1[which.max(result)]
    respmax=result[which.max(result)]
    mini=temp1[which.min(result)]
    respmin=result[which.min(result)]

    result1=round(result,0)
    fa=temp1[result1<=cardinal & temp1>maximo]
    if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
    fb=temp1[result1<=cardinal & temp1<maximo]
    if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}}
  cat("\n")
  graphs=data.frame("Parameter"=c("Optimum temperature",
                                  "Optimum temperature response",
                                  "Minimal temperature",
                                  "Minimal temperature response",
                                  "Predicted maximum basal value",
                           "Predicted minimum basal value",
                           "AIC","BIC","r-squared","RMSE"),
             "values"=c(maximo,
                        respmax,
                        mini,
                        respmin,
                        maxl,
                        minimo,
                        aic,
                        bic,
                        r2,
                        rmse))
  models=data.frame(models)
  models$Sig=ifelse(models$Pr...t..>0.05,"ns",ifelse(models$Pr...t..<0.01,"**","*"))
  colnames(models)=c("Estimate","Std Error","t value","P-value","")
  graficos=list("Coefficients"=models,
                "test"=graphs,
                grafico)
  print(graficos)
}
