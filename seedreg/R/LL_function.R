#' Analysis: Logistic regression
#'
#' Logistic models with three (LL.3) or four (LL.4) continuous data parameters. This model was extracted from the drc package and adapted for temperature analysis in seed germination.
#' @param trat Numerical or complex vector with treatments
#' @param resp Numerical vector containing the response of the experiment.
#' @param npar Number of model parameters
#' @param error Error bar (It can be SE - \emph{default}, SD or FALSE)
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab Treatments name (Accepts the \emph{expression}() function)
#' @param theme ggplot2 theme (\emph{default} is theme_bw())
#' @param legend.position Legend position (\emph{default} is c(0.3,0.8))
#' @param r2 Coefficient of determination of the mean or all values (\emph{default} is all)
#' @param cardinal Defines the value of y considered extreme (\emph{default} considers 0 germination)
#' @param scale Sets x scale (\emph{default} is none, can be "log")
#' @param width.bar Bar width
#' @param textsize Font size
#' @param pointsize shape size
#' @param linesize line size
#' @param pointshape format point (\emph{default} is 21)
#' @param font.family Font family (\emph{default} is sans)
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
#'   \item{\code{r-squared}}{Determination coefficient}
#'   \item{\code{RMSE}}{Root mean square error}
#'   \item{\code{grafico}}{Graph in ggplot2 with equation}
#'   }
#' @details The three-parameter log-logistic function with lower limit 0 is
#' \deqn{f(x) = 0 + \frac{d}{1+\exp(b(\log(x)-\log(e)))}}
#' The four-parameter log-logistic function is given by the expression
#' \deqn{f(x) = c + \frac{d-c}{1+\exp(b(\log(x)-\log(e)))}}
#' The function is symmetric about the inflection point (e).
#' @note if the maximum predicted value is equal to the maximum x, the curve does not have a maximum point within the studied range. If the minimum value is less than the lowest point studied, disregard the value.
#' @author Model imported from the drc package (Ritz et al., 2016)
#' @author Gabriel Danilo Shimizu
#' @author Leandro Simoes Azeredo Goncalves
#' @references Seber, G. A. F. and Wild, C. J (1989) Nonlinear Regression, New York: Wiley and Sons (p. 330).
#' @references Ritz, C.; Strebig, J.C.; Ritz, M.C. Package ‘drc’. Creative Commons: Mountain View, CA, USA, 2016.
#' @export
#'
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#'
#' #================================
#' # Germination
#' #================================
#' LL_model(trat,germ)
#'
#' #================================
#' # Germination speed
#' #================================
#' LL_model(trat, vel, ylab=expression("v"~(dias^-1)))


LL_model=function(trat,
                  resp,
                  npar="LL.3",
                  error="SE",
                  ylab="Germination (%)",
                  xlab=expression("Temperature ("^"o"*"C)"),
                  theme=theme_classic(),
                  legend.position="top",
                  cardinal=0,
                  r2="all",
                  width.bar=NA,
                  scale="none",
                  textsize=12,
                  pointsize=4.5,
                  linesize=0.8,
                  pointshape=21,
                  font.family="sans"){
  requireNamespace("drc")
  requireNamespace("crayon")
  requireNamespace("ggplot2")
  ymean=tapply(resp,trat,mean)
  if(is.na(width.bar)==TRUE){width.bar=0.01*mean(trat)}
  if(error=="SE"){ysd=tapply(resp,trat,sd)/sqrt(tapply(resp,trat,length))}
  if(error=="SD"){ysd=tapply(resp,trat,sd)}
  if(error=="FALSE"){ysd=0}
  desvio=ysd
  xmean=tapply(trat,trat,mean)
  if(npar=="LL.3"){mod=drm(resp~trat,fct=LL.3())
  coef=summary(mod)
  b=coef$coefficients[,1][1]
  d=coef$coefficients[,1][2]
  e=coef$coefficients[,1][3]
  if(r2=="all"){r2=cor(resp, fitted(mod))^2}
  if(r2=="mean"){r2=cor(ymean, predict(mod,newdata=data.frame(trat=unique(trat))))^2}
  r2=floor(r2*100)/100
  equation=sprintf("~~~y==frac(%0.3e, 1+e^(%0.3e*(log(x)-log(%0.3e)))) ~~~~~ italic(R^2) == %0.2f",
                   d,b,e,r2)
  xp=seq(min(trat),max(trat),length.out = 1000)
  preditos=data.frame(x=xp,
                      y=predict(mod,newdata = data.frame(trat=xp)))
  }
  if(npar=="LL.4"){mod=drm(resp~trat,fct=LL.4())
  coef=summary(mod)
  b=coef$coefficients[,1][1]
  c=coef$coefficients[,1][2]
  d=coef$coefficients[,1][3]
  e=coef$coefficients[,1][4]
  if(r2=="all"){r2=cor(resp, fitted(mod))^2}
  if(r2=="mean"){r2=cor(ymean, predict(mod,newdata=data.frame(trat=unique(trat))))^2}
  r2=floor(r2*100)/100
  equation=sprintf("~~~y == %0.3e + frac(%0.3e %s %0.3e, 1+e^(%0.3e*(log(x)-log(%0.3e)))) ~~~~~ italic(R^2) == %0.2f",
                   c,
                   d,
                   ifelse(c >= 0, "+", "-"),
                   abs(c),
                   b,
                   e,
                   r2)
  xp=seq(min(trat),max(trat),length.out = 1000)
  preditos=data.frame(x=xp,
                      y=predict(mod,newdata = data.frame(trat=xp)))}
  predesp=predict(mod)
  predobs=resp
  rmse=sqrt(mean((predesp-predobs)^2))
  x=preditos$x
  y=preditos$y
  s=equation
  data=data.frame(xmean,ymean)
  data1=data.frame(trat=xmean,resp=ymean)
  graph=ggplot(data,aes(x=xmean,y=ymean))
  if(error!="FALSE"){graph=graph+geom_errorbar(aes(ymin=ymean-ysd,ymax=ymean+ysd),
                                               width=width.bar,size=linesize)}
  graph=graph+
    geom_point(aes(color="black"),size=pointsize,shape=pointshape,fill="gray")+
    theme+
    geom_line(data=preditos,aes(x=x,
                                y=y,color="black"),size=linesize)+
    scale_color_manual(name="",values=1,label=parse(text = equation))+
    theme(axis.text = element_text(size=textsize,color="black",family = font.family),
          axis.title = element_text(family = font.family),
          legend.position = legend.position,
          legend.text = element_text(size=textsize,family = font.family),
          legend.direction = "vertical",
          legend.text.align = 0,
          legend.justification = 0)+
    ylab(ylab)+xlab(xlab)
  if(scale=="log"){graph=graph+scale_x_log10()}
  temp1=seq(min(trat),max(trat),length.out=10000)
  result=predict(mod,newdata = data.frame(temp=temp1),type="response")
  maximo=temp1[which.max(result)]
  respmax=result[which.max(result)]
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>maximo]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<maximo]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}
  aic=AIC(mod)
  bic=BIC(mod)
  graphs=data.frame("Parameter"=c("Optimum temperature",
                                  "Optimum temperature response",
                                  "Minimal temperature",
                                  "Minimal temperature response",
                                  "Predicted maximum basal value",
                                  "Predicted minimum basal value",
                                  "AIC","BIC","r-squared","RMSE"),
                    "values"=round(c(maximo,
                               respmax,
                               mini,
                               respmin,
                               maxl,
                               minimo,
                               aic,bic,r2,rmse),7))
  models=data.frame(coef$coefficients)
  models$Sig=ifelse(models$p.value>0.05,"ns",ifelse(models$p.value<0.01,"**","*"))
  colnames(models)=c("Estimate","Std Error","t value","P-value","")
  graficos=list("Coefficients"=models,
                "values"=graphs,
                graph)
  print(graficos)
}
