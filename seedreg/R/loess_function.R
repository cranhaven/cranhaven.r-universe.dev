#' Analysis: loess regression
#'
#' Fit a polynomial surface determined by one or more numerical predictors, using local fitting.
#' @param trat Numerical or complex vector with treatments
#' @param resp Numerical vector containing the response of the experiment.
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab treatments name (Accepts the \emph{expression}() function)
#' @param theme ggplot2 theme (\emph{default} is theme_bw())
#' @param error Error bar (It can be SE - \emph{default}, SD or FALSE)
#' @param legend.position legend position (\emph{default} is c(0.3,0.8))
#' @param cardinal defines the value of y considered extreme (\emph{default} considers 0 germination)
#' @param scale Sets x scale (\emph{default} is none, can be "log")
#' @param width.bar bar width
#' @param textsize Font size
#' @param pointsize shape size
#' @param linesize line size
#' @param pointshape format point (\emph{default} is 21)
#' @param font.family Font family (\emph{default} is sans)
#' @return
#'   \describe{
#'   \item{\code{Optimum temperature}}{Optimum temperature (equivalent to the maximum point)}
#'   \item{\code{Optimum temperature response}}{Response at the optimal temperature (equivalent to the maximum point)}
#'   \item{\code{Minimal temperature}}{Temperature that has the lowest response}
#'   \item{\code{Minimal temperature response}}{Lowest predicted response}
#'   \item{\code{Predicted maximum basal value}}{Lower basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{Predicted minimum basal value}}{Upper basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{grafico}}{Graph in ggplot2 with equation}
#'   }
#' @seealso \link{loess}
#' @export
#' @note if the maximum predicted value is equal to the maximum x, the curve does not have a maximum point within the studied range. If the minimum value is less than the lowest point studied, disregard the value.
#' @author Gabriel Danilo Shimizu
#' @author Leandro Simoes Azeredo Goncalves
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#'
#' #================================
#' # Germination
#' #================================
#' loess_model(trat,germ)
#'
#' #================================
#' # Germination speed
#' #================================
#' loess_model(trat, vel, ylab=expression("v"~(dias^-1)))


loess_model=function(trat,
                     resp,
                     ylab="Germination (%)",
                     xlab=expression("Temperature ("^"o"*"C)"),
                     theme=theme_classic(),
                     error="SE",
                     cardinal=0,
                     width.bar=NA,
                     legend.position="top",
                     scale="none",
                     textsize=12,
                     pointsize=4.5,
                     linesize=0.8,
                     pointshape=21,
                     font.family="sans"){
  requireNamespace("ggplot2")
  requireNamespace("crayon")
  ymean=tapply(resp,trat,mean)
  if(is.na(width.bar)==TRUE){width.bar=0.01*mean(trat)}
  if(error=="SE"){ysd=tapply(resp,trat,sd)/sqrt(tapply(resp,trat,length))}
  if(error=="SD"){ysd=tapply(resp,trat,sd)}
  if(error=="FALSE"){ysd=0}
  desvio=ysd
  xmean=tapply(trat,trat,mean)
  mod=loess(resp~trat)
  xp=seq(min(trat),max(trat),length.out = 1000)
  preditos=data.frame(x=xp,
                      y=predict(mod,newdata = data.frame(trat=xp)))
  x=preditos$x
  y=preditos$y
  data=data.frame(xmean,ymean)
  data1=data.frame(trat=xmean,resp=ymean)
  s="~~~ Loess~regression"
  graph=ggplot(data,aes(x=xmean,y=ymean))
  if(error!="FALSE"){graph=graph+geom_errorbar(aes(ymin=ymean-ysd,ymax=ymean+ysd),
                                               width=width.bar,size=linesize)}
  graph=graph+geom_point(aes(color="black"),size=pointsize,shape=pointshape,fill="gray")+
    theme+
    geom_line(data=preditos,aes(x=x,
                                y=y,color="black"),size=linesize)+
    scale_color_manual(name="",values=1,label="Loess regression")+
    theme(axis.text = element_text(size=textsize,color="black",family = font.family),
          legend.position = legend.position,
          axis.title = element_text(family = font.family),
          legend.text = element_text(size=textsize,family = font.family),
          legend.direction = "vertical",
          legend.text.align = 0,
          legend.justification = 0)+
    ylab(ylab)+xlab(xlab)
  if(scale=="log"){graph=graph+scale_x_log10()}
  temp1=seq(min(trat),max(trat),length.out=10000)
  result=predict(mod,newdata = data.frame(trat=temp1),type="response")
  maximo=temp1[which.max(result)]
  respmax=result[which.max(result)]
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>maximo]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>maximo])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<maximo]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<maximo])}else{minimo=NA}
  graphs=data.frame("Parameter"=c("Optimum temperature",
                                  "Optimum temperature response",
                                  "Minimal temperature",
                                  "Minimal temperature response",
                                  "Predicted maximum basal value",
                                  "Predicted minimum basal value"),
                    "values"=round(c(maximo,respmax,
                               mini,
                               respmin,maxl,minimo),7))
  graficos=list("test"=graphs,graph)
  print(graficos)
}
