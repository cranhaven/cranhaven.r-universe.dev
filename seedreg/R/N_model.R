#' Analysis: Graph for not significant trend
#' @author Gabriel Danilo Shimizu
#' @author Leandro Simoes Azeredo Goncalves
#' @description Graph for non-significant trend. Can be used within the multicurve command
#' @param trat Numerical vector with treatments (Declare as numeric)
#' @param resp Numerical vector containing the response of the experiment.
#' @param ylab Dependent variable name (Accepts the \emph{expression}() function)
#' @param xlab Independent variable name (Accepts the \emph{expression}() function)
#' @param error Error bar (It can be SE - \emph{default}, SD or FALSE)
#' @param theme ggplot2 theme (\emph{default} is theme_classic())
#' @param legend.position Legend position (\emph{default} is "top")
#' @param width.bar Bar width
#' @param legend Add the legend
#' @param textsize Font size
#' @param pointsize shape size
#' @param linesize line size
#' @param pointshape format point (\emph{default} is 21)
#' @param font.family Font family (\emph{default} is sans)
#' @return The function returns an exploratory graph of segments
#' @keywords non-significant
#' @export
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#'
#' #================================
#' # Germination
#' #================================
#' N_model(trat,germ)
#'
#' #================================
#' # Germination speed
#' #================================
#' N_model(trat, vel, ylab=expression("v"~(dias^-1)))


N_model=function(trat,
                 resp,
                 ylab="Germination (%)",
                 error="SE",
                 legend="not~signifcant",
                 xlab=expression("Temperature ("^"o"*"C)"),
                 theme=theme_classic(),
                 width.bar=NA,
                 legend.position="top",
                 textsize=12,
                 pointsize=4.5,
                 linesize=0.8,
                 pointshape=21,
                 font.family="sans"){
  requireNamespace("ggplot2")
  dados=data.frame(trat,resp)
  medias=c()
  if(is.na(width.bar)==TRUE){width.bar=0.01*mean(trat)}
  dose=tapply(trat, trat, mean)
  media=tapply(resp, trat, mean)
  if(error=="SE"){desvio=tapply(resp,trat,sd)/sqrt(tapply(resp,trat,length))}
  if(error=="SD"){desvio=tapply(resp,trat,sd)}
  if(error=="FALSE"){desvio=0}
  data1=data.frame(trat,resp)
  data1=data.frame(trat=as.numeric(as.character(names(media))),
                   resp=media,
                   desvio)
  temp1=dose
  result=media
  #s="not~significant"
  s=legend
  grafico=ggplot(data1,aes(x=trat,y=resp))+
    geom_errorbar(aes(ymin=resp-desvio, ymax=resp+desvio),width=width.bar,size=linesize)+
    geom_point(aes(color=as.factor(rep(1,length(resp)))),na.rm=TRUE,
               size=pointsize,fill="gray",shape=pointshape)+
    theme+
    ylab(ylab)+
    xlab(xlab)+
    scale_color_manual(values="black",label=c(parse(text=s)),name="")+
    theme(text = element_text(size=textsize,color="black",family = font.family),
          axis.text = element_text(size=textsize,color="black",family = font.family),
          axis.title = element_text(size=textsize,color="black",family = font.family),
          legend.position = legend.position,
          legend.text=element_text(size=textsize,family = font.family),
          legend.direction = "vertical",
          legend.text.align = 0,
          legend.justification = 0)
  graficos=list(teste="Not significant",grafico)
  print(graficos)
}
