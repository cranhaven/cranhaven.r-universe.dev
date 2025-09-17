#' Graph: Merge multiple curves into a single graph
#' @param plots list with objects of type LM_model, BC_model, CD_model, LL_model or normal_model
#' @param theme ggplot2 theme (\emph{default} is theme_classi())
#' @param legend.title caption title
#' @param trat name of the curves
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab treatments name (Accepts the \emph{expression}() function)
#' @param legend.position legend position (\emph{default} is c(0.3,0.8))
#' @param method marking method
#' @param width.bar bar width
#' @param pointsize shape size
#' @param linesize line size
#' @param textsize Font size
#' @param fill dot fill color in case gray=F
#' @param font.family Font family (\emph{default} is sans)
#' @return The function returns a graph joining the outputs of the functions LM_model, LL_model, BC_model, CD_model, loess_model, normal_model, piecewise_model and N_model
#' @author Gabriel Danilo Shimizu
#' @details The method argument defines the type of markup desired by the user. By default, method="shape_color" is used, which differentiates by color and dot shape. For gray scale, use method="shape_gray". To use only color, use method="color", in this case, the dot shape is 16 (filled circle). You can change the stitch pattern by setting the fill color in quotes followed by a space and the stitch number (eg "gray 21"). Still starting from this last method, if the user uses the change to point format without filling, such as 15, 16, 17 or 18, the function will ignore the first argument (ex. "gray 16"), however, of either way the user must define a color.
#' @export
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#' a=LM_model(trat,germ)
#' b=LL_model(trat,germ,npar = "LL.3")
#' c=BC_model(trat,germ, npar = "BC.4")
#' d=CD_model(trat,germ, npar = "CRS.4")
#' multicurve(list(a,b,c,d))

multicurve=function(plots,
                    theme=theme_classic(),
                    legend.title=NULL,
                    legend.position="top",
                    trat=NA,
                    method="shape_color",
                    fill="gray90",
                    ylab="Germination (%)",
                    xlab=expression("Temperature ("^"o"*"C)"),
                    width.bar=NA,
                    pointsize=4.5,
                    linesize=0.8,
                    textsize=12,
                    font.family="sans"){
  requireNamespace("ggplot2")
  equation=1:length(plots)
  grafico=ggplot()
  if(method=="color"){
    for(i in 1:length(plots)){
      equation[[i]]=plots[[i]][[3]]$plot$s
      x=plots[[i]][[3]]$plot$temp1
      y=plots[[i]][[3]]$plot$result
      if(is.na(width.bar)==TRUE){width.bar=0.01*mean(x)}
      data=data.frame(x,y,color=as.factor(i))
      pontosx=plots[[i]][[3]]$plot$data1$trat
      pontosy=plots[[i]][[3]]$plot$data1$resp
      desvio=plots[[i]][[3]]$plot$desvio
      pontos=data.frame(x=pontosx,
                        y=pontosy,
                        desvio=desvio,
                        color=as.factor(i))
      color=pontos$color
      grafico=grafico+
        geom_errorbar(data=pontos,
                      aes(x=x,
                          y=y,
                          ymin=y-desvio,
                          ymax=y+desvio,
                          color=color,
                          group=color),width=width.bar, size=linesize)+
        geom_line(data=data,aes(x=x,
                                y=y,
                                color=color,
                                group=color),size=linesize)+
        geom_point(data=pontos,aes(x=x,y=y,
                                   color=color,
                                   group=color),
                   size=pointsize,
                   shape=16,
                   fill=fill)
    }
    texto=parse(text=paste(trat,"~",unlist(equation)))
    grafico=grafico+
      scale_color_discrete(label=texto)+
      theme+labs(color=legend.title)+
      theme(axis.text = element_text(size=textsize,color="black",family = font.family),
            axis.title = element_text(family = font.family),
            legend.position = legend.position,
            legend.text = element_text(family = font.family),
            legend.justification='left',
            legend.direction = "vertical",
            legend.text.align = 0)+ylab(ylab)+xlab(xlab)}
  if(method!="color" & method!="shape_gray" & method!="shape_color"){
    formato=unlist(stringr::str_split(method,pattern = " "))
    for(i in 1:length(plots)){
      equation[[i]]=plots[[i]][[3]]$plot$s
      x=plots[[i]][[3]]$plot$temp1
      y=plots[[i]][[3]]$plot$result
      if(is.na(width.bar)==TRUE){width.bar=0.01*mean(x)}
      data=data.frame(x,y,color=as.factor(i))
      pontosx=plots[[i]][[3]]$plot$data1$trat
      pontosy=plots[[i]][[3]]$plot$data1$resp
      desvio=plots[[i]][[3]]$plot$desvio
      pontos=data.frame(x=pontosx,
                        y=pontosy,
                        desvio=desvio,
                        color=as.factor(i))
      color=pontos$color
      grafico=grafico+
        geom_errorbar(data=pontos,
                      aes(x=x,
                          y=y,
                          ymin=y-desvio,
                          ymax=y+desvio,
                          color=color,
                          group=color),width=width.bar, size=linesize)+
        geom_line(data=data,aes(x=x,
                                y=y,
                                color=color,
                                group=color),size=linesize)+
        geom_point(data=pontos,aes(x=x,y=y,
                                   color=color,
                                   group=color),
                   size=pointsize,
                   shape=as.numeric(formato[2]),
                   fill=formato[1])
    }
    texto=parse(text=paste(trat,"~",unlist(equation)))
    grafico=grafico+
      scale_color_discrete(label=texto)+
      theme+labs(color=legend.title)+
      theme(axis.text = element_text(size=textsize,color="black",family = font.family),
            axis.title = element_text(family = font.family),
            legend.position = legend.position,
            legend.text = element_text(family = font.family),
            legend.justification='left',
            legend.direction = "vertical",
            legend.text.align = 0)+ylab(ylab)+xlab(xlab)
  }
  if(method=="shape_gray"){
    for(i in 1:length(plots)){
      equation[[i]]=plots[[i]][[3]]$plot$s
      x=plots[[i]][[3]]$plot$temp1
      y=plots[[i]][[3]]$plot$result
      data=data.frame(x,y,color=as.factor(i))
      pontosx=plots[[i]][[3]]$plot$data1$trat
      pontosy=plots[[i]][[3]]$plot$data1$resp
      desvio=plots[[i]][[3]]$plot$desvio
      pontos=data.frame(x=pontosx,y=pontosy,desvio=desvio,color=as.factor(i))
      grafico=grafico+
        geom_errorbar(data=pontos,
                      aes(x=x,
                          y=y,
                          ymin=y-desvio,
                          ymax=y+desvio),width=width.bar, size=linesize)+
        geom_point(data=pontos,aes(x=x,
                                   y=y,
                                   pch=color,
                                   group=color),
                   size=pointsize,fill="gray")+
        geom_line(data=data,aes(x=x,
                                y=y,
                                lty=color,
                                group=color),size=linesize)
    }
    texto=parse(text=paste(trat,"~",unlist(equation)))
    grafico=grafico+
      scale_linetype_discrete(label=texto)+
      scale_shape_discrete(label=texto)+
      theme+labs(lty=legend.title,shape=legend.title)+
      theme(axis.text = element_text(size=textsize,color="black",family = font.family),
            legend.position = legend.position,
            axis.title = element_text(family = font.family),
            legend.text=element_text(size=textsize,family = font.family),
            legend.justification='left',
            legend.direction = "vertical",
            legend.text.align = 0)+ylab(ylab)+xlab(xlab)}
  if(method=="shape_color"){
    for(i in 1:length(plots)){
      equation[[i]]=plots[[i]][[3]]$plot$s
      x=plots[[i]][[3]]$plot$temp1
      y=plots[[i]][[3]]$plot$result
      if(is.na(width.bar)==TRUE){width.bar=0.01*mean(x)}
      data=data.frame(x,y,color=as.factor(i))
      pontosx=plots[[i]][[3]]$plot$data1$trat
      pontosy=plots[[i]][[3]]$plot$data1$resp
      desvio=plots[[i]][[3]]$plot$desvio
      pontos=data.frame(x=pontosx,
                        y=pontosy,
                        desvio=desvio,
                        color=as.factor(i))
      color=pontos$color
      grafico=grafico+
        geom_errorbar(data=pontos,
                      aes(x=x,
                          y=y,
                          ymin=y-desvio,
                          ymax=y+desvio,
                          color=color,
                          group=color),width=width.bar, size=linesize)+
        geom_line(data=data,aes(x=x,
                                y=y,
                                color=color,
                                group=color),size=linesize)+
        geom_point(data=pontos,aes(x=x,y=y,
                                   color=color,
                                   group=color,
                                   shape=color),
                   size=pointsize,
                   #shape=21,
                   fill=fill)}
    texto=parse(text=paste(trat,"~",unlist(equation)))
    grafico=grafico+
      scale_linetype_discrete(label=texto)+
      scale_shape_discrete(label=texto)+
      scale_color_discrete(label=texto)+
      theme+labs(color=legend.title,shape=legend.title,shape=legend.title)+
      theme(axis.text = element_text(size=textsize,color="black",family = font.family),
            axis.title = element_text(family = font.family),
            legend.position = legend.position,
            legend.text = element_text(family = font.family),
            legend.justification='left',
            legend.direction = "vertical",
            legend.text.align = 0)+ylab(ylab)+xlab(xlab)
  }
  print(grafico)
}
