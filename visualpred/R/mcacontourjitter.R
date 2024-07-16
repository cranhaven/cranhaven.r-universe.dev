#' Contour plots and MCA function for classification modeling
#'
#' This function is similar to mcacontour but points are jittered in every plot
#' @usage mcacontourjit(dataf=dataf,jit=0.1,alpha1=0.8,alpha2=0.8,alpha3=0.7,title="",...)
#' @inheritParams mcacontour
#' @param jit jit distance. Default 0.1.
#' @param ... options to be passed from mcacontour
#' @keywords MCA, classification, contour_curves
#' @export
#' @import MASS
#' @import ggplot2
#' @importFrom MBA mba.points
#' @importFrom magrittr %>%
#' @importFrom nnet nnet
#' @import gbm
#' @import e1071
#' @importFrom stats aggregate formula glm terms predict family binomial
#' @importFrom dplyr inner_join
#' @importFrom randomForest randomForest
#' @examples
#' data(breastwisconsin1)
#' dataf<-breastwisconsin1
#' listconti=c( "clump_thickness","uniformity_of_cell_shape","mitosis")
#' listclass=c("")
#' vardep="classes"
#' result<-mcacontourjit(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,jit=0.1)
#' @return A list with the following objects:\describe{
#' \item{graph1}{plot of points on MCA  two dimensions}
#' \item{graph2}{plot of points and variables}
#' \item{graph3}{plot of points and contour curves}
#' \item{graph4}{plot of points, contour curves and variables}
#' \item{graph5}{plot of points colored by fitted probability}
#' \item{graph6}{plot of points colored by abs difference}
#' }
# ROXYGEN_STOP


mcacontourjit<-function(dataf=dataf,jit=0.1,
alpha1=0.8,alpha2=0.8,alpha3=0.7,title="",...)

{
  minori<-numeric()
  Variable=character()
  x<-numeric()
  y<-numeric()
  z<-numeric()
  dife<-numeric()

  salida<-mcacontour(dataf=dataf,...)


  daf<-salida[[7]]
  uni<-salida[[8]]
  t<-salida[[9]]
  union<-salida[[10]]
  colScale<-salida[[13]]
  colScale2<-salida[[14]]
  ejex<-salida[[15]]
  ejey<-salida[[16]]
  title2<-salida[[17]]
  title3<-salida[[18]]

  breaks=c(0,0.25,0.50,0.75,0.95,1)

  miniz<-min(t$z)
  maxiz<-max(t$z)

  for (i in seq(1,5,by=1))
  {

    if (breaks[i]<=miniz & miniz<=breaks[i+1])
    {minimo<-i}
    if (breaks[i]<=maxiz  &  maxiz<=breaks[i+1])
    {maximo<-i}
  }

  inicio=0.8-0.12*(minimo-1)
  fin=0.2+0.12*(5-maximo)


  jitter=position_jitter(width = jit, height = jit)

  g2<-ggplot(daf, aes(x=x, y=y)) + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(aes(colour=minori,alpha =minori),position=jitter,size=1)+
    scale_alpha_manual(values = c(alpha1, alpha2),guide="none")+
    ggtitle(title,subtitle=title2)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )

  g2<-g2+colScale+geom_density2d(colour = "gray80",alpha=1,show.legend=FALSE)+
    xlab(ejex)+ylab(ejey)+
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)))

  g3<-g2+ geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+
    scale_fill_grey(start=inicio,end=fin)+
    ggtitle(title, subtitle = title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )


  g4<-ggplot(daf, aes(x=x, y=y)) + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(aes(colour=minori,alpha =minori),size=1,position=jitter)+
    scale_alpha_manual(values = c(alpha1, alpha2),guide="none")

  g4<-g4+geom_density2d(colour = "gray80",alpha=1,show.legend=FALSE)+
    xlab(ejex)+ylab(ejey)+
    geom_text(data = uni,aes(x =x, y =y,label = rownames(uni),
colour = Variable,fontface=1),size=sort(uni$tama),show.legend =F)+
    colScale2

  g4<-g4+  ggtitle(title, subtitle = title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)))

  g5<-g4+  geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+
    scale_fill_grey(start=inicio,end=fin)+
    ggtitle(title, subtitle = title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)))

  gradi<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(colour=z),alpha =alpha3,position=jitter,size=1)+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi<-gradi+scale_colour_gradient(low= "green1",high = "red",name="Fitted probability")+
    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )


    gradi2<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(fill=dife),alpha =alpha3,shape=21,color="orange",position=jitter,size=1)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi2<-gradi2+scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
    high = "red", space = "Lab",name="vardep-p" )

  gradi2<-gradi2+    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )


  cosa<-list(g2,g4,g3,g5,gradi,gradi2)
  names(cosa)<-c("graph1","graph2","graph3","graph4","graph5","graph6")
  return(cosa)
}

