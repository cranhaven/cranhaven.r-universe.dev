#' Contour plots and MCA function for classification modeling
#'
#' This function presents visual graphics by means of Multiple correspondence Analysis projection.
#' Interval variables are categorized to bins.
#' Dependent classification variable is set as supplementary variable.
#' Machine learning algorithm predictions are presented in a filled contour setting.
#' @usage mcacontour(dataf=dataf,listconti,listclass,vardep,proba="",bins=8,
#' Dime1="Dim.1",Dime2="Dim.2",classvar=1,intergrid=0,selec=0,
#' title="",title2="",listacol="",depcol="",alpha1=0.8,alpha2=0.8,alpha3=0.7,modelo="glm",
#' nodos=3,maxit=200,decay=0.01,sampsize=400,mtry=2,nodesize=5,
#' ntree=400,ntreegbm=500,shrink=0.01,bag.fraction=1,n.minobsinnode=10,C=100,gamma=10)
#' @param bins  Number of bins for categorize interval variables .
#' @inheritParams famdcontour
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
#' @importFrom pROC roc
#' @examples
#' data(breastwisconsin1)
#' dataf<-breastwisconsin1
#' listconti=c( "clump_thickness","uniformity_of_cell_shape","mitosis")
#' listclass=c("")
#' vardep="classes"
#' result<-mcacontour(dataf=dataf,listconti,listclass,vardep)
#' @details
#' This function applies MCA (Multiple Correspondence Analysis) in order to project points and categories of
#' class variables in the same plot. In addition, interval variables listed in listconti are categorized to
#' the number given in bins parameter (by default 8 bins). Further explanation about machine learning classification
#' and contour curves, see the famdcontour function documentation.
#' @return A list with the following objects:\describe{
#' \item{graph1}{plot of points on MCA  two dimensions}
#' \item{graph2}{plot of points and variables}
#' \item{graph3}{plot of points and contour curves}
#' \item{graph4}{plot of points, contour curves and variables}
#' \item{graph5}{plot of points colored by fitted probability}
#' \item{graph6}{plot of points colored by abs difference}
#' \item{df1}{ dataset used for graph1}
#' \item{df2}{ dataset used for graph2}
#' \item{df3}{ dataset used for graph3}
#' \item{df4}{ dataset used for graph4}
#' \item{listconti}{ interval variables used}
#' \item{listclass}{ class variables used}
#' \item{...}{color schemes and other parameters }
#' }
######################################################################
#
# mcacontour.R
#
# copyright (c) 2020-13-10, Javier Portela
#
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License,
#     version 3, as published by the Free Software Foundation.
#
#     This program is distributed in the hope that it will be useful,
#     but without any warranty; without even the implied warranty of
#     merchantability or fitness for a particular purpose.  See the GNU
#     General Public License, version 3, for more details.
#
#     A copy of the GNU General Public License, version 3, is available
#     at http://www.r-project.org/Licenses/GPL-3
#
######################################################################
mcacontour<-function(dataf=dataf,listconti,listclass,vardep,proba="",bins=8,
Dime1="Dim.1",Dime2="Dim.2",classvar=1,intergrid=0,selec=0,
title="",title2="",listacol="",depcol="",alpha1=0.8,alpha2=0.8,alpha3=0.7,
modelo="glm",nodos=3,maxit=200,decay=0.01,sampsize=400,mtry=2,nodesize=5,ntree=400,
ntreegbm=500,shrink=0.01,bag.fraction=1,n.minobsinnode=10,C=100,gamma=10)

{

    if (any(listacol=="")==TRUE)
  {

      listacol<-c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                  "#A65628", "#F781BF", "#999999", "#1B9E77", "#D95F02", "#7570B3",
                  "#E7298A", "#66A61E","#E6AB02", "#A6761D", "#666666", "#8DD3C7",
                  "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
                  "#FCCDE5", "#D9D9D9","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
                  "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")
  }

  minori<-numeric()
  dimf1<-numeric()
  dimf2<-numeric()
  Frecu<-numeric()
  Variable=character()
  fontface=numeric()
  z<-numeric()
  dife<-numeric()

  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])

  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  minori=1

  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)


salida<-mcamodelobis(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
Dime1=Dime1,Dime2=Dime2,bins=bins,selec=selec)


  daf<-as.data.frame(salida[[1]])

  etiquetas<-as.data.frame(salida[[2]])

  datafnuevo<-as.data.frame(salida[[3]])

  listconti<-salida[4]
  listclass<-salida[5]
  if (length(listclass)==0)
  {listclass<-c("")}

  ejex<-salida[6]
  ejey<-salida[7]

  variss<-salida[[8]]


  formu<-paste("factor(",vardep,")~.")
  formu<-stats::formula(formu)
  # *********************
  # MODELOS
  # *********************

  if (any(proba=="")==TRUE)
  {

  if (modelo=="nnet") {
    model <- nnet( formu,data=datafnuevo, size = nodos,
                   maxit = maxit, trace = FALSE,decay=decay,linout=FALSE)

    proba<- predict(model,dataf,type="raw")
  }

  if (modelo=="glm") {

    model <- glm(formula(formu),family=binomial(link='logit'),data=datafnuevo)
    proba<- predict(model,dataf,type="response")
  }

  if (modelo=="rf") {

    if (sampsize>nrow(dataf)) {sampsize=nrow(dataf)}

    model <- randomForest(formu,data=datafnuevo,sampsize=sampsize,
                          mtry=mtry,nodesize=nodesize,ntree=ntree)
    proba <- predict(model, dataf,type="prob")
    proba<-as.vector(proba[,2])
  }

  if (modelo=="gbm") {

    formu<-paste("clasefin~.")

    datafnuevo$clasefin<-ifelse(datafnuevo[,c(vardep)]==minoritaria,1,0)
    datafnuevo$clasefin<-as.character(datafnuevo$clasefin)

    a2<-datafnuevo
    a2[,c(vardep)]<-NULL

    dataf$clasefin<-ifelse(dataf[,c(vardep)]==minoritaria,1,0)
    dataf$clasefin<-as.character(dataf$clasefin)

    a3<-dataf
    a3[,c(vardep)]<-NULL


    model <- gbm(formula(formu),distribution="bernoulli", data=a2,
                 n.trees=ntreegbm,shrinkage = shrink,
                 bag.fraction = bag.fraction,n.minobsinnode =n.minobsinnode ,
                 interaction.depth = 2)

    proba <- predict(model,a3,type="response",n.trees=model$n.trees)
  }

  if (modelo=="svm") {

    a3<-datafnuevo
    a3$vardep2<-ifelse(a3[,c(vardep)]==minoritaria,1,0)
    a3[,c(vardep)]<-NULL

    formu<-paste("factor(vardep2)~.")

    model <- svm(formu, data=a3,
                 kernel = "radial",cost=C,gamma=gamma,probability = TRUE)
    proba<- predict(model, dataf,probability = TRUE)

    proba<-as.data.frame(attr(proba, "probabilities","dimnames"))
    proba<-as.data.frame(proba[,c("1")])
    names(proba)<-"V1"

    datafnuevo$vardep2<-NULL

  }

  }

  proba<-as.data.frame(proba)
  names(proba)<-"V1"

  union<-cbind(proba,daf)

  curvaroc<-suppressMessages(pROC::roc(response=union$vardep,predictor=union$V1))
  auc<-round(curvaroc$auc,2)

    if (any(title2=="")==TRUE)
  {
    title2<-paste(vardep,"  ","minor=",fremin,"%  ","totalobs=",totalobs,"  ","totalmin=",totalmin)
    title3<-paste(vardep,"  ","minor=",fremin,"%  ","totalobs=",totalobs,"  ","totalmin=",totalmin,"  ",
                  "algorithm=", modelo,"  auc=",auc)
  }
  else if (any(title2=="")==FALSE)
  {
    title3=title2
  }
  union$x<-union$dimf1
  union$y<-union$dimf2
  union$z<-union$V1


  unia<-union[,c("x","y","z")]

  unia<-unia[order(unia$x,unia$y,unia$z),]

  filasorig<-nrow(unia)

  aggdata <-aggregate(unia, by=list(unia$x,unia$y),
                      FUN=mean, na.rm=TRUE)

  filasfin<-nrow(aggdata)

  ag<-aggdata[order(aggdata$x,aggdata$y),]

  # Aquí miro min max
  minx<-min(aggdata$x)
  miny<-min(aggdata$y)
  maxx<-max(aggdata$x)
  maxy<-max(aggdata$y)

  filas<-nrow(aggdata)

  rango1<-maxy-miny
  rango2<-maxx-minx

  if (intergrid==0) {
    intergrid=0.5*rango1*rango2/filas
    intergrid=sqrt(intergrid)
  }



  f<-data.frame()
  for (x in seq(minx,maxx,intergrid)) {
    for (y in seq(miny,maxy,intergrid)){
      b<-cbind(x,y)
      f<-rbind(f,b)

    }
  }

  ag<-ag[,c("x","y","z")]

  # AQUÍ SE INTERPOLA EN LOS PUNTOS CREADOS EN EL GRID F ANTERIOR, CON BASE EN EL dataf AG
  otro<-mba.points(ag,xy.est=f)

  otro<-as.data.frame(otro)

  otro<-otro[order(otro$xyz.est.x,otro$xyz.est.y),]


  t<-otro

  t$x<-t$xyz.est.x
  t$y<-t$xyz.est.y
  t$z<-t$xyz.est.z

  t$z<-ifelse(t$z<0,0.00000001,t$z)
  t$z<-ifelse(t$z>1,0.9999999,t$z)


  t<-t[,c("x","y","z")]

  daf$x<-daf$dimf1
  daf$y<-daf$dimf2

  uni<-etiquetas
  uni$x<-uni$dimf1
  uni$y<-uni$dimf2

  uni$Variable<-as.character(uni$Variable)

  # GRAFICOS

  # COLORES

  if (any(depcol=="")==TRUE)
  {
    depcol<-c("darkolivegreen3", "red")
  }

  la<-c(mayoritaria,minoritaria)

  colScale <- scale_colour_manual(name = vardep,values = depcol,labels=la)

  #  k colors from  list

  # a<-paste(".",vardep,sep="")
  # uni<-uni[which(uni$Variable!=a),]

  ta<-as.data.frame(table(uni$Variable))
  longi<-nrow(ta)
  ta<-as.character(ta$Var1)

  listabis<-listacol[c(1:longi)]

  la2<-c(la,ta)

  colScale2<- scale_colour_manual(name = "",values =c(depcol,listabis),labels=la2)

  mxim<-max(as.numeric(names(table(daf$Frecu))))
  k<-nrow(table(daf$Frecu))

  vectorFre<-as.numeric(names(table(daf$Frecu)))


  if (k<=4)
  {
    bre<-vectorFre
    rango<-c(1,k)
  }


  if (k>=4)
  {
    bre<-numeric()
    for(i in seq(1,mxim,by=mxim/4))
    {
      bre<-append(bre,ceiling(i))
    }
    rango<-c(1,4)
  }

  union$minori<-as.numeric(union$minori)
  union$dife<-(union$minori-union$z)


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


  g2<-ggplot(daf, aes(x=x, y=y)) + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(aes(colour=minori,size=Frecu,alpha=minori))+
    scale_alpha_manual(values = c(alpha1, alpha2),guide="none")+
    scale_size(name="Freq",breaks=bre,range=rango)+
    ggtitle(title,subtitle=title2)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )

  if (classvar==1)
  {
    g2<-g2+
      geom_text(data = variss,aes(x =dimf1, y =dimf2,label = rownames(variss),
                                  fontface=2),size=5,show.legend=F,colour = "orangered4")
  }


  g2<-g2+colScale+geom_density2d(colour = "gray80",alpha=1,show.legend=FALSE)+
    xlab(ejex)+ylab(ejey)+
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)),
           size=guide_legend(title="Freq", override.aes = list(alpha = 1))
                       )
  g3<-g2+ geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+
    scale_fill_grey(start=inicio,end=fin)+
      ggtitle(title, subtitle = title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )


  g4<-ggplot(daf, aes(x=x, y=y)) + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(aes(colour=minori,size=Frecu,alpha =minori))+
    scale_alpha_manual(values = c(alpha1, alpha2),guide="none")+
    scale_size(name="Freq",breaks=bre,range=rango)

  if (classvar==1)
  {
    g4<-g4+
      geom_text(data = variss,aes(x =dimf1, y =dimf2,label = rownames(variss),
                                  fontface=2),size=5,show.legend=F,colour = "orangered4")
  }

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
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)),
           size=guide_legend(title="Freq", override.aes = list(alpha = 1))
    )

  g3<-g2+ geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+scale_fill_grey(start=inicio,end=fin)

  g5<-g4+ geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+scale_fill_grey(start=inicio,end=fin)+
    ggtitle(title, subtitle = title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)),
           size=guide_legend(title="Freq", override.aes = list(alpha = 1))
    )

  gradi<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(colour=z,size=Frecu),alpha =alpha3)+
    scale_size(name="Freq",breaks=bre,range=rango)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi<-gradi+scale_colour_gradient(low= "green1",high = "red",name="Fitted probability")+
    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(size=guide_legend(title="Freq", override.aes = list(alpha = 1)))


      gradi2<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(fill=dife,size=Frecu),alpha =alpha3,shape=21,color="orange")+
    scale_size(name="Freq",breaks=bre,range=rango)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi2<-gradi2+
    scale_fill_gradient2(midpoint = 0, low = "blue4", mid = "white",
high = "darkred", space = "Lab",name="vardep-p" )

  gradi2<-gradi2+    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title3)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(size=guide_legend(title="Freq", override.aes = list(alpha = 1)))


  cosa<-list(g2,g4,g3,g5,gradi,gradi2,daf,uni,t,union,listconti,listclass,
             colScale,colScale2,ejex,ejey,title2,title3)
  names(cosa)<-c("graph1","graph2","graph3","graph4","graph5","graph6","df1",
                 "df2","df3","df4","listconti","listclass")
  return(cosa)
}

