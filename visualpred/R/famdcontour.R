#' Contour plots and FAMD function for classification modeling
#'
#' This function presents visual graphics by means of FAMD.
#' FAMD function is Factorial Analysis for Mixed Data (interval and categorical)
#' Dependent classification variable is set as supplementary variable.
#' Machine learning algorithm predictions are presented in a filled contour setting
#' @usage famdcontour(dataf=dataf,listconti,listclass,vardep,proba="",
#' title="",title2="",depcol="",listacol="",alpha1=0.7,alpha2=0.7,alpha3=0.7,
#' classvar=1,intergrid=0,selec=0,modelo="glm",nodos=3,maxit=200,decay=0.01,
#' sampsize=400,mtry=2,nodesize=10,ntree=400,ntreegbm=500,shrink=0.01,
#' bag.fraction=1,n.minobsinnode=10,C=100,gamma=10,Dime1="Dim.1",Dime2="Dim.2")
#' @param dataf data frame.
#' @param listconti Interval variables to use, in format c("var1","var2",...).
#' @param listclass Class variables to use, in format c("var1","var2",...).
#' @param vardep  Dependent binary classification variable.
#' @param proba  vector of probability predictions obtained externally (optional)
#' @param Dime1,Dime2 FAMD Dimensions to consider. Dim.1 and Dim.2 by default.
#' @param intergrid scale of grid for contour:0 if automatic
#' @param selec 1 if stepwise logistic variable selection is required, 0 if not.
#' @param title plot main title
#' @param title2 plot subtitle
#' @param depcol vector of two colors for points
#' @param listacol vector of colors for labels
#' @param alpha1 alpha transparency for majoritary class
#' @param alpha2 alpha transparency for minoritary class
#' @param alpha3 alpha transparency for fit probability plots
#' @param classvar 1 if dependent variable categories are plotted as supplementary
#' @param modelo name of model: "glm","gbm","rf,","nnet","svm".
#' @param nodos nnet: nodes
#' @param maxit nnet: iterations
#' @param decay nnet: decay
#' @param sampsize rf: sampsize
#' @param mtry rf: mtry
#' @param nodesize rf: nodesize
#' @param ntree rf: ntree
#' @param ntreegbm gbm: ntree
#' @param shrink gbm: shrink
#' @param bag.fraction gbm: bag.fraction
#' @param n.minobsinnode gbm:n.minobsinnode
#' @param C svm Radial: C
#' @param gamma svm Radial: gamma
#' @keywords FAMD, classification, contour_curves
#' @export
#' @import MASS
#' @import ggplot2
#' @importFrom MBA mba.points
#' @importFrom magrittr %>%
#' @importFrom nnet nnet
#' @import gbm
#' @import e1071
#' @importFrom stats aggregate quantile
#' @importFrom dplyr inner_join
#' @importFrom randomForest randomForest
#' @importFrom FactoMineR FAMD
#' @importFrom pROC roc
#' @examples
#' data(breastwisconsin1)
#' dataf<-breastwisconsin1
#' listconti=c( "clump_thickness","uniformity_of_cell_shape","mitosis")
#' listclass=c("")
#' vardep="classes"
#' result<-famdcontour(dataf=dataf,listconti,listclass,vardep)
#' @details
#' FAMD algorithm from FactoMineR package is used to compute point coordinates on dimensions
#' (Dim.1 and Dim.2 by default).
#' Minority class on dependent variable category is represented as red, majority category as green.
#' Color scheme can be altered using depcol and listacol, as well as alpha transparency values.
#'
#'## Predictive modeling
#' For predictive modeling, selec=1 selects variables with a simple stepwise logistic regression.
#' By default select=0.
#' Logistic regression is used by default. Basic parameter setting is supported for algorithms nnet, rf,gbm and svm-RBF.
#' A vector of fitted probabilities obtained externally from other
#' algorithms can be imported in parameter proba=nameofvector. Contour curves are then
#' computed based on this vector.
#'
#' ## Contour curves
#' Contour curves are build by the following process: i) the chosen algorithm model is trained and all
#' observations are predicted-fitted. ii) A grid of points on the two chosen FAMD dimensions is built
#' iii) package MBA is used to interpol probability estimates over the grid, based on
#' previously fitted observations.
#'
#' ## Variable representation
#' In order to represent interval variables, categories of class variables, and points in the same plot,
#' a proportional projection of interval variables coordinates over the two dimensions range is applied.
#' Since space of input variables is frequently larger than two dimensions, sometimes overlapping of
#' points is produced; a frequency variable is used, and alpha values may be adjusted to avoid wrong interpretations
#' of the presence of dependent variable category/color.
#'
#' ## Troubleshooting
#'  * Check missings. Missing values are not allowed.
#'  * By default selec=0. Setting selec=1 may sometimes imply that no variables are selected; an error message is shown n this case.
#'  * Models with only two input variables could lead to plot generation problems.
#'  * Be sure that variables named in listconti are all numeric.
#'  * If some numeric variable is constant at one single value, process is stopped since numeric  Min-max standarization is performed,
#'  and NaN values are generated.
#'
#' @return A list with the following objects:\describe{
#' \item{graph1}{plot of points on FAMD first two dimensions}
#' \item{graph2}{plot of points and contour curves}
#' \item{graph3}{plot of points and variables}
#' \item{graph4}{plot of points variable and contour curves}
#' \item{graph5}{plot of points colored by fitted probability}
#' \item{graph6}{plot of points colored by abs difference}
#' \item{df1}{ data frame used for graph1}
#' \item{df2}{ data frame used for contour curves}
#' \item{df3}{ data frame used for variable names}
#' \item{listconti}{interval variables used-selected }
#' \item{listclass}{class variables used-selected }
#' }
#' @references Pages J. (2004). Analyse factorielle de donnees mixtes. Revue Statistique Appliquee.  LII (4). pp. 93-111.
######################################################################
#
# famdcontour.R
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
famdcontour<-function(dataf=dataf,listconti,listclass,vardep,proba="",
title="",title2="",depcol="",listacol="",alpha1=0.7,alpha2=0.7,alpha3=0.7,
classvar=1,intergrid=0,selec=0,modelo="glm",
nodos=3,maxit=200,decay=0.01,sampsize=400,mtry=2,nodesize=10,ntree=400,ntreegbm=500,
shrink=0.01,bag.fraction=1,n.minobsinnode=10,C=100,
gamma=10,Dime1="Dim.1",Dime2="Dim.2")


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


    count.dups <- function(DF){
    DT <- data.table::data.table(DF)
    DT[,.N, by=list(dimf1=DF$dimf1,dimf2=DF$dimf2)]
    }
    .N<-numeric()
    minori<-numeric()
    dimf1<-numeric()
    dimf2<-numeric()
    Frecu<-numeric()
    Variable<-character()
    fontface<-numeric()
    z<-numeric()
    dife<-numeric()

    # class minor
    tabla1<-as.data.frame(table(dataf[,vardep]))
    tabla1<-tabla1[order(tabla1$Freq),]
    minoritaria<-as.character(tabla1[1,c("Var1")])
    tabla1<-tabla1[order(-tabla1$Freq),]
    mayoritaria<-as.character(tabla1[1,c("Var1")])

    if (minoritaria==mayoritaria)
    {
      tabla1<-tabla1[order(tabla1$Freq),]
      mayoritaria<-as.character(tabla1[2,c("Var1")])
    }

    cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
    fremin<-100*round(min(cosa$Freq),2)
    totalobs=nrow(dataf)


    cosa<-as.data.frame(table(dataf[[vardep]]))
    totalmin<-round(min(cosa$Freq),2)


    if (any(listclass==c(""))==TRUE)
    {
      dataf<-dataf[,c(listconti,vardep)]
    }

    if (any(listclass==c(""))==FALSE)
    {
      if (any(listconti==c(""))==FALSE)
      {dataf<-dataf[,c(listconti,listclass,vardep)]}

      if (any(listconti==c(""))==TRUE)
      {dataf<-dataf[,c(listclass,vardep)]}
    }


    #  -INTERVAL:
    #    1) STANDARD TO 0,1
    #  2) BINNING
    #  -CLASS VARIABLES: NOTHING

    #  ALL ARE RECODED TO A FACTOR
    #

    #  0-1
    if (any(listconti==c(""))==FALSE)
    {
      normFunc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
      dataf[c(listconti)] <- apply(dataf[c(listconti)], 2, normFunc)
    }
    # CLASS TO  FACTOR
    if (any(listclass==c(""))==FALSE)
    {
      dataf[c(listclass,vardep)]<-
        lapply(dataf[c(listclass,vardep)],as.factor)
    }

    if (any(listclass==c(""))==FALSE)
    {

    for (i in listclass)
    {
      levels(dataf[,i])<-paste(i,"_",levels(dataf[,i]),sep="")
    }

    }

    # SELECTION IF IT IS REQUIRED

    if (selec==1)
    {

      formu1<-paste("factor(",vardep,")~.")
      full.model <-stats::glm(stats::formula(formu1), data = dataf, family = binomial(link="logit"))
      step.model <- full.model %>% stepAIC(trace = FALSE)
      cosa<-attr(stats::terms(step.model), "term.labels")


      # Reduce data frame

      if (any(listclass==c(""))==FALSE)
      {
        listclass <- listclass[listclass %in%cosa]

        if (any(listconti==c(""))==FALSE)
        {
          listconti <- listconti[listconti %in%cosa]
        }

      }
      if (any(listclass==c(""))==TRUE)
      {
        listconti <- listconti[listconti %in%cosa]
      }

      if (any(listclass==c(""))==TRUE)
      {
        dataf<-dataf[,c(listconti,vardep)]
      }
      if (any(listclass==c(""))==FALSE)
      {
        if (any(listconti==c(""))==FALSE)
        {dataf<-dataf[,c(listconti,listclass,vardep)]}
        if (any(listconti==c(""))==TRUE)
        {dataf<-dataf[,c(listclass,vardep)]}
      }

    }

    # LA DEPENDIENTE A char

    vectordep<-as.data.frame(dataf[,vardep])

    dataf[,c(vardep)]<-as.character(dataf[,c(vardep)])


  # # QUITO LA VAR. DEPENDIENTE Y AÑADO VARIABLE FANTASMA COSA POR SI ACASO
  # if (any(listclass==c(""))==FALSE)
  # {
  #   if (any(listconti==c(""))==FALSE)
  #   {
  #     dataf<-dataf[,c(listconti,listclass)]
  #   }
  #   if (any(listconti==c(""))==TRUE)
  #   {
  #     dataf<-dataf[,c(listclass)]
  #   }
  # }
  #
  # if (any(listclass==c(""))==TRUE||length(listclass)==0)
  # {
  #   dataf<-dataf[,c(listconti)]
  #   dataf<-as.data.frame(dataf)
  # }
co=0
if (any(listclass==c(""))==TRUE||length(listclass)==0)
    {
      co=1
      dataf$cosa<-1
      dataf$cosa<-as.character(dataf$cosa)
      dataf<-as.data.frame(dataf)
}

  # FUNCIÓN MCA CON FAMD
  colu<-which(colnames(dataf)==vardep)
  mca1= FAMD(dataf,sup.var=colu,graph=FALSE,ncp=5)
  # mca1_vars_df1 = data.frame(mca1$var$coord,Variable=row.names(mca1$var$coord))
if (co==1)
  {
  dataf$cosa<-NULL
}

  mca1_vars_df1=data.frame(mca1$quanti.var$coord,Variable=row.names(mca1$quanti.var$coord))

  if (length(listclass)==1 & any(listclass==c(""))==FALSE)
      {
    ncats1 = nlevels(as.factor(dataf[,listclass]))
    cats1 = levels(as.factor(dataf[,listclass]))
    mca1_vars_df2 = data.frame(mca1$quali.var$coord, Variable = (cats1))
  }
  if (length(listclass)>1)
  {
    cats1 = apply(dataf[,listclass], 2, function(x) nlevels(as.factor(x)))
    mca1_vars_df2 = data.frame(mca1$quali.var$coord, Variable = rep(names(cats1), cats1))
  }
  if (any(listclass==c(""))==TRUE||length(listclass)==0)
  {
    mca1_vars_df2 = data.frame(mca1$quali.var$coord, Variable = row.names(mca1$quali.var$coord))
  }

  variss<-data.frame(mca1$quali.sup$coord)
  variss$Variable<-paste(".",vardep,sep="")

  mca1$ind$coord= data.frame(mca1$ind$coord)
  mca1$ind$coord<-cbind(mca1$ind$coord,vardep=vectordep[,1])

  mca1$ind$coord$dimf1<-mca1$ind$coord[,c(Dime1)]
  mca1$ind$coord$dimf2<-mca1$ind$coord[,c(Dime2)]

  mca1_vars_df2$dimf1<-mca1_vars_df2[,c(Dime1)]
  mca1_vars_df2$dimf2<-mca1_vars_df2[,c(Dime2)]

  maxi1<-max(mca1$ind$coord$dimf1)
  mini1<-max(mca1$ind$coord$dimf1)
  maxi2<-max(mca1$ind$coord$dimf2)
  mini2<-max(mca1$ind$coord$dimf2)

  # Retoco coordenadas var. quanti

  mca1_vars_df1$dimf1<-mca1_vars_df1[,c(Dime1)]
  mca1_vars_df1$dimf2<-mca1_vars_df1[,c(Dime2)]

  mca1_vars_df1$dimf1<-ifelse(mca1_vars_df1$dimf1>=0,mca1_vars_df1$dimf1*maxi1,
                              mca1_vars_df1$dimf1*abs(mini1))

  mca1_vars_df1$dimf2<-ifelse(mca1_vars_df1$dimf2>=0,mca1_vars_df1$dimf2*maxi2,
                              mca1_vars_df1$dimf2*abs(mini2))

  if (co==0)
  {
    mca1_vars_df2$dimf1<-mca1_vars_df2[,c(Dime1)]
    mca1_vars_df2$dimf2<-mca1_vars_df2[,c(Dime2)]

    mca1_vars_df1<-rbind(mca1_vars_df1,mca1_vars_df2)
  }

# SUPLEMENTARIA
  variss$dimf1<-variss[,c(Dime1)]
  variss$dimf2<-variss[,c(Dime2)]


  # CONTEO DE FRECUENCIAS EN EL dataf mca1_obs_df

  au<-as.data.frame(count.dups(mca1$ind$coord))
  au$Frecu<-au$N
  au$N<-NULL

  mca1$ind$coord<-inner_join(mca1$ind$coord,au, by = c("dimf1","dimf2"))


  # CONSTRUCCIÓN DE REJILLA Y PREDICCIÓN CONTOUR

  dataf[,vardep]<-factor(dataf[,vardep],levels=c(mayoritaria,minoritaria))

  formu<-paste("factor(",vardep,")~.")
  formu<-stats::formula(formu)

  # *********************
  # MODELOS
  # *********************
if (any(proba=="")==TRUE)
{

  if (modelo=="nnet") {
    model <- nnet( formula(formu),data=dataf, size = nodos,
                   maxit = maxit, trace = FALSE,decay=decay,linout=FALSE)

    proba<- predict(model,dataf,type="raw")
  }

  if (modelo=="glm") {

    model <- glm(formula(formu),
                 family=binomial(link='logit'),data=dataf)

    proba<- predict(model,dataf,type="response")

  }

  if (modelo=="rf") {

    if (sampsize>nrow(dataf)) {sampsize=nrow(dataf)}

    model <- randomForest(formula(formu),data=dataf,sampsize=sampsize,
                          mtry=mtry,nodesize=nodesize,ntree=ntree)
    proba <- predict(model, dataf,type="prob")
    proba<-as.vector(proba[,2])
  }

  if (modelo=="gbm") {

    formu<-paste("clasefin~.")

    dataf$clasefin<-ifelse(dataf[,c(vardep)]==minoritaria,1,0)
    dataf$clasefin<-as.character(dataf$clasefin)

    a3<-dataf
    a3[,c(vardep)]<-NULL

    model <- gbm(formula(formu),distribution="bernoulli", data=a3,
                 n.trees=ntreegbm,shrinkage = shrink,
                 bag.fraction = bag.fraction,n.minobsinnode=n.minobsinnode,
                 interaction.depth = 2,verbose=FALSE)

    proba <- predict(model,a3,type="response",n.trees=model$n.trees)


  }

  if (modelo=="svm") {

    dataf$vardep2<-ifelse(dataf[,c(vardep)]==minoritaria,1,0)
    vector1<-dataf$vardep
    dataf$vardep<-NULL

    formu<-paste("factor(vardep2)~.")

    model <- svm(formula(formu), data=dataf,
                 kernel = "radial",cost=C,gamma=gamma,probability = TRUE)
    proba<- predict(model, dataf,probability = TRUE)

    proba<-as.data.frame(attr(proba, "probabilities","dimnames"))
    proba<-as.data.frame(proba[,c("1")])
    names(proba)<-"V1"

    dataf$vardep2<-NULL
    dataf$vardep<-vector1

  }

}

  proba<-as.data.frame(proba)
  names(proba)<-"V1"

  union<-cbind(proba,mca1$ind$coord)

  union$V1<-as.numeric(as.character(union$V1))

  curvaroc<-suppressMessages(pROC::roc(response=union$vardep,predictor=union$V1))
  auc<-round(curvaroc$auc,2)

  if (any(title2=="")==TRUE)
  {
    title2<-paste(vardep,"  ","minor=",
                  fremin,"%  ","totalobs=",totalobs,"  ","totalmin=",totalmin,"  ",
                  "algorithm=", modelo, "   auc=", auc)
  }

  union$x<-union$dimf1
  union$y<-union$dimf2
  union$z<-union$V1
  union$minori<-ifelse(union$vardep==minoritaria,1,0)

  unia<-union[,c("x","y","z")]

  unia<-unia[order(unia$x,unia$y,unia$z),]

  filasorig<-nrow(unia)

  t <-aggregate(unia, by=list(unia$x,unia$y),
                      FUN=mean, na.rm=TRUE)


  filasfin<-nrow(t)

  t<-t[order(t$x,t$y),]

  # Aquí miro min max
  minx<-min(t$x)
  miny<-min(t$y)
  maxx<-max(t$x)
  maxy<-max(t$y)

  maxx<-maxx+intergrid
  maxy<-maxy+intergrid

  filas<-nrow(t)

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

  filas2<-nrow(f)

  t<-t[,c("x","y","z")]

  # AQUÍ SE INTERPOLA EN LOS PUNTOS CREADOS EN EL GRID F ANTERIOR, CON BASE EN EL data t
  t<-mba.points(t,xy.est=f)

  t<-as.data.frame(t)

  t<-t[order(t$xyz.est.x,t$xyz.est.y),]

  t$x<-t$xyz.est.x
  t$y<-t$xyz.est.y
  t$z<-t$xyz.est.z

  t$z<-ifelse(t$z<0,0.00000001,t$z)
  t$z<-ifelse(t$z>1,0.9999999,t$z)


  t<-t[,c("x","y","z")]

  mca1$ind$coord$x<-mca1$ind$coord$dimf1
  mca1$ind$coord$y<-mca1$ind$coord$dimf2

  # GRAFICOS

  # Dimensiones

  first<-as.numeric(substr(c(Dime1),5,5))
  second<-as.numeric(substr(c(Dime2),5,5) )


  ejex<-paste(Dime1,"(",trunc(mca1$eig[first,2]*10)/10,"%)",sep="")
  ejey<-paste(Dime2,"(",trunc(mca1$eig[second,2]*10)/10,"%)",sep="")


  # COLORS

  lista1<-c("gray70","red")

  if (any(depcol=="")==TRUE)
  {
    depcol<-c("darkolivegreen3", "red")
  }
  la<-c(mayoritaria,minoritaria)

  colScale <- scale_colour_manual(name = "vardep",values = depcol,labels=la)

  #  k colors from  list

  ta<-as.data.frame(table(as.character(mca1_vars_df1$Variable)))
  longi<-nrow(ta)
  ta<-as.character(ta$Var1)

  listabis<-listacol[c(1:longi)]

  la2<-c(la,ta)

  colScale2<- scale_colour_manual(name = "",values =c(depcol,listabis),labels=la2)

  mxim<-max(as.numeric(names(table(mca1$ind$coord$Frecu))))

  mca1$ind$coord$minori<-ifelse(mca1$ind$coord$vardep==minoritaria,1,0)

  mca1$ind$coord$minori<-as.character(mca1$ind$coord$minori)


  mxim<-max(as.numeric(names(table(mca1$ind$coord$Frecu))))
  k<-nrow(table(mca1$ind$coord$Frecu))

  vectorFre<-as.numeric(names(table(mca1$ind$coord$Frecu)))


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

  # Add identification variable

  mca1$ind$coord$Idt<-1:nrow(mca1$ind$coord)
  union$Idt<-1:nrow(union)

  breaks=c(0,0.25,0.50,0.75,0.95,1)

  g2<-ggplot(mca1$ind$coord, aes(x=x, y=y)) + theme_bw()+
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


     g3<-g2+ geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+
     scale_fill_grey(start=inicio,end=fin)



  gradi<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(colour=z,size=Frecu),alpha =alpha3)+
    scale_size(name="Freq",breaks=bre,range=rango)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi<-gradi+scale_colour_gradient(low= "green1",high = "red",name="Fitted probability")+
    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title2)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(size=guide_legend(title="Freq", override.aes = list(alpha = 1)))


  gradi2<-ggplot(union, aes(x=x, y=y)) + theme_bw()+
    geom_point(aes(fill=dife,size=Frecu),alpha =alpha3,shape=21,color="orange")+
    scale_size(name="Freq",breaks=bre,range=rango)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  gradi2<-gradi2+scale_fill_gradient2(midpoint = 0, low = "blue4", mid = "white",
                                      high = "red", space = "Lab",name="vardep-p" )

  gradi2<-gradi2+    xlab(ejex)+ylab(ejey)+    ggtitle(title,subtitle=title2)+
    theme(
      plot.title = element_text(hjust=0.5,color="blue"),
      plot.subtitle= element_text(hjust=0.5,color="orangered4")
    )+
    guides(size=guide_legend(title="Freq", override.aes = list(alpha = 1)))



  etivar<-ggplot(mca1$ind$coord, aes(x=x, y=y)) + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(aes(colour=minori,size=Frecu,alpha=minori))+
    scale_alpha_manual(values = c(alpha1, alpha2),guide="none")+
    scale_size(name="Freq",breaks=bre,range=rango)


  etivar<-etivar+geom_density2d(colour = "gray80",alpha=1,show.legend=FALSE)

  if (classvar==1)
  {
    etivar<-etivar+
      geom_text(data = variss,aes(x =dimf1, y =dimf2,label = rownames(variss),
                                  fontface=2),size=5,show.legend=F,colour = "orangered4")
  }


  etivar<-etivar+
    geom_text(data = mca1_vars_df1 ,aes(x =dimf1,y =dimf2,label = rownames(mca1_vars_df1 ),
    colour = Variable))+colScale2

  etivar<-etivar+geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70")+
    ggtitle(title,subtitle=title2)+
      theme(
        plot.title = element_text(hjust=0.5,color="blue"),
        plot.subtitle= element_text(hjust=0.5,color="orangered4")
      )+  xlab(ejex)+ylab(ejey)

  etivar<-etivar+guides(colour = guide_legend("", override.aes = list(size = 4,alpha = 1)),
    size=guide_legend(title="Freq", override.aes = list(alpha = 1))
       )

  etivar2<-etivar+geom_contour_filled(data=t, aes(x=x, y=y,z=z),alpha=0.65,breaks=breaks)+
    scale_fill_grey(start=inicio,end=fin)


  cosa<-list(g2,g3,etivar,etivar2,gradi,gradi2,mca1$ind$coord,t,mca1_vars_df1,union,
             listconti,listclass,colScale,colScale2)
  names(cosa)<-c("graph1","graph2","graph3","graph4","graph5","graph6",
                 "df1","df2","df3","listconti","listclass")
    return(cosa)
}


