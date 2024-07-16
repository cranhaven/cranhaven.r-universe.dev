#' Outliers in Contour plots and FAMD function for classification modeling
#'
#' This function adds outlier marks to famdcontour using ggrepel package.
#' @inheritParams famdcontour
#' @param Idt Identification variable, default "", row number
#' @param inf,sup Quantiles for x,y outliers
#' @param cutprob cut point for outliers based on prob.estimation error
#' @param ... options to be passed from famdcontour
#' @keywords FAMD, classification, contour_curves, outliers
#' @export
#' @import MASS
#' @import ggplot2
#' @importFrom MBA mba.points
#' @importFrom magrittr %>%
#' @importFrom nnet nnet
#' @import gbm
#' @import e1071
#' @importFrom stats aggregate
#' @importFrom dplyr inner_join
#' @importFrom randomForest randomForest
#' @importFrom FactoMineR FAMD
#' @importFrom pROC roc
#' @importFrom ggrepel geom_label_repel
#' @examples
#' data(breastwisconsin1)
#' dataf<-breastwisconsin1
#' listconti=c( "clump_thickness","uniformity_of_cell_shape","mitosis")
#' listclass=c("")
#' vardep="classes"
#' result<-famdcontourlabel(dataf=dataf,listconti=listconti,
#' listclass=listclass,vardep=vardep)
#' @details
#' An identification variable can be set in Idt parameter. By default, number of row is used.
#' There are two source of outliers: i) outliers in the two FAMD dimension space, where the cutpoints are set
#' as quantiles given (inf=0.1 and sup=0.9 in both dimensions by default) and ii) outliers with respect
#' to the fitted probability. The dependent variable is set to 1 for the mimority class, and 0 for the majority class.
#' Points considered outliers are those for which abs(vardep-fittedprob) excede parameter cutprob.
#'
#' @return A list with the following objects:\describe{
#' \item{graph1_graph6}{plots for dimension outliers}
#' \item{graph7_graph12}{plots for fit outliers}
#' }
######################################################################
#
# famdcontourlabel.R
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
famdcontourlabel<-function(dataf=dataf,Idt="",inf=0.10,sup=0.9,cutprob=0.5,...)

{
  if (any(Idt=="")==FALSE)

  {
    vectorIdt<-dataf[,c(Idt)]

    salida<-famdcontour(dataf=dataf,...)

    salida[[7]]$Idt<-vectorIdt
    salida[[10]]$Idt<-vectorIdt

  }

  if (any(Idt=="")==TRUE)
{

salida<-famdcontour(dataf=dataf,...)

}

    corte1<-quantile(salida[[7]]$x, probs = inf) # decile
    corte2<-quantile(salida[[7]]$x, probs = sup) # decile
    corte3<-quantile(salida[[7]]$y, probs = inf) # decile
    corte4<-quantile(salida[[7]]$y, probs = sup) # decile

    salida[[7]]$condicion<-ifelse(salida[[7]]$x<corte1|salida[[7]]$x>corte2|salida[[7]]$y<corte3|salida[[7]]$y>corte4,1,0)


    glabel1<-salida[[1]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel2<-salida[[2]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel3<-salida[[3]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel4<-salida[[4]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel5<-salida[[5]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel6<-salida[[6]]+geom_label_repel(aes(label = ifelse(salida[[7]]$condicion==1,
    as.character(salida[[7]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)


    salida[[10]]$condicion2<-ifelse(abs(salida[[10]]$dife)>cutprob,1,0)

    glabel7<-salida[[1]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel8<-salida[[2]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel9<-salida[[3]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel10<-salida[[4]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel11<-salida[[5]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)
    glabel12<-salida[[6]]+geom_label_repel(aes(label = ifelse(salida[[10]]$condicion2==1,
    as.character(salida[[10]]$Idt),'')),box.padding   = 0.35, point.padding = 0.4,segment.color = 'grey50',size=3)


    outliers1<-salida[[7]][which(salida[[7]]$condicion==1),]

    outliers1$est.prob<-outliers1$z

    outliers2<-salida[[10]][which(salida[[10]]$condicion2==1),]

    outliers2$est.prob<-outliers2$z


return(list(glabel1,glabel2,glabel3,glabel4,glabel5,glabel6,glabel7,
            glabel8,glabel9,glabel10,glabel11,glabel12,outliers1,outliers2))

}




