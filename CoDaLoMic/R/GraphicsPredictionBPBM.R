#' Plots the time series
#'
#' This function takes into account the data used to estimate and the data used to predict. We use this function when we want to observe the results obtained with the BPBM model.
#'
#'@param especie.All Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param ExpectedValue.All Matrix that contains at row i the expected value of the bacterial taxa i at all time points. The bacteria must be placed in the same order than in \code{especie}. This matrix must  comply: dim(ExpectedValue.All)[2]=dim(especie.All)[2]-1
#'@param names.especie Vector with the names of the bacteria in the same order that are placed in the \code{especie} matrix.
#'@param VarianceValue.All Matrix that contains at row i the variance of the bacterial taxa i at all time points. The bacteria must be placed in the same order than in \code{especie}.This matrix must  comply: dim(VarianceValue.All)[2]=dim(especie.All)[2]-1
#'@param Plot.Tipe Character. If \code{Plot.Tipe==Data} the function displays a graphic of the dataset, if \code{Plot.Tipe==DataExpected} the function displays a graphic of the data and the expected values, if \code{Plot.Tipe==All} the function displays a graphic with the data, the expected values and the variance (the varmas and varmenos parameters are introduce and are taken into account to plot the variance of the predicted part).If  \code{Plot.Tipe==Var} the function returns the boxplot of the variance at each time point and the variance of each bacteria. If  \code{Plot.Tipe==OnlyVar} the function returns the boxplots of the variance at each time points.
#'@param Pred Number. Indicates the time point in which we start predicting.
#'@param Varmas Matrix. Output of "PredictionBPBM" adding "$ExpVarmas". Matrix that contains at row i the expected value plus two times the sqrt(variance) of the bacteria i at all time points t=\code{Tt},...,K, the rest of the time points has 0 values. The bacteria are placed at the same order than in \code{especies}.
#'@param Varmenos Matrix. Output of "PredictionBPBM" adding "$ExpVarmenos". Matrix that contains at row i the expected value minus two times the sqrt(variance) of the bacteria i at all time points t=\code{Tt},...,K, the rest of the time points has 0 values. The bacteria are placed at the same order than in \code{especies}.
#'@param Detail Character. If \code{Detail==no} the graphic obtained when \code{Plot.Tipe==DataExpected} and \code{Plot.Tipe==All} will have the same y axis for all the taxa. If \code{Detail==yes} these functions will have different y axis.
#'
#'
#'@return Returns the indicated plots with a vertical line when the time point is equal to \code{Tt}=Pred-1, in Pred=\code{Tt}+1 the predicction has started.
#'
#'
#' @examples
#'
#'
#'names.especie=c("Bact1", "Bact2", "Bact3")
#'especie.All=cbind(c(0.5,0.3,0.2),
#'                  c(0.6,0.3,0.1),
#'                  c(0.4,0.1,0.5),
#'                  c(0.4,0.1,0.5),
#'                  c(0.4,0.1,0.5),
#'                  c(0.4,0.1,0.5))
#'ExpectedValue.All=especie.All[,-1]+0.1
#'VarianceValue.All=matrix(c(runif(15,0.001,0.004)), 3,5)
#'Pred=4
#'Varmas=cbind(matrix(0,3,2),matrix(c(runif(9,0.001,0.004)) ,3 ,3 ))
#'Varmenos=cbind(matrix(0,3,2),matrix(c(runif(9,0.001,0.004)) ,3 ,3 ))
#'
#'GraphicsPredictionBPBM(especie.All,
#'                       names.especie,
#'                       ExpectedValue.All,
#'                       VarianceValue.All ,
#'                       Pred ,
#'                       "Data",
#'                       Varmas,
#'                       Varmenos,
#'                       "no")
#'
#'GraphicsPredictionBPBM(especie.All,
#'                       names.especie,
#'                       ExpectedValue.All,
#'                       VarianceValue.All ,
#'                       Pred ,
#'                       "DataExpected",
#'                       Varmas,
#'                       Varmenos,
#'                       "no")
#'
#'GraphicsPredictionBPBM(especie.All,
#'                       names.especie,
#'                       ExpectedValue.All,
#'                       VarianceValue.All,
#'                       Pred ,
#'                       "All",
#'                       Varmas,
#'                       Varmenos,
#'                       "no")
#'
#'GraphicsPredictionBPBM(especie.All,
#'                       names.especie,
#'                       ExpectedValue.All,
#'                       VarianceValue.All ,
#'                       Pred ,
#'                       "Var",
#'                       Varmas,
#'                       Varmenos,
#'                       "no")
#'
#'GraphicsPredictionBPBM(especie.All,
#'                       names.especie,
#'                       ExpectedValue.All,
#'                       VarianceValue.All ,
#'                       Pred ,
#'                       "OnlyVar",
#'                       Varmas,
#'                       Varmenos,
#'                       "no")
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#' @export
#'
#'
#'


#    CoDaLoMic. Compositional Models to Longitudinal Microbiome Data.
#    Copyright (C) 2024  Irene Creus Martí
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

GraphicsPredictionBPBM<-function(especie.All, names.especie, ExpectedValue.All, VarianceValue.All,Pred ,Plot.Tipe,Varmas,Varmenos,Detail){



  if(Plot.Tipe=="Data"){

    time=c(1:dim(especie.All)[2])
    time.prep<-as.vector(sapply(time, function (x) rep(x,dim(especie.All)[1])))
    Names<-rep(names.especie, length(time))
    especie.prep<-as.vector(especie.All)

    df.graphic1<-data.frame(cbind(especie.prep,time.prep, Names))
    df.graphic1$especie.prep=as.numeric(df.graphic1$especie.prep)
    df.graphic1$time.prep=as.numeric(df.graphic1$time.prep)

    graphic1<-ggplot2::ggplot(df.graphic1,  ggplot2::aes(x=time.prep, y=especie.prep, colour=Names)) +
      ggplot2::geom_line()+
      ggplot2::labs(x = "Time",
           y = "Relative Abundance")

    return(graphic1)
  }

  time=c(1:(dim(especie.All)[2]-1))
  time.prep<-as.vector(sapply(time, function (x) rep(x,dim(especie.All)[1])))
  Names<-rep(names.especie, length(time))
  especie.prep<-as.vector(especie.All)

  esperanza.prep<-as.vector(ExpectedValue.All)
  especiemodi<-especie.All[,-1]
  especiemodi.prep<-as.vector(especiemodi)



  if(Plot.Tipe=="DataExpected"){

    if (Detail=="no"){
    df.graphic2<-data.frame(cbind(especiemodi.prep,esperanza.prep,time.prep, Names))
    df.graphic2$especiemodi.prep=as.numeric(df.graphic2$especiemodi.prep)
    df.graphic2$esperanza.prep=as.numeric(df.graphic2$esperanza.prep)
    df.graphic2$time.prep=as.numeric(df.graphic2$time.prep)
    df2=reshape2::melt(df.graphic2, id=c("time.prep","Names"))
    value=df2$value #Not necessary
    variable=df2$variable #Not necessary

    graphic2<-ggplot2::ggplot(df2,  ggplot2::aes(x=time.prep, y=value, group=variable)) +
      ggplot2::geom_line(data=subset(df2, variable %in% "esperanza.prep"),ggplot2::aes(linetype=variable))+
      ggplot2::geom_point(data=subset(df2, variable %in% "especiemodi.prep"), ggplot2::aes(size=variable))+
      ggplot2::geom_vline(xintercept=Pred-1)+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::facet_grid(Names~ .)+
      ggplot2::labs(x = "Time",
           y = "Relative Abundance")+
      ggplot2::scale_linetype_discrete(name  ="",
                              breaks=c("esperanza.prep"),
                              labels=c("ExpectedValue"))+
      ggplot2::scale_size_discrete(name  ="",
                          breaks=c("especiemodi.prep"),
                          labels=c("Data"))
    return(graphic2)
    }

    if (Detail=="yes"){
      df.graphic2<-data.frame(cbind(especiemodi.prep,esperanza.prep,time.prep, Names))
      df.graphic2$especiemodi.prep=as.numeric(df.graphic2$especiemodi.prep)
      df.graphic2$esperanza.prep=as.numeric(df.graphic2$esperanza.prep)
      df.graphic2$time.prep=as.numeric(df.graphic2$time.prep)
      df2=reshape2::melt(df.graphic2, id=c("time.prep","Names"))
      value=df2$value #Not necessary
      variable=df2$variable #Not necessary

      graphic2<-ggplot2::ggplot(df2,  ggplot2::aes(x=time.prep, y=value, group=variable)) +
        ggplot2::geom_line(data=subset(df2, variable %in% "esperanza.prep"),ggplot2::aes(linetype=variable))+
        ggplot2::geom_point(data=subset(df2, variable %in% "especiemodi.prep"), ggplot2::aes(size=variable))+
        ggplot2::geom_vline(xintercept=Pred-1)+
        ggplot2::theme(legend.position="bottom")+
        ggplot2::facet_grid(Names~ .,scales="free_y")+
        ggplot2::labs(x = "Time",
             y = "Relative Abundance")+
        ggplot2::scale_linetype_discrete(name  ="",
                                breaks=c("esperanza.prep"),
                                labels=c("ExpectedValue"))+
        ggplot2::scale_size_discrete(name  ="",
                            breaks=c("especiemodi.prep"),
                            labels=c("Data"))
      return(graphic2)
    }


  }

  if(Plot.Tipe=="All"){

    if(Detail=="no"){

      T=Pred-1

      VarmasI=ExpectedValue.All[,c(1:(T-1))]+2*sqrt(VarianceValue.All[,c(1:(T-1))])
      VarmenosI=ExpectedValue.All[,c(1:(T-1))]-2*sqrt(VarianceValue.All[,c(1:(T-1))])
      Varmas.join=cbind(VarmasI,Varmas[,-c(1:(T-1))])
      Varmenos.join=cbind(VarmenosI,Varmenos[,-c(1:(T-1))])
      Varmas.prep<-as.vector(Varmas.join)
      Varmenos.prep<-as.vector(Varmenos.join)
      df.graphic3<-data.frame(cbind(especiemodi.prep,esperanza.prep,Varmas.prep,Varmenos.prep,time.prep, Names))
      df.graphic3$especiemodi.prep=as.numeric(df.graphic3$especiemodi.prep)
      df.graphic3$esperanza.prep=as.numeric(df.graphic3$esperanza.prep)
      df.graphic3$Varmas.prep=as.numeric(df.graphic3$Varmas.prep)
      df.graphic3$Varmenos.prep=as.numeric(df.graphic3$Varmenos.prep)
      df.graphic3$time.prep=as.numeric(df.graphic3$time.prep)


      df3=reshape2::melt(df.graphic3, id=c("time.prep","Names"))
      value=df3$value #Not necessary
      variable=df3$variable #Not necessary

      graphic3<-ggplot2::ggplot(df3,  ggplot2::aes(x=time.prep, y=value, group=variable)) +
        ggplot2::geom_line(data=subset(df3, variable %in% c("esperanza.prep","Varmas.prep","Varmenos.prep")),ggplot2::aes(linetype=variable))+
        ggplot2::geom_point(data=subset(df3, variable %in% "especiemodi.prep"), ggplot2::aes(size=variable))+
        ggplot2::geom_vline(xintercept=Pred-1)+
        ggplot2::theme(legend.position="bottom")+
        ggplot2::facet_grid(Names~ .)+
        ggplot2::labs(x = "Time",
             y = "Relative Abundance")+
        ggplot2::scale_linetype_discrete(name  ="",
                                breaks=c("esperanza.prep","Varmas.prep","Varmenos.prep"),
                                labels=c("ExpectedValue (e)",expression(e + 2*sqrt(Variance)),expression(e - 2*sqrt(Variance))))+
        ggplot2::scale_size_discrete(name  ="",
                            breaks=c("especiemodi.prep"),
                            labels=c("Data"))
      return(graphic3)

    }




    if(Detail=="yes"){
      T=Pred-1

      VarmasI=ExpectedValue.All[,c(1:(T-1))]+2*sqrt(VarianceValue.All[,c(1:(T-1))])
      VarmenosI=ExpectedValue.All[,c(1:(T-1))]-2*sqrt(VarianceValue.All[,c(1:(T-1))])
      Varmas.join=cbind(VarmasI,Varmas[,-c(1:(T-1))])
      Varmenos.join=cbind(VarmenosI,Varmenos[,-c(1:(T-1))])
      Varmas.prep<-as.vector(Varmas.join)
      Varmenos.prep<-as.vector(Varmenos.join)
      df.graphic3<-data.frame(cbind(especiemodi.prep,esperanza.prep,Varmas.prep,Varmenos.prep,time.prep, Names))
      df.graphic3$especiemodi.prep=as.numeric(df.graphic3$especiemodi.prep)
      df.graphic3$esperanza.prep=as.numeric(df.graphic3$esperanza.prep)
      df.graphic3$Varmas.prep=as.numeric(df.graphic3$Varmas.prep)
      df.graphic3$Varmenos.prep=as.numeric(df.graphic3$Varmenos.prep)
      df.graphic3$time.prep=as.numeric(df.graphic3$time.prep)


      df3=reshape2::melt(df.graphic3, id=c("time.prep","Names"))
      value=df3$value #Not necessary
      variable=df3$variable #Not necessary

      graphic3<-ggplot2::ggplot(df3,  ggplot2::aes(x=time.prep, y=value, group=variable)) +
        ggplot2::geom_line(data=subset(df3, variable %in% c("esperanza.prep","Varmas.prep","Varmenos.prep")),ggplot2::aes(linetype=variable))+
        ggplot2::geom_point(data=subset(df3, variable %in% "especiemodi.prep"), ggplot2::aes(size=variable))+
        ggplot2::geom_vline(xintercept=Pred-1)+
        ggplot2::theme(legend.position="bottom")+
        ggplot2::facet_grid(Names~ .,scales="free_y")+
        ggplot2::labs(x = "Time",
             y = "Relative Abundance")+
        ggplot2::scale_linetype_discrete(name  ="",
                                breaks=c("esperanza.prep","Varmas.prep","Varmenos.prep"),
                                labels=c("ExpectedValue (e)",expression(e + 2*sqrt(Variance)),expression(e - 2*sqrt(Variance))))+
        ggplot2::scale_size_discrete(name  ="",
                            breaks=c("especiemodi.prep"),
                            labels=c("Data"))
      return(graphic3)

    }
  }



  if(Plot.Tipe=="Var"){

    variance.prep<-as.vector(VarianceValue.All)
    df.graphic4<-data.frame(cbind(time.prep, Names,variance.prep))
    df.graphic4$time.prep=as.numeric(df.graphic4$time.prep)
    df.graphic4$variance.prep=as.numeric(df.graphic4$variance.prep)


    graphic4 <- ggplot2::ggplot(df.graphic4,  ggplot2::aes(x=time.prep, y=variance.prep)) +
      ggplot2::geom_boxplot(ggplot2::aes(group=time.prep))+
      ggplot2::geom_line( data = df.graphic4,ggplot2::aes(x = time.prep, y = variance.prep, color=Names))+
      ggplot2::geom_vline(xintercept=Pred-1)+
      ggplot2::labs(x = "Time",
           y = "Variance")+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::theme(legend.title=ggplot2::element_blank())

    return(graphic4)

  }


  if(Plot.Tipe=="OnlyVar"){

    variance.prep<-as.vector(VarianceValue.All)
    df.graphic5<-data.frame(cbind(time.prep, Names,variance.prep))
    df.graphic5$time.prep=as.numeric(df.graphic5$time.prep)
    df.graphic5$variance.prep=as.numeric(df.graphic5$variance.prep)


    graphic5 <- ggplot2::ggplot(df.graphic5,  ggplot2::aes(x=time.prep, y=variance.prep)) +
      ggplot2::geom_boxplot(ggplot2::aes(group=time.prep))+
      ggplot2::geom_vline(xintercept=Pred-1)+
      ggplot2::labs(x = "Time",
           y = "Variance")

    return(graphic5)

  }

}


