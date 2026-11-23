#' Plotting a dendogram
#'
#' Plots the dendogram obtained using the  Ward’s method for obtaining the principal balances. The process follow in this function is explained  in Section 3.1 of (Creus-Martí et al, 2022)
#'
#'
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param names  Vector with the names of the bacteria in the same order that are written in \code{especie}.
#'
#'
#'@return Returns a list with: the dendogram.
#'
#'\itemize{
#'   \item Num: List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the principal balance i are placed.
#'   \item Dem:  List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the principal balance i are placed.
#'   \item dendogram:  Plots the dendogram.
#'   }
#' @examples
#'
#'names2=c("Bact1","Bact2","Bact3","Bact4","Bact5")
#'set.seed(314)
#'esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
#'
#'
#'PlotDendogram(esp2,names2)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#' @export
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

PlotDendogram<-function(especie, names){

  #Creating a matrix with the same information than especies but with the new names
  especieEndo=especie
  row.names(especieEndo)=names

  #Plotting the dendogram
  datos.c1<-compositions::clr(t(especieEndo))#clr trasformation of the data
  datos.c=t(datos.c1)
  d <- stats::dist(datos.c, method = "euclidean") #Obtaining the euclidean distance of the clr-transform data in pairs.
  fit <- stats::hclust(d, method="ward.D2") #Grouping taking into account the variance
  #plot(fit,main="Ward hierarchical clustering",ylab="",xlab="",sub="")#Plotting the dendogram

  #Extracting the numeratos and the denominator of the principal balances
  Merged=fit$merge
  Merged=fit$merge
  Num<-vector("list", length = dim(Merged)[1])
  Dem<-vector("list", length = dim(Merged)[1])

  for (i in 1:(dim(Merged)[1])){

    if(length(which(Merged[i,]<0))==2){
      Num[[i]]=-Merged[i,1]
      Dem[[i]]=-Merged[i,2]
    }


    if(length(which(Merged[i,]<0))==1){
      if(which(Merged[i,]<0)==1){
        Num[[i]]=c(Num[[Merged[i,2]]],Dem[[Merged[i,2]]])
        Dem[[i]]=-Merged[i,1]
      }
    }


    if(length(which(Merged[i,]>0))==2){
      Num[[i]]=c(Num[[Merged[i,1]]],Dem[[Merged[i,1]]])
      Dem[[i]]=c(Num[[Merged[i,2]]],Dem[[Merged[i,2]]])
    }

  }

  returned<-list(Num,Dem,plot(fit,main="Ward hierarchical clustering",ylab="",xlab="",sub=""))
  names(returned)<-c("Num","Dem","Dendogram")

  return(returned)
  }






