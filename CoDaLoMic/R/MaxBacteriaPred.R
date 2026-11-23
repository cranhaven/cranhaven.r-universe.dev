#' Putting the reference bacteria at the last row
#'
#' This function calculates the mean abundance of each bacteria taking into account the time points used to estimate the model (t=1,2,...,\code{Tt}). Then, it creates a matrix where each row contains the abundance of one bacteria at all time points but  the bacteria with maximum (or minimum) mean abundance (or the bacterial indicated by the user) is placed at the last row
#'
#'
#'
#'@param nombresOriginal  Vector with the bacterial names at the same order than in DaTa. it must be fulfilled that lenght(nombresOriginal)==dim(DaTa)[2]-1
#'@param especieOriginal  Matrix that contains at row i the bacterial taxa of bacteria i at t=1,2,...,\code{Tt}, with \code{Tt}=Pred-1.
#'@param E                Number of bacteria available
#'@param Tt                Number of time points used to estimate the model (\code{Tt}=Pred-1)
#'@param K                Number of time points at the data
#'@param especieOriginal.All Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param which.esp            If \code{which=="Max"} this function puts in the last position of the matrix the bacteria with maximum mean abundance.  If \code{which=="Min"} this function puts in the last position of the matrix the bacteria with minimum mean abundance.  If which is equal to a number this function puts in the last position of the matrix the bacteria that is in the "which" row of the \code{especieOriginal} matrix.
#'@param Pred  Number. The data at t=1,...,Pred-1 will be used to estimate the model. The rest of the time points will be used to study the capacity of the model to predict. If \code{Pred==0} all the datatset will be used to estimate the model.
#'
#'@return Returns a list with
#' \itemize{
#'   \item \code{especie} - Matrix that contains at row i the bacterial taxa of bacteria i at time points t=1,2,...,\code{Tt} but  the bacteria with with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.
#'   \item \code{especiemodi} - Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt} but  the bacteria with with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.
#'    \item \code{nombres} - Vector with the bacteria's names placed in the order in which appear in the rows of the matrices \code{especie} and \code{especiemodi}
#'   \item \code{EE}  -  Row in which the bacterial with maximum (or minimum) mean abundance was (or the value of "which" if which is numerical).
#'    \item  \code{EspecieMaxima} - Row in which the bacterial with with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is in \code{especie}.)
#' #' #'   \item \code{especie.all}  - Matrix that contains at row i the bacterial taxa of bacteria i at all time points (t=1,2,...,K) but  the bacteria with with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.
#'    \item  \code{especiemodi.all} - Matrix that contains at row i the bacterial taxa of bacteria i at all time points (t=2,...,K) but  the bacteria with with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.

#'
#'
#' }
#' @examples
#'
#'names2=c("Bact1","Bact2","Bact3","Bact4","Bact5")
#'set.seed(314)
#'esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
#'e2=5
#'
#' MaxBacteriaPred(names2,esp2[,-c(4,5,6)],e2,3,Pred=4, 6,esp2, "Max")
#'
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

MaxBacteriaPred<- function(nombresOriginal, especieOriginal, E, Tt,Pred,K, especieOriginal.All, which.esp){

if(which.esp=="Max"){


  #Studying which familify is more abundant
  medias=rep(0,E)
  for (i in 1:E){
    medias[i]=mean(especieOriginal[i,])
  } #mean of each family.

  for (i in 1:E){ #We search the family with maximum mean
    if (medias[i]==max(medias)){
      EspecieMaxima=i#EspecieMaxima is position of the most abundant family.
    } else{
      i=i+1}
  }

  if(max(medias[-EspecieMaxima])==max(medias)){
    warning("There are one or more families which mean is equal to the mean of the familiy detected as Especieamaxima")
  }

  #We move the maximum family  at the last position of the matrices and vectors.
  if (EspecieMaxima!=E){
    especie=matrix(0,E,Tt)
    for (i in 1:E){
      if(i==EspecieMaxima){
        especie[EspecieMaxima,]=especieOriginal[E,]
      } else if(i==E){
        especie[E,]=especieOriginal[EspecieMaxima,]
      }else{
        especie[i,]=especieOriginal[i,]
      }#especie is a matrix that contains at each row the bacterial taxa of one family at t=1,2,...,T where T=Pred-1. We have moved the maximum family to the last row of the matrix.


    }

    especiemodi=especie[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,T. We have moved the maximum family to the last row of the matrix.

    nombres=rep(0,E)

    for (i in 1:E){
      if(i==EspecieMaxima){
        nombres[EspecieMaxima]=nombresOriginal[E]
      } else if(i==E){
        nombres[E]=nombresOriginal[EspecieMaxima]
      }else{
        nombres[i]=nombresOriginal[i]
      }#nombres is a vector that contains the names of the families. We have moved the name of the maximum family to the last position.


    }


    #We move the maximum family  at the last position of the matrices and vectors.
      especie.All=matrix(0,E,K)
      for (i in 1:E){
        if(i==EspecieMaxima){
          especie.All[EspecieMaxima,]=especieOriginal.All[E,]
        } else if(i==E){
          especie.All[E,]=especieOriginal.All[EspecieMaxima,]
        }else{
          especie.All[i,]=especieOriginal.All[i,]
        }#especie.All is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


      }

      especiemodi.All=especie.All[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,K. We have moved the maximum family to the last row of the matrix.



      EE=EspecieMaxima #Original position of the maximum family
      EspecieMaxima=E
    }else{
      especie=especieOriginal
      especiemodi=especieOriginal[,-1]
      nombres=nombresOriginal
      EE=E
      especie.All=especieOriginal.All
      especiemodi.All=especieOriginal.All[,-1]
      }

    return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima,"especie.All"=especie.All, "especiemodi.All"=especiemodi.All))

  }


if(which.esp=="Min"){


    #Studying which familify is more abundant
    medias=rep(0,E)
    for (i in 1:E){
      medias[i]=mean(especieOriginal[i,])
    } #mean of each family.

    for (i in 1:E){ #We search the family with maximum mean
      if (medias[i]==min(medias)){
        EspecieMaxima=i#EspecieMaxima is position of the most abundant family.
      } else{
        i=i+1}
    }

    if(min(medias[-EspecieMaxima])==min(medias)){
      warning("There are one or more families which mean is equal to the mean of the familiy detected as Especieamaxima")
    }

    #We move the maximum family  at the last position of the matrices and vectors.
    if (EspecieMaxima!=E){
      especie=matrix(0,E,Tt)
      for (i in 1:E){
        if(i==EspecieMaxima){
          especie[EspecieMaxima,]=especieOriginal[E,]
        } else if(i==E){
          especie[E,]=especieOriginal[EspecieMaxima,]
        }else{
          especie[i,]=especieOriginal[i,]
        }#especie is a matrix that contains at each row the bacterial taxa of one family at t=1,2,...,T where T=Pred-1. We have moved the maximum family to the last row of the matrix.


      }

      especiemodi=especie[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,T. We have moved the maximum family to the last row of the matrix.

      nombres=rep(0,E)

      for (i in 1:E){
        if(i==EspecieMaxima){
          nombres[EspecieMaxima]=nombresOriginal[E]
        } else if(i==E){
          nombres[E]=nombresOriginal[EspecieMaxima]
        }else{
          nombres[i]=nombresOriginal[i]
        }#nombres is a vector that contains the names of the families. We have moved the name of the maximum family to the last position.


      }


      #We move the maximum family  at the last position of the matrices and vectors.
      especie.All=matrix(0,E,K)
      for (i in 1:E){
        if(i==EspecieMaxima){
          especie.All[EspecieMaxima,]=especieOriginal.All[E,]
        } else if(i==E){
          especie.All[E,]=especieOriginal.All[EspecieMaxima,]
        }else{
          especie.All[i,]=especieOriginal.All[i,]
        }#especie.All is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


      }

      especiemodi.All=especie.All[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,K. We have moved the maximum family to the last row of the matrix.



      EE=EspecieMaxima #Original position of the maximum family
      EspecieMaxima=E
    }else{
      especie=especieOriginal
      especiemodi=especieOriginal[,-1]
      nombres=nombresOriginal
      EE=E
      especie.All=especieOriginal.All
      especiemodi.All=especieOriginal.All[,-1]
    }

    return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima,"especie.All"=especie.All, "especiemodi.All"=especiemodi.All))

}

if(which.esp!="Max" & which.esp!="Min"){

  EspecieMaxima=which.esp


    #We move the maximum family  at the last position of the matrices and vectors.
    if (EspecieMaxima!=E){
      especie=matrix(0,E,Tt)
      for (i in 1:E){
        if(i==EspecieMaxima){
          especie[EspecieMaxima,]=especieOriginal[E,]
        } else if(i==E){
          especie[E,]=especieOriginal[EspecieMaxima,]
        }else{
          especie[i,]=especieOriginal[i,]
        }#especie is a matrix that contains at each row the bacterial taxa of one family at t=1,2,...,T where T=Pred-1. We have moved the maximum family to the last row of the matrix.


      }

      especiemodi=especie[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,T. We have moved the maximum family to the last row of the matrix.

      nombres=rep(0,E)

      for (i in 1:E){
        if(i==EspecieMaxima){
          nombres[EspecieMaxima]=nombresOriginal[E]
        } else if(i==E){
          nombres[E]=nombresOriginal[EspecieMaxima]
        }else{
          nombres[i]=nombresOriginal[i]
        }#nombres is a vector that contains the names of the families. We have moved the name of the maximum family to the last position.


      }


      #We move the maximum family  at the last position of the matrices and vectors.
      especie.All=matrix(0,E,K)
      for (i in 1:E){
        if(i==EspecieMaxima){
          especie.All[EspecieMaxima,]=especieOriginal.All[E,]
        } else if(i==E){
          especie.All[E,]=especieOriginal.All[EspecieMaxima,]
        }else{
          especie.All[i,]=especieOriginal.All[i,]
        }#especie.All is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


      }

      especiemodi.All=especie.All[,-1]#especiemodi is a matrix that contains at each row the bacterial taxa of one family at time point 2,3,...,K. We have moved the maximum family to the last row of the matrix.



      EE=EspecieMaxima #Original position of the maximum family
      EspecieMaxima=E
    }else{
      especie=especieOriginal
      especiemodi=especieOriginal[,-1]
      nombres=nombresOriginal
      EE=E
      especie.All=especieOriginal.All
      especiemodi.All=especieOriginal.All[,-1]
    }

    return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima,"especie.All"=especie.All, "especiemodi.All"=especiemodi.All))

  }
}
