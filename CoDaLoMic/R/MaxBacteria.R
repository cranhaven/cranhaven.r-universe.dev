#' Putting the reference bacteria at the last row
#'
#' This function calculates the mean abundance of each bacteria. Then, it creates a matrix where each row contains the abundance of one bacteria at all time points but  the bacteria with maximum (or minimum or a bacteria indicated by the user) mean abundance is placed at the last row
#'
#'
#'
#'@param nombresOriginal  Vector with the bacterial names at the same order than in DaTa. it must be fulfilled that lenght(nombresOriginal)==dim(DaTa)[2]-1
#'@param especieOriginal  Matrix that contains at row i the bacterial taxa of bacteria i at all time points
#'@param E                Number of bacteria available
#'@param Tt                Number of bacteria available
#'@param which.esp            If \code{which=="Max"} this function puts in the last position of the matrix the bacteria with maximum mean abundance.  If \code{which=="Min"} this function puts in the last position of the matrix the bacteria with minimum mean abundance.  If which is equal to a number this function puts in the last position of the matrix the bacteria that is in the "which" row of the \code{especieOriginal} matrix.
#'
#'
#'@return Returns a list with
#' \itemize{
#'   \item \code{especie} - Matrix that contains at row i the bacterial taxa of bacteria i at all time points but  the bacteria with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.
#'   \item \code{especiemodi} - Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt} but  the bacteria with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is placed at the last row.
#'    \item \code{nombres} - Vector with the bacteria's names placed in the order in which appear in the rows of the matrices \code{especie} and \code{especiemodi}
#'   \item \code{EE}  - Row in which the bacterial with maximum (or minimum) mean abundance was (or the value of "which" if which is numerical).
#'    \item  \code{EspecieMaxima} - Row in which the bacterial with maximum (or minimum) mean abundance (or the bacteria indicated by the user) is in \code{especie}.)
#' #' }
#' @examples
#'
#' names1=c("Bact1","Bact2","Bact3")
#' set.seed(314)
#' esp1=t(gtools::rdirichlet(n=4, c(1,3,1)))
#' e1=3
#' t1=4
#'
#' MaxBacteria(names1,esp1,e1,t1,"Max")
#'
#'
#' names3=c("Bact1","Bact2","Bact3","Bact4","Bact5")
#'set.seed(314)
#'esp3=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
#'e3=5
#'t3=6
#'MaxBacteria(names3,esp3,e3,t3,"Min")
#'
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

MaxBacteria<- function(nombresOriginal, especieOriginal, E, Tt,which.esp) {


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
      }#especie is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


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
    EE=EspecieMaxima #Original position of the maximum family
    EspecieMaxima=E
  }else{
    especie=especieOriginal
    especiemodi=especieOriginal[,-1]
    nombres=nombresOriginal
    EE=E
  }

  return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima))


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
  warning("There are one or more families which mean is equal to the mean of the familiy detected as EspecieMaxima")
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
    }#especie is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


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
  EE=EspecieMaxima #Original position of the maximum family
  EspecieMaxima=E
}else{
  especie=especieOriginal
  especiemodi=especieOriginal[,-1]
  nombres=nombresOriginal
  EE=E
}

return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima))


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
        }#especie is a matrix that contains at each row the bacterial taxa of one family at all time points. We have moved the maximum family to the last row of the matrix.


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
      EE=EspecieMaxima #Original position of the maximum family
      EspecieMaxima=E
    }else{
      especie=especieOriginal
      especiemodi=especieOriginal[,-1]
      nombres=nombresOriginal
      EE=E
    }

    return(list("especie"=especie, "especiemodi"=especiemodi,"nombres"=nombres, "EE"=EE,"EspecieMaxima"=EspecieMaxima))

  }

}
