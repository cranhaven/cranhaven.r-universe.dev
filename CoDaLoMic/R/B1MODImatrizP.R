
#' Ridge regression matrix
#'
#' Defining the part of the ridge regression matrix  that  carries the information of the bacteria \code{especieII}.
#'
#' @param especieII Number. The number of the row in which the bacteria that we want to use is placed in the matrix \code{especie}.
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#' @param E Number of bacteria available
#' @param Tt Number of time points available
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}. This is the bacteria that is going to be at the denominator of the balance and at the denominator of the alr transformation.
#'
#' @return Returns a matrix. The first column contain the number 1 repeated \code{Tt} times. The second column contains
#' the alr transformation of the \code{especieII} in all time points. The third column
#' contains the balance (whose numerator has all the bacteria except \code{especieII} and \code{EspecieMaxima} and the denominator
#' contains the \code{EspecieMaxima}) in all time points.
#'
#'
#' @examples
#'
#'
#' Tt=2
#' especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
#' especieII=1
#' E=3
#' EspecieMaxima=3
#'
#' B1MODImatrizP(Tt, especieII,especie1, E, EspecieMaxima)
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


B1MODImatrizP=function(Tt, especieII,especie, E, EspecieMaxima){ #This function calculates the piece of the ridge regression matrix that correspondence to the especieII.
  #Tt is the number of time points that the ridge regression matrix takes into account
  uno=rep(1,Tt)

  CocienteLogaridmos=rep(0,Tt)
  for (j in 1:Tt){
    CocienteLogaridmos[j]=log(especie[especieII,j]/especie[EspecieMaxima,j])
  }


  VectorBalances=rep(0,Tt)
  VectorBalances=Balance(Tt,especieII,especie, E, EspecieMaxima)


  matrizP=matrix(0,Tt,3)

  matrizP=cbind(uno, CocienteLogaridmos, VectorBalances)


  return(matrizP)
}

