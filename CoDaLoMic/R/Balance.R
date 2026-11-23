#' Calculating the balance of the FBM model
#'
#' Defining a balance where we compare all the bacteria (except the one chosen as reference and the \code{especieI}) with the one chosen as reference.
#'
#'
#' @param A Number of time points for which we calculate the balance
#' @param especieI  Number. The bacteria that we do not include in the balance. We must write the number of the row in which the bacteria is placed in the matrix \code{especie}.
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#' @param E Number of bacteria available
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}.This bacteria is used as reference in the alr tranformation that the model does and it is placed at the denominator of the balance)
#'
#' @return Returns a vector with the value of the balance for all the time points indicated.
#'
#' @examples
#'
#' Balance(2,2,cbind(c(0.1,0.1,0.8),c(0.2,0.1,0.7)),3,3)
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




Balance=function(A,especieI,especie, E, EspecieMaxima){ #Defining a balance when we compare all the families except the most abundance and the especieI with the most abundance family.
  #A means the number of time points for which we calculate the balance.
  Logespecie=log(especie)
  B=rep(0,A)
  for(i in 1:A){
    B[i]=((1/(E-2))*(sum(Logespecie[,i][-c(EspecieMaxima,especieI)]))-Logespecie[EspecieMaxima,i])
  }

  return(B)
}
