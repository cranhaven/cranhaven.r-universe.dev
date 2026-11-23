#' Obtaining the principal balances values
#'
#' Calculates the value of the principal balances (Martín-Fernández et al, 2018) at all time points.
#'
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param Tt  Number of time points available
#'@param Num List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the principal balance i are placed.
#'@param Dem List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the principal balance i are placed.
#'
#'
#'@return Returns a matrix where the row i has the value of the principal balance at all time points. The principal balance of the row i has at its numerator the bacteria placed in the rows \code{Num[[i]]} of the \code{especie}. The principal balance of the row i has at its denominator the bacteria placed in the rows \code{Dem[[i]]} of the \code{especie}.
#'
#'
#'
#'
#' @examples
#'
#'set.seed(314)
#'esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
#'
#'Num2<-list(3,c(3,5),1,c(3,5,4))
#'Dem2<-list(5,4,2,c(1,2))
#'
#'ObtainigValuePB(Num2,Dem2,esp2,6)
#'
#'@references
#'\itemize{
#'\item Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#'\item Martín-Fernández, J. A., Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosona-Delgado, R. (2018). Advances in principal balances for compositional data. Mathematical Geosciences, 50, 273-298.
#' }
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
ObtainigValuePB<-function(Num, Dem, especie, Tt){

  if(length(Num)!=length(Dem)){
    stop("Num and Dem must have the same length")
    }

  PB=matrix(0,length(Num),Tt)
  for (i in 1:length(Num)){
  PB[i,]=PBalance(Tt,Num[[i]],Dem[[i]], especie)
  }

  return(PB)
}
