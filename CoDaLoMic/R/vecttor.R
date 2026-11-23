#' Alr of a bacteria
#'
#' Writtes a vector with the alr transformation of the bacteria \code{i} at time points t=2,...,\code{Tt}.
#'
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of the matrix will be used as reference in the alr transformation and will be at the denominator of the balance.
#' @param Tt Number of time points available
#' @param EspecieMaxima Row in which the bacteria used as reference is in \code{especie}. This is the bacteria that is going to be at the denominator of the balance and the denominator of the alr transformation. As a result, in this function, \code{EspecieMaxima} must be equal to \code{E}
#' @param i Number. Position of the bacteria that we make the alr in the matrix \code{especie}. \code{i} must be different that \code{EspecieMaxima}.
#'
#' @return Returns a vector with the alr transformation of the bacteria \code{i} at time points t=2,...,\code{Tt}.
#'
#'
#' @examples
#'
#'
#' set.seed(123)
#' especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
#' Tt=10
#' EspecieMaxima=5
#' i=2
#'
#' vecttor(i,especie,Tt,EspecieMaxima)
#'
#'
#' @export
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




vecttor<-function(i,especie,Tt,EspecieMaxima){
  vectorF=rep(0,Tt-1)
  for (j in 1:(Tt-1)){
    vectorF[j]=log(especie[i,j+1]/especie[EspecieMaxima,j+1])
  }
  return(vectorF)
}
