#' Ridge regression
#'
#' Ridge regression
#'
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of the matrix will be used as reference in the alr transformation and will be at the denominator of the balance.
#' @param E Number of bacteria available.
#' @param Tt Number of time points available
#' @param EspecieMaxima Row in which the bacteria used as reference is in \code{especie}. This is the bacteria that is going to be at the denominator of the balance and the denominator of the alr transformation. As a result, in this function, \code{EspecieMaxima} must be equal to \code{E}
#' @param seed Number. Set a seed. Default \code{seed=NULL}.
#'
#' @return Returns the result of the ridge regression, object of class "ridgelm".
#'
#'
#' @examples
#'
#'
#' set.seed(123)
#' especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
#' Tt=10
#' E=5
#' EspecieMaxima=5
#'
#'ridgeregression(Tt,especie, E, EspecieMaxima, 558562316)
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


ridgeregression<-function(Tt,especie, E, EspecieMaxima, seed=NULL){

  mmatriz0=matrix(0,(Tt-1)*(E-1),3*(E-1))
  for(i in 1:(E-1)){
    mmatriz0[c((i*(Tt-1)-(Tt-1)+1):(i*(Tt-1))), c((3*i-2):(3*i))]= B1MODImatrizP(Tt-1,i,especie, E, EspecieMaxima)
  }
  matrizFinal=mmatriz0

  #Writing ridge regression vector
  A=rep(0,(Tt-1)*(E-1))
  for(i in 1:(E-1)){
    A[c((i*(Tt-1)-(Tt-1)+1):(i*(Tt-1)))]=vecttor(i,especie,Tt, EspecieMaxima)
  }


  if(!is.null(seed)){
    set.seed(seed)
  }


  # Selection of ridge regression constant
  ridgec <- MASS::lm.ridge(A~matrizFinal, lambda = seq(0, 1000, .01))
  #select(ridgec)


  choosenlabda=as.numeric(broom::glance(ridgec)[1,3])

  #Applying ridge regression
  ridge.final <- MASS::lm.ridge (A~matrizFinal, lambda = choosenlabda)

  return(ridge.final)
}
