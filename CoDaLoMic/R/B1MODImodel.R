#' Obtaining the regression value of the FBM
#'
#' This function calculates the value of the FBM regression, defined by:  \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'
#' @param A  Matrix of dimensions (\code{E}-1)x3 that contains all the parameters of the model except tau
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#' @param E Number of bacteria available
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}.This bacteria is used as reference in the alr tranformation that the model does and it is placed at the denominator of the balance)
#' @param Tt Number of time points available.
#'
#' @return Returns a matrix. The row i contains the regression values of the bacteria i at all time points.
#'
#' @examples
#'
#'
#' df<-data.frame(cbind(c(0.1,0.1,0.8),c(0.2,0.1,0.7)))
#' E=3
#' EspecieMaxima=3
#' set.seed(724)
#' A=matrix(c(-2:3),2,3)
#' Tt=2
#'
#' B1MODImodel(A,df, E, EspecieMaxima,Tt)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2021). A Dirichlet autoregressive model for the analysis of microbiota time-series data. Complexity, 2021, 1-16.
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

B1MODImodel=function(A,especie,E,EspecieMaxima,Tt){#A is a matrix of dimensions (E-1)x3 that contains all the parameters of the model except tau
  #This function applies the proposed model to the data
  CocienteLogaridmos=matrix(0,E-1,Tt)
  for (i in 1:E-1){
    for (j in 1:Tt){
      CocienteLogaridmos[i,j]=log(especie[i,j]/especie[EspecieMaxima,j])
    }}


  MatrizBalances=matrix(0,E-1,Tt)
  for (i in 1:E-1){
    MatrizBalances[i,]=Balance(Tt,i,especie, E, EspecieMaxima)
  }

  uno=rep(1,E-1)
  mu=matrix(0,E-1,Tt)

  for (j in 1:Tt){
    MatrizAR=rbind(uno, CocienteLogaridmos[,j], MatrizBalances[,j])
    for (i in 1:(E-1)){
      mu[i,j]=A[i,]%*%MatrizAR[,i]
    }
  }
  return(mu)
}


