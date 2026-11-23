#' Obtaining the regression value of the BPBM
#'
#' This function calculates the value of the BPBM regression, defined by:  \deqn{\mu_{it}=a_{i0}+a_{i1}\cdot\text{SPBal}_{1,t-1}+\cdots+a_{iM}\cdot\text{SPBal}_{M,t-1}}
#'
#'@param MatrizPBmodelo Matrix. Output of "ObtainingValueSPBal" called "MatrixSPBal".
#'@param E Number of bacteria in the dataset.
#'@param Tt Number of time points.
#'@param A  Matrix that contains all the parameters of the model. The parameters are written in the matrix in the following order (in an example with three bacteria):
#'
#'    \tabular{rrrrr}{
#'  a10 \tab  a11  \tab  a12\tab ...\tab  a1M \cr
#'  a20 \tab  a21  \tab  a22\tab ...\tab  a2M \cr
#'  a30 \tab  a31  \tab  a32\tab ...\tab  a3M}
#'
#'
#' @return Returns a matrix. The row i contains the regression values of the bacteria i at all time points.
#'
#' @examples
#'
#'A=rbind(c(1,2,3),c(4,5,6),c(7,8,9))
#'MatrizPBmodelo=cbind(c(1,2,3),c(4,5,6),c(7,8,9))
#'E=3
#'Tt=3
#'
#'
#' PBmodel(A,MatrizPBmodelo, E,Tt)
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

PBmodel=function(A,MatrizPBmodelo,E,Tt){ #This function writes the equations of the proposed model
  #A is the matrix of parameters


  mu=matrix(0,E,Tt)
  for (j in 1:Tt){
    mu[,j]=A%*%MatrizPBmodelo[,j]
  }

  return(mu)
}
