#' Obtains the matrix of covariates of the BPBM
#'
#' This function writes the matrix of covariates of the BPBM.
#'
#' In an example with two SPBal and three time points, the covariates are written in the following  order:
#'
#'  \tabular{rrr}{
#'  1 \tab  1  \tab  1\cr
#'  \eqn{SPBal_{1,t-1}} \tab  \eqn{SPBal_{1,t-2}} \tab   \eqn{SPBal_{1,t-3}} \cr
#'  \eqn{SPBal_{2,t-1}} \tab  \eqn{SPBal_{2,t-2}} \tab   \eqn{SPBal_{2,t-3}} }
#'
#'
#'@param PB Matrix. Each line os the matrix PB contains the values of one principal balance at all time points.
#'@param rows.position Vector. Vector with the number of the rows where the SPBal are in the matrix PB. We write first the row in which the balance with higher variance is placed, then the row in which the balance with second higher variance is placed...
#'@param Tt Number of time points available.
#'
#'@return Returns a matrix with the covariates of the model.
#'
#'
#' @examples
#'
#'matt=matrix(c(1:12),3,4)
#'rows.position=c(2,3)
#'BPBM_Matrix(rows.position,matt,4)
#'
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

BPBM_Matrix<-function(rows.position, PB, Tt){
  #After studying the percentage of variance we know that the following balances are the  selected principal balances (SPBal)
  MatrizPBmodelo=matrix(0,length(rows.position)+1,Tt)
  MatrizPBmodelo[1,]=rep(1,Tt)


  for(i in 1:length(rows.position)){
    MatrizPBmodelo[i+1,]=PB[rows.position[i],]
  }

    #The first row of this matrix is 1 in all the columns. Following he first row, it contains at each row a selected principal balance in all time points.

  return(MatrizPBmodelo)

}


