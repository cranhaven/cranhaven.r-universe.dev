#' Obtaining a vector with the covariates of the prediction
#'
#' Calculates a vector with the covariates of the BPBM model in one time point.
#'
#'@param v   Vector. Vector with a coda composition. The bacteria ar in the same orden than the matrix \code{especie}
#'@param MatrizPBmodelo  the matrix that contains the covariates of the model. The first line es equal to 1 for all columns. The other rows contain the value of one SPBal at all time points. The selected principal balance of the row i+1 has at its numerator the bacteria placed in the rows \code{NumSPBal[[i]]} of the "especie". The selected principal balance of the row i+1 has at its denominator the bacteria placed in the rows \code{DemSPBal[[i]]} of the "especie".
#'@param NumSPBal List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the principal balance i are placed.
#'@param DemSPBal List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the principal balance i are placed.
#'
#'
#'@return Returns a vector where the first component is a 1 and the following components have the values of the SPBal.  The SPBal in the component i+1  has at its numerator the bacteria placed in the rows \code{Num[[i]]} of the \code{especie}. The SPBal of the component i+1  has at its denominator the bacteria placed in the rows \code{Dem[[i]]} of the \code{especie}.
#'
#'
#'
#'
#' @examples
#'
#'v=c(0.1,0.1,0.2,0.3,0.3)
#'Num2<-list(3,c(3,5),1,c(3,5,4))
#'Dem2<-list(5,4,2,c(1,2))
#'MatrizPBmodelo=rbind(c(1,1,1,1),
#'                     c(-0.3,0.2,0.5,0.6),
#'                     c(-0.4,0.3,0.5,0.6),
#'                     c(0.5,0.3,0.2,0.7),
#'                     c(-0.2,0.9,0.2,0.1)   )
#'
#'FVectorPBmodeloPredi(Num2,Dem2,v,MatrizPBmodelo)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#'@export
#'
#'
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

FVectorPBmodeloPredi=function(NumSPBal,DemSPBal,v,MatrizPBmodelo){

  if(length(NumSPBal)!=length(DemSPBal)){
    stop("Num and Dem must have the same length")
  }

  VectorPBmodeloPredi=rep(0,dim(MatrizPBmodelo)[1])
  VectorPBmodeloPredi[1]=1
  for (i in 1: length(NumSPBal)){
    VectorPBmodeloPredi[i+1]=PBalancePredi(NumSPBal[[i]],DemSPBal[[i]],v)
  }


  return(VectorPBmodeloPredi)
}
