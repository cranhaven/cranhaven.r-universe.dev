#' Obtainig the value of the dirichlet parameters, the expected value and the variance.
#'
#' This function calculates the value of the dirichlet parameters, the expected value and the variance for the BPBM model.
#'
#' The regression of this model is defined by:
#'
#'
#'\deqn{\mu_{it}=a_{i0}+a_{i1}\cdot\text{SPBal}_{1,t-1}+\cdots+a_{iM}\cdot\text{SPBal}_{M,t-1}}
#'
#'
#'
#' @param Estimated.Param Vector with the estimate parameters. Column "mean" of the output of "StudyingParam" function.
#' @param E Number of bacteria available.
#' @param Tt Number of time points available.
#' @param MatrizPBmodelo Matrix. Output of "ObtainingValueSPBal" called "MatrixSPBal".
#'
#' @return Returns a list with:
#'
#'\itemize{
#'   \item Dirichlet.Param:  Matrix. Matrix that contains at row i the dirichlet parameter of the bacteria i at all time points.
#'   \item Expected.Value:  Matrix. Matrix that contains at row i the expected value of the bacteria i at all time points. The bacterias are placed at the same orden than in \code{especies}.
#'    \item Variance.Value:  Matrix. Matrix that contains at row i the variance of the bacteria i at all time points. The bacterias are placed at the same orden than in \code{especies}.
#'   }
#'
#' @examples
#'
#'Tt=3
#'E=3
#'Estimated.Param=c(0.1 ,0.4, 0.7, 0.2 ,0.5, 0.8 ,0.3, 0.6, 0.9,
#'                  0.1, 0.4 ,0.7, 0.2, 0.5, 0.8, 0.3 ,0.6, 0.9)
#'MatrizPBmodelo=rbind(c(1,1,1),c(0.3,0.6,-0.1),c(0.2,-0.4,0.3))
#'ExpectedValuess_BPBM(Estimated.Param,MatrizPBmodelo,E,Tt)
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

ExpectedValuess_BPBM<-function(Estimated.Param,MatrizPBmodelo,E,Tt){#Writing the estimate parameters in a matrix form

   Param.a=FromVectorToMatrix_BPBM(Estimated.Param,MatrizPBmodelo,E)

  mu=PBmodel(Param.a,MatrizPBmodelo,E,Tt) #Obtaining log(alpha)

  alpha=exp(mu) #Obtaining dirichlet parameters (alphas)

  #Obtaining expected values and variance

  tau=rep(0,Tt)
  for (i in 1:Tt){
    tau[i]=sum(alpha[,i])
  }

  esperanza=matrix(0,E,Tt-1)
  for (j in 1:E){
    for (i in 1:(Tt-1)){
      esperanza[j,i]=(alpha[j,i])/(tau[i])
    }
  }#This matrix contains the expected value of the family i at time point j


  Var=matrix(0,E,Tt-1)
  for (j in 1:E) {
    for (i in 1:(Tt-1)) {
      Var[j,i]=((alpha[j,i])*(tau[i]-alpha[j,i]))/((tau[i]+1)*(tau[i])^(2))
    }}#This matrix contains the variance of the family i at time point j

  Values.final<-list(alpha, esperanza, Var)
  names(Values.final)<-c("Dirichlet.Param", "Expected.Value", "Variance.Value")

  return(Values.final)

}
