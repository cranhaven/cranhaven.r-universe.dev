#' Obtaining the value of tau and the estimate value of the rest of the parameters
#'
#' This function estimates the parameters of the FBM model.
#'
#' We give to the parameter tau the value 1,2,...,\code{ttau}. We estimate the FBM model for all this values (using the function "Estimate_param_FBM")
#' and we select the value of tau that minimizes the AIC. The regression of this model is defined by
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'@param ttau Number. We estimate de FBM model for the values of tau: 1, 2,..., ttau
#'@param Iter.EstParmFunc  Number. Number of iterations. Default: 80 iterations.
#'@param ridge.final Object of class "ridgelm". Values obtained with the ridge regression.
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of the matrix will be used as reference in the alr transformation and will be at the denominator of the balance.
#'@param Tt Number of time points available
#'@param EspecieMaxima Row in which the bacteria used as reference is in \code{especie}. This is the bacteria that is going to be at the denominator of the balance and at the denominator of the alr transformartion. As a result, in this function, \code{EspecieMaxima} must be equal to \code{E}
#'@param E Number. Number of bacteria available.
#'@param seed Number. Set a seed. Default \code{seed=NULL}.
#'
#'@return Returns a list with:
#'\itemize{
#'   \item EstimateParameters:  Vector with the estimated parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#'   \item AIC Number: Value of the AIC.
#'  \item All.iter:  Matrix. Each row has the parameters obtained in each iteration. The parameters are in the columns written in the same order that they are written in \code{Param.Estimates }. In this matrix we must observe that in the last iterations the values has really similar or equal values, if not, we need to increase the value of \code{Iter.EstParmFunc}.
#'
#'   }
#'
#'@examples
#'
#'set.seed(123)
#'especie=t(gtools::rdirichlet(5,c(1,3,1)))
#'Tt=5
#'E=3
#'EspecieMaxima=3
#'ridge.final=ridgeregression(Tt,especie, E, EspecieMaxima)
#'ttau=10
#'Iter.EstParmFunc=10
#'
#'TauAndParameters_EstParmFunc_FBM(ttau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E,714)
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2021). A Dirichlet autoregressive model for the analysis of microbiota time-series data. Complexity, 2021, 1-16.
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

TauAndParameters_EstParmFunc_FBM<-function(ttau=30,ridge.final,Iter.EstParmFunc=80, especie,EspecieMaxima,Tt,E,seed=NULL){


#We try the values of tau: 1,2,...,30
Dif<-list()
for (l in 1:ttau){
  Dif[[l]]=Estimate_Param_FBM(l,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E, seed)
}

Dif.AIC=rep(0, ttau)
for (i in 1:ttau){
Dif.AIC[i]=Dif[[i]]$AIC
}


#We search the vale of tau that minimizes the AIC
for (i in 1:length(Dif.AIC)){
  if (Dif.AIC[i]==min(Dif.AIC)){
    AICminimo=i
  } else{
    i=i+1}
}

if(min(Dif.AIC[-AICminimo])==min(Dif.AIC)){
  warning("There are one or more values of tau that minimixes the AIC")
}



AICfinal=Dif[[AICminimo]]$AIC #AIC obtained with the adequate tau
paramEstimadosFinal=Dif[[AICminimo]]$Param.Estimates#Estimated parameters obtained with the adequate tau
MatrizConvergenceFinal=Dif[[AICminimo]]$All.iter

EstimatesFinal<-list(AICfinal,paramEstimadosFinal,MatrizConvergenceFinal)
names(EstimatesFinal)<-c("AIC", "EstimateParameters", "All.iter")


return(EstimatesFinal)
}
