#' Obtainig the value of the dirichlet parameters, the expected value and the variance.
#'
#' This function calculates the value of the dirichlet parameters, the expected value and the variance for the FBM model.
#'
#' The regression of this model is defined by
#'
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'
#' @param paramEstimadosFinal The estimate parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#' @param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#' @param E Number of bacteria available.
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}.This bacteria is used as reference in the alr tranformation that the model does and it is placed at the denominator of the balance).
#' @param Tt Number of time points available.
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
#'set.seed(123)
#'especie=t(gtools::rdirichlet(2,c(1,1,3)))
#'Tt=2
#'E=3
#'tau=5
#'EspecieMaxima=3
#'Iter.EstParmFunc=5
#'parms11=c(0.1,0.2,0.3,0.4,0.5,0.6,tau)
#'
#'ExpectedValues_EstParmFunc_FBM(parms11 , especie,E,EspecieMaxima,Tt)
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

ExpectedValues_EstParmFunc_FBM<-function(paramEstimadosFinal,especie,E,EspecieMaxima,Tt){


#Obtaining dirichlet parameters (alphas) and expected values
H=length(paramEstimadosFinal)

a<-rep(0,H)
for (i in 1:H){
  a[i]=paramEstimadosFinal[i]
}

params=a[-H]
MatrizParametros=matrix(0,E-1,3)
MatrizParametros[1,]=params[1:3]
for (i in 1:(E-2)){
  MatrizParametros[i+1,]=params[(1+3*i):(3+3*i)]
}

mu=B1MODImodel(MatrizParametros,especie,E,EspecieMaxima,Tt) #Applying the proposed model


denominador<-rep(0,Tt)

ExpMu=exp(mu)
for (i in 1:Tt){
  denominador[i]= 1+ sum(ExpMu[,i])
}


#Obtainig dirichlet parameters
alpha=matrix(0,E,Tt)
for(j in 1:E){
  if (j==EspecieMaxima){
    for(i in 1:Tt){
      alpha[j,i]=a[H]/denominador[i]
    }
  }else{
    for(i in 1:Tt){
      alpha[j,i]=a[H]*ExpMu[j,i]/denominador[i]
    }
  }
}

esperanza=matrix(0,E,Tt-1)
for (j in 1:E){
  for (i in 1:(Tt-1)){
    esperanza[j,i]=(alpha[j,i])/(a[H])
  }
}#This matrix contains the expected value of the family i at time point j


Var=matrix(0,E,Tt-1)
for (j in 1:E) {
  for (i in 1:(Tt-1)) {
    Var[j,i]=((alpha[j,i])*(a[H]-alpha[j,i]))/((a[H]+1)*(a[H])^(2))
  }}#This matrix contains the variance of the family i at time point j

Values.final<-list(alpha, esperanza, Var)
names(Values.final)<-c("Dirichlet.Param", "Expected.Value", "Variance.Value")

return(Values.final)

}
