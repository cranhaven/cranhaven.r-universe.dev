#' Predicting using FBM
#'
#' This function calculates the expected value and variance of the bacteria at time point \code{Tt}. Then, this function calculates the expected value and variance of the bacteria at time point t=(\code{Tt}+1),...,K
#'
#' The regression of this model is defined by
#'
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'
#' @param paramEstimadosFinal The estimate parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#' @param Var Matrix that contains at row i the variance of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param esperanza Matrix that contains at row i the expected value of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param alpha Matrix that contains at the row i the Dirichlet parameter of the bacteria i at t=1,2,3,...,\code{Tt}.
#' @param E Number of bacteria available
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}.This bacteria is used as reference in the alr transformation that the model does and it is placed at the denominator of the balance)
#' @param Tt Number of bacteria available
#' @param K Number. The function will calculate the value of the expected value and the variance at \code{Tt} and predict for the time points t=\code{Tt}+1,..,K. To predict all the time points available at the data we K=dim(especie.All)-1
#'
#' @return Returns a list with:
#'
#'\itemize{
#'    \item ExpectedValue.All:  Matrix. Matrix that contains at row i the expected value of the bacteria i at all time points t=1,2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'    \item VarianceValue.All:  Matrix. Matrix that contains at row i the variance of the bacteria i at all time points t=1,2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'    \item DirichlerParam.All: Matrix.   Matrix that contains at row i the dirichlet parameter of the bacteria i at all time points t=1,2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'   }
#' @examples
#'
#'Tt=2
#'E=3
#'tau=5
#'EspecieMaxima=3
#'K=3
#'parms11=c(0.1,0.2,0.3,0.4,0.5,0.6,tau)
#'alpha=cbind(c(1.726793,1.892901,1.380306),
#'            c(1,1,3))
#'Expected=cbind(c(alpha[1,1]/tau, alpha[2,1]/tau, alpha[3,1]/tau  ),
#'               c(alpha[1,2]/tau,alpha[2,2]/tau,alpha[3,2]/tau))
#'Variance=cbind(c(0.03768101, 0.03920954, 0.03330857 ),
#'               c( 0.03683242,0.02784883, 0.0413761 ))
#'Expected.final=Expected[,-2]
#'Variance.final=Variance[,-2]
#'
#'PredictionFBM(parms11,EspecieMaxima, alpha,K,Expected.final,Variance.final,E,Tt )
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

PredictionFBM<-function(paramEstimadosFinal,EspecieMaxima, alpha,K,esperanza,Var,E,Tt ){


H=length(paramEstimadosFinal)
a<-rep(0,H)
for (i in 1:H){
  a[i]=paramEstimadosFinal[i]#Vector with the estimated parameters
}


#Writting in a matrix form the estimated parameters (all of them except tau)
params=a[-H]
MatrizParametros=matrix(0,E-1,3)
MatrizParametros[1,]=params[1:3]
for (i in 1:(E-2)){
  MatrizParametros[i+1,]=params[(1+3*i):(3+3*i)]
}

A=MatrizParametros #Matrix with all the estimated parameters except tau

#Obtaining expected values and variance at time point Tt
esperanzaPred=matrix(0,E,K)
for (j in 1:E){
  esperanzaPred[j,Tt]=(alpha[j,Tt])/(a[H])
}
VarPred=matrix(0,E,K)
for (j in 1:E) {
  VarPred[j,Tt]=((alpha[j,Tt])*(a[H]-alpha[j,Tt]))/((a[H]+1)*(a[H])^(2))
}


muPred=matrix(0,E-1,K)
alphaPred=matrix(0,E,K)
CocienteLogaridmosPredi=matrix(0,E-1,K)
LogEspeciePred=matrix(0,E,K)
MatrizBalances=matrix(0,E-1,K)

denominador=rep(0,K)
#Predicting expected values and variance at time point t=(Tt+1),...,K
for (i in (Tt+1):K){

  #Calculating alr transformation of the expected value obtained with the model at t-1
  for (j in 1:E-1){
    CocienteLogaridmosPredi[j,i-1]=log(esperanzaPred[j,i-1]/esperanzaPred[EspecieMaxima,i-1])
  }

  #Calculating balances using the expected value obtained with the model at t-1
  LogEspeciePred[,i-1]=log(esperanzaPred[,i-1])

  for(j in 1:(E-1)){
    MatrizBalances[j,i-1]=((1/(E-2))*(sum(LogEspeciePred[,i-1][-c(EspecieMaxima,j)]))-LogEspeciePred[,i-1][EspecieMaxima])#(sqrt((E-2)/(E-2+1)))*
  }

  uno=rep(1,E-1)

  MatrizMetodo=rbind(uno, CocienteLogaridmosPredi[,i-1], MatrizBalances[,i-1])#Matrix that contains the coefficients of the model parameters

  for (j in 1:(E-1)){
    muPred[j,i]=A[j,]%*%MatrizMetodo[,j] #Applying the model to predict
  }

  #Calculating dirichlet parameters (alphas)
  ExpMuPred=exp(muPred[,i])

  denominador[i]= 1+ sum(ExpMuPred)

  for(j in 1:E){
    if (j==EspecieMaxima){
      alphaPred[j,i]=a[H]/denominador[i]
    }else{
      alphaPred[j,i]=a[H]*(exp(muPred[j,i]))/denominador[i]
    }
  }

  #Calculating the expected values obtained with the prediction
  for (j in 1:E){
    esperanzaPred[j,i]=(alphaPred[j,i])/(a[H])
  }

  #Calculating the variance obtained with the prediction
  for (j in 1:E) {
    VarPred[j,i]=((alphaPred[j,i])*(a[H]-alphaPred[j,i]))/((a[H]+1)*(a[H])^(2))
  }

}

EsperanzaFinal=cbind(esperanza, esperanzaPred[,-c((1:(Tt-1)))])#Joining in a matrix the expected values obtained when estimating the model and when predicting
colnames(EsperanzaFinal)<-NULL
VarFinal=cbind(Var, VarPred[,-c((1:(Tt-1)))])#Joining in a matrix the variance obtained when estimating the model and when predicting
colnames(VarFinal)<-NULL
alphaFinal=cbind(alpha, alphaPred[,-c((1:Tt))])
colnames(alphaFinal)<-NULL

return.pred<-list(EsperanzaFinal,VarFinal,alphaFinal)
names(return.pred)<-c("ExpectedValue.All", "VarianceValue.All","DirichlerParam.All")
return(return.pred)

}
