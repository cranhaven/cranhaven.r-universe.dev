#' Predicting using dirichl-gLV
#'
#'This function calculates the expected value and variance of the bacteria at time point \code{Tt}. Then, this function calculates the expected value and variance of the bacteria at time point t=(\code{Tt}+1),...,K
#'
#' The regression of this model, in an example with three bacteria, is defined by
#'
#' \deqn{r_{1}\cdot log(x_{1}(t)/x_{3}(t))+log(x_{1}(t)/x_{3}(t))\cdot [a_{11}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{12}\cdot log(x_{2}(t)/x_{3}(t))] }
#' \deqn{r_{2}\cdot log(x_{2}(t)/x_{3}(t))+log(x_{2}(t)/x_{3}(t))\cdot [a_{21}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{22}\cdot log(x_{2}(t)/x_{3}(t))] }
#'
#'
#' @param paramEstimadosFinal The estimate parameters. Vector equal to \code{c(tau,as.vector( pam))} where:
#'
#'\itemize{
#'   \item pam  Matrix. Each row has the parameters of each bacteria. Following our example, pam has the parameters placed as follows:
#'   \tabular{rrrr}{
#'  r1 \tab  a11  \tab  a12\cr
#'  r2 \tab  a21 \tab   a22 }
#'   \item tau  Number. Value of the tau parameter in the model
#'   }
#'
#' @param Var Matrix that contains at row i the variance of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param esperanza Matrix that contains at row i the expected value of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param alpha Matrix that contains at the row i the dirichlet parameter of the bacteria i at t=1,2,3,...,\code{Tt}.
#' @param E Number of bacteria available
#' @param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}.This bacteria is used as reference in the alr transformation that the model does.
#' @param Tt Number of time points available
#' @param K Number. The function will calculate the value of the expected value and the variance at \code{Tt} and predict for the time points t=\code{Tt}+1,..,K. To predict all the time points available at the data we K=dim(especie.All)-1
#'
#' @return Returns a list with:
#'
#'\itemize{
#'    \item ExpectedValue.All:  Matrix. Matrix that contains at row i the expected value of the bacteria i at all time points t=2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'    \item VarianceValue.All:  Matrix. Matrix that contains at row i the variance of the bacteria i at all time points t=2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'   \item DirichlerParam.All:  Matrix. Matrix that contains at row i the dirichlet parameter of the bacteria i at all time points t=2,...,K. The bacteria are placed at the same order than in \code{especies}.
#'   }
#' @examples
#'
#'pam.ini=rbind(c(0.1,0.2,0.3),c(0.4,0.5,0.6))
#'paramEstimadosFinal=c(5, as.vector(pam.ini))
#'EspecieMaxima=3
#'alpha=cbind(c(2,2,3),c(1,1,3))
#'K=3
#'esperanza=cbind(c(0.2,0.3,0.5))
#'Var=cbind(c(0.2,0.3,0.5))
#'E=3
#'Tt=2
#'
#'PredictionEstParmFunc(paramEstimadosFinal,EspecieMaxima, alpha,K,esperanza,Var,E,Tt )
#'
#'@references Creus-Martí, I. and Moya, A. and Santonja, F. J. (2018). A Statistical Model with a Lotka-Volterra Structure for Microbiota Data. Lucas Jodar, Juan Carlos Cortes and Luis Acedo,  Modelling or engineering and human behavior 2018, Instituto Universitario de Matematica Multidisciplinar. ISBN: 978-84-09-07541-6
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

PredictionEstParmFunc<-function(paramEstimadosFinal,EspecieMaxima, alpha,K,esperanza,Var,E,Tt ){


  parms.vector=paramEstimadosFinal
  tau=parms.vector[1]
  parms.vector.m=parms.vector[-1]
  parms=matrix(0,dim(esperanza)[1]-1,dim(esperanza)[1])
  m=length(parms.vector.m)/(dim(esperanza)[1]-1)
  parms[,1]=parms.vector.m[c(1:dim(parms)[1])]
  for(i in 1:(m-1)){
    parms[,i+1]=parms.vector.m[c((1+i*dim(parms)[1]):(1+i*dim(parms)[1]+dim(parms)[1]-1))]
  }


  A=parms #Matrix with all the estimated parameters except tau

  #Obtaining expected values and variance at time point Tt
  esperanzaPred=matrix(0,E,K)
  for (j in 1:E){
    esperanzaPred[j,Tt]=(alpha[j,Tt])/tau
  }
  VarPred=matrix(0,E,K)
  for (j in 1:E) {
    VarPred[j,Tt]=((alpha[j,Tt])*(tau-alpha[j,Tt]))/((tau+1)*(tau)^(2))
  }



  alphaPred=matrix(0,E,K)
  alr.Expected=matrix(0,E-1,K)
  regre=matrix(0,E-1,K)
  exp.regre=matrix(0,E-1,K)

  denominador=rep(0,K)
  #Predicting expected values and variance at time point t=(Tt+1),...,K
  for (i in (Tt+1):K){

    alr.Expected[,i]=compositions::alr(esperanzaPred[,i-1])

    regre[,i]<- rxnrate( alr.Expected[,i],A)


    exp.regre[,i]=exp(regre[,i])



    denominador[i]=1+sum(exp.regre[,i])

    for(j in 1:E){
      if (j==EspecieMaxima){
        alphaPred[j,i]=tau/denominador[i]
      }else{
        alphaPred[j,i]=tau*(exp.regre[j,i])/denominador[i]
      }
    }

    #Calculating the expected values obtained with the prediction
    for (j in 1:E){
      esperanzaPred[j,i]=(alphaPred[j,i])/tau
    }

    #Calculating the variance obtained with the prediction
    for (j in 1:E) {
      VarPred[j,i]=((alphaPred[j,i])*(tau-alphaPred[j,i]))/((tau+1)*(tau)^(2))
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
