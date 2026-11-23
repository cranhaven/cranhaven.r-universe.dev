#' Writting the loglikelihood of the dirichlet
#'
#' This function calculates the loglikelihood of the dirichlet for the FBM model.
#'
#'
#' The regression of this model is defined by
#'
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'
#'
#'@param especie Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param param Vector with the parameters in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#'@param E Number of bacteria available
#'@param EspecieMaxima Row in which the bacterial with maximum mean abundance is in \code{especie}.This bacteria is used as reference in the alr transformation that the model does and it is placed at the denominator of the balance)
#'@param Tt Number of bacteria available
#'@param especiemodi Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt}. The bacteria are placed in the same order than in \code{especie}.
#'
#'
#'@return Returns a number with the value of the dirichlet loglikelihood.
#'
#' @examples
#'
#'
#'especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
#'especiemodi=especie1[,-1]
#'tau1=0.4
#'parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
#'parms11=c(as.vector( t(parms1)),tau1)
#'
#'EstParmFunc_FBM(parms11,especie1,3 ,3  , 2,especiemodi)
#'
#' @references Creus-Martí, I., Moya, A., Santonja, F. J. (2021). A Dirichlet autoregressive model for the analysis of microbiota time-series data. Complexity, 2021, 1-16.
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

EstParmFunc_FBM=function(param,especie,E,EspecieMaxima,Tt, especiemodi){ #This function writes the loglikelihood function
  #param is a vector that contains all the parameters, the parameter tau is at the last position
  H=length(param)
  a=rep(0,H)
  for (i in 1:H){
    a[i]=param[i]
  }

  #Converting the vector of parameters into a matrix (E-1)x3
  params=a[-H]
  MatrizParametros=matrix(0,E-1,3)
  MatrizParametros[1,]=params[1:3]
  for (i in 1:(E-2)){
    MatrizParametros[i+1,]=params[(1+3*i):(3+3*i)]
  }

  mu=B1MODImodel(MatrizParametros,especie,E,EspecieMaxima,Tt) #Applying the model


  denominador<-rep(0,Tt)

  ExpMu=exp(mu)
  for (i in 1:Tt){
    denominador[i]= 1+ sum(ExpMu[,i])
  }

  #Defining dirichlet parameters called alpha
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


  #Writting loglikelihhod function
  AlphaMenos1=alpha-1
  LogEspecieModi=log(especiemodi)
  MatrizSumas=t(AlphaMenos1)%*%LogEspecieModi
  loglike<-rep(0,Tt-1)
  for(i in 1:(Tt-1)){
    loglike[i]=log(gamma(a[H]))-sum(log(gamma(alpha[,i])))+diag(MatrizSumas)[i]
  }
  loglikeVerdad=sum(loglike)

  return(loglikeVerdad)
}

