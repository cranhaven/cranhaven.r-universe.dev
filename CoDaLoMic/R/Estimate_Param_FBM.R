#' Estimating Parameters of EstParmFunc_FBM
#'
#' This function estimates the parameters of the FBM model.
#'
#'
#' Maximum likelihood estimation is used. This function makes an iterative process, for a given
#' value of tau, it obtains the value of the rest of the parameters that maximize the dirichlet loglikelihood
#' (defined in EstParmFunc_FBM) using the Nelder-Mead method and the values obtained in the ridge regression as initial parameters.
#' Then it uses the values obtained as initial parameters and repeats the process \code{Iter.EstParmFunc} times.
#'
##' The regression of this model is defined by
#'
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#'
#'@param Iter.EstParmFunc  Number. Number of iterations. Default: 80 iterations.
#'@param ridge.final Object of class "ridgelm". Values obtained with the ridge regression.
#'@param tau Number. Value of the tau parameter.
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of the matrix will be used as reference in the alr transformation and will be at the denominator of the balance.
#'@param Tt Number of time points available
#'@param EspecieMaxima Row in which the bacteria chosen as reference is in \code{especie}. This is the bacteria that is going to be at the denominator of the balance and in the deniminator of the alr tranformation. As a result, in this function, \code{EspecieMaxima} must be equal to \code{E}.
#'@param E Number. Number of bacteria available.
#'@param seed Number. Set a seed. Default \code{seed=NULL}.
#'
#'@return Returns a list with:
#'\itemize{
#'   \item All.iter:  Matrix. Each row has the parameters obtained in each iteration. The parameters are in the columns written in following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}. In this matrix we must observe that in the last iterations the values has really similar or equal values, it not, we need to increase the value of \code{Iter.EstParmFunc}.
#'   \item Param.Estimates:  Vector with the estimated parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#'   \item AIC Number: Value of the AIC.
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
#'tau=20
#'Iter.EstParmFunc=40
#'
#'Estimate_Param_FBM(tau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E, 714)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2021). A Dirichlet autoregressive model for the analysis of microbiota time-series data. Complexity, 2021, 1-16.
#' @export



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

Estimate_Param_FBM<-function(tau,ridge.final,Iter.EstParmFunc=80, especie,EspecieMaxima,Tt,E, seed=NULL){

  old<-options()
  on.exit(options(old))
  options(max.print=1000000000)
  paramini<-c(stats::coef(ridge.final)[-1],tau)
  especiemodi=especie[,-1]

  matrizzz=matrix(0,Iter.EstParmFunc , length(paramini), byrow=F)
  for (i in 1:Iter.EstParmFunc){


    if(!is.null(seed)){
      set.seed(seed)
    }
    optimo<-stats::optim(paramini,EstParmFunc_FBM, especie=especie,E=E,EspecieMaxima=EspecieMaxima,Tt=Tt, especiemodi=especiemodi,method='Nelder-Mead',control = list(fnscale=-1))
    ParametrosObtenidos=optimo$par

    for(j in 1:(dim(matrizzz)[2])){
      matrizzz[i,j]=ParametrosObtenidos[j]
    }



    paramini=ParametrosObtenidos

    i=i+1
  }

  matrizzz
  rownames(matrizzz)<-c(paste0("iteration", c(1:Iter.EstParmFunc )))
  paramEstimados1=paramini


  H=length(paramEstimados1)
  a<-rep(0,H)
  for (i in 1:H){
    a[i]=paramEstimados1[i]
  }
  loglikee=EstParmFunc_FBM(a,especie,E,EspecieMaxima,Tt, especiemodi) #Obtaining loglikelihood
  AIC=2*H-2*log(loglikee) #Obtaining AiC


  F.list=list(All.iter<- matrizzz, Param.Estimates<-paramEstimados1, AIC<-AIC)
  names(F.list)<-c("All.iter","Param.Estimates","AIC")
  return(F.list)


}
