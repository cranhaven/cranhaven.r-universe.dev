#' Estimating Parameters of EstParmFunc
#'
#' This function calculates the estimated parameters of the Dirich-gLV model.
#'
#'Maximum likelihood estimation is used. This function makes an iterative process, it obtains the value of the parameter that maximize the Dirichlet loglikelihood (defined in EstParmFunc) using the Nelder-Mead method and some initial parameters. Then it uses this value as initial parameters and repeats the process \code{Iter.EstParmFunc} times.
#'
#'
#'
#'@param Iter.EstParmFunc  Number. Number of iterations.
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of the matrix will be used as reference in the alr transformation.
#'@param paramini Initial values of the parameters. Vector equal to \code{c(tau.ini, as.vector(pam.ini))} where:
#'@param seed Number. Set a seed. Default \code{seed=NULL}.
#'
#'\itemize{
#'   \item pam.ini  Matrix. Each row has the parameters of each bacteria, following the same structure than pam in EstParmFunc
#'   \item tau.ini  Number. Initial value of the tau parameter in the model
#'   }
#'
#'@return Returns a list with:
#'\itemize{
#'   \item All.iter:  Matrix. Each row has the parameters obtained in each iteration. The parameters are in the columns written in the same order that they are written in \code{paramini}. In this matrix we must observe that in the last iterations the values has really similar or equal values, it not, we need to increase the value of \code{Iter.EstParmFunc}.
#'  \item Param.Estimates: The estimated parameters. The parameters are in the columns written in the same order that they are written in \code{paramini}.
#'   }
#'
#'
#'
#'
#'
#'
#'@examples
#'
#'especie=cbind(c(0.5,0.3,0.2),c(0.1,0.3,0.6))
#'paramini=c(100,2,3,4,5,6,7)
#'Estimate_Param_EstParmFunc(5, paramini , especie,714)
#'
#'
#'@references Creus-Martí, I. and Moya, A. and Santonja, F. J. (2018). A Statistical Model with a Lotka-Volterra Structure for Microbiota Data. Lucas Jodar, Juan Carlos Cortes and Luis Acedo,  Modelling for engineering and human behavior 2018, Instituto Universitario de Matematica Multidisciplinar. ISBN: 978-84-09-07541-6
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

Estimate_Param_EstParmFunc<-function(Iter.EstParmFunc,paramini, especie, seed=NULL){

  matrizzz=matrix(0,Iter.EstParmFunc , length(paramini), byrow=F)
  for (i in 1:Iter.EstParmFunc){


    if(!is.null(seed)){
      set.seed(seed)
    }

    optimo<-stats::optim(paramini,EstParmFunc, especie=especie,method='Nelder-Mead',control = list(fnscale=-1))
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

  F.list=list(All.iter<- matrizzz, Param.Estimates<-paramEstimados1)
  names(F.list)<-c("All.iter","Param.Estimates")
  return(F.list)


}
