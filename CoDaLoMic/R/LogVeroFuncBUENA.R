#' Writting the loglikelihood of the dirichlet
#'
#' This function calculates the loglikelihood of the dirichlet for the BPBM model.
#'
#'
#'@param param Vector. Column "mean" of the output of "StudyingParam" function.
#'@param E Number f bacteria in the dataset.
#'@param MatrizPBmodelo Matrix with the covariates of the model. In an example with two SPBal and three time points, the covariates are written in the following  order:
#'  \tabular{rrr}{
#'  1 \tab  1  \tab  1\cr
#'  \eqn{SPBal_{1,t-1}} \tab  \eqn{SPBal_{1,t-2}} \tab   \eqn{SPBal_{1,t-3}} \cr
#'  \eqn{SPBal_{2,t-1}} \tab  \eqn{SPBal_{2,t-2}} \tab  \eqn{ SPBal_{2,t-3}} }
#'
#'
#'@param Tt Number of time points available
#'@param especiemodi Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt}.
#'
#'
#'@return Returns a number with the value of the dirichlet loglikelihood.
#'
#' @examples
#'
#'
#'set.seed(314)
#'especie=t(gtools::rdirichlet(n=2, c(1,2,3)))
#'E=3
#'Tt=2
#'MatrizPBmodelo=rbind(c(1,1),c(-0.3,0.4),c(0.3,0.5))
#'set.seed(314)
#'est=Estimating_BPBM(especie,
#'                    Tt,
#'                    E,
#'                    MatrizPBmodelo,
#'                    nn.chain=3,
#'                    nn.burnin=1000,
#'                    nn.sample=5000,
#'                    nn.thin=10)
#'
#'param=est$SamplesAllChains
#'
#'especiemodi=especie[,-1]
#'
#'
#'LogVeroFuncBUENA(param,MatrizPBmodelo,E,Tt,especiemodi)
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


LogVeroFuncBUENA=function(param,MatrizPBmodelo,E,Tt,especiemodi){ #Returns the loglikelihood function of the dirichlet


  a.param=FromVectorToMatrix_BPBM(param,MatrizPBmodelo,E)


  mu=PBmodel(a.param,MatrizPBmodelo,E,Tt)

  alpha=exp(mu)
  tau=rep(0,Tt)
  for (i in 1:Tt){
    tau[i]=sum(alpha[,i])
  }

  AlphaMenos1=alpha-1
  LogEspecieModi=log(especiemodi)
  MatrizSumas=t(AlphaMenos1)%*%LogEspecieModi
  loglike<-rep(0,Tt-1)
  for(i in 1:(Tt-1)){

    loglike[i]=lgamma(tau[i])-sum(lgamma(alpha[,i]))+diag(MatrizSumas)[i]
  }
  loglikeVerdad=sum(loglike)

  return(loglikeVerdad)
}

