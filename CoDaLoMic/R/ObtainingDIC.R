#' Writting the loglikelihood of the dirichlet
#'
#' This function calculates the loglikelihood of the dirichlet for the BPBM model. Then, it calculates the loglikelihood with the parameters of each iteration of the MCMC chains and introduces the values in a vector called vectorD. DIC=(1/2)*var(vectorD)+mean(VectorD)
#'
#'@param cadenas Matrix with the iterations (in rows) of all the Markov Chains obtained in th estimation. It is the output of "StudyingParam" adding "$AllChainsJoined".
#'@param E Number f bacteria in the dataset.
#'@param MatrizPBmodelo Matrix with the covariates of the model. In an example with two SPBal and three time points, the covariates are written in the following  order:
#'  \tabular{rrr}{
#'  1 \tab  1  \tab  1\cr
#' \eqn{ SPBal_{1,t-1}} \tab  \eqn{SPBal_{1,t-2}} \tab   \eqn{SPBal_{1,t-3}} \cr
#'  \eqn{SPBal_{2,t-1}} \tab  \eqn{SPBal_{2,t-2}} \tab   \eqn{SPBal_{2,t-3}} }
#'
#'
#'@param Tt Number of time points available
#'@param especiemodi Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt}.
#'
#'
#'@return Returns a data.frame with the DIC value (using the rule, pD = var/2).
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
#'SumFinal=StudyingParam(est$R2jagsOutput$BUGSoutput$summary   ,est$SamplesAllChains)
#'cadenas=SumFinal$AllChainsJoined
#'especiemodi=especie[,-1]
#'
#'ObtainingDIC(cadenas,MatrizPBmodelo,E,Tt,especiemodi)
#' @references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
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
ObtainingDIC<-function(cadenas,MatrizPBmodelo,E,Tt,especiemodi){

SAM=dim(cadenas)[1]
VectorD=rep(0,SAM)
for (i in 1:SAM){
  VectorD[i]=-2*LogVeroFuncBUENA(cadenas[i,],MatrizPBmodelo,E,Tt,especiemodi)
}#VectorD contains at each i the loglikelihood obtained with the parameters of the sample i of the  mcmc


param=apply(cadenas,2,mean)

D=mean(VectorD)#D value of the DIC


pD_ped=(1/2)*stats::var(VectorD)#pD value of DIC_ped
DIC_ped=pD_ped+mean(VectorD)#DIC_ped value




#Summary of DIC value
CalidadBueno=data.frame(rbind(c("D","pD_ped","DIC_ped"),c(round(D,2),round(pD_ped,2),round(DIC_ped,2))))
return(CalidadBueno)


}

