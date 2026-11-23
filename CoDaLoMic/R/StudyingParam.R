#' Controlling quality of the convergence in BPBM
#'
#' This function controls that the value of the Rhat is between 0.9 and 1.1. In addition, it controls that the effective sample size is bigger than 100 and that the zero is not at the center of the credible interval (the interval between 2.5 and 97.5). We consider that the zero is in the center of the credible interval when the zero is between the 25 and the 75 quantile of the distribution formed by the limits if the credible interval.
#'
#' @param Sum Matrix with the summary of the "Estimating_BPBM".It is the output of the "Estimating_BPBM" adding "$R2jagsOutput$BUGSoutput$summary".
#' @param MCMC.CHAINS Matrix with the values of all the Markov chains for all parameters. It is the output of the "Estimating_BPBM" adding "$SamplesAllChains".
#'
#' @return Returns a list with:
#'\itemize{
#'   \item Param.Summary:  Matrix. The matrix Sum with a zero in the column \code{Sum[,"mean"]} when a parameter has the zero in the center of its credible interval.
#'    \item AllChainsJoined:  Matrix. The matrix MCMC.CHAINS with a zero in all the iterations of the chain when a parameter has the zero in the center of its credible interval.
#'   }
#' @examples
#'
#'set.seed(314)
#'especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
#'E=5
#'Tt=6
#'MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))
#'
#'est=Estimating_BPBM(especie,
#'                    Tt,
#'                    E,
#'                    MatrizPBmodelo,
#'                    nn.chain=3,
#'                    nn.burnin=1000,
#'                    nn.sample=5000,
#'                    nn.thin=10)
#'
#'StudyingParam(est$R2jagsOutput$BUGSoutput$summary,est$SamplesAllChains)
#'
#'
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


StudyingParam<-function(Sum,MCMC.CHAINS){

  #Controling the quality of the estimation
  if(length(which(Sum[,"Rhat"]<1.1))!=length(Sum[,8])){stop("The Rhat is bigger than 1.1, increase the nn.samples")}
  if(length(which(0.9<Sum[,"Rhat"]))!=length(Sum[,8])){stop("The Rhat is not near 1, increase the nn.samples")}
  if(length(which(Sum[,"n.eff"]<100))!=0){stop("The effective sample size is smaller than 100. Increase the nn.sample and the nn.thin")}

  #Controling if there are zeros in the center of the credible interval
  MCMC.CHAINS.new=MCMC.CHAINS
  for (i in 1:(length(Sum[,"2.5%"]))  ){

    if(Sum[i,"2.5%"]<0 & Sum[i,"97.5%"]>0){
      q=stats::quantile(c(Sum[i,"2.5%"],Sum[i,"97.5%"]), prob=c(0.25,0.75))
      if(q[1]<0 & q[2]>0){
        Sum[i,"mean"]=0
        MCMC.CHAINS.new[,i]=rep(0,dim(MCMC.CHAINS)[1])}
    }

    if(Sum[i,"2.5%"]>0 & Sum[i,"97.5%"]<0){
      stop("There are a credible interval where the 2.5% is smaller than the 97.5%")
    }
  }

  return.f.f<-list(Sum,MCMC.CHAINS.new)
  names(return.f.f)<-c("Param.Summary","AllChainsJoined")
  return(return.f.f)
}
