#' Estimating BPBM
#'
#' The estimation of the BPBM model is carried out using MCMC. To execute this function it is necessary to have the program Just Another Gibbs Sampler (JAGS) (Plummer, 2003) program installed.
#'
#'
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param Tt  Number of time points available.
#'@param E  Number of bacteria in the dataset.
#'@param MatrizPBmodelo Matrix with the covariates of the model. In an example with two SPBal and three time points, the covariates are written in the following  order:
#'  \tabular{rrr}{
#'  1 \tab  1  \tab  1\cr
#'  \eqn{SPBal_{1,t-1}} \tab  \eqn{SPBal_{1,t-2}} \tab   \eqn{SPBal_{1,t-3}} \cr
#'  \eqn{SPBal_{2,t-1}} \tab  \eqn{SPBal_{2,t-2}} \tab   \eqn{SPBal_{2,t-3} }}
#'
#'
#'
#'@param nn.chain the number of chains to use with the simulation. Default is 3, minimum2.
#'@param nn.burnin the number of burnin iterations. Default is 5000.
#'@param nn.sample the  number of iterations to take. Default: 20000. The markov chain will have ("sample"-"burnin")/"thin" iterations.
#'@param nn.thin  the thinning interval to be used. Default: 10.
#'@param seed Number. Set a seed. Default \code{seed=NULL}.
#'
#'@return Returns a list with:
#'
#'\itemize{
#'   \item List with:
#'   \itemize{
#'   \item R2jagsOutput:  R2jags object with the information of the estimation.
#'   \item SamplesAllChains:  Matrix. Matrix that has the iterations of all the Markov chains joined.
#'   }
#'
#'
#'
#'   }
#'
#'
#' @examples
#'
#'set.seed(314)
#'especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
#'E=5
#'Tt=6
#'MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))
#'
#'
#'
#'Estimating_BPBM(especie,
#'                Tt,
#'                E,
#'                MatrizPBmodelo,
#'                nn.chain=3,
#'                nn.burnin=1000,
#'                nn.sample=2000,
#'                nn.thin=10,
#'                714)
#'
#'@references
#'\itemize{
#'\item Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#'
#'\item Plummer, M. (2003, March). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling. In Proceedings of the 3rd international workshop on distributed statistical computing (Vol. 124, No. 125.10, pp. 1-10).
#' }
#'
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

Estimating_BPBM<-function(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=5000,nn.sample=20000,nn.thin=10, seed=NULL){


  # model

  modelbpbm = "
model{
  for (i in 2:I){

    y[1:ep,i] ~ ddirich(alpha[1:ep,i])

    for (j in 1:ep){
      alpha[j,i] <-  exp(mu[j,i-1])
    }

    for (j in 1:ep){
      mu[j,i-1]<-inprod(a[j,],vectorPB[,i-1])
    }
  }


  for (j in 1:ep){
    for (k in 1:BB){

      a[j,k]~dnorm(0,sigma[j,k])
      sigma[j,k]<-pow(sdgamma[j,k],-2)
      sdgamma[j,k]~dunif(0,5)
    }
  }


}"
  writeLines( modelbpbm , con="modelBPBM.txt" )#writtes an txt with the model



   #Data
  Data <- list(y =especie,I=Tt,ep=E,BB=dim(MatrizPBmodelo)[1],vectorPB=MatrizPBmodelo)





   #estimation
  if(!is.null(seed)){
    set.seed(seed)
  }
  #with r2jags package
  JAGS.tot.Final2<-do.call(R2jags::jags.parallel,
                           list(data=Data, parameters.to.save=c("a","sdgamma"), model.file = "modelBPBM.txt" ,
                                n.chains = nn.chain, n.iter = nn.sample, n.burnin = nn.burnin, n.thin = nn.thin,DIC=TRUE,n.cluster = 2))

  unlink("modelBPBM.txt")#Delete the file with the model information

  returnfi <-list(JAGS.tot.Final2,JAGS.tot.Final2$BUGSoutput$sims.matrix)
names(returnfi)<-c("R2jagsOutput","SamplesAllChains")
  return(returnfi)

}


