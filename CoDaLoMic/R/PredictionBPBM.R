#' Predicting using BPBM
#'
#' This function calculates the expected value and variance of the bacteria at time point \code{Tt}. Then, this function calculates the expected value and variance of the bacteria at time point t=(\code{Tt}+1),...,K. It calculates the expected value at each time point for each markov chain iteration. The expected value for each time point is the mean of the expected values of all iterations.mAnalogous with the variance, the dirichlet parameters and the expected valur plus(and minus) two times the sqrt of the variance.
#'
#' @param NumSPBal List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the principal balance i are placed.
#' @param DemSPBal List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the principal balance i are placed.
#' @param MCMC.CHAINS Matrix with the iterations of all the chains for all the parameters. Each column has all the iteration of one parameter. If the cero is in the center of the credible interval of one parameter all its iteration in the Marchov Chain have the value 0. It is output of the "StudyingParam" function adding "$AllChainsJoined".
#' @param Var Matrix that contains at row i the variance of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param esperanza Matrix that contains at row i the expected value of the bacterial taxa of bacteria i at t=1,2,3,...,\code{Tt}-1.
#' @param alpha Matrix that contains at the row i the Dirichlet parameter of the bacteria i at t=1,2,3,...,\code{Tt}.
#' @param E Number of bacteria available
#' @param Tt Number of bacteria available
#' @param K Number. The function will calculate the value of the expected value and the variance at \code{Tt} and predict for the time points t=\code{Tt}+1,..,K. To predict all the time points available at the data we K=dim(especie.All)-1
#' @param MatrizPBmodelo is the matrix that contains the covariates of the model. The first line es equal to 1 for all columns. The other rows contain the value of one SPBal at all time points. The selected principal balance of the row i+1 has at its numerator the bacteria placed in the rows \code{NumSPBal[[i]]} of the "especie". The selected principal balance of the row i+1 has at its denominator the bacteria placed in the rows \code{DemSPBal[[i]]} of the "especie".
#'
#' @return Returns a list with:
#'
#'\itemize{
#'    \item ExpectedValue.All:  Matrix. Matrix that contains at row i the expected value of the bacteria i at all time points t=1,2,...,K. The bacterias are placed at the same order than in \code{especies}.
#'    \item VarianceValue.All:  Matrix. Matrix that contains at row i the variance of the bacteria i at all time points t=1,2,...,K. The bacterias are placed at the same order than in \code{especies}.
#'    \item DirichlerParam.All: Matrix.   Matrix that contains at row i the dirichlet parameter of the bacteria i at all time points t=1,2,...,K. The bacterias are placed at the same order than in \code{especies}.
#'    \item ExpVarmas:  Matrix. Matrix that contains at row i the expected value plus two times the sqrt(variance) of the bacteria i at all time points t=\code{Tt},...,K, the rest of the time points has 0 values. The bacterias are placed at the same order than in \code{especies}.
#'    \item ExpVarmenos:  Matrix. Matrix that contains at row i the expected value plus two times the sqrt(variance) of the bacteria i at all time points t=\code{Tt},...,K,the rest of the time points has 0 values. The bacterias are placed at the same order than in \code{especies}.
#'
#'   }
#'
#' @examples
#'
#'NumSPBal=list(1,c(1,2))
#'DemSPBal=list(2,3)
#'MCMC.CHAINS=cbind(c(0.1,0.11),
#'                  c(0.2,0.21),
#'                  c(0.3,0.31),
#'                  c(-0.1,-0.11),
#'                  c(0.15,0.105),
#'                  c(0.44,0.41),
#'                  c(0.3,0.31),
#'                  c(0.201,0.221),
#'                  c(0.13,0.113) )
#'alpha=cbind(c(0.1,0.2,0.1),c(0.1,0.5,0.3))
#'K=3
#'esperanza=cbind(c(0.2,0.2,0.6))
#'Var=cbind(c(0.1,0.01,0.11))
#'E=3
#'Tt=2
#'MatrizPBmodelo=cbind(c(1,0.3,0.2))
#'
#'PredictionBPBM(NumSPBal,DemSPBal,MCMC.CHAINS, alpha,K,esperanza,Var,E,Tt,MatrizPBmodelo )
#'
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

PredictionBPBM<-function(NumSPBal,DemSPBal,MCMC.CHAINS, alpha,K,esperanza,Var,E,Tt,MatrizPBmodelo ){


#Obtaining the expected value and variance at time point t=Tt
tauPred=rep(0,K)
tauPred[Tt]=sum(alpha[,Tt])

esperanzaPred=matrix(0,E,K)
for (j in 1:E){
  esperanzaPred[j,Tt]=(alpha[j,Tt])/(tauPred[Tt])
}

VarPred=matrix(0,E,K)
for (j in 1:E) {
  VarPred[j,Tt]=((alpha[j,Tt])*(tauPred[Tt]-alpha[j,Tt]))/((tauPred[Tt]+1)*(tauPred[Tt])^(2))
}

VarmasPred=matrix(0,E,K)
for (j in 1:E) {
  VarmasPred[j,Tt]=esperanzaPred[j,Tt]+2*sqrt(VarPred[j,Tt])
}
VarmenosPred=matrix(0,E,K)
for (j in 1:E) {
  VarmenosPred[j,Tt]=esperanzaPred[j,Tt]-2*sqrt(VarPred[j,Tt])
}



alphaPred=matrix(0,E,K)
MatrizPBmodeloPredi=as.matrix(0,dim(MatrizPBmodelo)[1],K)

Cadenas=MCMC.CHAINS#All Markov Chains
#Predicting for t=Tt+1,...,K
for (i in (Tt+1):K){

  #Iterations of the Markov Chain
  alphaPrediter=matrix(0,E,dim(MCMC.CHAINS)[1])
  esperanzaPrediter=matrix(0,E,dim(MCMC.CHAINS)[1])
  tauPrediter=rep(0,dim(MCMC.CHAINS)[1])
  VarPrediter=matrix(0,E,dim(MCMC.CHAINS)[1])
  VarmasPrediter=matrix(0,E,dim(MCMC.CHAINS)[1])
  VarmenosPrediter=matrix(0,E,dim(MCMC.CHAINS)[1])
  muPred=matrix(0,E,dim(MCMC.CHAINS)[1])
  for(l in 1:(dim(MCMC.CHAINS)[1])){

    #Writing the parameters of the iteration of the Markov Chain l in matrix form
    mm=FromVectorToMatrix_BPBM(MCMC.CHAINS[l,],MatrizPBmodelo,E)

    muPred[,l]=mm%*%FVectorPBmodeloPredi(NumSPBal,DemSPBal,esperanzaPred[,i-1],MatrizPBmodelo)#Obtaing log(alpha) using the equations of the model

    #Obtaining the dirichlet parameters (alphas)
    for(j in 1:E){
      alphaPrediter[j,l]=exp(muPred[j,l])
    }

    tauPrediter[l]=sum(alphaPrediter[,l])


    for (j in 1:E){
      esperanzaPrediter[j,l]=(alphaPrediter[j,l])/( tauPrediter[l])
    }#Expected value predicted with the model

    for (j in 1:E) {
      VarPrediter[j,l]=((alphaPrediter[j,l])*( tauPrediter[l]-alphaPrediter[j,l]))/(( tauPrediter[l]+1)*( tauPrediter[l])^(2))
    }#Variance value predicted with the model

    for (j in 1:E) {
      VarmasPrediter[j,l]=esperanzaPrediter[j,l]+2*sqrt(VarPrediter[j,l])
    }

    for (j in 1:E) {
      VarmenosPrediter[j,l]=esperanzaPrediter[j,l]-2*sqrt(VarPrediter[j,l])
    }



  }

  esperanzaPred[,i]=apply(esperanzaPrediter,1,mean)#Mean of the expected values predicted with the model using the parameters obtained at each iterarion of the Markov Chain l
  VarPred[,i]=apply(VarPrediter,1,mean)#Mean of the variance predicted with the model using the parameters obtained at each iterarion of the Markov Chain l
  alphaPred[,i]=apply(alphaPrediter,1,mean)
  VarmasPred[,i]=apply(VarmasPrediter,1,mean)
  VarmenosPred[,i]=apply(VarmenosPrediter,1,mean)

}


EsperanzaFinal=cbind(esperanza, esperanzaPred[,-c((1:(Tt-1)))])#Joining in a matrix the expected values obtained when estimating the model and when predicting
colnames(EsperanzaFinal)<-NULL
VarFinal=cbind(Var, VarPred[,-c((1:(Tt-1)))])#Joining in a matrix the variance obtained when estimating the model and when predicting
colnames(VarFinal)<-NULL
alphaFinal=cbind(alpha, alphaPred[,-c((1:Tt))])
colnames(alphaFinal)<-NULL
colnames(VarmasPred)<-NULL
colnames(VarmenosPred)<-NULL

return.pred<-list(EsperanzaFinal,VarFinal,alphaFinal,VarmasPred,VarmenosPred)
names(return.pred)<-c("ExpectedValue.All", "VarianceValue.All","DirichlerParam.All", "ExpVarmas","ExpVarmenos")
return(return.pred)

}
