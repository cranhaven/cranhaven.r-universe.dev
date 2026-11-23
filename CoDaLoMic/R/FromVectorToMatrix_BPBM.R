
#' Writting the parameters in the matrix form required in BPBM model
#'
#' \code{StudyingParam} returns a matrix where the value of the parameters is in the column "mean". This function inputs this columns and outputs the parameters in the matrix form required by the BPBM model.
#'
#'
#'
#'@param param Vector. Column "mean" of the output of "StudyingParam" function.
#'@param E Number of bacteria in the dataset.
#'@param MatrizPBmodelo Matrix with the covariates of the model. In an example with two SPBal and three time points, the covariates are written in the following  order:
#'  \tabular{rrr}{
#'  1 \tab  1  \tab  1\cr
#'  \eqn{SPBal_{1,t-1}} \tab  \eqn{SPBal_{1,t-2}} \tab   \eqn{SPBal_{1,t-3}} \cr
#'  \eqn{SPBal_{2,t-1}} \tab  \eqn{SPBal_{2,t-2}} \tab   \eqn{SPBal_{2,t-3}}
#'   }
#'
#'
#'@return Returns a matrix with the parameters in the order required by the BPBM model.
#'
#'@examples
#'
#'set.seed(314)
#'especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
#'E=5
#'Tt=6
#'MatrizPBmodelo=rbind(c(1,1,1,1,1,1),
#'                    c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),
#'                    c(0.3,0.5,-0.3,0.1,0.4,0.1))
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
#'mm=StudyingParam(est$R2jagsOutput$BUGSoutput$summary,est$SamplesAllChains)
#'FromVectorToMatrix_BPBM(mm$Param.Summary[,"mean"],MatrizPBmodelo,5)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
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

FromVectorToMatrix_BPBM<-function(param,MatrizPBmodelo,E){
  BB=dim(MatrizPBmodelo)[1]
  a=matrix(0,E,BB)
  a[,1]=param[c(1:E)]
  for(i in 1:(BB-1)){
    a[,i+1]=param[c(i*E+1):((i+1)*E)]
  }

  return(a)
}


