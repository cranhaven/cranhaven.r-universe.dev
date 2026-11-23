#' Obtaining the value of the Dirichlet parameters, the expected value and the variance.
#'
#' This function calculates the value of the Dirichlet parameters, the expected value and the variance for the Dirich-gLV model.
#'
#'@param especie Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of this matrix is the one used as reference in the alr transformation that the model applies.
#'@param Param.Estimates Vector with the estimated parameters. This value is the output of the Estimate_Param_EstParmFunc function.
#'
#'
#'@return Returns a list with:
#'
#'\itemize{
#'   \item Dirichlet.Param:  Matrix. Matrix that contains at row i the Dirichlet parameter of the bacteria i at all time points.
#'   \item Expected.Value:  Matrix. Matrix that contains at row i the expected value of the bacteria i at all time points. The bacterias are placed at the same orden than in \code{especies}.
#'    \item Variance.Value:  Matrix. Matrix that contains at row i the variance of the bacteria i at all time points. The bacterias are placed at the same orden than in \code{especies}.
#'   }
#' @examples
#'
#'
#'especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
#'tau1=0.4
#'parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
#'parms11=c(tau1,as.vector( parms1))
#'
#'ExpectedValue_EstParmFunc(parms11,especie1)
#' @references Creus-Martí, I. and Moya, A. and Santonja, F. J. (2018). A Statistical Model with a Lotka-Volterra Structure for Microbiota Data. Lucas Jodar, Juan Carlos Cortes and Luis Acedo,  Modelling or engineering and human behavior 2018, Instituto Universitario de Matematica Multidisciplinar. ISBN: 978-84-09-07541-6
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

ExpectedValue_EstParmFunc=function(Param.Estimates, especie){

  parms.vector=Param.Estimates
  tau=parms.vector[1]
  parms.vector.m=parms.vector[-1]
  parms=matrix(0,dim(especie)[1]-1,dim(especie)[1])
  m=length(parms.vector.m)/(dim(especie)[1]-1)
  parms[,1]=parms.vector.m[c(1:dim(parms)[1])]
  for(i in 1:(m-1)){
    parms[,i+1]=parms.vector.m[c((1+i*dim(parms)[1]):(1+i*dim(parms)[1]+dim(parms)[1]-1))]
  }

  State=apply(especie, 2,compositions::alr
) #The bacteria in the denominator is is bacteria at the last row of the matrix especie

  regre=matrix(0, dim(State)[1] , dim(State)[2])
  for(i in 1:(dim(State)[2])){
    regre[,i]=rxnrate( State[,i],parms)
  }

  exp.regre=exp(regre)

   denominador=1+apply(exp.regre,2,sum)



  alpha=matrix(0, dim(State)[1]+1 , dim(State)[2])
  for(i in 1:(dim(State)[1])){
    for (j in 1: (dim(State)[2])){#i es el tiempo
      alpha[i,j]=tau*exp.regre[i,j]/denominador[j]
    }}

  for (j in 1: (dim(State)[2])){
    alpha[dim(State)[1]+1, j]= tau/denominador[j]
  }


  Esperanza=alpha/tau

  Var=matrix(0,dim(Esperanza)[1],dim(Esperanza)[2])
  for (i in 1:dim(Esperanza)[1]) {
    for (j in 1:dim(Esperanza)[2]) {
      Var[i,j]=((alpha[i,j])*(tau-alpha[i,j]))/((tau+1)*(tau)^(2))
    }}

   Esperanza.final<-Esperanza[,-dim(Esperanza)[2]]
   Var.final<-Var[,-dim(Var)[2]]
  return.E.A<-list(Dirichlet.Param<-alpha, Expected.Value<-Esperanza.final, Variance.Value<-Var.final)
  names(return.E.A)<-c("Dirichlet.Param", "Expected.Value","Variance.Value")

   return(return.E.A)

}
