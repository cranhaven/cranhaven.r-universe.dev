#' Writting the loglikelihood of the dirichlet
#'
#' This function calculates the loglikelihood of the dirichlet for the Dirich-gLV model.
#'
#'In an example with three bacteria, the regression of this model is defined by
#'
#' \deqn{r_{1}\cdot log(x_{1}(t)/x_{3}(t))+log(x_{1}(t)/x_{3}(t))\cdot [a_{11}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{12}\cdot log(x_{2}(t)/x_{3}(t))] }
#' \deqn{r_{2}\cdot log(x_{2}(t)/x_{3}(t))+log(x_{2}(t)/x_{3}(t))\cdot [a_{21}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{22}\cdot log(x_{2}(t)/x_{3}(t))] }
#'
#'@param especie Matrix that contains at row i the bacterial taxa of bacteria i at all time points . The bacteria placed in the last row of this matrix is the one used as reference in the alr transfromation that the model apply
#'@param parms.vector Vector equal to \code{c(tau,as.vector( pam))} where:
#'
#'\itemize{
#'   \item pam:  Matrix. Each row has the parameters of each bacteria. Following our example, pam has the parameters placed as follows:
#'   \tabular{rrrr}{
#'  r1 \tab  a11  \tab  a12\cr
#'  r2 \tab  a21 \tab   a22 }
#'   \item tau:  Number. Value of the tau parameter in the model
#'   }
#'
#'@return Returns a number with the value of the dirichlet loglikelihood.
#'
#'@examples
#'
#'
#'especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
#'tau1=0.4
#'parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
#'parms11=c(tau1,as.vector( parms1))
#'
#'EstParmFunc(parms11,especie1)
#'
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

EstParmFunc=function(parms.vector, especie){

  tau=parms.vector[1]
  parms.vector.m=parms.vector[-1]
  parms=matrix(0,dim(especie)[1]-1,dim(especie)[1])
  m=length(parms.vector.m)/(dim(especie)[1]-1)
  parms[,1]=parms.vector.m[c(1:dim(parms)[1])]
  for(i in 1:(m-1)){
    parms[,i+1]=parms.vector.m[c((1+i*dim(parms)[1]):(1+i*dim(parms)[1]+dim(parms)[1]-1))]
  }


  State=apply(especie, 2,compositions::alr) #The bacteria in the denominator is is bacteria at the last row of the matrix especie

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



   especiemodi=as.matrix(especie[,-1])
   Tt=dim(State)[2]
  loglike<-rep(0,T-1)
  for(k in 1:Tt-1) {
    loglike[k]=log(gamma(tau))-sum(log(gamma(alpha[,k])))+sum((alpha[,k]-1)*log(especiemodi[,k]))
  }
  loglikeVerdad=sum(loglike)

  return(loglikeVerdad)
}
