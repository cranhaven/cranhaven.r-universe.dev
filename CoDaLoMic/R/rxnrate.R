#' Solving the right side of the gLV equations
#'
#' This function calculates the right side of the gLV equation.
#'
#'
#' For instance, if we want to solve the following gLV equations:
#' \deqn{\frac{dx_{1}(t)}{dt}=r_{1}\cdot x_{1}(t)+x_{1}(t)\cdot[a_{11}\cdot x_{1}(t)+a_{12}\cdot x_{2}(t)+a_{13}\cdot x_{3}(t)]}
#' \deqn{\frac{dx_{2}(t)}{dt}=r_{2}\cdot x_{2}(t)+x_{2}(t)\cdot[a_{21}\cdot x_{1}(t)+a_{22}\cdot x_{2}(t)+a_{23}\cdot x_{3}(t)]}
#' \deqn{\frac{dx_{3}(t)}{dt}=r_{3}\cdot x_{3}(t)+x_{3}(t)\cdot[a_{31}\cdot x_{1}(t)+a_{32}\cdot x_{2}(t)+a_{33}\cdot x_{3}(t)]}
#'
#'
#'This function returns a vector with the value of:
#'
#' \deqn{r_{1}\cdot x_{1}(t)+x_{1}(t)\cdot[a_{11}\cdot x_{1}(t)+a_{12}\cdot x_{2}(t)+a_{13}\cdot x_{3}(t)]}
#' \deqn{r_{2}\cdot x_{2}(t)+x_{2}(t)\cdot[a_{21}\cdot x_{1}(t)+a_{22}\cdot x_{2}(t)+a_{23}\cdot x_{3}(t)]}
#' \deqn{r_{3}\cdot x_{3}(t)+x_{3}(t)\cdot[a_{31}\cdot x_{1}(t)+a_{32}\cdot x_{2}(t)+a_{33}\cdot x_{3}(t)]}
#'
#'
#'@param State  Vector with a CoDa composition
#'@param parms  Matrix. Each row has the parameters of each differential equation. following our example, parms has the parameters placed as follows:
#'
#'  \tabular{rrrr}{
#'  r1 \tab  a11  \tab  a12 \tab   a13 \cr
#'  r2 \tab  a21 \tab   a22 \tab  a23 \cr
#'  r3 \tab  a31  \tab  a32 \tab  a33}
#'
#'
#'@return Returns a vector with the value of the right side of the gLV equations.
#'
#' @examples
#'
#'
#'cinit1<-c(x1<-0.7,x2<-0.2,x3<-0.1)
#'parms1= cbind(c(0.1,0.2,-0.1),c(-0.2,0.1,-0.1),c(0.3,0.2,0.3),c(0.1,0.22,0.2))
#'rxnrate(cinit1,parms1)
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





rxnrate=function(State,parms){
  #State is a vector with a CoDa composition.
  #parms is a matrix with the parameters
  #   r1 a11  a12  a13
  #   r2 a21  a22  a23
  #   r3 a31  a32  a33
  Dx=rep(0,dim(parms)[1])

  First=parms[,-1]
  F.matrix=First%*%State
  S.matrix=State*F.matrix
  T.matrix=parms[,1]*State

  Dx=T.matrix+S.matrix

  return(Dx)

}

