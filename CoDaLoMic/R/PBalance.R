#' Calculating balances
#'
#' This function calculates the balance that has at the numerator the bacteria placed at \code{Num} and has at the denominator the bacteria placed at \code{Dem}
#'
#'@param especie Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param Num  vector that contains the position in the matrix \code{especies} of the families that  we position at the numerator of the balance.
#'@param Dem  vector that contains the position in the matrix \code{especies} of the families that  we position at the denumerator of the balance
#'@param A    Number. The balance will be calculated for t=1,2,...,\code{A} time points.
#'
#'@return Returns a vector with the value of the balance in each time point.
#'
#'
#' @examples
#'
#'especie1=cbind(c(0.5,0.3,0.1,0.1), c(0.1,0.3,0.6,0.1))
#'Num=c(1,2)
#'Dem=c(3,4)
#'A=2
#'PBalance(A,Num,Dem,especie1)
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

PBalance=function(A,Num,Dem,especie){#Calculating balances
  #Num<- vector that contains the position in the matrix especies of the families that  we position at the numerator of the balance
  #Dem<- vector that contains the position in the matrix especies of the families that  we position at the denominator of the balance
  #A<-We calculate the balance for A time points.
  Logespecie=log(especie)
  B=rep(0,A)

  N=length(Num)
  D=length(Dem)

  for(i in 1:A){
    B[i]=sqrt((D*N)/(D+N))*(
      (1/N)*sum(Logespecie[,i][Num])-
        (1/D)*sum(Logespecie[,i][Dem])

    )

  }
  return(B)
}
