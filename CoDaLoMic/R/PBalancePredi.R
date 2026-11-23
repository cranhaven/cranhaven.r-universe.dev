#' Calculating balances for a composition
#'
#' This function calculates the balance that has at the numerator de bacteria placed at \code{Num} and has at the denominator the bacteria placed at \code{Dem}
#'
#'@param DatosEsperanzas Vector with a coda composition. The bacteria are in the same orden than the matrix \code{especie}
#'@param Num  vector that contains the position in the matrix \code{especies} of the families that  we position at the numerator of the balance.
#'@param Dem  vector that contains the position in the matrix \code{especies} of the families that  we position at the denumerator of the balance
#'
#'@return Returns the value of the balance
#'
#' @examples
#'
#'Num=c(1,2)
#'Dem=c(3,4)
#'DatosEsperanzas=c(0.1,0.3,0.4,0.2)
#'
#'PBalancePredi(Num,Dem,DatosEsperanzas)
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
PBalancePredi=function(Num,Dem,DatosEsperanzas){#Calculating balances using the expected values obtained with the model
  #Num<- vector that contains the position in the matrix especies of the families that  are positioned at the numerator of the balance
  #Dem<- vector that contains the position in the matrix especies of the families that  are positioned at the denominator of the balance
  #A<-We calculate the balance for A time points.


  Logespecie=log(DatosEsperanzas)


  N=length(Num)
  D=length(Dem)


  B=sqrt((D*N)/(D+N))*(
    (1/N)*sum(Logespecie[Num])-
      (1/D)*sum(Logespecie[Dem])

  )

  return(B)
}
