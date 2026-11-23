#' Percentage of variance
#'
#' This function calculates the variance of each row of the matrix \code{PB}. Returns the percentage of variance of each row of the matrix \code{PB}.
#'
#'@param PB Matrix.
#'
#'@return Returns a vector with  percentage of variance of each row of the matrix \code{PB}.
#'
#'
#' @examples
#'
#'matt=matrix(c(1:4),2,2)
#'
#'
#'Percen_Variance(matt)
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
Percen_Variance<-function(PB){


#Studying the variance of the principal balances
VarianzaPB=rep(0,dim(PB)[1])
for (i in 1:length(VarianzaPB)){
  VarianzaPB[i]=stats::var(PB[i,])
}

#Studying the percentage of variance of the principal balances
PorcentajeVarPB=VarianzaPB/sum(VarianzaPB)*100

return(PorcentajeVarPB)


}
