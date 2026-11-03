## Copyright (C) 2016  Clayton Vieira Fraga Filho
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##  
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' @title define Classes 2
#' @description creates a list with the class interval of a frequency distribution
#' @param dados a vector of numbers
#' @param amplitude integer Class amplitude range
#' @examples
#' dados <- c(1,2,3,4)
#' defineClasses2(dados,2)
#' @export
defineClasses2 = function(dados, amplitude) {
  (pCentro = seq(floor(min(dados)),ceiling(max(dados)), amplitude))

  (pClasse = cbind(as.matrix(pCentro)-(amplitude/2),as.matrix(pCentro+amplitude)-(amplitude/2)))

  if (pClasse[dim(pClasse)[1], 2] == max(dados)) {
    pClasse[dim(pClasse)[1], 2] =  max(dados)+0.00000001
  }

  return(list(centro=pCentro, classe=pClasse))
}
