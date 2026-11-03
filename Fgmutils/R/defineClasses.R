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

#' @title define Classes
#' @description creates a list with the class interval of a frequency distribution
#' @param limiteMin the lowest list number
#' @param limiteMax the largest number in the list
#' @param amplitude List amplitude
#' @param decrescente order by true decreasing , false increasing
#' @param getDataFrame return a data.frame default false because old uses
#' @param verbose show status default false
#' @export
defineClasses <- function (limiteMin, limiteMax, amplitude, decrescente = TRUE, getDataFrame = FALSE, verbose=FALSE) {

  if (getDataFrame == TRUE)
  {
    if (verbose) print(paste0(" generating class for ", limiteMin, " to ", limiteMax, " amplitude ",  amplitude))
    if (limiteMin <= limiteMax && limiteMin >= 0){
      if (limiteMin == limiteMax)
        return (data.frame(indice=1, linf=limiteMin, centro=limiteMin, lsup=limiteMin, NCLASSES=0, NhaClasse=0))
      nroClasses = ceiling((limiteMax-limiteMin)/amplitude)
      classes = data.frame(indice=double(), linf=double(), centro=double(), lsup=double())
      i = 1
      inicial = floor(limiteMin)
      while (inicial<ceiling(limiteMax)) {
        if (decrescente) {
          df = data.frame(
            indice=nroClasses,
            linf=inicial,
            centro=((inicial+(inicial+amplitude))/2),
            lsup=inicial+amplitude)
        } else {
          df =  data.frame(
            indice=i,
            linf=inicial,
            centro=((inicial+(inicial+amplitude))/2),
            lsup= inicial+amplitude)
        }
        classes <- rbind(classes,df)
        inicial = inicial + amplitude
        i = i+1
        nroClasses = nroClasses - 1
      }
      classes$NCLASSES=0
      classes$NhaClasse=0
      return (classes)
    }
    else
      print(paste0('erro ao criar classe para ', limiteMin, ' e ', limiteMax))

    return(data.frame())
  }

  nroClasses = ceiling((limiteMax-limiteMin)/amplitude)

  classes = list()
  i = 1
  inicial = floor(limiteMin)
  while (inicial<ceiling(limiteMax)) {
    if (decrescente) {
      classes[[i]] = c(nroClasses,c(inicial,((inicial+(inicial+amplitude))/2), inicial+amplitude))
    } else {
      classes[[i]] = c(i,c(inicial,((inicial+(inicial+amplitude))/2), inicial+amplitude))
    }

    inicial = inicial + amplitude
    i = i+1
    nroClasses = nroClasses - 1
  }
  return (classes)
}
