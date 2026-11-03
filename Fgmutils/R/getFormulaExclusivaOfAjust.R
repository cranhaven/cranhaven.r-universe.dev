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

##' @title get Formula Exclusive Of Ajust
##' @description this function returns the formula of the model used in ajust
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @return will be returned a string which is the formula of ajust
##' @export
getFormulaExclusivaOfAjust <- function(ajuste){
  if(!(FALSE %in% (c("m", "data") %in% names(ajuste)))){
    saida = capture.output(ajuste$m$formula())
    formula = saida[[1]]
    if(2 <= (length(saida)-1))
      for(i in 2:(length(saida)-1)){
        if (grepl("<environment:", saida[[i]]) > 0)
          break
        formula = paste0(formula, saida[[i]])
      }
    return(formula)
  }
  if("terms" %in% names(ajuste)){
    saida = capture.output(formula(ajuste$terms))
    formula = saida[[1]]
    if(2 <= (length(saida)-1))
      for(i in 2:(length(saida)-1)){
        if (grepl("<environment:", saida[[i]]) > 0)
          break
        formula = paste0(formula, saida[[i]])
      }
    return(formula)
  }
  stop(paste0("nao foi possivel obter a formula para ajuuste do tipo ", class(ajuste)))
}
