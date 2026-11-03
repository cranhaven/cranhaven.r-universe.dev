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

##' @title Predict
##' @description this function is the replacement predict, she tries to predict if the return zero predict it calculates the prediction with the coefficients reported in the parameter setting
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param newdata dataframe where fields will be update
##' @param force force the calculation without using predict?
##' @param ... only for compatibility with other functions
##' @return will be returned list of values predicts
##' @import stats
##' @export
predizer <- function(ajuste, newdata, force = FALSE, ...){
  baseValidacao = newdata[, getColumnsOfAjust(ajuste=ajuste, dfDados=newdata, excludeY1andY2=F)]
  predito = predict(ajuste, newdata = baseValidacao)
  if ((table(predito)[[1]] == nrow(baseValidacao)) || force)
  {
    coeficientes = coef(summary(ajuste))
    formula = getFormulaExclusivaOfAjust(ajuste)
    if(1 <= nrow(coeficientes))
      for(i in 1:nrow(coeficientes))
        formula = gsub(rownames(coeficientes)[i], paste0("(", coeficientes[i], ")"), formula)
    if(1 <= length(names(baseValidacao)))
      for(i in 1:length(names(baseValidacao)))
        formula = gsub(names(baseValidacao)[i], paste0(" @", names(baseValidacao)[i], " "), formula)
    formula = gsub(".*~", "predito =", gsub("@", "baseValidacao$", formula), perl = T)
    warning(paste0("predizendo.... ", formula))
    eval(parse(text = formula))
    if (table(predito)[[1]] == nrow(baseValidacao))
      warning(paste0("mesmo em tentativa bruta nao foi possivel predizer ",  formula))
  }
  return(predito)
}
