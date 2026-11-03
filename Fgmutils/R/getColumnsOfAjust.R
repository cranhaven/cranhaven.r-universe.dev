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

##' @title get Columns used in Ajust
##' @description this function returns an array with the column names that are on the model and reported basis or basis used in ajust
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param dfDados data.frame optional
##' @param excludeY1andY2 delete Y1 and Y2 fields? del formula(y1~y2...)
##' @return will be returned list of columns used in ajust
##' @export
getColumnsOfAjust <- function(ajuste, dfDados=NULL, excludeY1andY2 = T){
  if (is.null(dfDados))
    dfDados = getBaseOfAjust(ajuste)
  formula = getFormulaExclusivaOfAjust(ajuste)
  if(excludeY1andY2)
    formula = gsub("^.*~ \\S+", "", formula, perl = T)
  return(getColumnsOfBase(base=dfDados, strColumns = formula))
}
