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

##' @title get Parametros Of Model
##' @description this function retona columns the base of the parameter or setting present in the model
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param base optional data.frame whose fields name is present in formula
##' @param formula string containing name fields of the base
##' @return will be returned list of columns used in ajust or in formula
##' @export
getParametrosOfModel <- function(ajuste, base=NULL, formula = NULL){
  if (is.null(ajuste)){
    if(is.null(base) || is.null(formula))
      stop("se ajuste eh null base e formula devem ser informados e vice versa")
    return(getColumnsOfBase(base=base, strColumns = formula))
  }
  return(getColumnsOfAjust(ajuste = ajuste))
}
