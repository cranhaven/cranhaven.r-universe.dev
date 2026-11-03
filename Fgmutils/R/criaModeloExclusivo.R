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

##' @title Create Exclusive Model for a database
##' @description this function returns a unique model is variable receive each mapeda variable ex .: criaModeloExclusivo (modeloCamposLeite, c ("age1", "age2", "bai1", "s"))
##' @param modeloGenerico model of pattern criaModeloGenerico
##' @param variaveis list of name fields (strings) in database and model, the order of variables matter
##' @param palpite string containing start values of function of regression
##' @return will be returned a function with exclusive model
##' @export
criaModeloExclusivo <- function(modeloGenerico, variaveis, palpite=NULL){ 
  var = NULL
  eval(parse(text = "var = modeloGenerico()"))
  if (is.null(var)) stop("modelo generico invalido")
  if (length(var) <= 2 || (length(var) - 2) != length(variaveis)) stop("numero de parametros invalido")
  str = paste0(var[3] , " = \"", variaveis[1], "\"")
  if(2 <= length(variaveis))
    for(i in 2:length(variaveis))
      str = paste0(str, ", ", var[2+i], " = \"", variaveis[i],"\"")
  if(!is.null(palpite))
    str = paste0(", palpite=c(", palpite, ")")
  modeloExclusivo = NULL
  eval(parse(text = paste0("modeloExclusivo = modeloGenerico(", str, ")")))
  return (modeloExclusivo)
}
