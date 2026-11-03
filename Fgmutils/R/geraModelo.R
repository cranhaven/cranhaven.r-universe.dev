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

##' @title Generates function to work with a model
##' @description this function generates unique model given: A formula and a guess (optional: name, funcaoRegressao, maisParametros, requires - proidido: custom)] or[A string saying how the return will be obtained eg custom = "lm (dap2 dap1 ~ * b 0)" (if the formula can not be passed just go empty, ex .: formula = "")]
##' @param nome is the name of model
##' @param formula is the string formula begin with y2~y1
##' @param funcaoRegressao is the function that will make the regression, ex.: 'nlsLM'
##' @param palpite param start of funcaoRegressao
##' @param maisParametros string add in funcaoRegressao, ex lm(y2~y1, data=base, maisParametros)
##' @param requires list of string of packges used to work with funcaoRegressao
##' @param customizado if you want to write as the return will be obtained report as a string
##' @return will be returned a function with exclusive model
##' @export
geraModelo <- function(nome="modelo sem nome", formula, funcaoRegressao="nlsLM", palpite=NULL, maisParametros=NULL, requires=NULL, customizado=NULL){

  if(is.null(maisParametros))
    maisParametros=  ""
  else
    maisParametros = paste0(", " , maisParametros)

  if(is.null(requires))
    requires = ""

  f = NULL
  funcao = paste0( "f <- function(y1=NULL, y2=NULL, base=NULL){
                   if (is.null(y1) || is.null(y2) || is.null(base))
                   return (c(\"", nome, "\", \"", formula, "\" ))
                   ", requires, "
                   return( ")
  pp = ""
  if(!is.null(palpite))
    pp = paste0(", start=c(", palpite, ")")
  if (is.null(customizado))
    funcao = paste0( funcao, "eval(parse(text=paste0(\"",funcaoRegressao, "(", gsub("y2~y1", "\",y2,\"~\", y1, \"", formula), ", data=base", pp, maisParametros, " ) \"))) ) }")
  else
    funcao = paste0( funcao, customizado, " ) }")

  eval(parse(text = funcao))

  return(f)
}
