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

##' @title Create function with generic model
##' @description This function creates a generic model that will be a funcao that has parameters for the variables that can be mapped to each different base. her return will be a generic model that should be mapped to be used by the function avaliaEstimativas
##' @param nome is the name of model
##' @param formula is the string formula begin with y2~y1
##' @param funcaoRegressao is the function that will make the regression, ex.: 'nlsLM'
##' @param variaveis list variables that are present in the model that are field database
##' @param palpite param start of funcaoRegressao
##' @param maisParametros string add in funcaoRegressao, ex lm(y2~y1, data=base, maisParametros)
##' @param requires list of string of packges used to work with funcaoRegressao
##' @return will be returned function with generic model to map to a base
##' @export
criaModeloGenerico <- function(nome, formula, funcaoRegressao, variaveis, palpite=NULL, maisParametros=NULL, requires=NULL){
  
  if( is.null(nome) || is.null(formula) || is.null(funcaoRegressao) || is.null(variaveis))
    stop("os parametros nome, formula, funcaoRegressao e variaveis sao obrigatorios!")
  
  #if( !startsWith(formula, "y2~y1") && !startsWith(formula, "Y2~Y1") )
  #  stop("a formula deve iniciar com y2~y1")
  
  if(!is.null(palpite) && length(palpite) > 1)
    stop("informe o palpite desta forma: palpite=\"b0=1, b1=3, ... \"")
  
  st = paste0("\"", variaveis[1], "\"")
  st2 = paste0("\", ", variaveis[1], ", \"")
  str = paste0(variaveis[[1]], "=NULL")
  str1 = variaveis[1]
  str2 = paste0("is.null(", variaveis[1], ")")
  if(2 <= length(variaveis))
    for(i in 2:length(variaveis)){
      st = paste0(st, ",\"", variaveis[i], "\"")
      st2 = paste0(st2, ", \", ", variaveis[i], ", \"")
      str = paste0(str, ",", variaveis[i], "=NULL")
      str1 = paste0(str1, ",", variaveis[i])
      str2 = paste0(str2, " || is.null(", variaveis[i], ")")
    }
  
  pp =""
  pp2=""
  pp3=""
  if (!is.null(palpite)){
    str = paste0(str, ", palpite=c(", palpite, ")")
    pp= paste0(", palpite=c(", palpite, ")")
    pp2 = ", start=', pp, '"
    pp3 ="pp = ''
    if (!is.null(palpite) && (1 <= length(palpite))){
    pp = 'c('
    for (i in 1:length(palpite)) 
    pp = paste0(pp, names(palpite)[[i]], '=', palpite[[i]], ', ')
    pp = gsub(', $', ')', pp, perl = T)
    }
    "
  } 
  
  require = ""
  if (!is.null(requires)){
    if(1 <= length(requires))
      for(i in 1:length(requires))
        require = paste0(require, "require(", requires[i], ")\n ")
  }
  
  if (is.null(maisParametros))
    maisParametros = ""
  else
    maisParametros = paste0(", ", maisParametros)
  
  formula2 = paste0(gsub("y2~y1", "y2,'~',y1,'", formula, ignore.case = T), "'")
  if(1 <= length(variaveis))
    for(i in 1:length(variaveis))
      formula2= gsub(variaveis[i], paste0("\\\",", variaveis[i], ",\\\""), formula2)
  
  formula3 = paste0("formula = gsub('", variaveis[1], "', ", variaveis[1], ",formula) \n ")
  if(2 <= length(variaveis))
    for(i in 2:length(variaveis))
      formula3 = paste0(formula3, "formula = gsub('", variaveis[i], "', ", variaveis[i], ",formula) \n ")
  
  
  modelo = NULL
  modelogenerico = paste0(
    "modelo <- function(", str, "){\n ",
    "nome=\"", nome, "\"\n ",
    "formula=\"", formula, "\"\n ",
    "if(", str2, ") return (c( nome , formula, ", st, "))\n ",
    "modelo=NULL\n ",
    formula3,
    "eval(parse(text = paste0(\"modelo <- function(y1=NULL, y2=NULL, base=NULL", pp, "){
    'mapeamento{(", str1, ")=(", st2, ")}'
    ",
    require,
    pp3,
    "if (is.null(y1) || is.null(y2) || is.null(base)) 
    return (c('\", nome, \"','\", formula, \"')) else ",
    "return (eval(parse(text=paste0('", funcaoRegressao, "(', ", formula2,", ', data=base",pp2, maisParametros, ")'))))
}\"
              )))\n return (modelo)
}" )
    eval(parse(text = modelogenerico))
    return(modelo)
    }