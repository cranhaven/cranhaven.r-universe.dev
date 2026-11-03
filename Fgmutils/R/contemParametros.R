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

##' @title which parameters are missing?
##' @description this function checks whether the labels of the parameters list to move to the functions is sufficient
##' @param funcoes is a or set of functions whose param will be verify
##' @param parametro is list whose labels is name of param in funcoes, list of args to funcoes ex list(a="1", b="2")
##' @param addParametro list of param included
##' @param addArgs more param required
##' @param exclui3pontos verify por ... ? in f<-function(a, ...){ }
##' @import  methods
##' @return will be returned the parameters that have not been reported in parametro and addParametro
##' @export
contemParametros<- function(funcoes, parametro, addParametro = c(), addArgs = c(), exclui3pontos = T){
  if(is.function(funcoes)) funcoes = c(funcoes)
  if(exclui3pontos) addParametro = c(addParametro, "...")
  for(i in 1:length(funcoes))
    if(length( (diff = setdiff(union((d = data.frame(k = sapply(formals(funcoes[[i]]), FUN = function(X) { return(is.symbol(X) && X == "") }),
                                                     j= formalArgs(funcoes[[i]])))[d$k,"j"], addArgs),
                               c(labels(parametro), addParametro)))) > 0){
      return (diff)
    }
  return (NULL)
}
