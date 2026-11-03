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

##' @title get Columns Of Base present in the string
##' @description this function returns the columns of a base whose names are present in the string strColumns
##' @param base data.frame
##' @param strColumns string containing name fields of the base
##' @return will be returned list with fields whose name are present in the string
##' @export
getColumnsOfBase <- function(base, strColumns){
  colunas = vector()
  nomes = names(base)
  j=0
  if( 1 <= length(names(base)))
    for(i in 1: length(names(base)))
      if (grepl(nomes[[i]], strColumns) > 0)
        colunas[[(j=j+1)]] = nomes[[i]]
  return(colunas)
}
