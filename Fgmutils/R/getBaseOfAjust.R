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

##' @title get database Of Ajust
##' @description this function returns the database used in the setting
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @return will be returned a string which is the database of ajust
##' @export
getBaseOfAjust <- function(ajuste){
  if(!(FALSE %in% (c("m", "data") %in% names(ajuste))))
    return(get(toString(ajuste$data), envir =  ajuste$m$getEnv()))
  if("terms" %in% names(ajuste)){
    s = gsub(")",  "", capture.output(ajuste$call))
    s = gsub("\\s", "", gsub("^.*\\(", "", s, perl = T), perl = T)
    s =str_split(s, pattern = ",")[[1]]
    for(i in 1:length(s))
      if(grepl("data", str_split(s[[i]], "=")[[1]][1]))
        return(get( str_split(s[[i]], "=")[[1]][2], envir = attributes(ajuste$terms)$.Environment))
  }
  stop(paste0("nao foi possivel obter a base para ajuuste do tipo ", class(ajuste)))
}
