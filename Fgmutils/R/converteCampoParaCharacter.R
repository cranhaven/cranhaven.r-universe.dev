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

##' @title Field Converts To Character
##' @description converts a column of a dataframe to String
##' @param nomeCampo the column name you want to convert
##' @param base the column having dataFrame, that you want to convert to String
##' @return base dataFrame with a column converted to String
##' @examples
##' measurement_date <- c(02/2009,02/2010,02/2011,02/2011)
##' plot <- c(1,2,3,4)
##' test <- data.frame(measurement_date,plot)
##' converteCampoParaCharacter("measurement_date",test)
##' @export
converteCampoParaCharacter <- function (nomeCampo, base){
  eval(parse(text=paste0("base$",nomeCampo," = as.character(base$", nomeCampo, ")")))
  return (base);
}
