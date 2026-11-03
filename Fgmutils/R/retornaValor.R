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

#' @title return value
#' @description this feature is designed to fix variables that its content was a command
#' @param valor any variable
#' @return the variable converted to its value
#' @examples
#' a = 5
#' retornaValor(a)
#' @export
retornaValor <- function(valor){
  if(typeof(valor) == "character")
  {

    return (as.character(valor));
  }
  return (as.numeric(valor));

}
