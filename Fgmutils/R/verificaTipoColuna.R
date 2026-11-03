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

#' @title Check de type of Column
#' @description this function returns the type of a column of a dataFrame, if it is numeric or character.
#' @param coluna column of dataframe
#' @examples
#' ID_REGIAO <- c(1,2,3,4)
#' CD_PLANTIO <- c("ACD","CDB","CDC","CDD")
#' test <- data.frame(ID_REGIAO,CD_PLANTIO)
#' verificaTipoColuna(test$ID_REGIAO)
#' @export
verificaTipoColuna <- function(coluna){
  if(typeof(coluna) == "character")
  {

    return ("as.character()");
  }
  return ("as.numeric()");

}
