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

##' @title save function with Model
##' @description save function with Model of type criaModeloGenerico or criaModeloExclusivo
##' @param modelo function with Model the save
##' @param diretorio directory to save the file, if not informed saved in the work directory
##' @import utils
##' @export
salvaModelo <- function(modelo, diretorio = ""){
  if(!is.null(diretorio)){
    if (! dir.exists(diretorio)) dir.create(diretorio, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    param = modelo()
    nome = param[1]
    type = " modelo exclusivo.txt"
    if (length(param) > 2)
      type = " modelo generico.txt"
    capture.output(modelo, file = paste0(diretorio, nome, type))
  }
}
