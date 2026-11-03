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

##' @title percent RMSE Estatistics
##' @description this function returns a data.frame containing fields rmsePERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame containing field rmse
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with rmse PERCENTUAL calc
##' @export
estatisticasRmsePERCENTUAL <- function(observado, estimado, dfEstatisticas, ...){
  if(!("rmse" %in% dfEstatisticas$estatisticasDoModelo$name))
    stop("ERROR: FIRST CALC RMSE")
  dfEstatisticas$estatisticasDoModelo = rbind(dfEstatisticas$estatisticasDoModelo,
                                              data.frame(name = "rmsePERCENTUAL", value = calculaPerc(dfEstatisticas$estatisticasDoModelo[dfEstatisticas$estatisticasDoModelo$name=="rmse", "value"], observados = observado)))
  return(dfEstatisticas)
}
