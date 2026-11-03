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

##' @title Get Histogram of Residuals absolute
##' @description this function displays/saves a histogram graph illustrating the frequency of waste in classes
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param estatisticas data.frame containing field 'residuo'
##' @param save If you want to save enter the directory as a string
##' @param vetorial save picture in vector type? (Default TRUE)
##' @param ... only for compatibility with other functions
##' @import grDevices
##' @export
getGraphicHistogram <- function(titulo="residuos", nome ="observadoXestimado", estatisticas, save=NULL, vetorial = T, ...){
  if (!is.null(save)){
    if (vetorial)
      postscript(paste0(save, nome, "Histogram.postscript"))
    else
      png(paste0(save, nome, "Histogram.png"))
  }
  hist(estatisticas$estatisticas$residuo, xlab=iconv("Residuos"), breaks=100, main=iconv("Histograma de residuos"))
  if (!is.null(save)){
    dev.off()
  }
}
