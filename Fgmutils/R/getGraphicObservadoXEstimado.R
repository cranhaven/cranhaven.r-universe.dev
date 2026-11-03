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

##' @title Get Graphic Observed X Estimated
##' @description this function display/save a graphic scatter.smooth illustrating the difference between the observed and estimated
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param showTestF draw results of test F in graphic?
##' @param save If you want to save enter the directory as a string
##' @param labsX label x
##' @param labsy label y
##' @param vetorial save picture in vector type? (Default TRUE)
##' @param ... only for compatibility with other functions
##' @import grDevices
##' @export
getGraphicObservadoXEstimado <- function(titulo="observadoXestimado", nome ="observadoXestimado", observado, estimado, showTestF = TRUE, save=NULL, labsX = "observado",  labsy = "estimado", vetorial = T, ...){
  if (!is.null(save)){
    if (vetorial)
      postscript(paste0(save, nome, "ObservadoXEstimado.postscript"))
    else
      png(paste0(save, nome, "ObservadoXEstimado.png"))
  }
  scatter.smooth(observado, estimado, col="black", xlab=labsX, ylab=labsy, main=titulo, pch=18)
  abline(0,1)
  if (showTestF == TRUE)
  {
    coeficientes = lm(estimado ~ observado)
    mtext("Test F:", side=3, line=0.8)
    eval(parse(text = paste0(   "mtext(expression(paste(
                                beta, \"0 = ", round(coeficientes$coefficients[1], digits = 3), " \",",
                                "beta, \"1 = ", round(coeficientes$coefficients[2], digits = 3), "\"",
                                ")), side=3, line=-0.1)"  )))
  }
  if (!is.null(save)){
    dev.off()
  }
}
