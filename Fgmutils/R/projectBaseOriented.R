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

##' @title Project Base Oriented
##' @description this function build a list of dataframe with projects of ages between 'firstAge' and 'lastAge' params
##' @param firstAge the first age to predict
##' @param lastAge the last age to predict
##' @param fitDAP a fit get function inherit lm to DAP
##' @param fitHT a fit get function inherit lm to HT
##' @param base data base
##' @param mapper the label used in fields to age, dap and ht
##' @param calcVolume function to calc volume
##' @param forcePredict force calc base coefficients or se predict()?
##' @return will be returned a list of volume predict to ages in dataframe and/or param
##' @export
projectBaseOriented <- function(firstAge = NaN, lastAge = NaN, fitDAP, fitHT, base,
                                mapper = list(age1="idadearred1", dap1="dap1", dap2="dap2", ht1="ht1", ht2="ht2"),
                                calcVolume = calculaVolumeDefault, forcePredict = F){

  if (is.nan(firstAge)) firstAge = min(base[,mapper$age1])
  if (is.nan(lastAge)) lastAge = max(base[,mapper$age1])

  retorno = list()

  for (i in firstAge:lastAge) {
    b2 = base[base[, mapper$age1] == i,]
    if (nrow(b2) > 0){
      b2$volume1 = calcVolume( dap = b2[, mapper$dap1],  ht = b2[, mapper$ht1], base)
      b2$volume2 = calcVolume( dap = b2[, mapper$dap2],  ht = b2[, mapper$ht2], base)
      b2$dap2est = predizer(fitDAP, newdata = b2, force = forcePredict)
      b2$ht2est = predizer(fitHT, newdata = b2, force = forcePredict)
      b2$volume2est = calcVolume( dap = b2$dap2est, ht = b2$ht2est, base )
      eval(parse(text = paste0("retorno$result_", i, " = b2")))
    }
  }

  if (is.null(names(retorno)))
    stop(paste0("The base not contains data with ages ", firstAge, " and ", lastAge))
  return(retorno)
}
