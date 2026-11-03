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

##' @title Round Ages
##' @description this function approaching the age to the nearest age as an integer
##' @param plots is list of plots
##' @param ages is list of age
##' @param inYears ages are in year?
##' @param firstAge synchronize begin of ages with an age? what age?
##' @return will be returned a list of round ages
##' @export
roundAge  <- function(plots, ages, inYears = F, firstAge = NaN){

  base = data.frame(parcela = plots, idade = ages, idade2 = -999)

  parcelas = unique(plots)

  incr = 12

  if(inYears) incr = 1

  for (i in 1:length(parcelas)) {

    parcela = parcelas[[i]]

    idades = sort(unique(base[base$parcela == parcela, "idade"]))

    if(length(idades) > 1)
      for(i in 2:length(idades))
        if( abs(idades[[i]] - idades[[i -1]]) > incr + (incr / 2))
          warning(paste0("pronounced difference between age ", idades[[i - 1]], " and ", idades[[i]], " in  ", parcela))

    idademin = round( min(idades) )
    if(!is.nan(firstAge))
      idademin = firstAge
    at = seq( idademin, (round(max(idades)) + incr), incr)

    for (j in 1:length(idades))
      base[base$parcela == parcela & base$idade == idades[[j]], "idade2"] = at[[j]]

  }
  return (base$idade2)
}
