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

#' @title calculates residue percentage
#' @description this function calculates the vector residue percentage.
#' @param observados vector of values observed.
#' @param estimados vector of values estimated.
#' @details calculaPerc = ((valor)/mean(observados))*100
#' @export
residuoPerc <- function(observados, estimados)
{
  ifelse(observados==0,0, ((observados-estimados)/observados)*100)
}
