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

##' @title calculates Volume Default
##' @description this function calculates the volume based on the height and volume of literature of the equation
##' @param ht is list of height of individuals
##' @param dap is list of diameter of individuals
##' @param ... only for compatibility with other functions
##' @return will be returned a list of volume calc
##' @export
calculaVolumeDefault <- function(ht, dap, ...){
  p1 = list()
  p1$b0 = -10.1399754928663
  p1$b1 = 1.86835930704287
  p1$b2 = 1.07778665273381
  return(exp(p1$b0+p1$b1*log(dap) + p1$b2*log(ht)))
}
