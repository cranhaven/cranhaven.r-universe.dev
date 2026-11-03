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

#' @title R29a
#' @description To avoid any problems and confusion on the part of the data analyst, it seems a safe recommendation to use R21a consistently for any type of model with the appropeiate a value, rather than adjusting any of the other.
#' @param observados vector of values observed.
#' @param estimados vector of values estimated.
#' @param k is the number of model parameters
#' @details R29a <- 1-a*(1 - R29)
#' @export
R29a <- function(observados, estimados, k) {
  #without intercept and nonlinear
  y <- observados
  yest <- estimados
  a <- calculaA(length(y), k)

  R29 = 1 - median((abs(y-yest))^2)/median((abs(y-mean(y)))^2)
  R29a <- 1-a*(1 - R29)

  return(R29a)
}
