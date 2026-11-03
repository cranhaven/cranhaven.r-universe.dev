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

#' @title R21a
#' @description To avoid any problems and confudion on the part of the data analyst, it seems a safe recommendation to use R21a consistently for any type of model with the appropeiate a value, rather than ajusting any of the other
#' @param observados vector of values observed.
#' @param estimados vector of values estimated.
#' @param k is the number of model parameters
#' @details R21a <- 1-a*(1 - R21)
#' @export
R21a <- function(observados, estimados, k) {
  #with intercept
  y <- observados
  yest <- estimados
  a <- calculaA(length(y), k)

  R21 = 1 - sum((y-yest)^2)/sum((y-mean(y))^2)
  R21a <- 1-a*(1 - R21)

  return(R21a)
}
