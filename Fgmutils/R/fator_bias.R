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

#' @title Fator Bias
#' @description The bias factor indicates the average of the observed values is above or below the equity line.
#' @param observados vector of values observed.
#' @param estimados vector of values estimated.
#' @param n the size of the vector of regression model data
#' @details fator_bias = 10^(sum(log(estimados/observados)/n))
#' #' @references see \url{https://www.sciencedirect.com/science/article/pii/S0165176599001949} for more details.
#' @export
fator_bias <- function(observados, estimados, n)
{
  10^(sum(log(estimados/observados)/n))
}
