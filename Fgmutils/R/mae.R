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

#' @title mean absolute error (mae)
#' @description is a quantity used to measure how close forecasts or predictions are to the eventual outcomes. The mean absolute error is given by.
#' @param observados vector of values observed.
#' @param estimados vector of regression model data.
#' @details mae = mean(abs(observados-estimados))
#' @references see \url{https://en.wikipedia.org/wiki/Mean_absolute_error} for more details.
#' @return Function that returns Mean Absolute Error
#' @export
mae <- function(observados, estimados)
{
  error = observados-estimados
  mean(abs(error))
}
