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

#' @title Root Mean Square Error
#' @description The root-mean-square error (RMSE) is a frequently used measure of the differences between values (sample and population values) predicted by a model or an estimator and the values actually observed.
#' @param observados vector of values observed.
#' @param estimados vector of regression model data.
#' @details rmse = sqrt(mean((observados - estimados)^2))
#' @references See \url{https://en.wikipedia.org/wiki/Root-mean-square_deviation} for more details.
#' @export
rmse <- function(observados, estimados)
{
  error = observados-estimados
  sqrt(mean(error^2))
}
