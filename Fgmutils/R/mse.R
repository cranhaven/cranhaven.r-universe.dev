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

#' @title Mean squared error
#' @description the MSE is the mean of the square of the errors, corresponding to the expected value of the squared error loss or quadratic loss. The difference occurs because of randomness or because the estimator doesn't account for information that could produce a more accurate estimate.
#' @param observados vector of values observed.
#' @param estimados vector of regression model data.
#' @param k the number of model parameters
#' @details mse = (sum(estimados-observados)^2)/(length(observados)-k)
#' @references See \url{https://en.wikipedia.org/wiki/Mean_squared_error} for more details.
#' @export
mse <- function(observados, estimados, k) {
  (sum(estimados-observados)^2)/(length(observados)-k)
}
