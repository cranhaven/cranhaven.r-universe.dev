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

#' @title relative root mean square error
#' @description relative root mean square error (RRMSE) is calculated by dividing the RMSE by the mean observed data
#' @param observados vector of values observed.
#' @param estimados vector of regression model data.
#' @export
rrmse=function(observados,estimados) {
  return(sqrt(mean((observados-estimados)^2)/length(observados))*1/mean(observados))
}
