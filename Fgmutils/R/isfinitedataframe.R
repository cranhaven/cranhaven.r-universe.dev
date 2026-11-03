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

#' @title is finite data frame
#' @description check if a data.frame has any non-finite elements
#' @param obj any object
#' @return TRUE if "x" is finite, FALSE if "x" is not finite
#' @examples
#' date <- c("02/2009","02/2010","02/2011","02/2012")
#' x <- c(1,2,3,4)
#' test <- data.frame(x,date)
#' isfinitedataframe(test)
#' isfinitedataframe(x)
#' @export
isfinitedataframe <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}
