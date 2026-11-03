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

#' @title add column
#' @description  take a data-frame and a vector and combine by columns, respectively.
#' @param dataf dataframe
#' @param vec vector
#' @param namevec the names of the columns of vector
#' @return dataf dataframe combined with the vector
#' @export
add.col <- function(dataf, vec, namevec) {
  options(warn=-1)
  if (nrow(dataf) < length(vec) ) {
    dataf <-  rbind(dataf, matrix(NA, length(vec)-nrow(dataf), ncol(dataf),	dimnames=list( NULL, names(dataf) ) ) )
  }
  length(vec) <- nrow(dataf) # pads with NA's
  dataf[, namevec] <- vec; # names new col properly
  options(warn=1)
  return(dataf)
}
