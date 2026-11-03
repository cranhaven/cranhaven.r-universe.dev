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

##' @title List to DataFrame
##' @description converts a list in a dataframe
##' @param dlist a list
##' @examples
##' a <- 1:5
##' listToDataFrame(a)
##' b = listToDataFrame(a)
##' @export
listToDataFrame = function (dlist) {
  n = length(dlist)
  if (n>0) {
    df = data.frame(dlist[[1]])
    if (n>1) {
      for (i in 2:n) {
        df = rbind(df, dlist[[i]])
      }
    }
  } else {
    stop("Enter with a non empty list")
  }
  return(df)
}
