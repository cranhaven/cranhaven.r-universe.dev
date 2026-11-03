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

##' @title Get List of DAP Classes
##' @description this function return a list of data.frame where each contains a number of dap classes according to reported basis
##' @param base the data.frame  containing fields limiteMin, limiteMax of parcela and idadearred
##' @param amplitude it is amplitude of dap class
##' @param verbose use TRUE to show status of process
##' @return list of data.frame
##' @export
getClasses <- function(base, amplitude, verbose=FALSE){
  if (verbose) print("getting class list")
  classes = list();
  linhas = nrow(base)
  for (i in 1:linhas){
    min = base[i, "limiteMin"]
    max = base[i, "limiteMax"]
    classes[[i]] = defineClasses(limiteMin = min, limiteMax = max, amplitude,  getDataFrame = TRUE, verbose = verbose)
    if (verbose) print(paste0(i, " class for ", linhas, " [ ", min, " to ", max, " ]"))
  }
  return (classes)
}
