# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

#' @encoding UTF-8
#' @title Data Structure and Manipulation Tool for Host and Viral Population 
#' @description Statistical Methods for Inferring Transmissions of Infectious Diseases from deep sequencing data (SMITID).
#'  It allow sequence-space-time host and viral population data storage, indexation and querying.
#' @aliases SMITIDstruct-package SMITIDstruct
#' 
#' @author Jean-Francois Rey \email{jean-francois.rey@@inra.fr}
#' 
#' Maintainer: Jean-Francois Rey \email{jean-francois.rey@@inra.fr}
#' @docType package
#' @name SMITIDstruct-package
#' @details \tabular{ll}{
#'          Package: \tab SMITIDstruct\cr
#'          Type: \tab Package\cr
#'          Version: \tab 0.0.5\cr
#'          Date: \tab 2019-06-14\cr
#'          License: \tab GPL (>=2)\cr
#'          }
#'
#' The SMITIDstruct package contains functions and methods for manipulating Host and Viral population genotico-space-time data.
#'          
#' @seealso \code{\link{demo.SMITIDstruct.run}}
#' @examples 
#' \donttest{
#' ## Run a simulation
#' library("SMITIDstruct")
#' demo.SMITIDstruct.run() 
#' }
#'
#' @import methods
#' @importFrom utils read.table
#' @import ggplot2
#'
"_PACKAGE"
