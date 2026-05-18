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

#' @title Class ViralPop
#' @name ViralPop
#'
#' @description
#' Viral population data containing genotypes
#'
#' @rdname ViralPop-class
#' @aliases ViralPop-class
#'
#' @slot ID Host identifier 
#' @slot time Observation time as numeric since 1970/01/01
#' @slot size Qt of variants
#' @slot names list of variants id with same sequence
#' @slot genotypes all variants genotypes (as DNAStringSet)
#' @slot proportions proportions of each variants
#'
#' @import Biostrings
#'
#' @exportClass ViralPop
setClass(Class = "ViralPop",
         slots = list( ID = "character",
                       time = "numeric",
                       size= "numeric",
                       names = "list",
                       genotypes = "DNAStringSet",
                       proportions = "vector")
         )

setMethod("initialize", "ViralPop",
          function(.Object, ID, time, size, names, genotypes, proportions, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@ID <- ID
            if( is.numeric(time) ){ .Object@time <- time }
            else { .Object@time <- as.numeric(as.POSIXct(time))}
            .Object@size <- size
            .Object@names <- names
            .Object@genotypes <- genotypes
            .Object@proportions <- proportions
            .Object
          })

setValidity("ViralPop",
            function(object) {
              if( is.null(object@ID) | !is.character(object@ID) ){ return(FALSE)}
              if( is.null(object@size) | !is.numeric(object@size) | object@size <= 0 ){ return(FALSE)}
              if( length(object@genotypes) == length(object@proportions) ){ return(FALSE)}
              TRUE
            }
)
