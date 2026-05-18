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


#' @title Class Host
#' @name Host
#'
#' @description
#' Spatio-temporal information about Host.
#'
#' @details Object can be created by calling ...
#'
#' rdname Host-class
#' @aliases Host-class
#'
#' @slot ID Host identifier 
#' @slot coordinates Host coordinates in time (as sf)
#' @slot states Host States/Status (dob, Inf...)
#' @slot sources data.frame of time and host id who infected this host
#' @slot offsprings data.frame of time and host id who has been contamined by this host
#' @slot ID_V_POP data.frame of time and index of Viral population Observation
#' @slot covariates data.frame of time, cavariate and value of this host.
#'
#' @import sf
#'
#' @exportClass Host
setClass(
  Class = "Host",
  slots = list(ID = "character",
          #coordinates = "SpatialPointsDataFrame",
          coordinates = "sf",
          states = "data.frame",
          sources = "data.frame",
          offsprings = "data.frame",
          ID_V_POP = "data.frame",
          covariates = "data.frame")
)

setMethod("initialize", "Host",
          function(.Object, ID, coordinates, states, sources, offsprings, ID_V_POP, covariates, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@ID <- ID
            #if(missing(coordinates)) {.Object@coordinates <- SpatialPointsDataFrame(list(0,0),data.frame(list(time=0)))}
            if(missing(coordinates)) {.Object@coordinates <- st_sf(st_sfc())}
            if(missing(states)){ .Object@states <- data.frame(time=character(), value=character(), stringsAsFactors = FALSE)}
            if(missing(sources)){ .Object@sources <- stats::setNames(data.frame(matrix(ncol=3,nrow=0)),c("time","id","prob"))}
            if(missing(offsprings)){ .Object@offsprings <- stats::setNames(data.frame(matrix(ncol=3,nrow=0)),c("time","id","prob"))}
            if(missing(ID_V_POP)){ .Object@ID_V_POP <- data.frame(time=character(), id=character(), stringsAsFactors = FALSE)} #stats::setNames(data.frame(matrix(ncol=2,nrow=0)),c("time","id"))}
            if(missing(covariates)){ .Object@covariates <- data.frame(time=character(), name=character(), value=character(), stringsAsFactors = FALSE)}
            
            .Object
          })

setValidity("Host",
            function(object) {
              if( is.null(object@ID) | !is.character(object@ID) ){ return(FALSE)}
              TRUE
            }
)
