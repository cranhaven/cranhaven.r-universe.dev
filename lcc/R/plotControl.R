#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plotControl.R                                                 #
# Contains: plotControl function                                      #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 03/06/2020                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Specifying Graphical Control Values for \code{lcc} Class
##'
##' @description The values supplied in the \code{plotControl()} call
##'   replace the defaults, and a \code{\link{list}} with all settings
##'   is returned.
##'
##' @return a list with components for each of the possible arguments.
##'
##' @param plot an optional to include an initial plot. If \code{TRUE},
##'   the default, returns a \code{\link[ggplot2]{ggplot}} object with a
##'   initial plot for \code{lcc} class. If \code{FALSE} never includes.
##'
##' @param shape Draw points considering a shape parameter. Legal shape
##'   values are the numbers 0 to 25, and 32 to 127; see
##'   \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is
##'   \code{1}.
##'
##' @param colour an specification for lines color. Default is
##'   \code{"black"}.
##'
##' @param size an specification for lines size. Should be specified
##'   with a numerical value (in millimetres); see
##'   \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is
##'   \code{0.5}.
##'
##' @param xlab a title for the \code{x} axis.  Default is
##'   \code{"Time"}.
##'
##' @param ylab title for the \code{y} axis. Default is
##'   \code{"LCC"}.
##'
##' @return No return value, called for side effects
##' 
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @import ggplot2
##'
##' @keywords internal
plotControl<-function(plot= TRUE, shape=1, colour="black", size=0.5,
                       xlab = "Time", ylab = "LCC")
{
       list(plot = plot, shape=shape, colour=colour, size=size,
            xlab = xlab, ylab = ylab)
}
