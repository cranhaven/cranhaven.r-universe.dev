#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: time_lcc.R                                                    #
# Contains: time_lcc function                                         #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Regular Sequence for the Time Variable
##'
##' @description An list specifying control arguments to generate a
##'   regular sequence for the time variable returned by the function
##'   \code{time_lcc}, which is used to constructed the LCC, LPC and LA
##'   curves and its simultaneous confidence intervals. Default is
##'   \code{NULL}.
##'
##' @param time unique values of time variable
##'
##' @param from the starting (minimal) value of time variable.
##'
##' @param to the end (maximal) value of time variable.
##'
##' @param n an integer specifying the desired length of the
##'   sequence. Generally, \code{n} between 30 and 50 is adequate.
##' @keywords internal
##'
##' @return Return a regular sequence used  to create the time variable
##'
##' @examples
##' data(hue)
##' attach(hue)
##' time_lcc(time=Time, from=min(Time), to=max(Time), n=30)
##' detach(hue)
##'
##' @export

time_lcc <- function(time,from, to, n){
  tk.new <- unique(sort(c(seq.int(from = from, to = to, length.out = n),
                          time)))
  return(tk.new)
}
