#
# D2MCS provides a novel framework to able to automatically develop and deploy
# an accurate Multiple Classifier System (MCS) based on the feature-clustering
# distribution achieved from an input dataset. D2MCS was developed focused on
# four main aspects: (i) the ability to determine an effective method to
# evaluate the independence of features, (ii) the identification of the optimal
# number of feature clusters, (iii) the training and tuning of ML models and
# (iv) the execution of voting schemes to combine the outputs of each classifier
# comprising the MCS.
#
# Copyright (C) 2021 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

#' @title Plotting feature clusters following bi-class problem.
#'
#' @description The \code{\link{BinaryPlot}} implements a basic plot for
#' bi-class problem.
#'
#' @seealso \code{\link{GenericPlot}}
#'
#' @keywords device color hplot
#'
#' @import R6
#'
#' @export BinaryPlot

BinaryPlot <- R6::R6Class(
  classname = "BinaryPlot",
  inherit = GenericPlot,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description Plots feature-clustering data from a bi-class problem.
    #'
    #' @param summary A \link{data.frame} comprising the elements to be plotted.
    #'
    #' @import ggplot2
    #'
    plot = function(summary) {
      if (!is.data.frame(summary)) {
        stop("[", class(self)[1], "][FATAL] Summary parameter must be defined ",
             "as 'data.frame' type. Aborting...")
      }
      super$plot(summary) + ggplot2::labs(title = "Binary Data") +
        ggplot2::theme_light() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5))
    }
  )
)
