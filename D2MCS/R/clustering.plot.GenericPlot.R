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

#' @title Pseudo-abstract class for creating feature clustering plots.
#'
#' @description The \code{\link{GenericPlot}} implements a basic plot.
#'
#' @seealso \code{\link{BinaryPlot}}
#'
#' @keywords device color hplot
#'
#' @import R6
#'
#' @export GenericPlot

GenericPlot <- R6::R6Class(
  classname = "GenericPlot",
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description Implements a generic plot to visualize basic
    #' feature-clustering data.
    #'
    #' @param summary A \link{data.frame} comprising the elements to be plotted.
    #'
    #' @import ggplot2 ggrepel
    #'
    plot = function(summary) {
      if (!is.data.frame(summary)) {
        stop("[", class(self)[1], "][FATAL] Summary parameter must be defined ",
             "as 'data.frame' type. Aborting...")
      }
      min <- data.frame(x = summary[which.min(summary[, 2]), ][, 1], y = min(summary[, 2]))
      max <- data.frame(x = summary[which.max(summary[, 2]), ][, 1], y = max(summary[, 2]))
      ggplot2::ggplot(summary, ggplot2::aes(k, dispersion)) +
        ggplot2::geom_point(ggplot2::aes(color = dispersion), position = ggplot2::position_jitter()) +
        ggplot2::scale_color_continuous(name = "", low = "blue", high = "red", guide = FALSE) +
        ggrepel::geom_text_repel(ggplot2::aes(x, y, label = sprintf("%s", format(min["y"], digits = 2, scientific = TRUE))),
                                 min, hjust = 0.5, vjust = 0, point.padding = 0.25, color = 'blue', size = 3) +
        ggrepel::geom_text_repel(ggplot2::aes(x, y, label = sprintf("%s", format(max["y"], digits = 2, scientific = TRUE))),
                                 max, hjust = 0.5, vjust = 1, point.padding = 0.25, color = 'red', size = 3) +
        ggplot2::scale_y_continuous(name = "Dispersion (using logaritmic scale)", trans = "log2", breaks = c(min[["y"]], max[["y"]])) +
        ggplot2::scale_x_continuous(name = "Number of clusters", breaks = seq(from = 2, to = nrow(summary) + 1))
    }
  )
)
