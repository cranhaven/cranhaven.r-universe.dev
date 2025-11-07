# Author: Darren Colby
# Date: 8/31/2021
# Purpose: To provide helper functions for point simulations for the PointSim
# class

# Helper functions for the PointSim class ---------------------------------


#' Simulate a Poisson point process
#'
#' @description helper function to simulate a Poisson point process
#'
#' @usage poisson_process(n, win)
#'
#' @details This function should not be called directly
#'
#' @param n the number of points to simulate
#' @param win a window of class spacejamr to use as the spatial extent
#'
#' @return A ppp object from the 'spatstat' package that contains
#' simulated points
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
poisson_process <- function(n, win) {

    # Homogeneous point process
    point_process <- spatstat.random::rpoint(n = n, win = win)

    return(point_process)

}


#' Simulate a 2D Halton Sequence
#'
#' @description helper function to simulate a 2D Halton Sequence
#'
#' @usage halton(n, win)
#'
#' @details This function should not be called directly
#'
#' @param n the number of points to simulate
#' @param win a window of class spacejamr to use as the spatial extent
#'
#' @return A ppp object from the 'spatstat.geom' package that contains
#' contains simulated points
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
halton <- function(n, win) {

    # Simulates the Halton sequence
    halton_seq <- spatstat.geom::rQuasi(n, win, "Halton")

    return(halton_seq)

}
