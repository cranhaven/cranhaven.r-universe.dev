# Author: Darren Colby
# Date: 8/31/2021
# Purpose: To create spatial interaction functions for the NetSim class

# Spatial interaction functions -------------------------------------------


#' Standard power law function
#'
#' @description helper function to estimate tie probability using a standard
#' power law
#'
#' @usage standard(dist, base_prob, scale, power)
#'
#' @details This function should not be called directly
#'
#' @param dist the distance between a pair of points
#'
#' @return A numeric object representing the proability of two nodes sharing a
#' connection
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
standard <- function(dist, base_prob, scale, power) {

    prob <- (base_prob / ((1 + (scale * dist)) ^ abs(power)))

    return(prob)

}



#' Attenuated power law function
#'
#' @description helper function to estimate tie probability using an attenuated
#' power law
#'
#' @usage attenuated(dist, base_prob, scale, power)
#'
#' @details This function should not be called directly
#'
#' @param dist the distance between a pair of points
#'
#' @return A numeric object representing the proability of two nodes sharing a
#' connection
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
attenuated <- function(dist, base_prob, scale, power) {

    prob <- (base_prob / (1 + (scale * dist) ^ abs(power)))


    return(prob)

}


#' Arctangent probability law
#'
#' @description helper function to estimate tie probability using an arctangent
#' probability law
#'
#' @usage arctan(dist, base_prob, scale, power)
#'
#' @details This function should not be called directly
#'
#' @param dist the distance between a pair of points
#'
#' @return A numeric object representing the proability of two nodes sharing a
#' connection
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
arctan <- function(dist, base_prob, scale, power) {

    prob <- base_prob * (1 - ((2 / pi) * atan((scale * dist))))

    return(prob)

}


#' Exponential decay law
#'
#' @description helper function to estimate tie probability using an exponential
#' decay law
#'
#' @usage decay(dist, base_prob, scale, power)
#'
#' @details This function should not be called directly
#'
#' @param dist the distance between a pair of points
#'
#' @return A numeric object representing the proability of two nodes sharing a
#' connection
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
decay <- function(dist, base_prob, scale, power) {

    prob <- base_prob / exp((dist * scale))

    return(prob)

}


#' Logistic probability law
#'
#' @description helper function to estimate tie probability using a logistic
#' probability law
#'
#' @usage logistic(dist, base_prob, scale, power)
#'
#' @details This function should not be called directly
#'
#' @param dist the distance between a pair of points
#'
#' @return A numeric object representing the proability of two nodes sharing a
#' connection
#'
#' @author Darren Colby
#' Email: dscolby17@@gmail.com
#' @noRd
logistic <- function(dist, base_prob, scale, power) {

    prob <- (2 * base_prob) / (1 + (exp(dist * scale)))

    return(prob)
}

