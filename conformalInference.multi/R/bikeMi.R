#' Log of all bike rentals in Milan in 2016 form January to March
#'
#' A dataset containing the log of all the bike trips in Milan (using the BikeMi
#' service), in the period from 25th of January to the 6th of March from Duomo to Duomo,
#' as well as meteorological data.
#'
#' @format A data frame with 41 rows and 6 variables:
#' \describe{
#'   \item{start}{number of trips started in Duomo a given day}
#'   \item{end}{number of trips ended in Duomo a given day}
#'   \item{we}{is weekend?If true, than we is 1}
#'   \item{rain}{mean amount of rain during the day}
#'   \item{dtemp}{difference between average temperature of the day and of the period}
#'   \item{we_rain}{interaction between weekend and rain}

#' }
#' @source \url{https://www.mate.polimi.it/biblioteca/add/qmox/19-2019.pdf}
"bikeMi"
