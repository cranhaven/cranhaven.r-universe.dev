
#' Oskar van Deventer set of dice.
#'
#' A dataset containing the Oskar van Deventer dice, non-transitive
#' set of dice where  A beats {B,C,E}; B beats {C,D,F}; C beats {D,E,G}; #' D beats {A,E,F}; E beats {B,F,G}; F beats {A,C,G}; G beats {A,B,D}. #' Consequently, for arbitrarily chosen two dice there is a third one
#' that beats both of them.
#'
#' @docType data
#'
#' @usage data(oskar)
#'
#' @format A data table with 6 columns. Each column represents a die
#' with six faces.
#'
#' @keywords datasets
#' @examples
#' data(oskar)
#' is.nonTransitive(oskar)
"oskar"
