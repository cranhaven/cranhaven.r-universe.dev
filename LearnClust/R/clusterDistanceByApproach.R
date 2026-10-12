#' @title To calculate the distance by approach option.
#' @description To calculate the distance depending on \code{option} given.
#' @param distances is a numeric vector.
#' @param approach is a string. Type of function to apply.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' distance value from \code{distances}.
#' @details \code{approach} indicates the algorithm used to get the value. Possible values: {\code{'MAX'},
#' \code{'MIN'}, \code{'AVG'}}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return max, min or average from a vector.
#' @examples
#'
#' distances1 <- c(4,14,24,34)
#'
#' distances2 <- c(1:10)
#'
#' clusterDistanceByApproach(distances1,'MAX')
#'
#' clusterDistanceByApproach(distances2,'MIN')
#'
#' @export

clusterDistanceByApproach <-
  function(distances,approach){
    switch(approach,
           MAX = max(distances),
           MIN = min(distances),
           AVG = mean(distances))
  }

