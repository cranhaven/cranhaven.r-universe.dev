# --------------------------------------------
# Generic function for simulating a red card
# --------------------------------------------

#' Simulate red card(s)
#' 
#' Simulates red card(s) in the united and returns the adjusted lineup.
#' 
#' @param obj object of the class \code{formation}
#' @param lineup lineup of the corresponding object \code{obj}
#' @param Hard matrix of hardness to be used
#' 
#' @return \code{list} with two elements: 
#' \itemize{
#'  \item{\code{vector}}{ adjusted lineup for the red card(s)}
#'  \item{\code{numeric}}{ number of red cards}
#' }
#'
#'
#' @name simRedCard
NULL

#' @rdname simRedCard
#' 
#' @export
setGeneric("simRedCard", function(obj, lineup, Hard) standardGeneric("simRedCard"))