#' @import methods
NULL

#' Interval
#'
#' An S4 class to represent a named numeric interval.
#'
#' @slot start the lower end of the interval
#' @slot end the higher end of the interval
#' @slot key the name of the interval
#' @export Interval
#' @exportClass Interval


Interval <- setClass(
  # Set the name for the class
  "Interval",

  # Define the slots
  slots = c(
    start = "numeric",
    end   = "numeric",
    key   = "character"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    start = 0.0,
    end   = 0.0,
    key = ""
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(object@end < object@start) {
      return("Invalid interval: end < start.")
    }
    return(TRUE)
  }
)

#' isOverlap
#'
#' Method for checking if an interval is overlapping with a single number (start = end)
#' or a pair of numbers (start < end). A pair of intervals (start1, end1) and (start2, end2)
#' are overlapping if (end2 >= start1 and start2 <= end1).
#'
#' @param theObject an Interval object
#' @param someNumbers a vector of one or two numbers to test overlap.
#' If two numbers are provided, they are treated as an interval (start, end)
#' @return a logical value TRUE or FALSE
#' @examples
#' i1 <- new("Interval", start=1.1, end=1.2, key="dummy")
#' isOverlap(i1, c(1.0, 1.5))
#' isOverlap(i1, 1.0)
#' \dontrun{
#' isOverlap(i1, c(2.0, 1.5))  # generate an error
#' isOverlap(i1, c(1.0, 1.5, 2))  # generate an error
#' }
#'
#' @export isOverlap
#' @exportMethod isOverlap
setGeneric(name="isOverlap",
           def=function(theObject, someNumbers)
           {
             standardGeneric("isOverlap")
           }
)

#' isOverlap
#'
#' Method for checking if an interval is overlapping with a single number (start = end)
#' or an ordered pair of numbers (start < end). Two intervals (start1, end1) and (start2, end2)
#' are overlapping if (end2 >= start1 and start2 <= end1).
#'
#' @param theObject an Interval object
#' @param someNumbers a vector of one or two numbers to test overlap.
#' If two numbers are provided, they are treated as an interval (start, end)
#' @return a logical value TRUE or FALSE
#' @examples
#' i1 <- new("Interval", start=1.1, end=1.2, key="dummy")
#' isOverlap(i1, c(1.0, 1.5))
#' isOverlap(i1, 1.0)
#' \dontrun{
#' isOverlap(i1, c(2.0, 1.5))  # generate an error
#' isOverlap(i1, c(1.0, 1.5, 2))  # generate an error
#' }
#' @export isOverlap
#' @exportMethod isOverlap

setMethod(f="isOverlap",
          signature=c("Interval", "numeric"),
          definition=function(theObject, someNumbers)
          {

            if(length(someNumbers) == 1){
              low <- someNumbers
              high <- someNumbers
            }else if(length(someNumbers) == 2){
              low <- someNumbers[1]
              high <- someNumbers[2]
              if(high < low){
                stop("invalid input. The first number must be smaller than the second number.")
              }
            }else{
              stop("invalid input. Requires 1 or 2 numbers.")
            }

            if(high >= theObject@start & low <= theObject@end){
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
)


