

#' Pass only unique values to a computationally expensive function that returns an output of the
#' same length as the input.
#'
#' In importing and working with tidy data, it is common to have index columns, often including time stamps
#' that are far from unique. Some funcitons to work with these such as text conversion, various \code{grep()}-based
#' functions, and often the /code{cut()} function are relatively slow when working with tens of millions of rows or more.
#'
#' This wrapper function pares down the items to process to only unique values using hte \code{unique()} function.
#' For a function that takes in a vector or list and returns a vector or list the same length, the inputs
#' and outputs are the same as they would be otherwise ... it just happens faster.
#'
#' @param x A list or vector to be passed to \code{.f}
#' @param .f The function to be called. It take as in input the incoming \code{x} vector or list and
#' it must return a vector or list of the same length as the input.
#' @param  ... Any other arguments to be passed to \code{.f}.
#' @return The normal output of \code{.f()} as long as it is of the same length os \code{x}
#' @examples
#' #Create a sample of some date text with repeats
#' ts_sample <-
#'   sample(
#'     as.character(
#'       seq(from = as.POSIXct('2020-03-01'), to = as.POSIXct('2020-03-15'), by = 'day')
#'       ),
#'     size = 30, replace = TRUE
#'   )
#'
#' #Now convert the time text back to POSIXct timestamps:
#' as.POSIXct(ts_sample)
#'
#' #Do the same with the calcUnique function:
#' calcUnique(ts_sample, as.POSIXct)
#'
#' #Just to show that the output is the same with and without calcUnique:
#' all.equal(as.POSIXct(ts_sample),calcUnique(ts_sample, as.POSIXct))
#'
#' #An example for when the function doesn't take the vector as the first argument:
#' gsub("00","$$", ts_sample)
#' calcUnique(ts_sample, function(i) gsub("00","$$", i))
#' @export
calcUnique <-
  function(x,
           .f,
           ...) {

    #Trim down the input to its unique entries
    x_u <- unique(x)

    #Run the function on the unique input
    y_u <- .f(x_u, ...)

    #check to see if the output of .f() is of the same length as the input
    if(length(y_u) != length(x_u)) {
      stop(".f must return an object of the same length as the input vector")
    }

    #Use the match() function to reconstruct what y would have been and return that
    return(
      y_u[match(x = x, table = x_u)]
    )
  }



