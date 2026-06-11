
#' @title Bin data into longer windows
#' @description Bin minute level data into different time resolutions
#'
#' @param x  \code{vector} of activity data.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#' @param window window size used to bin the original 1440 dimensional data into. Window size
#' should be an integer factor of 1440
#' @return a vector of binned data

#'
#' @importFrom zoo rollapply

#'
#' @export
#'
#'
#'
 


bin_data2 = function(
  x = x,
  window = 1,
  method = c("average","sum")
){

  if(length(x) != 1440){
   # stop("Please inpute 1440 dimensional minute-level activity data!")  #gw 5/5/21
  }

  if(1440 %% window != 0){
    stop("Only use window size that is an integer factor of 1440")
  }


  method = match.arg(method)

  if(method == "sum"){
    binx = rollapply(x, width = window, by = window, FUN = sum)
  }
  if(method == "average"){
    binx = rollapply(x, width = window, by = window, FUN = mean)
  }

  return(binx)

}

 