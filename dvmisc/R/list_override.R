#' Add Elements of Second List to First List, Replacing Elements with Same Name
#' 
#' Adds each element of \code{list2} to \code{list1}, overriding any elements of 
#' the same name. Similar to \code{modifyList} function in 
#' \pkg{utils} package, but either list can be \code{NULL}. Useful for 
#' \code{\link[base]{do.call}} statements, when you want to combine a list of 
#' default inputs with a list of user-specified inputs.
#' 
#' @param list1 Initial list that has some number of named elements. Can be 
#' \code{NULL} or an empty list.
#' @param list2 List with named elements that will be added to \code{list1}, 
#' replacing any elements with the same name. Can be \code{NULL} or an empty 
#' list.
#' 
#' @return List containing the named elements initially in \code{list1} and not 
#' in \code{list2}, any additional named elements in \code{list2}, and any named 
#' elements in \code{list1} that were replaced by elements of the same name in
#' \code{list2}.
#' 
#' @examples 
#' # Create list that has default inputs to the plot function
#' list.defaults <- list(x = 1: 5, y = 1: 5, type = "l", lty = 1)
#' 
#' # Create list of user-specified inputs to the plot function
#' list.user <- list(main = "A Straight Line", lty = 2, lwd = 1.25)
#' 
#' # Combine the two lists into one, giving priority to list.user
#' list.combined <- list_override(list.defaults, list.user)
#' 
#' # Plot data using do.call
#' do.call(plot, list.combined)
#' 
#' @export
list_override <- function(list1, list2) {
  
  # If list2 is length 0, just return list1
  if (length(list2) == 0) {
    return(list1)
  }
  
  # Get names of elements of list1 and list2
  names.list1 <- names(list1)
  names.list2 <- names(list2)
  
  # Loop through elements of list 2. If in list 1, remove, then add; if not in
  # list 1, add.
  for (ii in 1: length(list2)) {
    
    element.name <- names.list2[ii]
    loc.list1 <- which(names.list1 == element.name)
    if (length(loc.list1) > 0) {
      list1[loc.list1] <- list2[ii]
    } else {
      list1 <- c(list1, list2[ii])
    }
    
  }
  
  # Return list1, which has its original elements plus any extras/overrides from
  # list2
  return(list1)
  
}