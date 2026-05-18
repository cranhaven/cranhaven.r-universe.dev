#' Convert character vector to a double list
#'
#' @description Structurize a character vector to a double list. Layers in the double list will be determined
#' by the given pattern.
#' @param x a character vector to be converted.
#' @param splt a string pattern to make defination for spliting each layer of double list.
#'
#' @return return a character double list splited by defined pattern, the default pattern is "-".
#' @export chrvec2dl
#'
#' @examples
#' #example using default dataset:
#'
#' charvector <- colnames(datatest)
#' chrvec2dl(charvector, "-")
#' @keywords Double.List
chrvec2dl <- function(x, splt="-"){

  m <- splt

  layers <- length(strsplit(x[1], split = m)[[1]])
  rpt <- length(x)

  ept_list <- list()
  result <- list(ept_list)[rep(1,layers)]

  # condition of one element
  if (rpt==1){
    x <- as.character(x)
    i <- 1
    for (i in 1:layers) {
      result[[i]] <- strsplit(x[1], split = m)[[1]][i]
    }
  }

  # input data diagnose
  if (is.vector(x) != TRUE){
    stop("input data should be a vector", call. = FALSE)
  }

  if (is.character(x) != TRUE){
    x <- as.character(x)
  }

  # pointer to check the length
  length_vec <- c(1:rpt)

  i <- 1
  for (i in 1:rpt) {
    length_vec[i] <- length(strsplit(x[i], split = m)[[1]])
  }

  layers <- max(length_vec)
  result <- list(ept_list)[rep(1,layers)]

  j <- 1
  i <- 1
  while (j <= layers) {
    temp_vec <- c()
    for (i in 1:rpt) {
      if (j > length_vec[i]){
        temp_vec <- append(temp_vec, "*") # length extension by "*" if present element is empty
      } else {
        temp_vec <- append(temp_vec, strsplit(x[i], split = m)[[1]][j])
      }
    }
    result[[j]] <- temp_vec
    j <- j + 1
  }

  i <- 1
  for (i in 1:layers) {
    result[[i]] <- levels(factor(result[[i]]))
  }

  class(result) <- "Double.List"
  return(result)
}
