#' Dispersion of tokens in a text
#'
#' This function wraps a call to the dispersion service, which calculates the
#' dispersion of a list of tokens throughout a text in the National Library of
#' Norway's collection, given by the URN. The text is divided into chunks, and
#' the count of tokens in each chunk is returned.
#'
#' @param urn A National Library of Norway URN to a text object.
#' @param words A list or vector of words (tokens) to analyze for dispersion.
#' @param window The size of the text chunk to count the tokens within.
#' @param pr (Per) Determines the step size for moving forward to the next chunk.
#'        If 'pr' is equal to 'window', the text is divided into non-overlapping
#'        chunks of size 'window'. If 'pr' is smaller than 'window', the chunks
#'        will overlap, creating a smoother curve.
#'
#' @return A data frame with the count of tokens in each chunk.
#' @export
#'
#' @examples
#' urn <- "URN:NBN:no-nb_digibok_2013060406055"
#' words <- c("Dracula", "Mina", "Helsing")
#' window <- 1000
#' pr <- 1000
#' dispersion_result <- get_dispersion(urn, words, window, pr)
get_dispersion <- function(urn = NULL, words = list(".", ","), window = 500, pr = 100){

  if (!is.character(urn)) {
    urn <- as.character(urn)
  }

  url <- "https://api.nb.no/dhlab/dispersion"
  result_df <- data.frame()


  for (word in words) {
    params <- list("urn" = urn, "words" = word, "window" = window, "pr" = pr)
    #query <- POST(url, body = params, encode = "json")
    query <- api_call_wrapper(url, body = params, encode = "json")

    if  (is.null(query)) {
      return(NULL)
    }


    data <- as.data.frame(do.call(rbind, content(query)))
    colnames(data) <- word

    # If the result_df is empty, set it to temp_df; otherwise, bind the columns
    if (ncol(result_df) == 0) {
      result_df <- data
    } else {
      result_df[[word]] <-data[[word]]
    }

  }

  return(result_df)

}



