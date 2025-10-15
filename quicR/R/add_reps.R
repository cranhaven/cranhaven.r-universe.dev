#' Add replicates
#'
#' Adds replicate information to the sample IDs. Well IDs should be formatted
#' like so: A4, B9, H11, J24
#'
#' @param df A dataframe containing two columns for well IDs and Sample IDs
#' @param sep a character string to separate the terms.
#' @return A dataframe with replicate numbers pasted to the Sample IDs
#'
#' @export
add_reps <- function(df, sep = "_") {
  if (ncol(df) > 2) {
    stop("Dataframe should only have two columns with well IDs and Sample IDs.")
  }
  df[, "C"] <- NA
  count_list <- list()
  for (i in 1:nrow(df)) {
    samp <- df[i, 2]
    count_list <- append(count_list, samp)
    df[i, 3] <- sum(count_list == samp)
  }
  df[, "B"] <- paste(df$B, df$C, sep = sep)
  df <- df[, 1:2]
  return(df)
}
