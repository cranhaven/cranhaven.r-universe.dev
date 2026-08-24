#' Title
#'
#' @param data The dataframe containing the variables (the items).
#' @param scales A list of character vectors with the items in each scale,
#' where each vectors' name is the name of the scale.
#' @param append Whether to return the dataframe including the new variables
#' (`TRUE`), or a dataframe with only those new variables (`FALSE`).
#'
#' @return Either a dataframe with the newly created variables, or the
#'   supplied dataframe with the newly created variables appended.
#' @export
#'
#' @examples ### First generate a list with the scales
#' scales <- list(scale1 = c('mpg', 'cyl'), scale2 = c('disp', 'hp'));
#'
#' ### Create the scales and add them to the dataframe
#' makeScales(mtcars, scales);
makeScales <- function(data, scales, append=TRUE) {
  for (currentScale in 1:length(scales)) {
    if (length(unlist(scales[currentScale])) > 1) {
      data[[names(scales[currentScale])]] <-
        rowMeans(data[, unlist(scales[currentScale])], na.rm=TRUE);
      data[[names(scales[currentScale])]] <-
        ifelse(is.nan(data[[names(scales[currentScale])]]),
               NA,
               data[[names(scales[currentScale])]]);
    }
    else if (length(unlist(scales[currentScale])) == 1) {
      data[[names(scales[currentScale])]] <- data[[unlist(scales[currentScale])]];
    }
  }
  if (append) {
    return(data);
  } else {
    return(data[, names(scales)]);
  }
}
