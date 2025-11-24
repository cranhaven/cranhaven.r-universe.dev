#' Code level convert table (internal function)
#'
#' @param klass_data  Klass data frame to convert
#' @param code Name of variable containing code (Default = "code")
#' @param parentcode Name of variable of parent code (Default = "parentCode")
#' @param level Name of variable containing level (Default = "level")
#' @keywords internal
#' @return A dataframe
ConvertTable <- function(klass_data, code = "code", parentcode = "parentCode", level = "level") {
  levelset <- as.numeric(max(klass_data[, level]))
  uni <- unique(klass_data[klass_data[, level] == levelset, code])
  dx <- matrix(NA, length(uni), levelset)
  dx <- as.data.frame(dx)
  dx[, levelset] <- as.character(uni)
  for (i in 1:(levelset - 1)) {
    m <- match(dx[, levelset - i + 1], klass_data[, code])
    dx[, levelset - i] <- as.character(klass_data[, parentcode][m])
  }
  names(dx) <- paste("level", 1:levelset, sep = "")
  return(dx)
}
