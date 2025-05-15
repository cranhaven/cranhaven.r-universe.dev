#' clean_colnames
#'
#' @name clean_colnames
#' @aliases clean_colnames
#' @description This function is used to clean up variable names to conform to the RSDA format.
#' @usage clean_colnames(data)
#' @param data The conventional data.
#' @returns Data after cleaning variable names.
#' @examples
#' data(mushroom)
#' mushroom.clean <- clean_colnames(data = mushroom)
#' @export

clean_colnames <- function(data){
  colnames(data) <- gsub("_min|_max|_Min|_Max|.min|.max|.Min|.Max",
                         '', colnames(data))
  return(data)
}

