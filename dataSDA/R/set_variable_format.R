#' Set Variable Format
#'
#' @name set_variable_format
#' @aliases set_variable_format
#' @description This function changes the format of the set variables
#' in the data to conform to the RSDA format.
#' @usage set_variable_format(data, location, var)
#' @param data A conventional data.
#' @param location The location of the set variable in the data.
#' @param var The name of the set variable in the data.
#' @returns Return a dataframe in which a set variable is converted to one-hot encoding.
#' @examples
#' data("mushroom")
#' mushroom.set <- set_variable_format(data = mushroom, location = 8, var = "Species")
#' @export

set_variable_format <- function(data, location = NULL, var = NULL){
  if (is.null(location) != TRUE){
    data.set <- data
    nr <- nrow(data)
    nc <- ncol(data)
    y <- data[, location]
    set_table <- data.frame(matrix(0, nr, length(unique(y)) + 1))
    for (i in 1:length(unique(y))){
      set_table[, 1] <- y
      set_table[, i + 1] <- y %in% unique(y)[i]*1
      names(set_table)[i + 1] <- unique(y)[i]
    }
    set_table[, 1] <- rep(length(unique(y)), nr)
    if (location == ncol(data)){
      data <- cbind(data[, 1:location], set_table)
    } else{
      data <- cbind(data[, 1:location], set_table, data[,(location + 1):nc])
    }
    names(data)[location + 1] <- names(data.set)[location]
    data <- data[, -location]
  }
  if (is.null(var) != TRUE){
    location_var <- which(colnames(data) == var)
    data.set <- data
    nr <- nrow(data)
    nc <- ncol(data)
    y <- data[, location_var]
    set_table <- data.frame(matrix(0, nr, length(unique(y)) + 1))
    for (i in 1:length(unique(y))){
      set_table[, 1] <- y
      set_table[, i + 1] <- y %in% unique(y)[i]*1
      names(set_table)[i + 1] <- unique(y)[i]
    }
    set_table[, 1] <- rep(length(unique(y)), nr)
    if (location_var == ncol(data)){
      data <- cbind(data[, 1:location_var], set_table)
    } else{
      data <- cbind(data[, 1:location_var], set_table, data[,(location_var + 1):nc])
    }
    names(data)[location_var + 1] <- names(data.set)[location_var]
    data <- data[, -location_var]
  }
  return(data)
}
