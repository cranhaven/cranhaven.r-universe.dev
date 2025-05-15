#' RSDA Format
#'
#' @name RSDA_format
#' @aliases RSDA_format
#' @description This function changes the format of the data to conform to RSDA format.
#' @usage RSDA_format(data, sym_type1 = NULL, location = NULL, sym_type2 = NULL, var = NULL)
#' @param data A conventional data.
#' @param sym_type1 The labels I means an interval variable and $S means set variable.
#' @param location The location of the sym_type in the data.
#' @param sym_type2 The labels I means an interval variable and $S means set variable.
#' @param var The name of the symbolic variable in the data.
#' @returns Return a dataframe with a label added to the previous column of symbolic variable.
#' @examples
#' data("mushroom")
#' mushroom.set <- set_variable_format(data = mushroom, location = 8, var = "Species")
#' mushroom.tmp <- RSDA_format(data = mushroom.set, sym_type1 = c("I", "S"),
#'                             location = c(25, 31), sym_type2 = c("S", "I", "I"),
#'                             var = c("Species", "Stipe.Length_min", "Stipe.Thickness_min"))
#' @export

RSDA_format <- function(data, sym_type1 = NULL, location = NULL,
                        sym_type2 = NULL, var = NULL){
  nc <- ncol(data)
  nr <- nrow(data)
  data.rep <- rep(NA, nr)
  if (is.null(sym_type1) != TRUE && is.null(sym_type2) == TRUE){
    if(length(sym_type1) != length(location)){return("Error")}
    n <- length(location)
    lc <- c(location, nc)
    gap <- NULL
    for (i in 1:n) {
      gap[i] <- lc[(i + 1)] - lc[i]
      gap.data <- data[, lc[i]:(lc[i] + gap[i] - 1)]
      rep.money <- rep(paste0("$", sym_type1[i]), nr)
      data.rep <- cbind(data.rep, rep.money, gap.data)
    }
    if (location[n] == nc){
      data.rep <- data.rep[, -c(1, length(data.rep))]
    } else {
      data.rep <- data.rep[, -1]
      data.rep <- cbind(data.rep, data[, nc])
    }
    if (length(location) == 1){
      if (location[1] != 1){
        data.rep <- cbind(data[, 1:location[1] - 1], data.rep)
      }
    } else {
      if (location[1] != 1){
        if (location[1] == 2){
          data.rep <- cbind(data[, 1], data.rep)
          names(data.rep)[1] <- names(data)[1]
        } else {
          data.rep[, 1:location[1] - 1] <- data[, 1:location[1] - 1]
        }
      }
    }
    index <- lc[1:n] + c(1:n) - 1
    var.name <- lc[1:n] + c(1:n)
    names(data.rep)[index] <- c(paste0("$", sym_type1))
    names(data.rep)[var.name] <- names(data)[location]
    names(data.rep)[ncol(data.rep)] <- names(data)[nc]
  }
  if (is.null(sym_type1) == TRUE && is.null(sym_type2) != TRUE){
    location_fun <- function(x){
      return(x %in% var)
    }
    location_var <- which(apply(matrix(colnames(data), nrow = 1), 1, location_fun))
    if(length(sym_type2) != length(location_var)){return("Error")}
    n <- length(location_var)
    lc <- c(location_var, nc)
    gap <- NULL
    for (i in 1:n) {
      gap[i] <- lc[(i + 1)] - lc[i]
      gap.data <- data[, lc[i]:(lc[i] + gap[i] - 1)]
      rep.money <- rep(paste0("$", sym_type2[i]), nr)
      data.rep <- cbind(data.rep, rep.money, gap.data)
    }
    if (location_var[n] == nc){
      data.rep <- data.rep[, -c(1, length(data.rep))]
    } else {
      data.rep <- data.rep[, -1]
      data.rep <- cbind(data.rep, data[, nc])
    }
    if (length(location_var) == 1){
      if (location_var[1] != 1){
        data.rep <- cbind(data[, 1:location_var[1] - 1], data.rep)
      }
    } else {
      if (location_var[1] != 1){
        if (location_var[1] == 2){
          data.rep <- cbind(data[, 1], data.rep)
          names(data.rep)[1] <- names(data)[1]
        } else {
          data.rep[, 1:location_var[1] - 1] <- data[, 1:location_var[1] - 1]
        }
      }
    }
    index <- lc[1:n] + c(1:n) - 1
    var.name <- lc[1:n] + c(1:n)
    names(data.rep)[index] <- c(paste0("$", sym_type2))
    names(data.rep)[var.name] <- names(data)[location_var]
    names(data.rep)[ncol(data.rep)] <- names(data)[nc]
  }
  if (is.null(sym_type1) != TRUE && is.null(sym_type2) != TRUE){
    location_fun <- function(x){
      return(x %in% var)
    }
    location_var <- which(apply(matrix(colnames(data), nrow = 1), 1, location_fun))
    if(length(sym_type1) != length(location)){return("Error")}
    if(length(sym_type2) != length(location_var)){return("Error")}
    location_sort <- sort(c(location, location_var), index.return = TRUE)
    location_merge <- location_sort$x
    location_index <- location_sort$ix
    sym_type_merge <- c(sym_type1, sym_type2)
    sym_type <- sym_type_merge[location_index]
    n <- length(location_merge)
    lc <- c(location_merge, nc)
    gap <- NULL
    for (i in 1:n) {
      gap[i] <- lc[(i + 1)] - lc[i]
      gap.data <- data[, lc[i]:(lc[i] + gap[i] - 1)]
      rep.money <- rep(paste0("$", sym_type[i]), nr)
      data.rep <- cbind(data.rep, rep.money, gap.data)
    }
    if (location_merge[n] == nc){
      data.rep <- data.rep[, -c(1, length(data.rep))]
    } else {
      data.rep <- data.rep[, -1]
      data.rep <- cbind(data.rep, data[, nc])
    }
    if (length(location_merge) == 1){
      if (location_merge[1] != 1){
        data.rep <- cbind(data[, 1:location_merge[1] - 1], data.rep)
      }
    } else {
      if (location_merge[1] != 1){
        if (location_merge[1] == 2){
          data.rep <- cbind(data[, 1], data.rep)
          names(data.rep)[1] <- names(data)[1]
        } else {
          data.rep[, 1:location_merge[1] - 1] <- data[, 1:location_merge[1] - 1]
        }
      }
    }
    index <- lc[1:n] + c(1:n) - 1
    var.name <- lc[1:n] + c(1:n)
    names(data.rep)[index] <- c(paste0("$", sym_type))
    names(data.rep)[var.name] <- names(data)[location_merge]
    names(data.rep)[ncol(data.rep)] <- names(data)[nc]
  }
  return(data.rep)
}

