#'Print method for s2dv_cube objects
#' 
#'This is an S3 method of the generic 'print' for the class 's2dv_cube'. When 
#'you will call 'print' on an 's2dv_cube' object, this method will display the 
#'content of the object in a clear and informative way.
#' 
#'The object will be displayed following 's2dv_cube' class conventions. The 
#'top-level elements are: 'Data', a multidimensional array containing the 
#'object's data; 'Dimensions', the dimensions of the array; 'Coordinates', the 
#'array coordinates that match its dimensions, explicit coordinates have an 
#'asterisk (*) at the beginning while index coordinates do not; and 
#''Attributes', which contains all the metadata of the object. For more 
#'information about the 's2dv_cube', see \code{s2dv_cube()} and 
#'\code{as.s2dv_cube()} functions.
#' 
#'@param x An 's2dv_cube' object.
#'@param ... Additional arguments of print function.
#' 
#'@export
print.s2dv_cube <- function(x, ...) {
  if (is.atomic(x)) {
    cat(x, "\n")
  } else {
    cat("'s2dv_cube'\n")
    cat("Data         ", "[" , paste0(x$data[1:8], collapse = ", "), '...', "]", "\n")
    cat("Dimensions   ", "(", paste(names(x$dims), x$dims, sep = " = ", collapse = ', '), ")", "\n")
    cat("Coordinates  \n")
    for (coord in names(x$coords)) {
      if (!is.null(attr(x$coords[[coord]], 'indices'))) {
        if (attr(x$coords[[coord]], 'indices')) {
          cat("  ", coord, ":", paste(x$coords[[coord]], collapse = ", "), "\n")
        } else {
          cat(" *", coord, ":", paste(x$coords[[coord]], collapse = ", "), "\n")
        }
      } else {
        cat(" *", coord, ":", paste(x$coords[[coord]], collapse = ", "), "\n")
      }
    }
    cat("Attributes   \n")
    for (attr_name in names(x$attrs)) {
      if (attr_name == "Variable") {
        cat("  ", "varName  :", x$attrs$Variable$varName, "\n")
        cat("  ", "metadata : ", "\n")
        for (metadata_i in names(x$attrs$Variable$metadata)) {
          cat("  ", "  ", metadata_i, "\n")
          .print_metadata(x$attrs$Variable$metadata[[metadata_i]])
        }
      } else {
        cat("  ", attr_name, " : ")
        .print_beginning(x = x$attrs[[attr_name]], name = attr_name)
      }
    }
  }
 
}

## Auxiliary function for the print method
.print_beginning <- function(x, name, n = 5, j = 1) {
  if (inherits(x, 'numeric') | inherits(x, 'POSIXct') | inherits(x, 'Date')) {
    if (length(x) <= n) {
      cat(as.character(x), "\n")
    } else {
      cat(paste0(as.character(x[seq_len(n)])), "...", "\n")
    }
  } else if (name == "time_bounds") {
    cat("\n")
    for (param in names(x)) {
      cat("      ", "(", param,")", " : ")
      if (length(x[[param]]) <= n) {
        cat(as.character(x[[param]]), "\n")
      } else {
        cat(paste0(as.character(x[[param]][seq_len(n)])), "...", "\n")
      }
    }
  } else if (inherits(x, 'list')) {
    cat("\n")
    k = 1
    for (param in names(x)) {
      k = k + 1
      param_i <- x[[param]]
      if (!is.null(param_i)) {
        param_i <- lapply(param_i, function(x) {if (length(x[[1]]) > 1) {
          x[[1]] <- paste0(x[[1]][1],' ...')
        } else {
          x
        }})
        cat("      ", "(", param,")", " : ")
        cat(paste0(names(unlist(param_i)), " = ", unlist(param_i), collapse = ', '), "\n")
      } else {
        j = j + 1
      }
      if (k > j) {
        cat("      ", "...", "\n")
        break
      }
    }
  } else {
    if (length(x) > 1) {
      cat(x[[1]], "...", "\n")
    } else {
      cat(x[[1]], "\n")
    }
  }
}

## Auxiliary function for the print method
.print_metadata <- function(x) {
  if (inherits(x, 'list')) {
    info_names <- NULL
    for (info_i in names(x)) {
      if (info_i == 'units') {
        cat("  ", "  ", "  units :", x[[info_i]], "\n")
      } else if (info_i %in% c('longname', 'long_name')) {
        cat("  ", "  ", "  long name :", x[[info_i]], "\n")
      } else {
        info_names <- c(info_names, info_i)
      }
    }
    cat("  ", "  ", "  other :", paste0(info_names, collapse = ', '), "\n")
  } else if (!is.null(attributes(x))) {
    if ('variables' %in% names(attributes(x))) {
      info_names <- NULL
      attrs <- attributes(x)[['variables']]
      for (attrs_i in names(attrs)) {
        for (info_i in names(attrs[[attrs_i]])) {
          if (!inherits(attrs[[attrs_i]][[info_i]], 'list')) {
            if (info_i == 'units') {
              cat("  ", "  ", "  units :", attrs[[attrs_i]][[info_i]], "\n")
            } else if (info_i %in% c('longname', 'long_name')) {
              cat("  ", "  ", "  long name :", attrs[[attrs_i]][[info_i]], "\n")
            } else {
              info_names <- c(info_names, info_i)
            }
          }
        }
      }
      cat("  ", "  ", "  other :", paste0(info_names, collapse = ', '), "\n")
    } else {
      attrs <- attributes(x)
      info_names <- NULL
      for (info_i in names(attrs)) {
        if (info_i == 'cdo_grid_name') {
          cat("  ", "  ", "  cdo_grid_name :", attrs[[info_i]], "\n")
        } else {
          info_names <- c(info_names, info_i)
        }
      }
      cat("  ", "  ", "  other :", paste0(info_names, collapse = ', '), "\n")
    }
  }
}