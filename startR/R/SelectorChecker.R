#'Translate a set of selectors into a set of numeric indices
#'
#'This is a selector checker function intended for use as parameter 
#''selector_checker' in a Start() call. It translates a set of selectors which
#'is the value for one dimension into a set of numeric indices corresponding to
#'the coordinate variable. The function complies with the input/output interface 
#'required by Start() defined in the documentation for the parameter 
#''selector_checker' of Start().
#'
#'@param selectors A vector or a list of two of numeric indices or variable 
#'  values to be retrieved for a dimension, automatically provided by Start(). 
#'  See details in the documentation of the parameters 'selector_checker' and 
#'  '\dots' of the function Start().  
#'@param var A vector of values of a coordinate variable for which to search 
#'  matches with the provided indices or values in the parameter 'selectors', 
#'  automatically provided by Start(). See details in the documentation of the 
#'  parameters 'selector_checker' and '\dots' of the function Start(). The 
#'  default value is NULL. When not specified, SelectorChecker() simply returns
#'  the input indices.
#'@param return_indices A logical value automatically configured by Start(), 
#'  telling whether to return the numeric indices or coordinate variable values 
#'  after the matching. The default value is TRUE.
#'@param tolerance A numeric value indicating a tolerance value to be used in 
#'  the matching of 'selectors' and 'var'. See documentation on 
#'  '<dim_name>_tolerance' in \code{\dots} in the documentation of the function
#'  Start(). The default value is NULL.
#'
#'@return A vector of either the indices of the matching values (if 
#'  return_indices = TRUE) or the matching values themselves (if return_indices
#'  = FALSE).
#'@examples
#'# Get the latitudes from 10 to 20 degree
#'sub_array_of_selectors <- list(10, 20)
#'# The latitude values from original file
#'sub_array_of_values <- seq(90, -90, length.out = 258)[2:257]
#'SelectorChecker(sub_array_of_selectors, sub_array_of_values)
#'
#'@importFrom methods is
#'@export
SelectorChecker <- function(selectors, var = NULL, return_indices = TRUE,
                            tolerance = NULL) {
  if (length(selectors) == 0) {
    stop("No selectors provided in 'selectors'.")
  }
  if (return_indices) {
    if (is.list(selectors)) {
      if (length(selectors) != 2) {
        stop("'selectors' provided in a wrong format.")
      }
      crescent_selectors <- TRUE
      if (all(sapply(selectors, 
                     function(x) {
                       any(c("numeric", "integer", "POSIXct", "POSIXlt", "POSIXt", "Date") %in% class(x))
                     }))) {
        if (selectors[[2]] < selectors[[1]]) {
          crescent_selectors <- FALSE
        }
      }
      for (i in 1:length(selectors)) {
        if (is.null(var)) {
          if (!is.numeric(selectors[[i]])) {
            stop("No selector values provided in 'var'.")
          } else {
            selectors[[i]] <- round(selectors[[i]])
          }
        } else if (is.na(selectors[[i]])) {
          if (i == 1) {
            if (crescent_selectors) {
              selectors[[i]] <- 1
            } else {
              selectors[[i]] <- length(var)
            }
          }
          else {
            if (crescent_selectors) {
              selectors[[i]] <- length(var)
            } else {
              selectors[[i]] <- 1
            }
          }
        } else if (is.character(selectors[[i]])) {
          if (is.character(var)) {
            candidate <- which(var == selectors[[i]])
            if (length(candidate) > 0) {
              selectors[[i]] <- candidate[1]
            } else {
              stop("Selector value not found in 'var'.")
            }
          } else {
            stop("Character selectors provided but possible values in 'var' are not character.")
          }
        } else if (is.numeric(selectors[[i]])) {
          if (is.numeric(var)) {
            
            tol <- 0
            if (!is.null(tolerance)) {
              if (!is(tolerance, "numeric")) {
                stop("Expected a numeric *_tolerance.")
              }
              tol <- tolerance
            }
            
            val <- selectors[[i]]
            
            if (i == 1) {
              if (crescent_selectors) {
                val <- val - tol
                if (var[1] < var[2]) {
                  selectors[[i]] <- which(var >= val)[1]
                } else if (var[1] > var[2]) {
                  selectors[[i]] <- rev(which(var >= val))[1]
                }
                
              } else {
                val <- val + tol
                if (var[1] < var[2]) {
                  selectors[[i]] <- rev(which(var <= val))[1]
                } else if (var[1] > var[2]) {
                  selectors[[i]] <- which(var <= val)[1]
                }
              }
            }
            else if (i == 2) {
              if (crescent_selectors) {
                val <- val + tol
                if (var[1] < var[2]) {
                  selectors[[i]] <- rev(which(var <= val))[1]
                } else if (var[1] > var[2]) {
                  selectors[[i]] <- which(var <= val)[1]
                }
                
              } else {
                val <- val - tol
                if (var[1] < var[2]) {
                  selectors[[i]] <- which(var >= val)[1]
                } else if (var[1] > var[2]) {
                  selectors[[i]] <- rev(which(var >= val))[1]
                }
              }
            }
            
            
          } else {
            stop("Numeric selectors provided but possible values in 'var' are not numeric.")
          }
        } else if (any(c("POSIXct", "POSIXlt", "POSIXt", "Date") %in% class(selectors[[i]]))) {
          # TODO: Here, change to as above (numeric part).
          if (any(c("POSIXct", "POSIXlt", "POSIXt", "Date") %in% class(var))) {
            val <- selectors[[i]]
            tol <- 0
            if (!is.null(tolerance)) {
              if (!is(tolerance, "difftime")) {
                stop("Expected a difftime *_tolerance.")
              }
              tol <- tolerance
            }
            if (i == 1) {
              if (crescent_selectors) {
                val <- val - tol
                selectors[[i]] <- which(var >= val)[1]
              } else {
                val <- val + tol
                selectors[[i]] <- rev(which(var <= val))[1]
              }
            }
            else {
              if (crescent_selectors) {
                val <- val + tol
                selectors[[i]] <- rev(which(var <= val))[1]
              } else {
                val <- val - tol
                selectors[[i]] <- which(var >= val)[1]
              }
            }
          } else {
            stop("Datetime selectors provided but possible values in 'var' are not datetime.")
          }
        }
      }
      
      # The checker is returning a list of two indices.
      ##selectors[[1]]:selectors[[2]]
      selectors
    } else if (is.numeric(selectors)) {
      if (is.null(var)) {
        ## TODO: Crash if negative indices?
        round(selectors)
      } else {
        if (is.numeric(var)) {
          if (!all(selectors %in% var)) {
            .warning(paste0("Numeric selectors have been ",
                            "provided for a dimension defined along a ",
                            "numeric variable, but no exact match ",
                            "found for all the selectors. Taking the index of the ",
                            "nearest values."))
          }
          if (!is.null(tolerance)) {
            if (!is(tolerance, 'numeric')) {
              stop("Expected a numeric *_tolerance.")
            }
          }
          sapply(selectors, function(x) {
            dif <- abs(var - x)
            res <- which.min(dif)[1]
            if (!is.null(tolerance)) {
              if (dif[res] > tolerance) {
                stop("Could not find a value in 'var' close ",
                     "enough to one of the 'selectors', ",
                     "according to 'tolerance'.")
              }
            }
            res
          })
        } else {
          stop("Numeric selectors provided but possible values in 'var' are not numeric.")
        }
      }
    } else if (any(c('POSIXct', 'POSIXlt', 'POSIXt', 'Date') %in% class(selectors))) {
      if (is.null(var)) {
        stop("Numeric selectors have been provided for a dimension ",
             "defined along a date variable, but no possible values ",
             "provided in 'var'.")
      }
      if (!all(selectors %in% var)) {
        .warning(paste0("Date selectors have been ",
                        "provided for a dimension defined along a ",
                        "date variable, but no exact match ",
                        "found for all the selectors. Taking the index of the ",
                        "nearest values."))
      }
      if (!is.null(tolerance)) {
        if (!is(tolerance, 'difftime')) {
          stop("Expected a difftime *_tolerance.")
        }
      }
      sapply(selectors, function(x) {
        dif <- abs(var - x)
        res <- which.min(dif)[1]
        if (!is.null(tolerance)) {
          if (dif[res] > tolerance) {
            res <- NA
            #stop("Could not find a value in 'var' close ",
            #     "enough to one of the 'selectors', ",
            #     "according to 'tolerance'.")
          }
        }
        res
      })
    } else {
      if (is.null(var)) {
        stop("No selector values provided in 'var'.")
      } else {
        if ((length(selectors) == 1) && 
            (selectors %in% c('all', 'first', 'last'))) {
          if (selectors == 'all') {
            1:length(var)
          } else if (selectors == 'first') {
            1
          } else {
            length(var)
          }
        } else {
          if (!identical(class(var), class(selectors))) {
            stop("Class of provided selectors does not match class of 'var'.")
          }
          candidates <- match(as.vector(selectors), as.vector(var))
          if (length(candidates) == 0 | any(is.na(candidates))) {
            stop("Selectors do not match values in 'var'.")
          } else if (length(candidates) != length(selectors)) {
            stop("Some selectors do not match values in 'var'.")
          }
          candidates
        }
      }
    }
  } else {
    if (!is.null(var)) {
      if (is.list(selectors)) {
        if (length(selectors) != 2) {
          stop("'selectors' provided in a wrong format.")
        } else {
          var[selectors[[1]]:selectors[[2]]]
        }
      } else if (is.numeric(selectors)) {
        if (length(selectors) > 0) {
          var[selectors]
        } else {
          stop("No selectors provided.")
        }
      } else {
        if ((length(selectors) == 1) && 
            (selectors %in% c('all', 'first', 'last'))) {
          if (selectors == 'all') {
            var
          } else if (selectors == 'first') {
            head(var, 1)
          } else {
            tail(var, 1)
          }
        } else {
          selectors
        }
      }
    } else {
      selectors
    }
  }
}
