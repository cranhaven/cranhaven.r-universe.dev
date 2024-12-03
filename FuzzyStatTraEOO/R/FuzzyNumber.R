#' @title R6 Class representing a 'FuzzyNumber'.
#'
#' @description
#' A 'FuzzyNumber' is an array of dimension nl x 3 x 1. It must be valid.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author Andrea Garcia Cernuda <uo270115@uniovi.es>
#'
#' @import R6
#'
#' @export FuzzyNumber
FuzzyNumber <- R6::R6Class(
  classname = "FuzzyNumber",
  private = list(
    alphaLevels = NULL,

    infimums = NULL,

    supremums = NULL,

    # Checks whether the inner conditions are met.
    # fnLevels array of dimension nl x 3 x 1.
    # R must fulfill the next conditions:
    # 1) The number of columns should be 3.
    # 2) All the fuzzy numbers have to have the same column of \eqn{\alpha}-levels.
    # 3) The minimum \eqn{\alpha}-level should be 0 y the maximum 1.
    # 4) The \eqn{\alpha}-levels have to increase from 0 to 1.
    # 5) The infimum values have to be non-decreasing.
    # 6) The supremum values have to be non-creasing.
    # 7) The infimum value has to be smaller or equal than the supremum value for each \eqn{\alpha}-level.
    #
    # verbose specifies if the user wants to see in the console the messages that could be written
    #
    # return TRUE whether the inner conditions are met, otherwise FALSE.
    checkValidity = function(fnLevels = NA, verbose = TRUE)
    {
      valid <- TRUE

      nl <- dim(fnLevels)[1]
      ncol <- dim(fnLevels)[2]
      r   <-   dim(fnLevels)[3]

      if (!is.na(r) && r != 1)
      {
        if (verbose) {
          print(
            "the array passed as argument in the constructor has more than 1 dimension; more than 1 dimension means more than 1 fuzzy number"
          )
        }
        valid <- FALSE
      }

      else
        if (ncol != 3)
        {
          if (verbose) {
            print(
              "each fuzzy number should be characterized by means of a matrix with 3 columns: the first column will be the alpha-levels, the second one their infimum values and the third one their supremum values"
            )
          }
          valid <- FALSE
        }

      else
        if (fnLevels[1, 1] != 0 |
            fnLevels[nl, 1] != 1)
        {
          if (verbose) {
            print("the minimum alpha-level should be 0 and the maximum 1")
          }
          valid <- FALSE
        }

      else
        if (length(unique(fnLevels[, 1])) != nl |
            all(fnLevels[, 1] == sort(fnLevels[, 1])) == FALSE)
        {
          if (verbose) {
            print("the alpha-levels have to increase from 0 to 1")
          }
          valid <- FALSE
        }

      else
        if (all(abs(fnLevels[, 2] - apply(as.matrix(fnLevels[, 2]),
                                          2,
                                          sort)) <=
                10 ^ (-10))
            == FALSE)
        {
          if (verbose) {
            print("the infimum values have to be non-decreasing")
          }
          valid <- FALSE
        }

      else
        if (all(abs(fnLevels[, 3] - apply(
          as.matrix(fnLevels[, 3]), 2, sort, decreasing =
          TRUE
        )) <= 10 ^ (-10))
        == FALSE)
        {
          if (verbose) {
            print("the supremum values have to be non-increasing")
          }
          valid <- FALSE
        }

      else
        if (all(fnLevels[nl, 3] - fnLevels[nl, 2] >= 0) == FALSE)
        {
          if (verbose) {
            print(
              "the infimum value has to be smaller or equal than the supremum value for each alpha-level"
            )
          }
          valid <- FALSE
        }

      if (!valid) {
        stop("An invalid FuzzyNumber cannot be created")
      }

      return (valid)
    }
  ),
  public = list(
    #' @description
    #' This method creates a valid 'FuzzyNumber' object with all its attributes set.
    #'
    #' @param fnLevels is an array of dimension nl x 3 x 1 (general fuzzy number).
    #' nl is the number of considered \eqn{\alpha}-levels and 3 is the number of
    #' columns of the array. The first column represents the number of considered
    #' \eqn{\alpha}-levels, the second one represents their infimum values and the
    #' third and last column represents their supremum values.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumber object created with all its attributes set if it is
    #' valid.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3)))
    #'
    #' # Example 2:
    #' FuzzyNumber$new(array(c(0.0,1.0,1,2,4,3),dim=c(2,3)))
    initialize = function(fnLevels = NA) {
      stopifnot(is.array(fnLevels))
      stopifnot(is.double(fnLevels))
      if (anyNA(fnLevels) ||
          any(is.infinite(fnLevels)) ||
          any(is.nan(fnLevels))) {
        stop("The FuzzyNumber cannot contain Inf, -Inf, Na, NaN.")
      }

      if (private$checkValidity(fnLevels)) {
        l <- length(fnLevels[, 1])
        private$alphaLevels <- array(fnLevels[, 1], dim = c(l, 1))
        private$infimums <- array(fnLevels[, 2], dim = c(l, 1))
        private$supremums <- array(fnLevels[, 3], dim = c(l, 1))
      }

    },

    #' @description
    #' This method gives the 'alphaLevels' array of the 'FuzzyNumber'.
    #'
    #' @details See examples.
    #'
    #' @return The array alphaLevels of the FuzzyNumber object.
    #'
    #' @examples
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$getAlphaLevels()
    getAlphaLevels = function() {
      return(private$alphaLevels)
    },

    #' @description
    #' This method gives the 'imfimums' array of the 'FuzzyNumber'.
    #'
    #' @details See examples.
    #'
    #' @return The array imfimums of the FuzzyNumber object.
    #'
    #' @examples
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$getInfimums()
    getInfimums = function() {
      return(private$infimums)
    },

    #' @description
    #' This method gives the 'supremums' array of the 'FuzzyNumber'.
    #'
    #' @details See examples.
    #'
    #' @return The array supremums of the FuzzyNumber object.
    #'
    #' @examples
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$getSupremums()
    getSupremums = function() {
      return(private$supremums)
    },

    #' @description
    #' This method shows in a graph the values of the alphaLevels, infimums and
    #' supremums attributes of the corresponding 'FuzzyNumber'.
    #'
    #' @param color is the color of the lines representing the number to be shown
    #' in the graph. The default value is grey, other colors can be specified, the
    #' option palette() too.
    #'
    #' @details See examples.
    #'
    #' @return a graph with the values of the alphaLevels, infimums and supremums
    #' attributes of the corresponding 'FuzzyNumber'.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$plot()
    #'
    #' # Example 2:
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 2, 1.7),dim=c(2,3))
    #' )$plot("blue")
    #'
    #' # Example 3:
    #' Simulation$new()$simulCase1(1L)$transfTra()$getDimension(1L)$plot(palette())
    #'
    #' # Example 4:
    #' Simulation$new()$simulCase1(1L)$transfTra()$getDimension(1L)$plot(palette()[7])
    plot = function(color = "grey") {
      plot(
        c(),
        c(),
        lty = 1,
        lwd = 3,
        xlim = c(private$infimums[[1]] - 0.25, private$supremums[[1]] +
                   0.25),
        ylim = c(0, 1),
        xlab = "",
        ylab = "",
        main = "FuzzyNumber"
      )
      x1 <- c()
      x2 <- c()
      y <- private$alphaLevels
      n <- length(y)
      for (i in seq(1, n, by = 2)) {
        x1 <- append(x1, private$infimums[[i]])
        x2 <- append(x2, private$supremums[[i]])
        if (i < n) {
          x1 <- append(x1, private$infimums[[i + 1]])
          x2 <- append(x2, private$supremums[[i + 1]])
        }
      }
      points(c(x1, rev(x2)), c(y, rev(y)), type = "l", col = color)
    }
  )
)
