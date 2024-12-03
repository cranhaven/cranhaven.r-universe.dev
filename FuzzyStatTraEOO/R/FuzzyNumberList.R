#' @title 'FuzzyNumberList' is a child class of 'StatList'.
#'
#' @description
#' 'FuzzyNumberList' must contain valid 'FuzzyNumbers'.
#' This class implements a version of the empty 'StatList' methods.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author(s) Andrea Garcia Cernuda <uo270115@uniovi.es>, Asun Lubiano <lubiano@uniovi.es>,
#' Sara de la Rosa de Saa
#'
#' @references
#' [1] Blanco-Fernandez, A.; Casals, R.M.; Colubi, A.; Corral, N.; Garcia-Barzana, M.; Gil, M.A.;
#' Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano, M.A.; Montenegro, M.; Ramos-Guajardo, A.B.;
#' de la Rosa de Saa, S.; Sinova, B.: Random fuzzy sets: A mathematical tool to develop
#' statistical fuzzy data analysis, Iranian Journal on Fuzzy Systems 10(2), 1-28 (2013)
#'
#' [2] Diamond, P.; Kloeden, P.: Metric spaces of fuzzy sets, Fuzzy Sets and Systems 35,
#' 241-249 (1990)
#'
#' [3] Sinova, B.; de la Rosa de Saa, S.; Gil, M.A.: A generalized L1-type metric between fuzzy
#' numbers for an approach to central tendency of fuzzy data, Information Sciences 242, 22-34
#' (2013)
#'
#' [4] Sinova, B.; Gil, M.A.; Van Aelst, S.: M-estimates of location for the robust central
#' tendency of fuzzy data, IEEE Transactions on Fuzzy Systems 24(4), 945-956 (2016)
#'
#' @import R6
#'
#' @export FuzzyNumberList
FuzzyNumberList <- R6::R6Class(
  classname = "FuzzyNumberList",
  inherit = StatList,
  private = list (
    # numbers is a collection of fuzzy numbers.
    numbers = NULL,

    # auxiliary method used in the dthetaphi public method
    # it calculates integrals by hand as sums
    # x is a vector (first column of fuzzy set)
    l2dist = function(x = NA) {
      k <- length(x) - 1
      delta <- 1 / k
      y <- x[1:k] + x[2:(k + 1)]
      values <- x[1:k] + x[2:(k + 1)] + 2 * y
      integral <- sum(values) * delta / 6
      invisible(integral)
    },

    # auxiliary method used in the rho1 public method
    # it calculates integrals by hand by Simpson's rule
    # x is a vector (first column of fuzzy set)
    rho1dist = function(x = NA) {
      k <- length(x) - 1
      delta <- 1 / k
      y <- x[1:k] + x[2:(k + 1)]
      values <- abs(x[1:k]) + abs(x[2:(k + 1)]) + 2 * abs(y)
      integral <- sum(values) * delta / 6
      invisible(integral)
    },

    # auxiliary method used in the dthetaphi, dwablphi and rho1 public methods
    # list is the second FuzzyNumberList that is going to be compared
    checkAlphaLevels = function(list = NA, verbose = TRUE) {
      aux <- private$numbers[[1]]$getAlphaLevels()
      result <- TRUE
      for (val in 1:list$getLength()) {
        if (!identical(aux,
                       list$getDimension(as.integer(val))$getAlphaLevels())) {
          if (verbose) {
            print("the fuzzy numbers of the two FuzzyNumberLists must have the same alpha-levels")
          }
          result <- FALSE
        }
        if (!result) {
          break
        }
      }
      return (result)
    },

    # This method checks that the numbers that contain this class are given in the correct format.
    # It checks that all the 'FuzzyNumbers' have the same column of \eqn{\alpha}-levels.
    # It also set all attributes.
    checking = function(numbers = NA) {
      aux <- numbers[[1]]$getAlphaLevels()
      if (length(numbers) > 1) {
        for (val in 2:length(numbers)) {
          if (!identical(aux,
                         numbers[[val]]$getAlphaLevels())) {
            stop("all fuzzy numbers must have the same alpha-levels")
          }
        }
      }

      private$rows <- length(aux)
      private$columns <- 3
      private$dimensions <- length(numbers)
      private$numbers <- numbers
    }
  ),
  public = list(
    #' @description
    #' This method creates a 'FuzzyNumberList' object with the columns and dimensions
    #' attributes set where the 'FuzzyNumbers' must be valid.
    #'
    #' @param numbers is a list of dimension nl x 3 x n which contains n
    #' fuzzy numbers. nl is the number of considered \eqn{\alpha}-levels and 3 is the number of
    #' columns of the list. The first column represents the number of considered
    #' \eqn{\alpha}-levels, the second one represents their infimum values and the
    #' third and last column represents their supremum values.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumberList object created with the columns and dimensions
    #' attributes set where the 'FuzzyNumbers' must be valid.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3)))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2.0, 1.5), dim = c(2, 3)))))
    initialize = function(numbers = NA) {
      stopifnot(is.list(numbers))
      for (val in 1:length(numbers)) {
        stopifnot(class(numbers[[val]])[1] == "FuzzyNumber")
      }

      private$checking(numbers)
    },

    #' @description
    #' This method calculates the mid/spr distance between the FuzzyNumbers contained
    #' in the current object and the one passed as parameter.
    #' See Blanco-Fernandez et al. (2013) [1].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of
    #' nl \eqn{\alpha}-levels each. The \eqn{\alpha}-levels of the FuzzyNumberList
    #' s should coincide with the ones of the current FuzzyNumberList (the method
    #' checks this condition).
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the mid/spr distances between the two previous
    #' mentioned FuzzyNumberLists. If the body's method inner conditions are not met,
    #' NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dthetaphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))),
    #' 1,5,1)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$dthetaphi(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))
    #' ), 1, 1, 1/3)
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(20L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$dthetaphi(S,1,5,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F$dthetaphi(S,2,1,1/3)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F=F$transfTra()
    #' S=S$transfTra(50L)
    #' F$dthetaphi(S,2,1,1)
    dthetaphi = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "FuzzyNumberList")
      super$dthetaphi(s, a, b, theta)

      if (private$checkAlphaLevels(s)) {
        r <- private$dimensions
        p <- s$getLength()
        alpha <- private$numbers[[1]]$getAlphaLevels()
        dthetaphicua <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          for (j in 1:p) {
            mid <- (((
              private$numbers[[i]]$getInfimums() +
                private$numbers[[i]]$getSupremums()
            ) -
              (
                s$getDimension(j)$getInfimums() +
                  s$getDimension(j)$getSupremums()
              )
            ) / 2) ^ 2 * dbeta(alpha, a, b)

            spr <- (((
              private$numbers[[i]]$getSupremums() -
                private$numbers[[i]]$getInfimums()
            ) -
              (
                s$getDimension(j)$getSupremums() -
                  s$getDimension(j)$getInfimums()
              )
            ) / 2) ^ 2 * dbeta(alpha, a, b)

            dthetaphicua[i, j] <-
              private$l2dist(mid) + theta * private$l2dist(spr)
          }
        }

        dthetaphi <- sqrt(dthetaphicua)
        return(dthetaphi)
      }

      return (NA)

    },

    #' @description
    #' This method calculates the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance between
    #' the 'FuzzyNumbers' contained in two 'FuzzyNumberLists'. The method checks
    #' if the \eqn{\alpha}-levels of all 'FuzzyNumbers' coincide.
    #' See Sinova et al. (2013) [3] and Sinova et al. (2016) [4].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of nl
    #' \eqn{\alpha}-levels each. The \eqn{\alpha}-levels should coincide with
    #' ones of the other FuzzyNumberList (the method checks this condition).
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' ldev and rdev in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distances
    #' between the two previous mentioned FuzzyNumberLists. If the body's
    #' method inner conditions are not met, NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dwablphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))),
    #' 1,5,1)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$dwablphi(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))
    #' ), 1, 1, 1/3)
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(3L)
    #' S=Simulation$new()$simulCase1(4L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$dwablphi(S,2,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F$dwablphi(S)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F=F$transfTra()
    #' S=S$transfTra(50L)
    #' F$dwablphi(S,2,1,1)
    dwablphi  = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "FuzzyNumberList")
      super$dwablphi(s, a, b, theta)

      if (private$checkAlphaLevels(s)) {
        r <- private$dimensions
        p <- s$getLength()
        alpha <- private$numbers[[1]]$getAlphaLevels()
        wablR <- vector()
        wablS <- vector()
        dwablphi <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          midR <-
            (private$numbers[[i]]$getInfimums() + private$numbers[[i]]$getSupremums()) /
            2
          integrandWablR <- midR * dbeta(alpha, a, b)
          wablR[i] <- private$l2dist(integrandWablR)
        }

        for (j in 1:p) {
          midS <-
            (s$getDimension(j)$getInfimums() + s$getDimension(j)$getSupremums()) /
            2
          integrandWablS <- midS * dbeta(alpha, a, b)
          wablS[j] <- private$l2dist(integrandWablS)
        }

        for (i in 1:r) {
          ldevR <- wablR[i] - private$numbers[[i]]$getInfimums()
          rdevR <-
            private$numbers[[i]]$getSupremums() - wablR[i]
          for (j in 1:p) {
            ldevS <- wablS[j] - s$getDimension(j)$getInfimums()
            rdevS <-
              s$getDimension(j)$getSupremums() - wablS[j]
            integrandldev <- abs(ldevR - ldevS) * dbeta(alpha, a, b)
            integrandrdev <- abs(rdevR - rdevS) * dbeta(alpha, a, b)
            dwablphi[i, j] <-
              abs(wablR[i] - wablS[j]) + (theta / 2) * private$l2dist(integrandldev) +
              (theta / 2) * private$l2dist(integrandrdev)
          }
        }

        return (dwablphi)
      }

      return (NA)

    },

    #' @description
    #' This method calculates the 1-norm distance between the 'FuzzyNumbers' contained
    #' in two 'FuzzyNumberLists'. The method checks if the \eqn{\alpha}-levels of
    #' all 'FuzzyNumbers' coincide.
    #' See Diamond and Kloeden. (1990) [2].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of nl
    #' \eqn{\alpha}-levels each. The method checks that the \eqn{\alpha}-levels
    #' should coincide with ones of the other FuzzyNumberList.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the 1-norm distances between the two previous
    #' mentioned FuzzyNumberLists. If the body's method inner conditions are not met,
    #' NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$rho1(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$rho1(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))))
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(4L)
    #' S=Simulation$new()$simulCase1(5L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$rho1(S)
    #' S$rho1(F)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(4L)
    #' S=Simulation$new()$simulCase1(5L)
    #' F=F$transfTra()
    #' S=S$transfTra(10L)
    #' F$rho1(S)
    #' S$rho1(F)
    rho1 = function(s = NA) {
      stopifnot(class(s)[2] == "StatList")
      stopifnot(class(s)[1] == "FuzzyNumberList")

      if (private$checkAlphaLevels(s)) {
        r <- private$dimensions
        p <- s$getLength()
        rho <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          for (j in 1:p) {
            inf <- private$numbers[[i]]$getInfimums() -
              s$getDimension(j)$getInfimums()

            sup <- private$numbers[[i]]$getSupremums() -
              s$getDimension(j)$getSupremums()
            rho[i, j] <-
              (private$rho1dist(inf) + private$rho1dist(sup)) * 0.5
          }
        }

        return(rho)
      }

      return (NA)

    },

    #' @description
    #' This method adds a 'FuzzyNumber' to the current collection of fuzzy numbers.
    #' Therefore, the dimensions' field is increased in a unit.
    #'
    #' @param n is the FuzzyNumber to be added to the current collection of fuzzy
    #' numbers.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return NULL.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3)))))$addFuzzyNumber(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3)))
    #' ))$addFuzzyNumber( FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75,
    #' 1.5), dim = c(3, 3))))
    addFuzzyNumber = function(n = NA, verbose = TRUE) {
      stopifnot(class(n)[1] == "FuzzyNumber")
      private$dimensions <- private$dimensions + 1
      private$numbers <- append(private$numbers, n)
      if (verbose) {
        cat("numbers updated, current dimension is", private$dimensions)
      }
    },

    #' @description
    #' This method removes a 'FuzzyNumber' to the current collection of fuzzy numbers.
    #' Therefore, the dimensions' field is decreased in a unit.
    #'
    #' @param i is the position of the FuzzyNumber to be removed in the current
    #' collection of fuzzy numbers.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return NULL.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1, -0.5, 1.5, 1.25), dim = c(2, 3)))
    #' ))$removeFuzzyNumber(1L)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$removeFuzzyNumber(2L)
    removeFuzzyNumber = function(i = NA, verbose = TRUE) {
      stopifnot(typeof(i) == "integer" &&
                  i > 0 && i <= private$dimensions)
      private$dimensions <- private$dimensions - 1
      private$numbers <- private$numbers[-i]
      if (verbose) {
        cat("numbers updated, current dimension is", private$dimensions)
      }
    },

    #' @description
    #' This method gives the number contained in the dimension passed as parameter
    #' when the dimension is greater than 0 and not greater than the dimensions
    #' of the 'FuzzyNumberList's' numbers array.
    #'
    #' @param i is the dimension of the FuzzyNumber wanted to be retrieved.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumber contained in the dimension passed as parameter or
    #' an error if the dimension is not valid.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$getDimension(1L)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$getDimension(2L)
    getDimension = function(i = NA) {
      stopifnot(typeof(i) == "integer" &&
                  i > 0 && i <= private$dimensions)
      return(private$numbers[[i]])
    },

    #' @description
    #' This method shows in a graph the values of the attribute numbers of the
    #' corresponding 'FuzzyNumberList'.
    #'
    #' @param color is the color of the lines representing the numbers to be shown
    #' in the graph. The default value is grey, other colors can be specified, the
    #' option palette() too.
    #'
    #' @details See examples.
    #'
    #' @return a graph with the values of the attribute numbers of the corresponding
    #' 'FuzzyNumberList'.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$plot()
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.85, 1.7), dim = c(2, 3))))
    #' )$plot("blue")
    #'
    #' # Example 3:
    #' Simulation$new()$simulCase1(8L)$transfTra()$plot(palette())
    #'
    #' # Example 4:
    #' Simulation$new()$simulCase1(5L)$transfTra()$plot(palette()[2:6])
    plot = function(color = "grey") {
      dims <- private$dimensions
      p <- (length(color) > 1 && dims > 1)
      minimo <- c()
      maximo <- c()
      for (i in 1:dims) {
        number <- private$numbers[[i]]
        minimo <- append(minimo, number$getInfimums()[[1]])
        maximo <- append(maximo, number$getSupremums()[[1]])
      }
      plot(
        c(),
        c(),
        type = "l",
        col = color,
        lty = 1,
        lwd = 3,
        xlim = c(min(minimo) - 0.25, max(maximo) + 0.25),
        ylim = c(0, 1),
        xlab = "",
        ylab = "",
        main = "FuzzyNumberList"
      )

      if (p) {
        for (i in 1:dims) {
          number <- private$numbers[[i]]
          y <- number$getAlphaLevels()
          n <- length(y)
          x1 <- c()
          x2 <- c()
          for (j in seq(1, n, by = 2)) {
            x1 <- append(x1, number$getInfimums()[[j]])
            x2 <- append(x2, number$getSupremums()[[j]])
            if (j < n) {
              x1 <- append(x1, number$getInfimums()[[j + 1]])
              x2 <- append(x2, number$getSupremums()[[j + 1]])
            }
          }
          points(c(x1, rev(x2)),
                 c(y, rev(y)),
                 type = "l",
                 col = color[runif(1, min =
                                     1, max = length(color))])

        }
      } else {
        for (i in 1:dims) {
          number <- private$numbers[[i]]
          y <- number$getAlphaLevels()
          n <- length(y)
          x1 <- c()
          x2 <- c()
          for (j in seq(1, n, by = 2)) {
            x1 <- append(x1, number$getInfimums()[[j]])
            x2 <- append(x2, number$getSupremums()[[j]])
            if (j < n) {
              x1 <- append(x1, number$getInfimums()[[j + 1]])
              x2 <- append(x2, number$getSupremums()[[j + 1]])
            }
          }
          points(c(x1, rev(x2)), c(y, rev(y)), type = "l", col = color)
        }
      }
    },

    #' @description
    #' This method returns the number of dimensions that are equivalent to the number
    #' of 'FuzzyNumbers' in the corresponding 'FuzzyNumberList'.
    #'
    #' @details See examples.
    #'
    #' @return the number of dimensions that are equivalent to the number of 'FuzzyNumbers'
    #' in the corresponding 'FuzzyNumberList'.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$getLength()
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))
    #' )$getLength()
    getLength = function() {
      return(private$dimensions)
    }
  )
)
