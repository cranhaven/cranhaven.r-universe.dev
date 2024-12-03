#' @title 'TrapezoidalFuzzyNumberList' is a child class of 'StatList'.
#'
#' @description
#' 'TrapezoidalFuzzyNumberList' must contain valid 'TrapezoidalFuzzyNumbers'.
#' This class implements a version of the empty 'StatList' methods.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author(s) Andrea Garcia Cernuda <uo270115@uniovi.es>, Asun Lubiano <lubiano@uniovi.es>,
#' Sara de la Rosa de Saa
#'
#' @references
#' [1] De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A.: Fuzzy
#' rating scale-based questionnaires and their statistical analysis, IEEE Transactions on
#' Fuzzy Systems 23(1), 111-126 (2015)
#'
#' [2] De la Rosa de Saa, S.; Lubiano M.A.; Sinova, B.; Filzmoser, P.: Robust scale
#' estimators for fuzzy data, Advances in Data Analysis and Classification 11(4), 731-758 (2017)
#'
#' [3] De la Rosa de Sáa, S.; Lubiano, M.A.; Sinova, B.; Filzmoser, P.; Gil, M.Á.: Location-free
#' robust scale estimates for fuzzy data, IEEE Transactions on Fuzzy Systems 29(6), 1682-1694 (2021)
#'
#' [4] Lubiano, M.A.; Gil, M.A.: f-Inequality indices for fuzzy random variables, in Statistical
#' Modeling, Analysis and Management of Fuzzy Data (Bertoluzza, C., Gil, M.A., Ralescu, D.A., Eds.),
#' Physica-Verlag, 43-63 (2002)
#'
#' [5] Lubiano, M.A.; Montenegro, M.; Sinova, B.; De la Rosa de Saa, S.; Gil, M.A.: Hypothesis
#' testing for means in connection with fuzzy rating scale-based data: algorithms and
#' applications, European Journal of Operational Research 251, 918-929 (2016)
#'
#' [6] Sinova, B.; De la Rosa de Saa, S.; Gil, M.A.: A generalized L1-type metric between fuzzy
#' numbers for an approach to central tendency of fuzzy data, Information Sciences 242,
#' 22-34 (2013)
#'
#' [7] Sinova, B.; De la Rosa de Saa, S.; Lubiano, M.A.; Gil, M.A.: An overview on the statistical
#' central tendency for fuzzy datasets, International Journal of Uncertainty, Fuzziness and
#' Knowledge-Based Systems 23 (Suppl. 1), 105-132 (2015)
#'
#' [8] Sinova, B.; Gil, M.A.; Colubi, A.; Van Aelst, S.: The median of a random fuzzy number. The
#' 1-norm distance approach, Fuzzy Sets and Systems 200, 99-115 (2012)
#'
#' [9] Sinova, B.; Gil, M.A.; Lopez, M.T.; Van Aelst, S.: A parameterized L2 metric between fuzzy
#' numbers and its parameter interpretation, Fuzzy Sets and Systems 245, 101-115 (2014)
#'
#' [10] Sinova, B.; Gil, M.A.; Van Aelst, S.: M-estimates of location for the robust central
#' tendency of fuzzy data, IEEE Transactions on Fuzzy Systems 24(4), 945-956 (2016)
#'
#' @import R6
#'
#' @export TrapezoidalFuzzyNumberList
TrapezoidalFuzzyNumberList <- R6::R6Class(
  classname = "TrapezoidalFuzzyNumberList",
  inherit = StatList,
  private = list (
    # numbers is a collection of trapezoidal fuzzy numbers.
    numbers = NULL,

    # auxiliary method used in the dthetaphi public method
    # x is a vector, a and b are the parameters of the dthetaphi public method
    integrand = function(x = NA,
                         a = NA,
                         b = NA) {
      return (x * dbeta(x, a, b))
    },

    # auxiliary method used in the dthetaphi public method
    # x is a vector, a and b are the parameters of the dthetaphi public method
    integrandsq = function(x = NA,
                           a = NA,
                           b = NA) {
      return (x ^ 2 * dbeta(x, a, b))
    },

    # auxiliary method used in the dwablphi public method
    # x is a vector,i is an integer and list is a TrapezoidalFuzzyNumberList
    inf = function(x = NA,
                   i = NA,
                   list = NA) {
      return (
        list$getDimension(i)$getInf0() + x * (
          list$getDimension(i)$getInf1() - list$getDimension(i)$getInf0()
        )
      )
    },

    # auxiliary method used in the dwablphi public method
    # x is a vector,i is an integer and list is a TrapezoidalFuzzyNumberList
    sup = function(x = NA,
                   i = NA,
                   list = NA) {
      return (
        list$getDimension(i)$getSup0() + x * (
          list$getDimension(i)$getSup1() - list$getDimension(i)$getSup0()
        )
      )
    },

    # auxiliary method used in the dwablphi public method
    # x is a vector,i is an integer and list is a TrapezoidalFuzzyNumberList
    mid = function(x = NA,
                   i = NA,
                   list = NA) {
      return ((private$inf(x, i, list) + private$sup(x, i, list)) / 2)
    },

    # auxiliary method used in the dwablphi public function
    # x is a vector, a and b are the parameters of the dwablphi public function
    # i is an integer and list is a TrapezoidalFuzzyNumberList
    integrandWabl = function(x = NA,
                             i = NA,
                             list = NA,
                             a = NA,
                             b = NA) {
      return (private$mid(x, i, list) * dbeta(x, a, b))
    },

    # auxiliary method used in the dwablphi public function
    # a and b are the parameters of the dwablphi public function
    # i is an integer and list is a TrapezoidalFuzzyNumberList
    wabl = function(i = NA,
                    list = NA,
                    a = NA,
                    b = NA) {
      return (integrate(
        private$integrandWabl,
        0,
        1,
        i = i,
        list = list,
        a =
          a,
        b = b
      )$val)
    },

    # auxiliary method used in the dwablphi public function
    # x is a vector, i is an integer, a and b are the parameters of the dwablphi public function
    # list is a TrapezoidalFuzzyNumberList
    ldev = function(x = NA,
                    i = NA,
                    list = NA,
                    a = NA,
                    b = NA) {
      return (private$wabl(i, list, a, b) - private$inf(x, i, list))
    },

    # auxiliary method used in the dwablphi public function
    # x is a vector, i is an integer, a and b are the parameters of the dwablphi public function
    # list is a TrapezoidalFuzzyNumberList
    rdev = function(x = NA,
                    i = NA,
                    list = NA,
                    a = NA,
                    b = NA) {
      return (private$sup(x, i, list) - private$wabl(i, list, a, b))
    },

    # auxiliary method used in the dwablphi public function
    # x is a vector, a and b are the parameters of the dwablphi public function
    # i and j are integers, list1 and list2 are TrapezoidalFuzzyNumberLists
    integrand1 = function(x = NA,
                          i = NA,
                          j = NA,
                          list1 = NA,
                          list2 = NA,
                          a = NA,
                          b = NA) {
      return (abs(
        private$ldev(x, i, list = list1, a, b) - private$ldev(x, i = j, list =
                                                                list2, a, b)
      ) * dbeta(x, a, b))
    },

    # auxiliary method used in the dwablphi public function
    # x is a vector, a and b are the parameters of the dwablphi public function
    # i and j are integers, list1 and list2 are TrapezoidalFuzzyNumberLists
    integrand2 = function(x = NA,
                          i = NA,
                          j = NA,
                          list1 = NA,
                          list2 = NA,
                          a = NA,
                          b = NA) {
      return (abs(
        private$rdev(x, i, list = list1, a, b) - private$rdev(x, i = j, list =
                                                                list2, a, b)
      ) * dbeta(x, a, b))
    },

    # auxiliary method used in the mEstimator public function
    # x is a vector and f is the name of the loss function
    weight = function(x = NA,
                      f = NA) {
      rho <- 0
      rhod <- 0
      w <- 0

      if (f == "Huber") {
        # Huber M-estimator
        rho <- min(x ^ 2, 1)
        rhod <- 2
      } else if (f == "Tukey") {
        # Tukey M-estimator
        rho <- min(3 * x ^ 2 - 3 * x ^ 4 + x ^ 6, 1)
        rhod <- 6
      } else if (f == "Cauchy") {
        # Cauchy M-estimator
        rho <- x ^ 2 / (x ^ 2 + 1)
        rhod <- 2
      }

      if (x == 0) {
        w <- rhod
      } else {
        w <- rho / x ^ 2
      }

      return(w)
    },

    # auxiliary method used in the gsi, hyperI, mean, medianWabl, transfTra and
    # wablphi public functions
    extractMatrix = function() {
      F <- matrix(ncol = 4)
      for (i in 1:private$dimensions) {
        F <-
          rbind(
            F,
            c(
              private$numbers[[i]]$getInf0(),
              private$numbers[[i]]$getInf1(),
              private$numbers[[i]]$getSup1(),
              private$numbers[[i]]$getSup0()
            )
          )
      }
      return (F)
    }
  ),
  public = list(
    #' @description
    #' This method creates a 'TrapezoidalFuzzyNumberList' object with all the attributes
    #' set if the 'TrapezoidalFuzzyNumbers' are valid.
    #'
    #' @param numbers is a list which contains n TrapezoidalFuzzyNumbers.
    #'
    #' @details See examples.
    #'
    #' @return The TrapezoidalFuzzyNumberList object created with all attributes
    #' set if the 'TrapezoidalFuzzyNumbers' are valid.
    #'
    #' @examples
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
    #' TrapezoidalFuzzyNumber$new(-8, -6, -4, -2),
    #' TrapezoidalFuzzyNumber$new(-1, -1, 2, 3),
    #' TrapezoidalFuzzyNumber$new(1, 2, 3, 3)))
    initialize = function(numbers = NA) {
      stopifnot(is.list(numbers))
      for (val in 1:length(numbers)) {
        stopifnot(class(numbers[[val]])[1] == "TrapezoidalFuzzyNumber")
      }

      private$rows <- 1
      private$columns <- 4
      private$dimensions <- length(numbers)
      private$numbers <- numbers
    },

    #' @description
    #' This method calculates the scale measure Average Distance Deviation (ADD)
    #' of a 'TrapezoidalFuzzyNumberList' with respect to a 'TrapezoidalFuzzyNumberList'
    #' or with respect to a 'FuzzyNumberList' containing a unique valid fuzzy number.
    #' The employed metric in the calculation can be the 1-norm distance, the mid/spr
    #' distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' See De la Rosa de Saa et al. (2017) [2].
    #'
    #' @param s is a TrapezoidalFuzzyNumberList containing a unique valid TrapezoidalFuzzyNumber
    #' or it is a FuzzyNumberList containing a unique valid FuzzyNumber.
    #' @param type positive integer 1, 2 or 3: if type==1, the 1-norm distance will
    #' be considered in the calculation of the measure ADD. If type==2, the mid/spr
    #' distance will be considered. By contrast, if type==3, the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev
    #' distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the scale measure ADD, which is a real number. If the body's method
    #' inner conditions are not met, NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$add(
    #' FuzzyNumberList$new(c(FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0,
    #' 3.0, 2.0, 1.0), dim = c(3, 3))))),1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$add(
    #' FuzzyNumberList$new(c(FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0,
    #' 3.0, 2.0, 1.0), dim = c(3, 3))))),2L,2,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$add(
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(5,6,7,8))),3L,1,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=F$mean()
    #' F$add(S,1L)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase1(100L)
    #' S=F$median1Norm()
    #' F$add(S,2L,2,1,1)
    #'
    #' # Example 6:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=Simulation$new()$simulCase2(1L)
    #' F$add(U,2L)
    #'
    #' # Example 7:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=F$transfTra()
    #' F$add(U,2L)
    #'
    #' # Example 8:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=Simulation$new()$simulCase2(2L)
    #' F$add(U,2L)
    add = function(s = NA,
                   type = NA,
                   a = 1,
                   b = 1,
                   theta = 1) {
      stopifnot(class(s)[2] == "StatList")
      stopifnot(typeof(type) == "integer" &&
                  type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      if (s$getLength() == 1) {
        u <- NULL
        if (class(s)[1] == "FuzzyNumberList") {
          u <-
            self$transfTra(as.integer(nrow(
              s$getDimension(1L)$getAlphaLevels()
            )))
        } else{
          # class(s)[1] == "TrapezoidalFuzzyNumberList"
          u <- self
        }
        add <- 0

        if (type == 1) {
          add <- mean(s$rho1(u))
        } else if (type == 2) {
          add <- mean(s$dthetaphi(u, a, b, theta))
        } else{
          # type == 3
          add <- mean(s$dwablphi(u, a, b, theta))
        }

        return (add)
      }

      return (NA)
    },

    #' @description
    #' This method calculates the mid/spr distance between the 'TrapezoidalFuzzyNumbers'
    #' contained in the current object and the one passed as parameter.
    #' See Lubiano et al. (2016) [5].
    #'
    #' @param s TrapezoidalFuzzyNumberList containing valid TrapezoidalFuzzyNumbers
    #' characterized by their four values inf0, inf1, sup1, sup0.
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
    #' mentioned TrapezoidalFuzzyNumberLists.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$dthetaphi(
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58))))
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$dthetaphi(
    #' TrapezoidalFuzzyNumberList$new( c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58))),1,1,1)
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(6L)
    #' S=Simulation$new()$simulCase1(8L)
    #' F$dthetaphi(S,1,5,1)
    dthetaphi = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "TrapezoidalFuzzyNumberList")
      super$dthetaphi(s, a, b, theta)

      r <- private$dimensions
      p <- s$getLength()
      c <- matrix(nrow = r, ncol = p)
      d <- matrix(nrow = r, ncol = p)
      e <- matrix(nrow = r, ncol = p)
      f <- matrix(nrow = r, ncol = p)
      dthetaphicua <- matrix(nrow = r, ncol = p)

      for (i in 1:r) {
        for (j in 1:p) {
          c[i, j] <-
            ((
              private$numbers[[i]]$getInf0() + private$numbers[[i]]$getSup0()
            ) -
              (
                s$getDimension(j)$getInf0() + s$getDimension(j)$getSup0()
              )
            ) / 2

          d[i, j] <-
            ((
              private$numbers[[i]]$getSup0() - private$numbers[[i]]$getInf0()
            ) -
              (
                s$getDimension(j)$getSup0() - s$getDimension(j)$getInf0()
              )
            ) / 2

          e[i, j] <-
            ((
              private$numbers[[i]]$getInf1() + private$numbers[[i]]$getSup1()
            ) -
              (
                s$getDimension(j)$getInf1() + s$getDimension(j)$getSup1()
              )
            ) / 2 - c[i, j]

          f[i, j] <-
            ((
              private$numbers[[i]]$getSup1() - private$numbers[[i]]$getInf1()
            ) -
              (
                s$getDimension(j)$getSup1() - s$getDimension(j)$getInf1()
              )
            ) / 2 - d[i, j]

          dthetaphicua[i, j] <-
            c[i, j] ^ 2 + theta * d[i, j] ^ 2 +
            (e[i, j] ^ 2 + theta * f[i, j] ^ 2) * integrate(private$integrandsq, 0, 1, a =
                                                              a, b = b)$val +
            2 * (c[i, j] * e[i, j] + theta * d[i, j] * f[i, j]) * integrate(private$integrand, 0, 1, a =
                                                                              a, b = b)$val

        }
      }

      dthetaphi <- sqrt(dthetaphicua)
      return(dthetaphi)
    },

    #' @description
    #' This method calculates the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance
    #' between the 'TrapezoidalFuzzyNumbers' contained in two 'TrapezoidalFuzzyNumberLists'.
    #' See Sinova et al. (2013) [6] and Sinova et al. (2016) [10].
    #'
    #' @param s TrapezoidalFuzzyNumberList containing valid TrapezoidalFuzzyNumbers
    #' characterized by their four values inf0, inf1, sup1, sup0.
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
    #' between the two previous mentioned TrapezoidalFuzzyNumberLists.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$dwablphi(
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58))))
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$dwablphi(
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58))),5,1,1)
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(20L)
    #' F$dwablphi(S)
    dwablphi  = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "TrapezoidalFuzzyNumberList")
      super$dwablphi(s, a, b, theta)

      r <- private$dimensions
      p <- s$getLength()
      wablR <- vector()
      wablS <- vector()
      dwablphi <- matrix(nrow = r, ncol = p)

      for (i in 1:r) {
        for (j in 1:p) {
          int1 <-
            integrate(
              private$integrand1,
              0,
              1,
              i = i,
              j = j,
              list1 = self,
              list2 = s,
              a = a,
              b = b
            )$val
          int2 <-
            integrate(
              private$integrand2,
              0,
              1,
              i = i,
              j = j,
              list1 = self,
              list2 = s,
              a = a,
              b = b
            )$val

          dwablphi[i, j] <-
            abs(private$wabl(i, list = self, a, b) - private$wabl(i =
                                                                    j, list = s, a, b)) + (theta /
                                                                                             2) * int1 + (theta / 2) * int2
        }
      }

      return (dwablphi)
    },

    #' @description
    #' This method calculates the Gini-Simpson diversity index for a sample of
    #' 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'.
    #' See De la Rosa de Saa et al. (2015) [1].
    #'
    #' @details See examples.
    #'
    #' @return the Gini-Simpson diversity index, which is a real number.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$gsi()
    #'
    #' # Example 2:
    #' F=Simulation$new()$simulCase1(50L)
    #' F$gsi()
    gsi = function() {
      F <- matrix(private$extractMatrix()[-1, ], ncol = 4)
      n <- nrow(F)
      U <-
        unique(F) # deletes the equal rows (equal fuzzy numbers)
      u <-
        nrow(U) # number of different fuzzy numbers in the matrix F
      fr = vector(length = u) # relative frequency of each fuzzy number of the matrix U
      for (i in 1:u) {
        cont <- 0
        for (j in 1:n) {
          if (sum(U[i,] == F[j,]) == ncol(F))
            cont <- cont + 1
        }
        fr[i] <- cont / n
      }

      gini <- 1 - sum(fr ^ 2)
      return(gini)
    },

    #' @description
    #' This method calculates the hyperbolic inequality index for a sample of
    #' 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'.
    #' The method checks if all 'TrapezoidalFuzzyNumbers' are positive.
    #' See De la Rosa de Saa et al. (2015) [1] and Lubiano and Gil (2002) [4].
    #'
    #' @param c number in [0,0.5]. The c*100% trimmed mean will be used in the calculation
    #' of the hyperbolic inequality index.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return the hyperbolic inequality index, which is a real number. If the body's
    #' method inner conditions are not met, NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$hyperI()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$hyperI(0.5)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,4,6,8)))$hyperI()
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulFRSTra(100L,0.05,0.35,0.6,2,1)
    #' F$hyperI()
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase2(10L)
    #' F$hyperI(0.5)
    hyperI = function(c = 0, verbose = TRUE) {
      stopifnot(is.double(c) && c >= 0 && c <= 0.5)

      for (i in 1:private$dimensions) {
        if (!private$numbers[[i]]$is_positive()) {
          if (verbose) {
            print("all the fuzzy numbers should be positive")
          }
          return (NA)
        }
      }

      F <- private$extractMatrix()
      n <- nrow(F)
      l <- vector()
      fmean <-
        apply(F[2:n,], 2, mean, trim = c) # mean of the trapezoidal fuzzy numbers
      a1 = fmean[3] - fmean[4]
      b1 = fmean[4]
      a2 = fmean[2] - fmean[1]
      b2 = fmean[1]
      for (i in 2:n) {
        c1 = F[i, 2] - F[i, 1]
        d1 = F[i, 1]
        c2 = F[i, 3] - F[i, 4]
        d2 = F[i, 4]

        if ((abs(c1) <= .Machine$double.eps) &
            (abs(c2) <= .Machine$double.eps))
          l[i] = (1 / d1) * (a1 / 2 + b1) + (1 / d2) * (a2 / 2 + b2)
        else if ((abs(c1) <= .Machine$double.eps) &
                 (abs(c2) > .Machine$double.eps))
          l[i] = (1 / d1) * (a1 / 2 + b1) + a2 / c2 + (b2 - ((a2 * d2) /
                                                               c2)) * (1 / c2) * (log(abs(c2 + d2)) - log(abs(d2)))
        else if ((abs(c1) > .Machine$double.eps) &
                 (abs(c2) <= .Machine$double.eps))
          l[i] = a1 / c1 + (b1 - ((a1 * d1) / c1)) * (1 / c1) * (log(abs(c1 +
                                                                           d1)) - log(abs(d1))) + (1 / d2) * (a2 / 2 + b2)
        else if ((abs(c1) > .Machine$double.eps) &
                 (abs(c2) > .Machine$double.eps))
          l[i] = a1 / c1 + (b1 - ((a1 * d1) / c1)) * (1 / c1) * (log(abs(c1 +
                                                                           d1)) - log(abs(d1))) + a2 / c2 + (b2 - ((a2 * d2) / c2)) * (1 / c2) * (log(abs(c2 +
                                                                                                                                                            d2)) - log(abs(d2)))
      }

      hyper = (1 / 2) * mean(l[2:length(l)], trim = c) - 1
      return(hyper)
    },

    #' @description
    #' This method calculates the M-estimator of scale with loss method given
    #' in a 'TrapezoidalFuzzyNumberList' containing 'TrapezoidalFuzzyNumbers'.
    #' For computing the M-estimator, a method called “iterative reweighting” is
    #' used. The employed metric in the M-equation can be the 1-norm distance, the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @param f is the name of the loss function. It can be "Huber", "Tukey" or "Cauchy".
    #' @param estInitial real number > 0.
    #' @param delta real number in (0,1). It is present in the f-equation.
    #' @param epsilon real number > 0. It is the tolerance allowed in the algorithm.
    #' @param type positive integer 1, 2 or 3: if type==1, the 1-norm distance will
    #' be considered in the calculation of the measure ADD. If type==2, the mid/spr
    #' distance will be considered. By contrast, if type==3, the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev
    #' distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the value of the M-estimator of scale, which is a real number.
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$mEstimator("Huber",0.321,0.5,10^(-5),
    #' 1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$mEstimator("Tukey",0.123,0.5,10^(-5),
    #' 2L,1,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$mEstimator("Cauchy",0.123,0.5,10^(-5),
    #' 3L,0.75,0.5,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(100L)
    #' U=F$median1Norm()
    #' estInitial=F$mdd(U,1L)
    #' delta=0.5
    #' epsilon=10^(-5)
    #' F$mEstimator("Huber",estInitial,delta,epsilon,1L)
    mEstimator = function(f = NA,
                          estInitial = NA,
                          delta = NA,
                          epsilon = NA,
                          type = NA,
                          a = 1,
                          b = 1,
                          theta = 1) {
      stopifnot(typeof(f) == "character" &&
                  (f == "Huber" || f == "Tukey" || f == "Cauchy"))
      stopifnot(typeof(estInitial) == "double" && estInitial > 0)
      stopifnot(typeof(delta) == "double" && delta > 0 && delta < 1)
      stopifnot(typeof(epsilon) == "double" && epsilon > 0)
      stopifnot(typeof(type) == "integer" &&
                  type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(estInitial) ||
          is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      x <-
        matrix() # distances between the numbers of F and the number 0
      t <-
        TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0, 0, 0, 0)))
      if (type == 1) {
        # metric Rho1
        x <- self$rho1(t)
      }
      else if (type == 2) {
        # metric Dthetaphi
        x <- self$dthetaphi(t, a, b, theta)
      }
      else if (type == 3) {
        # metric Dwablphi
        x <- self$dwablphi(t, a, b, theta)
      }

      # Iterative algorithm:
      n <- private$dimensions
      k <- 1
      sigma <- vector()
      sigma[k] <- estInitial

      omega <- vector(length = n)
      repeat {
        for (j in 1:n) {
          omega[j] <- private$weight((x[j] / sigma[k]), f)
        }
        k <- k + 1
        sigma[k] <- sqrt((1 / (n * delta)) * sum(omega * x ^ 2))
        if ((abs(sigma[k] / sigma[k - 1] - 1) < epsilon) |
            (sigma[k] < 10 ^ (-10))) {
          break
        }
      }

      return (sigma[length(sigma)]) # last sigma
    },

    #' @description
    #' This method calculates the scale measure Median Distance Deviation (MDD)
    #' of a 'TrapezoidalFuzzyNumberList' with respect to a 'TrapezoidalFuzzyNumberList'
    #' or with respect to a 'FuzzyNumberList' with a unique valid fuzzy number.
    #' The employed metric in the calculation can be the 1-norm distance, the mid/spr
    #' distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' See De la Rosa de Saa et al. (2015) [2] and De la Rosa de Saa et al. (2021) [3].
    #'
    #' @param s is a TrapezoidalFuzzyNumberList containing a unique TrapezoidalFuzzyNumber
    #' or it is a FuzzyNumberList containing a unique FuzzyNumber.
    #' @param type positive integer 1, 2 or 3: if type==1, the 1-norm distance will
    #' be considered in the calculation of the measure ADD. If type==2, the mid/spr
    #' distance will be considered. By contrast, if type==3, the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev
    #' distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the scale measure MDD, which is a real number.If the body's
    #' method inner conditions are not met, NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$mdd(
    #' FuzzyNumberList$new(c(FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0,
    #' 3.0, 2.0, 1.0), dim = c(3, 3))))),1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$mdd(
    #' FuzzyNumberList$new(c(FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0,
    #' 3.0, 2.0, 1.0), dim = c(3, 3))))),2L,2,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0,0.1,0.2,0.3),
    #' TrapezoidalFuzzyNumber$new(1,2,3,4),TrapezoidalFuzzyNumber$new(2,3,4,5)))$mdd(
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(5,6,7,8))),3L,1,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase3(10L)
    #' U=F$mean()
    #' F$mdd(U,3L,1,2,1)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=F$median1Norm()
    #' F$mdd(U,2L)
    #'
    #' # Example 6:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=Simulation$new()$simulCase2(1L)
    #' F$mdd(U,2L)
    #'
    #' # Example 7:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=F$transfTra()
    #' F$mdd(U,2L)
    #'
    #' # Example 8:
    #' F=Simulation$new()$simulCase2(10L)
    #' U=Simulation$new()$simulCase2(2L)
    #' F$mdd(U,2L)
    mdd = function(s = NA,
                   type = NA,
                   a = 1,
                   b = 1,
                   theta = 1) {
      stopifnot(class(s)[2] == "StatList")
      stopifnot(typeof(type) == "integer" &&
                  type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      if (s$getLength() == 1) {
        u <- NULL
        if (class(s)[1] == "FuzzyNumberList") {
          u <-
            self$transfTra(as.integer(nrow(
              s$getDimension(1L)$getAlphaLevels()
            )))
        } else{
          # class(s)[1] == "TrapezoidalFuzzyNumberList"
          u <- self
        }

        mdd <- 0

        if (type == 1) {
          mdd <- median(s$rho1(u))
        } else if (type == 2) {
          mdd <- median(s$dthetaphi(u, a, b, theta))
        } else{
          # type == 3
          mdd <- median(s$dwablphi(u, a, b, theta))
        }

        return (mdd)
      }

      return (NA)
    },

    #' @description
    #' Given a sample of 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList',
    #' the method calculates the Aumann-type mean of these numbers (which is a
    #' 'TrapezoidalFuzzyNumber' too).
    #' See Sinova et al. (2015) [7].
    #'
    #' @details See examples.
    #'
    #' @return the Aumann-type mean, given as a TrapezoidalFuzzyNumber contained in
    #' a TrapezoidalFuzzyNumberList.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$mean()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(
    #' c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$mean()
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(100L)
    #' F$mean()
    mean = function() {
      F <- private$extractMatrix()
      m <-
        t(as.matrix(apply(F[2:nrow(F),], 2, mean)))

      return(TrapezoidalFuzzyNumberList$new(c(
        TrapezoidalFuzzyNumber$new(m[1, 1], m[1, 2], m[1, 3], m[1, 4])
      )))
    },

    #' @description
    #' Given a sample of 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList',
    #' the method calculates the 1-norm median of these numbers, characterized
    #' by means of nl equidistant \eqn{\alpha}-levels (by default nl=101), including
    #' always the 0 and 1 levels, with their infimum and supremum values.
    #' See Sinova et al. (2012) [8].
    #'
    #' @param nl integer greater or equal to 2, by default nl=101. It indicates the
    #' number of desired \eqn{\alpha}-levels for characterizing the 1-norm median.
    #'
    #' @details See examples.
    #'
    #' @return the 1-norm median, given in form of a FuzzyNumber contained in a
    #' FuzzyNumberList.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$median1Norm()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$median1Norm(200L)
    #'
    #' # Example 3:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$median1Norm(200L)
    median1Norm = function(nl = 101L) {
      stopifnot(is.integer(nl) && nl >= 2)

      # transforms the current TrapezoidalFuzzyNumberList into a FuzzyNumberList
      fuzzyArray <- self$transfTra(nl)

      alpha <- seq(0, 1, len = nl) # alpha-levels
      result <- array(dim = c(nl, 3, 1))

      l <- fuzzyArray$getLength()
      F <- array(dim = c(nl, 3, l))
      for (i in 1:l) {
        F[, 2, i] <- fuzzyArray$getDimension(i)$getInfimums()
        F[, 3, i] <- fuzzyArray$getDimension(i)$getSupremums()
      }

      result[, 1, 1] = alpha
      result[, 2, 1] = apply(F[, 2,], 1, median) # second column: medians of the infimum
      # values of each alpha-level
      result[, 3, 1] = apply(F[, 3,], 1, median) # third column: medians of the supremum
      # values of each alpha-level

      return(FuzzyNumberList$new(c(FuzzyNumber$new(array(
        c(result[, 1, 1], result[, 2, 1], result[, 3, 1]), dim = c(length(result[, 1, 1]), 3)
      )))))
    },

    #' @description
    #' Given a sample of 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList',
    #' the method calculates the \eqn{\phi}-wabl/ldev/rdev median of these numbers,
    #' characterized by means of nl equidistant \eqn{\alpha}-levels (by default nl=101),
    #' including always the 0 and 1 levels, with their infimum and supremum values.
    #' See Sinova et al. (2013) [6] and Sinova et al. (2016) [10].
    #'
    #' @param nl integer greater or equal to 2, by default nl=101. It indicates the
    #' number of desired \eqn{\alpha}-levels for characterizing the \eqn{\phi}-wabl/ldev/rdev
    #' median.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #'
    #' @details See examples.
    #'
    #' @return the \eqn{\phi}-wabl/ldev/rdev median in form of a FuzzyNUmber given
    #' in a FuzzyNumberList.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$medianWabl()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$medianWabl(3L)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$medianWabl(3L,2.2,2.8)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$medianWabl(3L)
    medianWabl = function(nl = 101L,
                          a = 1,
                          b = 1) {
      stopifnot(is.integer(nl) && nl >= 2)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      if (is.infinite(a) || is.infinite(b)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      F <- matrix(private$extractMatrix()[-1, ], ncol = 4)
      n <- nrow(F)
      wablF <- vector()
      medianldev <- vector()
      medianrdev <- vector()

      for (i in 1:n) {
        wablF[i] <- private$wabl(i, list = self, a, b)
      }

      medianwabl <- median(wablF)

      alpha <- seq(0, 1, len = nl) # alpha-levels
      median <- array(dim = c(nl, 3, 1))
      median[, 1, 1] <- alpha  # first column: alpha-levels

      for (j in 1:nl) {
        ldevF <- wablF - (F[, 1] + alpha[j] * (F[, 2] - F[, 1]))
        medianldev[j] <- median(ldevF)
        rdevF <- F[, 4] + alpha[j] * (F[, 3] - F[, 4]) - wablF
        medianrdev[j] <- median(rdevF)
      }
      median[, 2, 1] <- medianwabl - medianldev # second column
      median[, 3, 1] <- medianwabl + medianrdev # third column

      return(FuzzyNumberList$new(c(FuzzyNumber$new(array(
        c(median[, 1, 1], median[, 2, 1], median[, 3, 1]), dim = c(length(median[, 1, 1]), 3)
      )))))
    },

    #' @description
    #' This method calculates scale measure Qn for a matrix of 'TrapezoidalFuzzyNumbers'
    #'contained in the current 'TrapezoidalFuzzyNumber'. The  employed metric
    #' in the calculation can be the 1-norm distance, the mid/spr distance or the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' See De la Rosa de Saa et al. (2021) [3].
    #'
    #' @param type integer number that can be 1, 2 or 3: if type==1, the 1-norm
    #' distance will be considered in the calculation of the measure ADD. If type==2,
    #' the mid/spr distance will be considered. By contrast, if type==3, the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the scale measure Qn, which is a real number.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$qn(1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$qn(2L,5,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$qn(3L,1,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$qn(3L,1,1,1)
    qn = function(type = NA,
                  a = 1,
                  b = 1,
                  theta = 1) {
      stopifnot(typeof(type) == "integer" && type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      n <- private$dimensions
      distMitad <- 0

      if (type == 1) {
        rho1 <- self$rho1(self)
        distMitad <-
          rho1[lower.tri(rho1)] # upper triangle of the matrix rho1
      } else if (type == 2) {
        dthetaphi <- self$dthetaphi(self, a, b, theta)
        distMitad <-
          dthetaphi[lower.tri(dthetaphi)] # upper triangle of the matrix dthetaphi
      } else {
        # type == 3
        dwablphi <- self$dwablphi(self, a, b, theta)
        distMitad <-
          dwablphi[lower.tri(dwablphi)] # upper triangle of the matrix dwablphi
      }

      return (sort(distMitad)[choose(floor(n / 2) + 1, 2)])
    },

    #' @description
    #' This method calculates the 1-norm distance between the 'TrapezoidalFuzzyNumbers'
    #' contained in two 'TrapezoidalFuzzyNumberLists'.
    #'
    #' @param s TrapezoidalFuzzyNumberList containing valid TrapezoidalFuzzyNumbers
    #' characterized by their four values inf0, inf1, sup1, sup0.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the 1-norm distances between the two previous
    #' mentioned TrapezoidalFuzzyNumberLists.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1)))$rho1(TrapezoidalFuzzyNumberList$new(
    #' c(TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58))))
    #'
    #' # Example 2:
    #' F=Simulation$new()$simulCase1(4L)
    #' S=Simulation$new()$simulCase1(5L)
    #' F$rho1(S)
    #' S$rho1(F)
    rho1 = function(s = NA) {
      stopifnot(class(s)[2] == "StatList")
      stopifnot(class(s)[1] == "TrapezoidalFuzzyNumberList")

      r <- private$dimensions
      p <- s$getLength()
      c <- matrix(nrow = r, ncol = p)
      d <- matrix(nrow = r, ncol = p)
      e <- matrix(nrow = r, ncol = p)
      f <- matrix(nrow = r, ncol = p)
      rho <- matrix(nrow = r, ncol = p)

      for (i in 1:r) {
        for (j in 1:p) {
          c[i, j] <-
            private$numbers[[i]]$getInf0() - s$getDimension(j)$getInf0()


          d[i, j] <-
            private$numbers[[i]]$getInf1() - s$getDimension(j)$getInf1()


          e[i, j] <-
            private$numbers[[i]]$getSup1() - s$getDimension(j)$getSup1()


          f[i, j] <-
            private$numbers[[i]]$getSup0() - s$getDimension(j)$getSup0()

          if ((abs(d[i, j] - c[i, j]) > .Machine$double.eps) &
              (abs(e[i, j] - f[i, j]) > .Machine$double.eps))
            rho[i, j] = (1 / 2) * ((d[i, j] * abs(d[i, j]) - c[i, j] *
                                      abs(c[i, j])) / (2 * (d[i, j] - c[i, j])) +
                                     (e[i, j] * abs(e[i, j]) - f[i, j] * abs(f[i, j])) /
                                     (2 * (e[i, j] - f[i, j])))
          if ((abs(d[i, j] - c[i, j]) <= .Machine$double.eps) &
              (abs(e[i, j] - f[i, j]) > .Machine$double.eps))
            rho[i, j] = (1 / 2) * (abs(c[i, j])  +
                                     (e[i, j] * abs(e[i, j]) - f[i, j] * abs(f[i, j])) /
                                     (2 * (e[i, j] - f[i, j])))
          if ((abs(d[i, j] - c[i, j]) > .Machine$double.eps) &
              (abs(e[i, j] - f[i, j]) <= .Machine$double.eps))
            rho[i, j] = (1 / 2) * ((d[i, j] * abs(d[i, j]) - c[i, j] *
                                      abs(c[i, j])) / (2 * (d[i, j] - c[i, j]))  +
                                     abs(f[i, j]))
          if ((abs(d[i, j] - c[i, j]) <= .Machine$double.eps) &
              (abs(e[i, j] - f[i, j]) <= .Machine$double.eps))
            rho[i, j] = (1 / 2) * (abs(c[i, j]) + abs(f[i, j]))

        }
      }

      return(rho)
    },

    #' @description
    #' This method calculates scale measure Sn for a matrix of 'TrapezoidalFuzzyNumbers'
    #'contained in the current 'TrapezoidalFuzzyNumber'. The employed metric
    #' in the calculation can be the 1-norm distance, the mid/spr distance or the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' See De la Rosa de Saa et al. (2021) [3].
    #'
    #' @param type integer number that can be 1, 2 or 3: if type==1, the 1-norm
    #' distance will be considered in the calculation of the measure ADD. If type==2,
    #' the mid/spr distance will be considered. By contrast, if type==3, the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the scale measure Sn, which is a real number.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$sn(1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$sn(2L,1,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$sn(3L,5,1,0.5)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$sn(2L,5,1,0.5)
    sn = function(type = NA,
                  a = 1,
                  b = 1,
                  theta = 1) {
      stopifnot(typeof(type) == "integer" && type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      n <- private$dimensions

      if (type == 1) {
        rho1 <- self$rho1(self)

        # this vector contains the n high medians calculated on each row of the matrix rho1
        medRho1Filas <- vector(length = n)

        for (i in 1:n) {
          medRho1Filas[i] <- sort(rho1[i, ])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medRho1Filas)[floor((n + 1) / 2)])) # low median

      } else if (type == 2) {
        dthetaphi <- self$dthetaphi(self, a, b, theta)

        # this vector contains the n high medians calculated on each row of the matrix dthetaphi
        medDthetaphiFilas <- vector(length = n)

        for (i in 1:n) {
          medDthetaphiFilas[i] <-
            sort(dthetaphi[i,])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medDthetaphiFilas)[floor((n + 1) / 2)])) # low median

      } else {
        # type == 3
        dwablphi <- self$dwablphi(self, a, b, theta)

        # this vector contains the n high medians calculated on each row of the matrix dwablphi
        medDwablphiFilas <- vector(length = n)

        for (i in 1:n) {
          medDwablphiFilas[i] <-
            sort(dwablphi[i,])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medDwablphiFilas)[floor((n + 1) / 2)])) # low median
      }
    },

    #' @description
    #' This method calculates scale measure Tn for a matrix of 'TrapezoidalFuzzyNumbers'
    #' contained in the current 'TrapezoidalFuzzyNumber'. The employed metric
    #' in the calculation can be the 1-norm distance, the mid/spr distance or the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' See De la Rosa de Saa et al. (2021) [3].
    #'
    #' @param type integer number that can be 1, 2 or 3: if type==1, the 1-norm
    #' distance will be considered in the calculation of the measure ADD. If type==2,
    #' the mid/spr distance will be considered. By contrast, if type==3, the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance will be used.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1] in the
    #' mid/spr distance or the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance and the weight of the ldev and rdev in the
    #' (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return the scale measure Tn, which is a real number.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$tn(1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$tn(2L,1,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$tn(3L,5,1,0.5)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$tn(1L)
    tn = function(type = NA,
                  a = 1,
                  b = 1,
                  theta = 1) {
      stopifnot(typeof(type) == "integer" && type >= 1L && type <= 3L)
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      n <- private$dimensions

      if (type == 1) {
        rho1 <- self$rho1(self)

        # this vector contains the n high medians calculated on each row of the matrix rho1
        medRho1Filas <- vector(length = n)

        for (i in 1:n) {
          medRho1Filas[i] <- sort(rho1[i, ])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medRho1Filas)[1:(floor(n / 2) + 1)]))

      } else if (type == 2) {
        dthetaphi <- self$dthetaphi(self, a, b, theta)

        # this vector contains the n high medians calculated on each row of the matrix dthetaphi
        medDthetaphiFilas <- vector(length = n)

        for (i in 1:n) {
          medDthetaphiFilas[i] <-
            sort(dthetaphi[i,])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medDthetaphiFilas)[1:(floor(n / 2) + 1)]))

      } else {
        # type == 3
        dwablphi <- self$dwablphi(self, a, b, theta)

        # this vector contains the n high medians calculated on each row of the matrix dwablphi
        medDwablphiFilas <- vector(length = n)

        for (i in 1:n) {
          medDwablphiFilas[i] <-
            sort(dwablphi[i,])[floor(n / 2) + 1] # high median
        }

        return (mean(sort(medDwablphiFilas)[1:(floor(n / 2) + 1)]))
      }
    },

    #' @description
    #' This method transforms a 'TrapezoidalFuzzyNumberList' containing valid
    #' 'TrapezoidalFuzzyNumbers' characterized by their four values inf0, inf1,
    #' sup1, sup0 into a 'FuzzyNumberList' containing these same amount of fuzzy
    #' numbers, characterized by means of nl equidistant \eqn{\alpha}-levels each
    #' (by default nl=101).
    #'
    #' @param nl integer greater or equal to 2, by default nl=101. It indicates the
    #' number of desired \eqn{\alpha}-levels for characterizing the trapezoidal
    #' fuzzy numbers.
    #'
    #' @details See examples.
    #'
    #' @return a FuzzyNumberList containing the transformed TrapezoidalFuzzyNumbers
    #' into FuzzyNumbers.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$transfTra()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$transfTra(3L)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$transfTra(10L)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase3(10L)
    #' F$transfTra(200L)
    transfTra = function(nl = 101L) {
      stopifnot(typeof(nl) == "integer" && nl >= 2)

      F <- matrix(private$extractMatrix()[-1, ], ncol = 4)
      n <- nrow(F)
      alpha <- seq(0, 1, len = nl) # alpha-levels
      inf <- matrix(nrow = n, ncol = nl)
      sup <- matrix(nrow = n, ncol = nl)

      for (i in 1:n) {
        for (j in 1:nl) {
          inf[i, j] <-
            (1 - alpha[j]) * F[i, 1] + alpha[j] * F[i, 2] # matrix n x nl
          sup[i, j] <-
            (1 - alpha[j]) * F[i, 4] + alpha[j] * F[i, 3] # matrix n x nl
        }
      }

      # inf is a matrix n x nl, whose element (i,j) is the
      # infimum of the alpha-level for the fuzzy number F(i), with
      # alpha=alpha(j)
      # sup is a matrix n x nl, whose element (i,j) is the
      # supremum of the alpha-level for the fuzzy number F(i), with
      # alpha=alpha(j)

      result <-
        FuzzyNumberList$new(c(FuzzyNumber$new(array(
          c(alpha, inf[1,], sup[1,]), dim = c(nl, 3)
        ))))

      if (n > 1) {
        for (i in 2:n) {
          result$addFuzzyNumber(FuzzyNumber$new(array(c(
            alpha, inf[i,], sup[i,]
          ), dim = c(nl, 3))))
        }
      }

      return(result)
    },

    #' @description
    #' Given a sample of 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList',
    #' the method calculates the variance of these numbers with respect to the
    #' mid/spr distance.
    #' See De la Rosa de Saa et al. (2017) [2].
    #'
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance.
    #'
    #' @details See examples.
    #'
    #' @return the variance of the sample with respect to the mid/spr distance,
    #' which is a real number.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$var()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$var(1,1,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$var(1/3,1/3,1/5)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' F$var(1,1,1)
    var = function(a = 1,
                   b = 1,
                   theta = 1) {
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if (is.infinite(a) || is.infinite(b) || is.infinite(theta)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      return (mean(self$dthetaphi(self$mean(), a, b, theta) ^ 2))
    },

    #' @description
    #' Given a sample of 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList',
    #' the method calculates the \eqn{\phi}-wabl value for each of these numbers.
    #' See Sinova et al. (2014) [9].
    #'
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #'
    #' @details See examples.
    #'
    #' @return a vector giving the \eqn{\phi}-wabl values of each TrapezoidalFuzzyNumber.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$wablphi()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$wablphi(2,1)
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),TrapezoidalFuzzyNumber$new(1.5,2,3.75,4),
    #' TrapezoidalFuzzyNumber$new(-4.2,-3.6,-2,-1.58)))$wablphi(2.2,1.1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase4(60L)
    #' F$wablphi(2,1)
    wablphi = function(a = 1, b = 1) {
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      if (is.infinite(a) || is.infinite(b)) {
        stop("The parameters cannot be Inf neither -Inf.")
      }

      F <- matrix(private$extractMatrix()[-1, ], ncol = 4)
      n <- nrow(F)
      wablF <- vector()

      for (i in 1:n) {
        wablF[i] <- private$wabl(i, list = self, a, b)
      }

      return (wablF)
    },

    #' @description
    #' This method adds a 'TrapezoidalFuzzyNumber' to the current collection inside
    #' the current 'TrapezoidalFuzzyNumberList'. Therefore, the dimensions' field
    #' is increased in a unit.
    #'
    #' @param n is the TrapezoidalFuzzyNumber to be added to the current collection
    #' inside the current TrapezoidalFuzzyNumberList.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return nothing.
    #'
    #' @examples
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4))
    #' )$addTrapezoidalFuzzyNumber(TrapezoidalFuzzyNumber$new(3,4,5,6))
    addTrapezoidalFuzzyNumber = function(n = NA, verbose = TRUE) {
      stopifnot(class(n)[1] == "TrapezoidalFuzzyNumber")
      private$dimensions <- private$dimensions + 1
      private$numbers <- append(private$numbers, n)
      if (verbose) {
        cat("numbers updated, current dimension is", private$dimensions)
      }
    },

    #' @description
    #' This method removes a 'TrapezoidalFuzzyNumber' to the current collection inside
    #' the current 'TrapezoidalFuzzyNumberList'. Therefore, the dimensions' field
    #' is decreased in a unit.
    #'
    #' @param i is the position of the TrapezoidalFuzzyNumber to be removed in the
    #' current collection inside the current TrapezoidalFuzzyNumberList.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return nothing.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,3,4,4)))$removeTrapezoidalFuzzyNumber(1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,3,4,4)))$removeTrapezoidalFuzzyNumber(2L)
    removeTrapezoidalFuzzyNumber = function(i = NA, verbose = TRUE) {
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
    #' of the TrapezoidalFuzzyNumberList's numbers array.
    #'
    #' @param i is the dimension of the TrapezoidalFuzzyNumber wanted to be retrieved.
    #'
    #' @details See examples.
    #'
    #' @return The TrapezoidalFuzzyNumber contained in the dimension passed as parameter
    #' or an error if the dimension is not valid.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,3,4,4)))$getDimension(1L)
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,3,4,4)))$getDimension(2L)
    getDimension = function(i = NA) {
      stopifnot(typeof(i) == "integer" &&
                  i > 0 && i <= private$dimensions)
      return(private$numbers[[i]])
    },

    #' @description
    #' This method shows in a graph the values of the attribute numbers of the
    #' corresponding 'TrapezoidalFuzzyNumberList'.
    #'
    #' @param color is the color of the lines representing the numbers to be shown
    #' in the graph. The default value is grey, other colors can be specified, the
    #' option palette() too.
    #'
    #' @details See examples.
    #'
    #' @return a graph with the values of the attribute numbers of the corresponding
    #' 'TrapezoidalFuzzyNumberList'.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4),
    #' TrapezoidalFuzzyNumber$new(2,3,4,5)))$plot()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),
    #' TrapezoidalFuzzyNumber$new(-6,0,1,4)))$plot()
    #'
    #' # Example 3:
    #' Simulation$new()$simulCase1(8L)$plot(palette())
    #'
    #' # Example 4:
    #' Simulation$new()$simulCase1(5L)$plot(palette()[2:6])
    plot = function(color = "grey") {
      dims <- private$dimensions
      p <- (length(color) > 1 && dims > 1)
      x <- c()
      minimo <- c()
      maximo <- c()
      for (i in 1:dims) {
        number <- private$numbers[[i]]
        x <-
          append(x,
                 c(
                   number$getInf0(),
                   number$getInf1(),
                   number$getSup1(),
                   number$getSup0()
                 ))
        minimo <- append(minimo, number$getInf0())
        maximo <- append(maximo, number$getSup0())
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
        main = "TrapezoidalFuzzyNumberList"
      )
      if (p) {
        for (i in seq(1, length(x), by = 4)) {
          points(x[i:(i + 3)], c(0, 1, 1, 0), type = "l", col = color[runif(1, min =
                                                                              1, max = length(color))])
        }
      } else {
      for (i in seq(1, length(x), by = 4)) {
        points(x[i:(i + 3)], c(0, 1, 1, 0), type = "l", col = color)
      }
      }
    },

    #' @description
    #' This method returns the number of dimensions that are equivalent to the number
    #' of 'TrapezoidalFuzzyNumbers' in the corresponding 'TrapezoidalFuzzyNumberList'.
    #'
    #' @details See examples.
    #'
    #' @return the number of dimensions that are equivalent to the number of
    #' 'TrapezoidalFuzzyNumbers' in the corresponding 'TrapezoidalFuzzyNumberList'.
    #'
    #' @examples
    #' # Example 1:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(1,2,3,4))
    #' )$getLength()
    #'
    #' # Example 2:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),
    #' TrapezoidalFuzzyNumber$new(-6,0,1,4)))$getLength()
    #'
    #' # Example 3:
    #' TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),
    #' TrapezoidalFuzzyNumber$new(-6,0,1,4),TrapezoidalFuzzyNumber$new(1,2,3,4))
    #' )$getLength()
    getLength = function() {
      return(private$dimensions)
    }
)
)
