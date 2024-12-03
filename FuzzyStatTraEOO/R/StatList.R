#' @title 'StatList' is an "abstract class" representing a super class, useful for
#' 'FuzzyNumberList' and 'TrapezoidalFuzzyNumberList' implementation.
#'
#' @description
#' 'StatList' defines the common attributes and methods of 'FuzzyNumberList' and
#' 'TrapezoidalFuzzyNumberList'.
#' All methods are empty except for some attribute checking, the child classes are
#' the ones that have to give the implementation for the empty methods.
#'
#' @note
#' In order to have the documentation completed, we had had to write the documentation of
#' this class. Taking into account that this class is part of the software design and
#' it cannot be initialized, all its documentation is not needed, in particular the
#' new and clone methods, apart from the Usage: section of each method documentation.
#' All its methods can be used and can be found in FuzzyNumberList's and TrapezoidalFuzzyNumberList's
#' documentation.
#'
#' We are working to improve this issue. In case you find (almost surely existing)
#' bugs or have recommendations for improving the method, comments are welcome
#' to the above mentioned mail addresses.
#'
#' @author Andrea Garcia Cernuda <uo270115@uniovi.es>
#'
#' @import R6
#'
#' @export StatList
StatList <- R6::R6Class(
  classname = "StatList",
  private = list (
    # an array can have several columns
    columns = NULL,

    # an array can have several rows
    rows = NULL,

    # an array can have several dimensions
    dimensions = NULL
  ),
  public = list(
    #' @description
    #' This method warns the user that this class can not be initialized as it is abstract.
    #'
    #' @return shows a message telling that this class can not be initialized.
    initialize = function() {
      stop(print("this an abstract class and it can't be initialized."))
    },

    #' @description
    #' This method calculates the mid/spr distance between the numbers contain in
    #' two 'StatLists'.
    #'
    #' @param s can be a FuzzyNumberList or a TrapezoidalFuzzyNumberList.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance.
    #'
    #' @return a matrix containing the mid/spr distances between the two previous
    #' mentioned StatLists.
    dthetaphi = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {

      stopifnot(class(s)[2] == "StatList")
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if(is.infinite(a) || is.infinite(b) || is.infinite(theta)){
        stop("The parameters cannot be Inf neither -Inf.")
      }

    },

    #' @description
    #' This method calculates the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance
    #' between the numbers contained in two 'StatLists'.
    #'
    #' @param s can be a FuzzyNumberList or a TrapezoidalFuzzyNumberList.
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' ldev and rdev in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @return a StatList containing the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distances
    #' between the two previous mentioned StatLists.
    dwablphi  = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {

      stopifnot(class(s)[2] == "StatList")
      stopifnot(typeof(a) == "double" && a > 0)
      stopifnot(typeof(b) == "double" && b > 0)
      stopifnot(typeof(theta) == "double" && theta > 0)
      if(is.infinite(a) || is.infinite(b) || is.infinite(theta)){
        stop("The parameters cannot be Inf neither -Inf.")
      }

    },

    #' @description
    #' This method calculates the 1-norm distance between the numbers contained
    #' in two 'StatLists'.
    #'
    #' @param s can be a FuzzyNumberList or a TrapezoidalFuzzyNumberList.
    #'
    #' @return a StatList containing the 1-norm distances between the two previous
    #' mentioned StatLists.
    rho1 = function(s = NA) {

    },

    #' @description
    #' This method shows in a graph the inner numbers of the corresponding 'StatList'.
    #'
    #' @param color is the color of the lines representing the numbers to be shown
    #' in the graph. The default value is grey, other colors can be specified, the
    #' option palette() too.
    #'
    #' @return a graph with the inner numbers of the corresponding 'StatList'
    #' represented.
    plot = function(color = "grey"){

    },

    #' @description
    #' This method returns the number of dimensions that are equivalent to the number
    #' of numbers in the corresponding 'StatList'.
    #'
    #' @return the number of dimensions that are equivalent to the number of numbers
    #' in the corresponding 'StatList'.
    getLength = function(){

    }
  )
)
