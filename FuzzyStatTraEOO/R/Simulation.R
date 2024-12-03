#' @title 'Simulation' contains several methods to simulate 'TrapezoidalFuzzyNumberLists'.
#'
#' @description
#' Simulation contains 5 different methods that gives the user a 'TrapezoidalFuzzyNumberList'.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method comments are welcome to the below mentioned mail addresses.
#'
#' @author(s) Andrea Garcia Cernuda <uo270115@uniovi.es>, Asun Lubiano <lubiano@uniovi.es>,
#' Sara de la Rosa de Saa
#'
#' @references
#' [1] De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A.: Fuzzy
#' rating scale-based questionnaires and their statistical analysis, IEEE Transactions on Fuzzy
#' Systems 23(1), 111-126 (2015)
#'
#' [2] Lubiano, M.A.; Salas, A.; Carleos, C.; De la Rosa de Sáa, S.; Gil, M.Á.: Hypothesis
#' testing-based comparative analysis between rating scales for intrinsically imprecise data,
#' International Journal of Approximate Reasoning 88, 128-147 (2017)
#'
#' [3] Sinova, B.; Gil, M.A.; Colubi, A.; Van Aelst, S.: The median of a random fuzzy number. The
#' 1-norm distance approach, Fuzzy Sets and Systems 200, 99-115 (2012)
#'
#' [4] Sinova, B.; Gil, M.A.; Van Aelst, S.: M-estimates of location for the robust central tendency of
#' fuzzy data, IEEE Transactions on Fuzzy Systems 24(4), 945-956 (2016)
#'
#' @import R6
#'
#' @export Simulation
Simulation <- R6::R6Class(
  classname = "Simulation",
  private = list(
    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 array
    obtainY2SecondProcedure = function(i = NA, y1 = NA)
    {
      return (runif(1, 0, min(1 / 10, y1[i], 1 - y1[i])))
    },

    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 and y2 array
    obtainY3SecondProcedure = function(i = NA,
                                       y1 = NA,
                                       y2 = NA)
    {
      return (runif(1, 0, min(1 / 5, y1[i] - y2[i])))
    },

    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 and y2 array
    obtainY4SecondProcedure = function(i = NA,
                                       y1 = NA,
                                       y2 = NA)
    {
      return (runif(1, 0, min(1 / 5, 1 - y1[i] - y2[i])))
    },

    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 array
    obtainY2ThirdProcedure = function(i = NA, y1 = NA)
    {
      return (if (y1[i] < 0.25) {
        rexp(1, 100 + 4 * y1[i])
      }
      else if (y1[i] <= 0.75) {
        rexp(1, 200)
      }
      else{
        rexp(1, 500 - 4 * y1[i])
      })
    },

    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 and y2 array
    obtainY3ThirdProcedure = function(i = NA,
                                      y1 = NA,
                                      y2 = NA)
    {
      return (if (y1[i] - y2[i] >= 0.25) {
        rgamma(1, 4, 100)
      }
      else{
        rgamma(1, 4, 100 + 4 * y1[i])
      })
    },

    # auxiliary method used in the SimulFRSTra public method
    # i is an integer that corresponds with the position of the y1 and y2 array
    obtainY4ThirdProcedure = function(i = NA,
                                      y1 = NA,
                                      y2 = NA)
    {
      return (if (y1[i] + y2[i] >= 0.25) {
        rgamma(1, 4, 100)
      }
      else{
        rgamma(1, 4, 500 - 4 * y1[i])
      })
    },

    # auxiliary method used in the SimulFRSTra public method
    # i and value are integers that correspond with the position of the array
    checkValue = function(i = NA,
                          value = NA,
                          array = NA) {
      return(if (array[i, value] >= 0 &&
                 array[i, value] <= 1) {
        array[i, value] <- array[i, value]
      }
      else if (array[i, value] < 0) {
        array[i, value] <- 0
      }
      else if (array[i, value] > 1) {
        array[i, value] <- 1
      })
    }
  ),
  public = list(
    #' @description
    #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
    #' from a symmetric distribution and with independent components (for a detailed
    #' explanation of the simulation see Sinova et al. (2012) [3], namely, the Case 1 for
    #' noncontaminated samples).
    #'
    #' @param n positive integer. It is the number of trapezoidal fuzzy numbers to
    #' be generated.
    #'
    #' @details See examples.
    #'
    #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers. Each
    #' one is characterized by its four values inf0, inf1, sup1, sup0.
    #'
    #' @examples
    #' Simulation$new()$simulCase1(10L)
    simulCase1 = function(n = NA)
    {
      stopifnot(is.integer(n) && n > 0)

      l <- list()

      mid1 <- cbind(rnorm(n)) # mid1
      spr1 <- cbind(rchisq(n, 1)) # spr1
      lspr0 <- cbind(rchisq(n, 1)) # lspr0 (=inf1-inf0)
      rspr0 <- cbind(rchisq(n, 1)) #rspr0 (=sup0-sup1)[j[]]

      for (j in 1:n) {
        inf1 <- mid1[j] - spr1 [j]
        sup1 <- mid1[j] + spr1[j]
        inf0 <- inf1 - lspr0[j]
        sup0 <- sup1 + rspr0[j]
        l <-
          append(l, TrapezoidalFuzzyNumber$new(inf0, inf1, sup1, sup0))
      }

      return(TrapezoidalFuzzyNumberList$new(l))
    },

    #' @description
    #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
    #' from a symmetric distribution and with dependent components (for a detailed
    #' explanation of the simulation see Sinova et al. (2012) [3], namely, the Case 2
    #' for noncontaminated samples).
    #'
    #' @param n positive integer. It is the number of trapezoidal fuzzy numbers to
    #' be generated.
    #'
    #' @details See examples.
    #'
    #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers. Each
    #' one is characterized by its four values inf0, inf1, sup1, sup0.
    #'
    #' @examples
    #' Simulation$new()$simulCase2(10L)
    simulCase2 = function(n = NA)
    {
      stopifnot(is.integer(n) && n > 0)

      l <- list()

      mid1 <- cbind(rnorm(n)) # mid1
      spr1 <-
        1 / (mid1 ^ 2 + 1) ^ 2 + 0.1 * cbind(rchisq(n, 1)) # spr1
      lspr0 <-
        1 / (mid1 ^ 2 + 1) ^ 2 + 0.1 * cbind(rchisq(n, 1)) # lspr0 (=inf1-inf0)
      rspr0 <-
        1 / (mid1 ^ 2 + 1) ^ 2 + 0.1 * cbind(rchisq(n, 1)) #rspr0 (=sup0-sup1)

      for (j in 1:n) {
        inf1 <- mid1[j] - spr1 [j]
        sup1 <- mid1[j] + spr1[j]
        inf0 <- inf1 - lspr0[j]
        sup0 <- sup1 + rspr0[j]
        l <-
          append(l, TrapezoidalFuzzyNumber$new(inf0, inf1, sup1, sup0))
      }

      return(TrapezoidalFuzzyNumberList$new(l))
    },

    #' @description
    #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
    #' from a asymmetric distribution and with independent components (for a detailed
    #' explanation of the simulation see Sinova et al. (2012) [4], namely, the Case 3
    #' for noncontaminated samples).
    #'
    #' @param n positive integer. It is the number of trapezoidal fuzzy numbers to
    #' be generated.
    #'
    #' @details See examples.
    #'
    #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers. Each
    #' one is characterized by its four values inf0, inf1, sup1, sup0.
    #'
    #' @examples
    #' Simulation$new()$simulCase3(10L)
    simulCase3 = function(n = NA)
    {
      stopifnot(is.integer(n) && n > 0)

      l <- list()
      p <- 5 # first parameter of the beta distribution
      q <- 1 # second parameter of the beta distribution

      numbers <- rbeta(n * 4, p, q)

      for (j in seq(1, length(numbers), by = 4)) {
        sorted <- sort(numbers[c(j, j + 1, j + 2, j + 3)])
        l <-
          append(l,
                 TrapezoidalFuzzyNumber$new(sorted[[1]], sorted[[2]], sorted[[3]], sorted[[4]]))
      }

      return(TrapezoidalFuzzyNumberList$new(l))

    },

    #' @description
    #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
    #' from a asymmetric distribution and with dependent components (for a detailed
    #' explanation of the simulation see Sinova et al. (2012) [4], namely, the Case 4
    #' for noncontaminated samples).
    #'
    #' @param n positive integer. It is the number of trapezoidal fuzzy numbers to
    #' be generated.
    #'
    #' @details See examples.
    #'
    #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers. Each
    #' one is characterized by its four values inf0, inf1, sup1, sup0.
    #'
    #' @examples
    #' Simulation$new()$simulCase4(10L)
    simulCase4 = function(n = NA)
    {
      stopifnot(is.integer(n) && n > 0)

      l <- list()
      p <- 5 # first parameter of the beta distribution
      q <- 1 # second parameter of the beta distribution

      mid1 <- cbind(rbeta(n, p, q)) # mid1
      spr1 <-
        cbind(runif(n, 0, apply(cbind(mid1, 1 - mid1), 1, min))) # spr1
      lspr0 <- cbind(runif(n, 0, mid1 - spr1)) # lspr0 (=inf1-inf0)
      rspr0 <-
        cbind(runif(n, 0, 1 - mid1 - spr1)) #rspr0 (=sup0-sup1)

      for (j in 1:n) {
        inf1 <- mid1[j] - spr1 [j]
        sup1 <- mid1[j] + spr1[j]
        inf0 <- inf1 - lspr0[j]
        sup0 <- sup1 + rspr0[j]
        l <-
          append(l, TrapezoidalFuzzyNumber$new(inf0, inf1, sup1, sup0))
      }

      return(TrapezoidalFuzzyNumberList$new(l))

    },

    #' @description
    #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
    #' based on the fuzzy rating scale. They are simulated mimicking the human behavior,
    #' considering for it a finite mixture of three different procedures (for a
    #' detailed explanation of the simulation see De la Rosa de Saa et al. (2012) [1]), and
    #' generated in the interval [0,1].
    #'
    #' @param n positive integer. It is the number of trapezoidal fuzzy numbers to
    #' be generated.
    #' @param w1 real number in [0,1]. It should be fulfilled that w1+w2+w3=1.
    #' @param w2 real number in [0,1]. It should be fulfilled that w1+w2+w3=1.
    #' @param w3 real number in [0,1]. It should be fulfilled that w1+w2+w3=1.
    #' @param p real number > 0. It is the first parameter of the beta distribution.
    #' @param q real number > 0. It is the second parameter of the beta distribution.
    #'
    #' @details See examples.
    #'
    #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers with
    #' values in the interval [0,1]. Each trapezoidal fuzzy rating response is
    #' characterized by its four values inf0, inf1, sup1, sup0.
    #'
    #' @examples
    #' Simulation$new()$simulFRSTra(100L,0.05,0.35,0.6,2,1)
    simulFRSTra = function(n = NA,
                           w1 = NA,
                           w2 = NA,
                           w3 = NA,
                           p = NA,
                           q = NA)
    {
      stopifnot(is.integer(n) && n > 0)
      stopifnot(is.double(w1) && w1 >= 0 && w1 <= 1)
      stopifnot(is.double(w2) && w2 >= 0 && w2 <= 1)
      stopifnot(is.double(w3) && w3 >= 0 && w3 <= 1)
      stopifnot(is.double(p) && p > 0)
      stopifnot(is.double(q) && q > 0)
      if(is.infinite(p) || is.infinite(q)){
        stop("The parameters cannot be Inf.")
      }

      # n: number of trapezoidal fuzzy rating responses to be generated
      # w1,w2,w3: w1*100% responses will be generated from the first procedure,
      # w2*100% from the second one and w3*100% from the third procedure
      # p,q: parameters of the beta distribution

      # Returns a fuzzy trapezoidal sample in [0,1] of size n

      # SAMPLE FOLLOWING THE DISTRIBUTIONS....

      if (sum(c(w1, w2, w3)) != 1)
        stop("it should be fulfilled that w1+w2+w3=1")

      n1 <- round(w1 * n, 0)
      # size of the random part
      n2 <- round(w2 * n, 0)
      # size of the simetric part
      n3 <- n - n1 - n2
      #size of the variable FRS

      ############ FIRST PROCEDURE ############

      if (n1 > 0) {
        t1 <- t(replicate(n1, sort(rbeta(4, p, q))))
      } #random sample

      ############ SECOND PROCEDURE ############

      if (n2 > 0) {
        t2 <- matrix(0, n2, 4) #simetrical sample
        y1 <- rbeta(n2, p, q) #y1
        y2 <-
          sapply(1:n2, private$obtainY2SecondProcedure, y1 = y1) #y2
        y3 <-
          sapply(1:n2,
                 private$obtainY3SecondProcedure,
                 y1 = y1,
                 y2 = y2) #y3
        y4 <-
          sapply(1:n2,
                 private$obtainY4SecondProcedure,
                 y1 = y1,
                 y2 = y2) #y4

        #characterization of trapezoidal data Tra(a,b,c,d)=Tra<x1,x2,x3,x4>
        t2[, 1] <- t(y1 - y2 - y3) #a
        t2[, 2] <- t(y1 - y2)        #b
        t2[, 3] <- t(y1 + y2)  #c
        t2[, 4] <- t(y1 + y2 + y4) #d
      }

      ############ THIRD PROCEDURE ############

      if (n3 > 0) {
        t3 <- matrix(0, n3, 4) #asimetrical sample
        y1 <- rbeta(n3, p, q) #y1
        y2 <-
          sapply(1:n3, private$obtainY2ThirdProcedure, y1 = y1) #y2
        y3 <-
          sapply(1:n3,
                 private$obtainY3ThirdProcedure,
                 y1 = y1,
                 y2 = y2) #y3
        y4 <-
          sapply(1:n3,
                 private$obtainY4ThirdProcedure,
                 y1 = y1,
                 y2 = y2) #y4
        #characterization of trapezoidal data Tra(a,b,c,d)=Tra<x1,x2,x3,x4>

        t3[, 1] <- t(y1 - y2 - y3) #a
        t3[, 2] <- t(y1 - y2)        #b
        t3[, 3] <- t(y1 + y2)  #c
        t3[, 4] <- t(y1 + y2 + y4) #d
      }

      if (n1 > 0 &&
          n2 > 0 && n3 > 0) {
        t <- rbind(t1, t2, t3)
      } #combined sample
      else if (n2 > 0 && n3 > 0) {
        t <- rbind(t2, t3)
      } #combined sample
      else if (n1 > 0    &&
               n3 > 0) {
        t <- rbind(t1   , t3)
      } #combined sample
      else if (n1 > 0 &&
               n2 > 0) {
        t <- rbind(t1, t2)
      } #combined sample
      else if (n1 > 0) {
        t <- t1
      } #combined sample
      else if (n2 > 0) {
        t <- t2
      } #combined sample
      else if (n3 > 0) {
        t <- t3
      } #combined sample

      t[, 1] <-
        t(sapply(1:n, private$checkValue, value = 1, array = t))
      t[, 2] <-
        t(sapply(1:n, private$checkValue, value = 2, array = t))
      t[, 3] <-
        t(sapply(1:n, private$checkValue, value = 3, array = t))
      t[, 4] <-
        t(sapply(1:n, private$checkValue, value = 4, array = t))

      l <- list()

      for (j in 1:n) {
        l <-
          append(l, TrapezoidalFuzzyNumber$new(t[j, 1], t[j, 2], t[j, 3], t[j, 4]))
      }

      return(TrapezoidalFuzzyNumberList$new(l))
    }
  )
)
