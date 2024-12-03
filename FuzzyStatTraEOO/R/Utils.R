#' @title 'Utils' to convert real data into the corresponding 'StatList'.
#'
#' @description
#' 'Utils' contain an auxiliary method that perform the conversion of an archive with
#' rda extension (R Data File), that contains real data, to the corresponding
#' 'TrapezoidalFuzzyNumberList'.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method comments are welcome to the below mentioned mail addresses.
#'
#' @author Andrea Garcia Cernuda <uo270115@uniovi.es>
#'
#' @import R6
#'
#' @export Utils
Utils <- R6::R6Class(classname = "Utils",
                     public = list(
                       #' @description
                       #' This method generates n 'TrapezoidalFuzzyNumbers' contained in a 'TrapezoidalFuzzyNumberList'
                       #' obtained from the rows and columns of R Data File. If the data contains any NA value, the
                       #' row will be deleted as a 'TrapezpidalFuzzyNumber' have to be created with double values.
                       #'
                       #' @param d is the R Data File already loaded in the environment with data("example").
                       #' If the user wants to use M1, M2, M3 or S1, they are already loaded in the
                       #' package environment through the archive data.R.
                       #'
                       #' @details See examples.
                       #'
                       #' @return a TrapezoidalFuzzyNumberList with n TrapezoidalFuzzyNumbers. Each
                       #' one is characterized by its four values inf0, inf1, sup1, sup0. The TrapezoidalFuzzyNumbers
                       #' are obtained from the rows and columns of R Data File. If the body's method inner conditions
                       #' are not met, NA will be returned.
                       #'
                       #' @examples
                       #' # Example 1:
                       #' Utils$new()$convertTra(M1)
                       #'
                       #' # Example 2:
                       #' Utils$new()$convertTra(M2)
                       #'
                       #' # Example 3:
                       #' Utils$new()$convertTra(M3)
                       #'
                       #' # Example 4:
                       #' Utils$new()$convertTra(S1)
                       #'
                       #' # Example 5:
                       #' m=as.data.frame(matrix(c(NA, 1, 2, NA, 3, 2,2,NA,1,3,NA,NA,6,4,NA,NA),ncol=4))
                       #' Utils$new()$convertTra(m)
                       convertTra = function(d = NA)
                       {
                         stopifnot(class(d) == "data.frame" && typeof(d) == "list")
                         stopifnot(ncol(d) == 4)

                         d2 <-
                           as.matrix(d[complete.cases(d), ]) # matrix without missing values

                         if (nrow(d2) > 0) {
                           stopifnot(typeof(d2) == "double")

                           result <-
                             TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(d2[1, 1], d2[1, 2], d2[1, 3], d2[1, 4])))

                           if (nrow(d2) >= 2) {
                             for (val in 2:nrow(d2)) {
                               result$addTrapezoidalFuzzyNumber(TrapezoidalFuzzyNumber$new(d2[val, 1], d2[val, 2], d2[val, 3], d2[val, 4]))
                             }
                           }

                           return (result)

                         }

                         return (NA)

                       }
                     ))
