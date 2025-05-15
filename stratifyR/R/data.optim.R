#' To implement the Dynamic Programming (DP) solution procedure on the stratification
#' problem presented in the form of a Mathematical Programming Problem (MPP)
#'
#' This function uses the Dynamic Programming (DP) solution procedure in solving the
#' objective function for the univariate stratification problem. It calculates
#' the objective function values using the brute-force algorithm and stores those
#' values in the matrices and keeps a copy in my_env so that a global minimum
#' could be obtained.
#'
#' @param k A numeric: number of strata
#' @param n A numeric: is the distance*1000
#' @param incf A numeric: 10e-3 when k=1 and 10e-5 for k>=2
#' @param minYk A numeric: index to access minimum elements in the matrix
#' @param maxYk A numeric: index to access maximum elements in the matrix
#' @param isFirstRun A boolean: TRUE/FALSE parameter
#' @param my_env The environment my_env has various constants and calculations stored
#' from earlier opeartions through various other functions
#'
#' @return \code{} returns the array filled with calculations of objective
#' function values
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr M
#' GM Khan <khan_mg@usp.ac.fj>
#'
data.optim <- function(k, n, incf, minYk, maxYk, isFirstRun=TRUE, my_env)
{
   if(length(k) > 1 || !is.numeric(k) || !is.finite(k) || k < 1)
      stop("choice of 'k' is not valid")

   d <- n*incf
   
   ch <- my_env$ch #get stratum costs 

   miny <- 0
   val <- 0

   if(k == 1)
   {
      y <- d
      c <- ch[k]
      dblRetVal <- data.root(d, y, c, my_env)
   }
   else
   {
      for(i in minYk:(maxYk-1))
      {
         y <- i*incf
         c <- ch[k]
         root <- data.root(d, y, c, my_env)
         if(root != -1)
         {
            col <- as.integer(n-i)
            if(my_env$minkf2[k, col+1] == -9999)
            {
               if(isFirstRun)
               {
                  val <- root + data.optim((k-1), col, incf, 0, col, TRUE, my_env)
               }
               else
               {
                  val <- root + data.optim((k-1), col, incf,
                                my_env$ylimits[k]-my_env$factor*my_env$z,
                                my_env$ylimits[k]+my_env$factor*my_env$z, FALSE, my_env)
               }
            }
            else
            {
               val <- root + my_env$minkf2[k, col+1]
            }
         }
         if (i == minYk)
         {
            min <- val
         }
         else
         {
            min <- minim.val(min, val)
         }
         if(min == val)
         {
            miny <- y
         }
      }
      dblRetVal <- min
   }
   col <- n
   my_env$minkf2[k+1, col+1] <- dblRetVal
   my_env$dk2[k+1, col+1] <- miny
   return(dblRetVal)
}
##################################################################################