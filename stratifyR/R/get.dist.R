#' To identify the best-fit distribution of a univariate data
#'
#' This function is called at the start of the stratification process where the
#' best-fit distribution and it parameters are estimated and returned for further
#' processing towards the computation of stratum boundaries. It basically takes in
#' the data and fits it with a list of 10 possible distributions and computes
#' the parameters for all given distributions. It selects the best-fit distribution
#' to be the one with the lowest AIC
#'
#' @param data A vector: usually a column in a given data frame
#' @param my_env My environment my_env has various constants and data that are
#' used by the get.dist() function
#'
#' @import fitdistrplus
#' @importFrom actuar rpareto
#' @import triangle
#' @import mc2d
#'
#' @return \code{} returns a list which contains the best-fit distribution and
#' its estimated parameters
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
get.dist <- function(data, my_env) #data is a single column data frame or numeric
{
   #initialize some key variables
   distr <- c("pareto", "triangle", "rtriangle",  "weibull",
              "gamma", "exp", "unif", "norm", "lnorm", "cauchy")

   #params-list of params, aic-vector of aic vals
   params <- list(); aic <- c()
   params1 <- list(); aic1 <- c()
   params2 <- list(); aic2 <- c()
   params3 <- list(); aic3 <- c()
   params4 <- list(); aic4 <- c()

   #pareto & (triangular & RT) use diff method while others use a similar manner
   #-----------------------------------------------------
   for(i in 1:1) #pareto is @ position 1 in the vector
   {
      params1 <- suppressWarnings(list(try(fitdist(data, distr = "pareto", method="mle",
                    start = list(shape = 1, scale = 1), lower=c(0,0))$estimate, silent=TRUE))) #catch error & silent
      aic1 <- suppressWarnings(try(fitdist(data, distr = "pareto", method="mle",
                  start = list(shape = 1, scale = 1), lower=c(0,0))$aic, silent=TRUE))
   }
   #-----------------------------------------------------
   for(i in 2:3)#triangle & right triangular
   {
      eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
      params2[i] <- suppressWarnings(list(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                       start = list(min = a-eps, max = b+eps, mode = c))$estimate, silent=TRUE)))
      aic2[i] <- suppressWarnings(c(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                       start = list(min = a-eps, max = b+eps, mode = c))$aic, silent=TRUE)))
   }
   #-----------------------------------------------------
   for(i in 4:5)
   {
     params3[i] <- suppressWarnings(list(try(fitdist(data, distr = distr[i], method="mle",
                      lower = c(0,0))$estimate, silent=TRUE)))
     aic3[i] <- suppressWarnings(c(try(fitdist(data, distr = distr[i], method="mle",
                      lower = c(0,0))$aic, silent=TRUE)))
   }
   #-----------------------------------------------------
   for(i in 6:(length(distr)))
   {
      params4[i] <- suppressWarnings(list(try(fitdist(data, distr = distr[i], method="mle")$estimate,
                             silent=TRUE)))
      aic4[i] <- suppressWarnings(c(try(fitdist(data, distr = distr[i], method="mle")$aic, silent=TRUE)))
   }
   #-----------------------------------------------------

   params <- c(params1, params2[-1], params3[-(1:3)], params4[-(1:5)]) #combine all params into a vector
   aic <- c(aic1, aic2[-1], aic3[-(1:3)], aic4[-(1:5)])

   names(params) <- distr #assign names
   names(aic) <- distr

   distrib <- names(aic[suppressWarnings(which.min(aic))]) #get name of distr with smallest aic
   param <- params[[distrib]]

   #combine both results in a list
   result <- list("distr"=distrib, "params"=param) #best-fit distrib with its params for scaled data

   return(result)
}
########################################################################################################
