#' Robust BOD function
#'
#' This function allows to compute Robust  BOD scores.
#'
#' @param output matrix (or vector) of indicators along which the units are evaluated.
#' @param m number of unit to be included in the reference set
#' @param B number of bootstrap replicates
#' @param RTS Default = "CRS". For more details see the dea function in the package Benchmarking. Text string or a number defining the underlying DEA technology / returns to scale assumption.
#' 0	fdh	Free disposability hull, no convexity assumption
#' 1	vrs	Variable returns to scale, convexity and free disposability
#' 2	drs	Decreasing returns to scale, convexity, down-scaling and free disposability
#' 3	crs	Constant returns to scale, convexity and free disposability
#' 4	irs	Increasing returns to scale, (up-scaling, but not down-scaling), convexity and free disposability
#' 5	irs2	Increasing returns to scale (up-scaling, but not down-scaling), additivity, and free disposability
#' 6	add	Additivity (scaling up and down, but only with integers), and free disposability; also known af replicability and free disposability, the free disposability and replicability hull (frh) -- no convexity assumption
#' 7	fdh+	A combination of free disposability and restricted or local constant return to scale
#' 10	vrs+	As vrs, but with restrictions on the individual lambdas via param
#' @param ORIENTATION Default = "in". For more details see the dea function in the package Benchmarking. Input efficiency "in" (1), output efficiency "out" (2), and graph efficiency "graph" (3). For use with DIRECT, an additional option is "in-out" (0).
#' @param alpha This allow to choose the size of the Confidence Intervals computed. By defaulta alpha = FALSE. In this case no confidence interval are computed
#' @param inclusion If inclusion = TRUE the unit under analysis is included in the reference set. So, no super efficient scores are allowed. By default inclusion = FALSE.
#' @param print If print = TRUE the number of the unit under evaluation is printed. In case of large sample the function could require some time, so it could be useful to control how many units have already been evaluated and which one still have to be evaluated. By default print = FALSE.
#' @concept Robust Benefit of the Doubt (BOD)
#' @import Benchmarking
#' @examples #Example with a very small sample to decrease computational time.
#'           y1 <-runif(50, 50, 75)
#'           y2 <-runif(50, 30, 75)
#'           y <- cbind(y1, y2)
#'
#'           #Robust BOD
#'           r_BOD <- robust_BOD(output = y, m = 30, B = 50,
#'                               RTS = "crs", ORIENTATION = "in", print = TRUE)
#'           summary(r_BOD$eff)
#'
#'           \dontrun{#Example with random data x and y
#'           y1 <-runif(100, 50, 75)
#'           y2 <-runif(100, 30, 75)
#'           y <- cbind(y1, y2)
#'
#'           #Robust BOD
#'           r_BOD <- robust_BOD(output = y, m = 30, B = 50,
#'                               RTS = "crs", ORIENTATION = "in", print = TRUE)
#'           summary(r_BOD$eff)}
#'
#' @return If the parameter alpha is specified, the function returns a data frame with three numeric columns.
#'         The first column is the vector representing the robust BOD scores (eff);
#'         the second column is the vector representing the lower bound of the condifence interval (ci_low);
#'         the third column is the vector representing the upper bound of the confidence interval (Ci_up).
#'         If alpha is not specified, the functions returns only the first column of the data frame (eff).
#' @export
#'


robust_BOD <- function(output, m, B,
                       alpha = FALSE,
                       RTS = "CRS", ORIENTATION = "in", inclusion = FALSE,
                       print=FALSE) {

  output <- as.data.frame(output)

  #define preliminary variables
  n <- nrow(output)
  ones <- rep(1, n)

  BOD <- robust_DEA(input = ones, output = output,
                    alpha = alpha,
                    m = m, B = B,
                    RTS = RTS, ORIENTATION = ORIENTATION,
                    inclusion = inclusion, print = print )
  eff <- BOD$eff

  if(alpha != FALSE) {
    ci_low <- BOD$ci_low
    ci_up <- BOD$ci_up
    save <- data.frame(eff, ci_low, ci_up)
  }
  else {
    save <- data.frame(eff)
  }
  return(save)
}

