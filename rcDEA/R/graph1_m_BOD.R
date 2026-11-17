#' Graph to select m
#'
#' This function allows to draw a graph that relates the number of super efficient units and the choice of m
#'
#' @param output matrix (or vector) of indicators along which the units are evaluated.
#' @param mseries vector containing the different values of f that needed to be tested.
#' @param check vector containing the values of the thresholds to be considered to define the superefficient units
#' @param col vector containing the colors. the vector col must contain the same number of element of the vector check.
#' @param B number of bootstrap replicates
#' @param RTS For more details see the dea function in the package Benchmarking. Text string or a number defining the underlying DEA technology / returns to scale assumption.
#' 0	fdh	Free disposability hull, no convexity assumption
#' 1	vrs	Variable returns to scale, convexity and free disposability
#' 2	drs	Decreasing returns to scale, convexity, down-scaling and free disposability
#' 3	crs	Constant returns to scale, convexity and free disposability
#' 4	irs	Increasing returns to scale, (up-scaling, but not down-scaling), convexity and free disposability
#' 5	irs2	Increasing returns to scale (up-scaling, but not down-scaling), additivity, and free disposability
#' 6	add	Additivity (scaling up and down, but only with integers), and free disposability; also known af replicability and free disposability, the free disposability and replicability hull (frh) -- no convexity assumption
#' 7	fdh+	A combination of free disposability and restricted or local constant return to scale
#' 10	vrs+	As vrs, but with restrictions on the individual lambdas via param
#' @param ORIENTATION For more details see the dea function in the package Benchmarking. Input efficiency "in" (1), output efficiency "out" (2), and graph efficiency "graph" (3). For use with DIRECT, an additional option is "in-out" (0).
#' @param print If print = TRUE the number of the unit under evaluation is printed. In case of large sample the function could require some time, so it could be useful to control how many units have already been evaluated and which one still have to be evaluated. By default print = FALSE.
#' @import graphics
#' @examples
#'  #Example with a very small sample to decrease computational time.
#'  y1 <-runif(20, 50, 75)
#'  y2 <-runif(20, 30, 75)
#'  y <- cbind(y1, y2)
#'
#'  check <- c(1, 1.05, 1.5)
#'  colors <- c("black", "red", "blue")
#'
#'  graph1_m_BOD(output = y, mseries = c(5, 10, 15),
#'               B = 50, RTS = "crs", ORIENTATION = "in",
#'               check = check, col = colors)
#'
#'   \donttest{#An example with a larger sample size.
#'   x1 <-runif(100, 50, 75)
#'   x2 <-runif(100, 30, 75)
#'   x <- cbind(x1, x2)
#'   y <- cbind(x+runif(100, -10, 0), rnorm(100, 15, 4))
#'
#'  graph1_m_BOD(output = y,
#'         mseries = c(20, 30, 40, 50, 60, 70, 80),
#'         B = 50,
#'         RTS = "crs", ORIENTATION = "in",
#'         check = c(1, 1.05, 1.2, 1.5),
#'         col = c("black", "red", "blue", "green")) }
#'
#' @return This function return a plot, representing the percentage of super-efficient units for the different values of m.
#'         A unit is defined as super-efficient if it gets a value higher than a certain treshold (normally 1) in the robust analysis.
#'         Each line of the plot represent different values of the tresholds.
#'
#' @export

graph1_m_BOD <- function(output, mseries, B,
                         RTS = "crs", ORIENTATION = "in",
                         check = c(1), col = c("black"), print = TRUE) {

  output <- as.data.frame(output)

  meff <- matrix(0, nrow = length(mseries), ncol = length(check))

  for (m in 1:length(mseries)) {
    if (print == TRUE) {
      sprintf("The code is now computing the Robust BOD scores for m = %s", mseries[m])
    }
    BOD <-  robust_BOD(output, m = mseries[m], B, RTS = RTS, ORIENTATION = ORIENTATION)$eff

    for(l in 1:length(check)) {
      meff[m,l] <- length(BOD[BOD>check[l]])/nrow(output)
    }

  }

  plot(x=mseries, y=meff[,1], type = "b", lwd = 2,
       main = "Percentage of super-efficient units",
       xlab = c("m"),
       ylab = c("Percentage of super-efficient units"),
       ylim=c(0, max(meff[,1])) )

  for(l in 2:length(check)) {
    graphics::lines(x=mseries, y=meff[,l], type = "b", lwd = 2, col = col[l])
  }

    legend <- legend("topright", legend = check, col = col, lwd = 2)
    legend

}
