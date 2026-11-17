#' Conditional DEA function
#'
#' This function allows to compute Robust and Conditional DEA scores.
#'
#' @param output matrix (or vector) of outputs along which the units are evaluated.
#' @param input matrix (or vector) of inputs along which the units are evaluated.
#' @param exogenous matrix (or vector) of exogenous variables involved in the conditional analysis. The similarity among the units is determined according to the exogeneous variable(s) using the function npudensbw and npudens (from the package np) with epanechnikov kernel.
#' @param similarity matrix of similarities. In alternative to provide the exogenous variables, the matrix of similarities can be directly provided. This allow to customize the estimation of the similarities.
#' @param m number of unit to be included in the reference set
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
#' @param alpha This allow to choose the size of the Confidence Intervals computed. By defaulta alpha = FALSE. In this case no confidence interval are computed
#' @param inclusion If inclusion = TRUE the unit under analysis is included in the reference set. So, no super efficient scores are allowed. By default inclusion = FALSE.
#' @param print If print = TRUE the number of the unit under evaluation is printed. In case of large sample the function could require some time, so it could be useful to control how many units have already been evaluated and which one still have to be evaluated. By default print = FALSE.
#' @concept Conditional Data Envelopment Analysis
#' @import np
#' @import Benchmarking
#' @examples #Example with a very small sample to decrease computational time.
#'           x1 <-runif(50, 50, 75)
#'           x2 <-runif(50, 30, 75)
#'           x <- cbind(x1, x2)
#'           e <- rnorm(50, 0, 36)
#'           a1 <- 0.4
#'           a2 <- 0.6
#'           y <- a1*x1 + a2*x2 + e
#'           z <- ifelse(rnorm(50, 0, 1)>0, 1, 0)
#'
#'           #Conditional DEA
#'           c_DEA <- conditional_DEA(input = x, output = y, exogenous = z,
#'                                    m = 30, B = 50,
#'                                    RTS = "crs", ORIENTATION = "in")
#'           summary(c_DEA$eff)
#'
#'           \donttest{#Example with bigger sample
#'           x1 <-runif(100, 50, 75)
#'           x2 <-runif(100, 30, 75)
#'           x <- cbind(x1, x2)
#'           a1 <- 0.4
#'           a2 <- 0.6
#'           y <- a1*x1 + a2*x2
#'           z <- ifelse(rnorm(100, 0, 1)>0, 1, 0)
#'
#'           #Conditional DEA
#'           c_DEA <- conditional_DEA(input = x, output = y, exogenous = z,
#'                                    m = 30, B = 50,
#'                                    RTS = "crs", ORIENTATION = "in")
#'           summary(c_DEA$eff)}
#'
#' @return If the parameter alpha is specified, the function returns a data frame with three numeric columns.
#'         The first column is the vector representing the conditional DEA scores (eff);
#'         the second column is the vector representing the lower bound of the condifence interval (ci_low);
#'         the third column is the vector representing the upper bound of the confidence interval (Ci_up).
#'         If alpha is not specified, the functions returns only the first column of the data frame (eff).
#'
#' @export
#'


conditional_DEA <- function(input, output, exogenous = FALSE,
                            alpha = FALSE,
                            m, B,
                            RTS = "crs", ORIENTATION = "in",
                            similarity = FALSE, inclusion = FALSE, print=FALSE) {

  input <- as.data.frame(input)
  output <- as.data.frame(output)
  exogenous <- as.data.frame(exogenous)

  #define preliminary variables
  n <- nrow(output)
  k <- ncol(output)
  s <- ncol(input)

  eff <- rep(0, n)
  DEA_B<- rep(0, B) #bootstrap BOD
  if (alpha != FALSE) {
    ci_low <- rep(0, n)
    ci_up <- rep(0,n)
  }

  if(is.data.frame(similarity) == FALSE) {
    if (print == TRUE) {
      print(c("R is now computing the bandwidth using the function npudensbw in the package np"))
    }
    bw = np::npudensbw(dat=exogenous)
  }

  if (print == TRUE) {
    print(c("R is now computing the conditional DEA"))
  }

  for (i in 1:n) {

    if(print == TRUE) {
      print(i)
    }

    #in case the code needs to compute the similarity for each obs i
    if(is.data.frame(similarity) == FALSE) {
      if(ncol(exogenous)>1) {
        kerz <- np::npudens(bws=bw,
                            cykertype="epanechnikov",cxkertype="epanechnikov",
                            tdat=exogenous[i,],edat=exogenous)
        similarity_i <- cbind(kerz$dens)
      }
      if(ncol(exogenous)==1) {
        kerz <- np::npudens(bws=bw,
                            cykertype="epanechnikov",cxkertype="epanechnikov",
                            tdat=exogenous[i,],edat=exogenous)
        similarity_i <- cbind(kerz$dens)
      }
    }


    #in case the matrix of similarities has already been computed
    if(is.data.frame(similarity) != FALSE) { #equivalent of asking if the values for the similarity are inserted
      similarity_i <- similarity[i,]
    }

    #consider only the units that perform at least as good as unit i
    y <- output[i, ]
    x <- input[i, ]
    Y_Rob <- output
    X_Rob <- input
    similarity_Rob <- similarity_i

    if (ORIENTATION =="out") {
      for (l in 1:k) { #NOTE: not change the order!! first select X than Y when selecting on Y
        similarity_Rob <- similarity_Rob[Y_Rob[, l] >= as.numeric(y[l])]
        X_Rob <- as.data.frame(X_Rob[Y_Rob[, l] >= as.numeric(y[l]), ])
        Y_Rob <- as.data.frame(Y_Rob[Y_Rob[, l] >= as.numeric(y[l]), ])
      }
    }
    if (ORIENTATION == "in") {
      for (l in 1:s) { #NOTE: not change the order!! first select Y than X when selecting on X
        similarity_Rob <- similarity_Rob[X_Rob[, l] <= as.numeric(x[l])]
        Y_Rob <- as.data.frame(Y_Rob[X_Rob[, l] <= as.numeric(x[l]), ])
        X_Rob <- as.data.frame(X_Rob[X_Rob[, l] <= as.numeric(x[l]), ])
      }
    }

    n_sample <- nrow(Y_Rob) #(equivalent to nrow(Y_Rob))

    #pick a sample of random unit in the reference set if there are at least 2 units in the ref
    if (n_sample < 2) {
      eff[i] <- 1
    }
    else {
      for (j in 1:B) {
        #select m random units
        m_sample <- sample(n_sample, m, prob = similarity_Rob, replace = TRUE)
        Y_ref <- as.data.frame(Y_Rob[m_sample,])
        X_ref <- as.data.frame(X_Rob[m_sample,])

        if (inclusion == TRUE) {#I add the same observation to be sure it is included
          Y_ref <- rbind(y, Y_ref)
          X_ref <- rbind(x, X_ref)
        }

        #compute the DEA for unit i
        DEA_B[j] <- Benchmarking::dea(X = x, Y = y, RTS = RTS, ORIENTATION = ORIENTATION, XREF = X_ref, YREF = Y_ref)$eff
      }
      eff[i] <- mean(DEA_B)
      if (alpha != FALSE) {
        ci_low[i] <- stats::quantile(DEA_B, alpha/2)
        ci_up[i] <- stats::quantile(DEA_B, 1-alpha/2)
      }
    }
  }

  if(alpha != FALSE) {
    save <- data.frame(eff, ci_low, ci_up)
  }
  else {
    save <- data.frame(eff)
  }

  return(save)
}





