#' Simulating evolutionary rate model
#'
#' \code{simulate_rate} Simulates three different evolutionary rates models. In
#' the two first, 'predictor_BM' and 'predictor_gBM', the evolution of y follows
#' a Brownian motion with variance linear in x, while the evolution of x either
#' follows a Brownian motion or a geometric Brownian motion, respectively. In
#' the third model, 'recent_evol', the residuals of the macroevolutionary
#' predictions of y have variance linear in x.
#'
#' @param tree A \code{\link[ape]{phylo}} object. Must be ultrametric and scaled to
#'   unit length.
#' @param startv_x The starting value for x (usually 0 for 'predictor_BM' and
#'   'recent_evol', and 1 for 'predictor_gBM').
#' @param sigma_x The evolutionary rate of x.
#' @param a A parameter of the evolutionary rate of y.
#' @param b A parameter of the evolutionary rate of y.
#' @param sigma_y The evolutionary rate for the macroevolution of y (Brownian
#'   motion process) used in the 'recent_evolution' model.
#' @param x Optional fixed values of x (only for the 'recent_evol' model), must
#'   equal number of tips in the phylogeny, must correspond to the order of the
#'   tip labels.
#' @param model Either a Brownian motion model of x 'predictor_BM', geometric
#'   Brownian motion model of x 'predictor_gBM', or 'recent_evol'.
#' @details See the vignette 'Analyzing rates of evolution' for an explanation
#'   of the evolutionary models. The data of the tips can be analyzed with the
#'   function \code{\link{rate_gls}}. Note that a large part of parameter space
#'   will cause negative roots in the rates of y (i.e. negative a+bx). In these
#'   cases the rates are set to 0. A warning message is given if the number of
#'   such instances is larger than 0.1\%. For model 1 and 2, it is possible to
#'   set `a='scaleme'`, if this chosen then `a` will be given the lowest
#'   possible value constrained by a+bx>0.
#' @return An object of \code{class} \code{'simulate_rate'}, which is a list
#'   with the following components: \tabular{llllll}{ \code{tips}
#'   \tab\tab\tab\tab A data frame of x and y values for the tips. \cr
#'   \code{percent_negative_roots} \tab\tab\tab\tab The percent of iterations
#'   with negative roots in the rates of y (not given for model =
#'   'recent_evol'). \cr \code{compl_dynamics}  \tab\tab\tab\tab A list with the
#'   output of the complete dynamics (not given for model = 'recent_evol'). }
#' @author Geir H. Bolstad
#' @examples
#' # Also see the vignette 'Analyzing rates of evolution'.
#' \dontrun{
#' # Generating a tree with 50 species
#' set.seed(102)
#' tree <- ape::rtree(n = 50)
#' tree <- ape::chronopl(tree, lambda = 1, age.min = 1)
#'
#' ### model = 'predictor_BM' ###
#' sim_data <- simulate_rate(tree, startv_x = 0, sigma_x = 0.25, a = 1, b = 1, 
#' model = "predictor_BM")
#' head(sim_data$tips)
#' par(mfrow = c(1, 3))
#' plot(sim_data)
#' plot(sim_data, response = "y")
#' plot(sim_data, response = "x")
#' par(mfrow = c(1, 1))
#'
#' ### model = 'predictor_gBM' ###
#' sim_data <- simulate_rate(tree, startv_x = 1, sigma_x = 1, a = 1, b = 0.1, 
#' model = "predictor_gBM")
#' head(sim_data$tips)
#' par(mfrow = c(1, 3))
#' plot(sim_data)
#' plot(sim_data, response = "y")
#' plot(sim_data, response = "x")
#' par(mfrow = c(1, 1))
#'
#' ### model = 'recent_evol' ###
#' sim_data <- simulate_rate(tree,
#'   startv_x = 0, sigma_x = 1, a = 1, b = 1, sigma_y = 1,
#'   model = "recent_evol"
#' )
#' head(sim_data$tips)
#' }
#' @export

simulate_rate <- function(tree, startv_x = NULL, sigma_x = NULL, a, b, 
                          sigma_y = NULL, x = NULL, model = "predictor_BM") {
  if(model == "predictor_gBM"){
      if(startv_x < 0) stop("Choose positive startv_x")
  }
  if(any(tree$edge.length<0)) stop("The tree has a negative edge length")
    
  #### predictor_BM or predictor_gBM ####
  if (model == "predictor_BM" | model == "predictor_gBM") {
    EDGE <- cbind(tree$edge, round(tree$edge.length, 3))
    EDGE[, 3][EDGE[, 3] == 0] <- 0.001 # adding one time step to the edges that 
    # was rounded down to zero
    EDGE <- EDGE[order(tree$edge[, 1]), ]
    x_evo <- list()
    y_evo <- list()
    timesteps <- list()
    rate_y <- list()

    # Simulating x-values
    for (i in 1:nrow(EDGE)) {
      if (EDGE[i, 1] %in% EDGE[1:i, 2]) {
        x0 <- rev(x_evo[[which(EDGE[1:i, 2] == EDGE[i, 1])]])[1] # If the 
        # ancestor already has a starting value
        time0 <- rev(timesteps[[which(EDGE[1:i, 2] == EDGE[i, 1])]])[1]
      } else {
        time0 <- 0
        if (model == "predictor_BM") {
          x0 <- startv_x
        }
        if (model == "predictor_gBM") {
          x0 <- log(startv_x)
        }
      }
      x_evo[[i]] <- x0 + cumsum(rnorm(n = EDGE[i, 3] * 1000, 
                                      mean = 0, 
                                      sd = sqrt(1 / 1000) * sigma_x
                                      )
                                )
      timesteps[[i]] <- (time0 + 1):(time0 + EDGE[i, 3] * 1000)
    }

    if (a == "scaleme") {
      if (model == "predictor_BM") {
        a <- -min(b * unlist(x_evo))
      }
      if (model == "predictor_gBM") {
        a <- -min(b * exp(unlist(x_evo)))
      }
    }

    # percentage of negative square roots
    if (model == "predictor_BM") {
      percent_negative <- 
          length(which((a + b * unlist(x_evo)) < 0)) / 
          length(unlist(x_evo)) * 100
    }
    if (model == "predictor_gBM") {
      percent_negative <- 
          length(which((a + b * exp(unlist(x_evo))) < 0)) / 
          length(exp(unlist(x_evo))) * 100
    }
    if (percent_negative > 0.1) {
      warning(paste("Number of negative a + bx is ", 
                    round(percent_negative, 1), 
                    "%. The term a + bx is set to zero for these values of x", 
                    sep = ""
                    )
              )
    }

    # Simulating y-values
    for (i in 1:nrow(EDGE)) {
      if (EDGE[i, 1] %in% EDGE[1:i, 2]) {
        y0 <- rev(y_evo[[which(EDGE[1:i, 2] == EDGE[i, 1])]])[1] # If the 
        # ancestor already has a starting value
      } else {
        y0 <- 0
      }
      if (model == "predictor_BM") {
        r <- a + b * x_evo[[i]]
      }
      if (model == "predictor_gBM") {
        r <- a + b * exp(x_evo[[i]])
      }
      r[r < 0] <- 0
      y_evo[[i]] <- y0 + 
          cumsum(sqrt(r) * rnorm(n = length(r), mean = 0, sd = 1 / sqrt(1000)))
      rate_y[[i]] <- sqrt(r)
    }


    EDGE <- cbind(EDGE, 
                  sapply(x_evo, function(x) rev(x)[1]), 
                  sapply(y_evo, function(x) rev(x)[1])
                  )
    DATA <- EDGE[EDGE[, 2] < EDGE[1, 1], c(2, 4, 5)]
    DATA <- DATA[order(DATA[, 1]), ]
    DATA <- as.data.frame(DATA)
    colnames(DATA) <- c("species", "x", "y")
    DATA$species <- tree$tip.label[DATA$species]
    if (model == "predictor_gBM") {
      DATA$x <- exp(DATA$x)
      x_evo <- lapply(x_evo, exp)
    }
  }

  #### recent_evol ####
  if (model == "recent_evol") {
    n <- length(tree$tip.label)
    A <- ape::vcv(tree)
    sigma_y <- sigma_y[1]
    if (is.null(x)) {
      x <- rnorm(n, startv_x, sigma_x)
    }
    if (is.null(sigma_y) | sigma_y <= 0) {
      stop("Specify a positive sigma_y")
    }
    Vmacro <- c(sigma_y^2) * A
    diag_Vmicro <- c(a + b * x)
    diag_Vmicro[diag_Vmicro < 0] <- 0 # Ensures that there are no negative 
    # variances
    Vmicro <- diag(x = diag_Vmicro)
    V <- Vmacro + Vmicro
    y <- t(chol(V)) %*% rnorm(n)
    DATA <- data.frame(species = tree$tip.label, x = x, y = y)
    percent_negative <- NULL
    timesteps <- NULL
    x_evo <- NULL
    y_evo <- NULL
    rate_y <- NULL
  }
  simout <- 
      list(tips = DATA, 
           percent_negative_roots = percent_negative,
           compl_dynamics = list(timesteps = timesteps, 
                                 x_evo = x_evo,
                                 y_evo = y_evo, 
                                 rate_y = rate_y
                                 )
           )
  class(simout) <- "simulate_rate"
  return(simout)
}


#' Plot of simulate_rate object
#'
#' \code{plot} method for class \code{'simulate_rate'}.
#'
#' @param x An object of class \code{'simulate_rate'}.
#' @param response The variable for the y-axis of the plot, can be 'rate_y',
#'   'y', or 'x'.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ... Additional arguments passed to \code{\link{plot}}.
#' @details No plot is returned if model = 'recent_evol'.
#' @return \code{plot} A plot of the evolution of the traits x or y, or the
#'  evolution of the evolutionary rate of y (i.e. \eqn{\sqrt{a + bx}}) in the
#'  simulation.
#' @examples
#' # See the vignette 'Analyzing rates of evolution'.
#' @author Geir H. Bolstad
#' @importFrom graphics plot lines
#' @export
plot.simulate_rate <- function(x, 
                               response = "rate_y", 
                               xlab = "Simulation timesteps",
                               ylab = "Evolutionary rate of y", 
                               ...
                               ) {
  dt <- x$compl_dynamics
  if (is.null(dt$timesteps)) {
    stop("There are no dynamics stored. Plotting is not implemented for the
         recent_evol model")
  }
  xvar_list <- dt$timesteps
  if (is.null(xlab)) {
    xlab <- "Simulation timesteps"
  }
  if (response == "rate_y") {
    yvar_list <- dt$rate_y
  }
  if (response == "y") {
    yvar_list <- dt$y_evo
    if (ylab == "Evolutionary rate of y") {
      ylab <- "y"
    }
  }
  if (response == "x") {
    yvar_list <- dt$x_evo
    if (ylab == "Evolutionary rate of y") {
      ylab <- "x"
    }
  }
  plot(xvar_list[[1]], 
       yvar_list[[1]],
       type = "l", 
       ylim = range(unlist(yvar_list)),
       xlim = range(unlist(xvar_list)), 
       xlab = xlab, 
       ylab = ylab
       )
  for (i in 2:length(xvar_list)) lines(xvar_list[[i]], yvar_list[[i]])
}
