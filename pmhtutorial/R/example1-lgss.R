##############################################################################
# State estimation in a LGSS model using particle and Kalman filters
#
# Johan Dahlin <uni (at) johandahlin.com.nospam>
# Documentation at https://github.com/compops/pmh-tutorial
# Published under GNU General Public License
##############################################################################

#' State estimation in a linear Gaussian state space model
#'
#' @description
#' Minimal working example of state estimation in a linear Gaussian state
#' space model using Kalman filtering and a fully-adapted particle filter.
#' The code estimates the bias and mean squared error (compared with the
#' Kalman estimate) while varying the number of particles in the particle
#' filter.
#' @details
#' The Kalman filter is a standard implementation without an input. The
#' particle filter is fully adapted (i.e. takes the current observation into
#' account when proposing new particles and computing the weights).
#' @return
#' Returns a plot with the generated observations y and the difference in the
#' state estimates obtained by the Kalman filter (the optimal solution) and
#' the particle filter (with 20 particles). Furthermore, the function returns
#' plots of the estimated bias and mean squared error of the state estimate
#' obtained using the particle filter (while varying the number of particles)
#' and the Kalman estimates.
#'
#' The function returns a list with the elements:
#' \itemize{
#' \item{y: The observations generated from the model.}
#' \item{x: The states generated from the model.}
#' \item{kfEstimate: The estimate of the state from the Kalman filter.}
#' \item{pfEstimate: The estimate of the state from the particle filter with
#' 20 particles.}
#' }
#' @references 
#' Dahlin, J. & Schon, T. B. "Getting Started with Particle 
#' Metropolis-Hastings for Inference in Nonlinear Dynamical Models." 
#' Journal of Statistical Software, Code Snippets,
#' 88(2): 1--41, 2019.
#' @author 
#' Johan Dahlin \email{uni@@johandahlin.com}
#' @note
#' See Section 3.2 in the reference for more details.
#' @example ./examples/example1
#' @keywords
#' misc
#' @export
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#' @importFrom graphics abline
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom stats acf
#' @importFrom stats density
#' @importFrom stats sd
#' @importFrom stats var

example1_lgss <- function() {

  # Set the random seed to replicate results in tutorial
  set.seed(10)

  ##############################################################################
  # Define the model and generate data
  # x[t + 1] = phi * x[t] + sigmav * v[t],    v[t] ~ N(0, 1)
  # y[t] = x[t] + sigmae * e[t],              e[t] ~ N(0, 1)
  ##############################################################################
  phi <- 0.75
  sigmav <- 1.00
  sigmae <- 0.10
  T <- 250
  initialState <- 0

  data <- generateData(c(phi, sigmav, sigmae), T, initialState)
  x <- data$x
  y <- data$y

  # Plot the latent state and observations
  layout(matrix(c(1, 1, 2, 2, 3, 4), 3, 2, byrow = TRUE))
  par   (mar = c(4, 5, 0, 0))

  grid <- seq(0, T)

  plot(
    grid,
    y,
    col = "#1B9E77",
    type = "l",
    xlab = "time",
    ylab = "observation",
    ylim = c(-6, 6),
    bty = "n"
  )
  polygon(c(grid, rev(grid)),
          c(y, rep(-6, T + 1)),
          border = NA,
          col = rgb(t(col2rgb("#1B9E77")) / 256, alpha = 0.25))


  ##############################################################################
  # State estimation using the particle filter and Kalman filter
  ##############################################################################
  # Using noParticles = 20 particles and plot the estimate of the latent state
  noParticles <- 20
  outputPF <- particleFilter(y, c(phi, sigmav, sigmae), noParticles, initialState)
  outputKF <- kalmanFilter(y, c(phi, sigmav, sigmae), initialState, 0.01)
  difference <- outputPF$xHatFiltered - outputKF$xHatFiltered[-(T + 1)]

  grid <- seq(0, T - 1)
  plot(
    grid,
    difference,
    col = "#7570B3",
    type = "l",
    xlab = "time",
    ylab = "error in state estimate",
    ylim = c(-0.1, 0.1),
    bty = "n"
  )
  polygon(
    c(grid, rev(grid)),
    c(difference, rep(-0.1, T)),
    border = NA,
    col = rgb(t(col2rgb("#7570B3")) / 256, alpha = 0.25)
  )

  # Compute bias and MSE
  logBiasMSE <- matrix(0, nrow = 7, ncol = 3)
  gridN <- c(10, 20, 50, 100, 200, 500, 1000)

  for (ii in 1:length(gridN)) {
    pfEstimate <-
      particleFilter(y, c(phi, sigmav, sigmae), gridN[ii], initialState)
    pfEstimate <- pfEstimate$xHatFiltered
    kfEstimate <- outputKF$xHatFiltered[-(T + 1)]

    logBiasMSE[ii, 1] <- gridN[ii]
    logBiasMSE[ii, 2] <- log(mean(abs(pfEstimate - kfEstimate)))
    logBiasMSE[ii, 3] <- log(mean((pfEstimate - kfEstimate) ^ 2))
  }

  ##############################################################################
  # Plot the bias and MSE for comparison
  ##############################################################################
  plot(
    gridN,
    logBiasMSE[, 2],
    col = "#E7298A",
    type = "l",
    xlab = "no. particles (N)",
    ylab = "log-bias",
    ylim = c(-7,-3),
    bty = "n"
  )
  polygon(
    c(gridN, rev(gridN)),
    c(logBiasMSE[, 2], rep(-7, length(gridN))),
    border = NA,
    col = rgb(t(col2rgb("#E7298A")) / 256, alpha = 0.25)
  )
  points(gridN, logBiasMSE[, 2], col = "#E7298A", pch = 19)

  plot(
    gridN,
    logBiasMSE[, 3],
    col = "#66A61E",
    lwd = 1.5,
    type = "l",
    xlab = "no. particles (N)",
    ylab = "log-MSE",
    ylim = c(-12,-6),
    bty = "n"
  )
  polygon(
    c(gridN, rev(gridN)),
    c(logBiasMSE[, 3], rep(-12, length(gridN))),
    border = NA,
    col = rgb(t(col2rgb("#66A61E")) / 256, alpha = 0.25)
  )
  points(gridN, logBiasMSE[, 3], col = "#66A61E", pch = 19)

  list(y=y, x=x, pfEstimate=pfEstimate, kfEstimate=kfEstimate, logBiasMSE=logBiasMSE)
}