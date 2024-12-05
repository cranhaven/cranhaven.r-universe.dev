#' Generic summary method for `iClusterVB` objects
#'
#' @param object A fitted iClusterVB object.
#' @param rho The minimum posterior inclusion probability of interest to count
#'   the number of features that are >= \code{rho}. Default is 0.5. Only works
#'   for VS_method = 1.
#' @param ... Potential further arguments
#' @return Returns a summary list for an `agnes` object.
#' @examples
#' # Setting up the data
#' dat1 <- list(
#'   gauss_1 = sim_data$continuous1_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   gauss_2 = sim_data$continuous2_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   poisson_1 = sim_data$count_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   multinomial_1 = sim_data$binary_data[c(1:20, 61:80, 121:140, 181:200), 1:75]
#' )
#'
#' # Recoding `0`s to `2`s
#' dat1$multinomial_1[dat1$multinomial_1 == 0] <- 2
#'
#' dist <- c(
#'   "gaussian", "gaussian",
#'   "poisson", "multinomial"
#' )
#'
#' fit_iClusterVB <- iClusterVB(
#'   mydata = dat1,
#'   dist = dist,
#'   K = 4,
#'   initial_method = "VarSelLCM",
#'   VS_method = 1,
#'   max_iter = 25
#' )
#'
#' ## S3 method for class 'iClusterVB'
#' summary(fit_iClusterVB, rho = 0.75)
#'
#' @export
#' @method summary iClusterVB
#' @useDynLib iClusterVB, .registration=TRUE


summary.iClusterVB <- function(object, rho = 0.5, ...) {
  # args <- list(...)
  #
  # if (!("rho" %in% names(args))) {
  #   rho <- 0.5
  # }


  cat("Total number of individuals:\n")
  print(object$data_summary$N)

  cat("\n")


  cat(paste("User-inputted maximum number of clusters:", object$K, sep = " "))
  cat("\n")
  cat(paste("Number of clusters determined by algorithm:", length(unique(object$cluster))))

  cat("\n")
  cat("\n")

  cat("Cluster Membership:")

  print(table(object$cluster))

  cat("\n")

  if (!is.null(object$model_parameters$rho)) {
    for (i in 1:length(object$mydata)) {
      name <- paste("View", i, "-", object$dist[i], sep = " ")
      cat(paste("# of variables above the posterior inclusion probability of", rho, "for View", i, "-", object$dist[i], sep = " "))
      cat("\n")
      print(paste(sum(object$model_parameters$rho[[i]] >= rho), "out of a total of", object$data_summary$p[[i]], sep = " "))
      cat("\n")
    }
  }
}






#' Generic plot method for `iClusterVB` objects
#'
#' @param x A fitted iClusterVB object.
#' @param ... Potential further arguments (unused)
#'
#' @return Returns an evidence lower bound (ELBO) plot and a barplot of cluster
#'   percentages.
#'
#'
#' @export
#' @method plot iClusterVB
#' @examples
#' # Setting up the data
#' dat1 <- list(
#'   gauss_1 = sim_data$continuous1_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   gauss_2 = sim_data$continuous2_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   poisson_1 = sim_data$count_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   multinomial_1 = sim_data$binary_data[c(1:20, 61:80, 121:140, 181:200), 1:75]
#' )
#'
#' # Recoding `0`s to `2`s
#' dat1$multinomial_1[dat1$multinomial_1 == 0] <- 2
#'
#' dist <- c(
#'   "gaussian", "gaussian",
#'   "poisson", "multinomial"
#' )
#'
#' fit_iClusterVB <- iClusterVB(
#'   mydata = dat1,
#'   dist = dist,
#'   K = 4,
#'   initial_method = "VarSelLCM",
#'   VS_method = 1,
#'   max_iter = 25
#' )
#'
#' plot(fit_iClusterVB)
#'
plot.iClusterVB <- function(x, ...) {
  fit <- x

  rm(x)

  plot(
    x = 1:fit$iter,
    y = fit$elbo[1:1:fit$iter],
    type = "o",
    lwd = 2,
    xlab = "Iteration",
    ylab = "ELBO"
  )

  bar_plot_y <- round(fit[["model_parameters"]][["ppi"]][round(fit[["model_parameters"]][["ppi"]], digits = 4) > 0] * 100, digits = 2)

  bar_plot <- barplot(bar_plot_y,
    ylim = c(-10, 100),
    yaxt = "n",
    ylab = "Cluster Percentage (%)",
    xlab = "Cluster"
  )
  abline(h = 0)
  axis(
    side = 2, at = seq(0, 100, 5),
    labels = paste(seq(0, 100, 5), "%", sep = "")
  )

  text(x = bar_plot, y = bar_plot_y + 5, labels = paste(bar_plot_y, "%", sep = ""))
  text(x = bar_plot, y = -5, labels = paste("Cluster", sort(unique(fit$cluster)), sep = " "))
}
