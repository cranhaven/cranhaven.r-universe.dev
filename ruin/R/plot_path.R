#' Plot a path of the simulated ruin process
#'
#' \code{plot_path()} takes a simulated ruin process as the argument and plots
#' its path.
#'
#' Under the hood, the function uses \code{\link{ggplot2}} package, therefore,
#' all functionality from \code{\link{ggplot2}} is available.
#'
#' @param path_object an S4 object of *Path class (e.g.,
#' \linkS4class{PathCramerLundberg}).
#'
#' @return A \code{\link{ggplot2}} object.
#'
#' @examples
#' model <- CramerLundberg(initial_capital = 10,
#'                         premium_rate = 1,
#'                         claim_poisson_arrival_rate = 1,
#'                         claim_size_generator = rexp,
#'                         claim_size_parameters = list(rate = 1))
#'
#' path <- simulate_path(model = model, max_time_horizon = 10)
#'
#' plot_path(path)
#'
#' @export
plot_path <- function(path_object) {

  # validate arguments
  #-----------------------------------------------------------------------------

  stopifnot(

    isS4(path_object),

    "path" %in% methods::slotNames(path_object)

  )

  # plot
  #-----------------------------------------------------------------------------

  data <- data.frame(path_object@path)

  aestetic <- ggplot2::aes_string(x = "time", y = "X")

  return(ggplot2::ggplot(data = data) + ggplot2::geom_line(mapping = aestetic))

}
