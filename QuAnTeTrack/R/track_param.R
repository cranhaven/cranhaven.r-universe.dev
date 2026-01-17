#' Print track parameters
#'
#' \code{track_param()} is a function to compute and print various parameters of tracks from a list of track data.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#'
#' @details This function calculates various movement parameters for each track in the provided data.
#' It uses the following helper functions from the \pkg{trajr} (Animal Trajectory Analysis) package:
#'    * \code{TrajAngles()}: Calculates the turning angles of the track.
#'    * \code{TrajDistance()}: Calculates the total distance covered by the track.
#'    * \code{TrajLength()}: Calculates the length of the track.
#'    * \code{TrajStepLengths()}: Calculates the step lengths of the track.
#'    * \code{TrajSinuosity2()}: Calculates the sinuosity of the track.
#'    * \code{TrajStraightness()}: Calculates the straightness of the track.
#'
#' @return A list of lists, where each sublist contains the computed parameters for a corresponding track.
#' The parameters included are:
#'    * \code{Turning_angles}: A vector of turning angles for the track (in degrees).
#'    * \code{Mean_turning_angle}: The mean of the turning angles (in degrees).
#'    * \code{Standard_deviation_turning_angle}: The standard deviation of the turning angles (in degrees).
#'    * \code{Distance}: The total distance covered by the track (in meters).
#'    * \code{Length}: The length of the track  (in meters).
#'    * \code{Step_lengths}: A vector of step lengths for the track  (in meters).
#'    * \code{Mean_step_length}: The mean of the step lengths  (in meters).
#'    * \code{Standard_deviation_step_length}: The standard deviation of the step lengths  (in meters).
#'    * \code{Sinuosity}: The sinuosity of the track (dimensionless).
#'    * \code{Straightness}: The straightness of the track (dimensionless).
#'
#' @return The reference direction, or 0 degrees, is considered to be along the positive x-axis. This means that angles are measured counterclockwise from the positive x-axis, with 0 degrees (or 0 degrees) pointing directly along this axis. For a detailed explanation and appropriate methods for analyzing circular data, refer to Batschelet (1981).
#'
#' @return Sinuosity is calculated according to Benhamou (2004), as defined in equation 8.
#' The formula used here is a refined version of the sinuosity index presented by Bovet & Benhamou (1988),
#' which is applicable to a broader range of turning angle distributions and does not require a constant step length.
#'
#' The sinuosity is computed using the formula:
#' \deqn{S = 2 \left[ p \left( \frac{1 + c}{1 - c} + b^2 \right) \right]^{-0.5}}
#' where:
#' \item{p}{is the mean step length (in meters),}
#' \item{c}{is the mean cosine of turning angles (in radians), and}
#' \item{b}{is the coefficient of variation of the step length (in meters).}
#'
#' @return The straightness index is defined as the ratio D/L, where:
#' \item{D}{is the beeline distance between the first and last points in the trajectory (in meters), and}
#' \item{L}{is the total path length traveled (in meters).}
#'
#' Straightness index is based on the method described by Batschelet (1981). According to Benhamou (2004),
#' the straightness index serves as a reliable measure of the efficiency of a directed walk. However, it is not suitable
#' for random trajectories, as the index for a random walk tends towards zero with increasing steps. Thus, it is recommended
#' to use this measure to compare the tortuosity of random walks only if they consist of a similar number of steps.
#'
#' @section Logo:
#' \if{html}{\figure{Logo.png}{options: width=30\%}}
#'
#' @author Humberto G. Ferrón
#' @author humberto.ferron@uv.es
#' @author Macroevolution and Functional Morphology Research Group (www.macrofun.es)
#' @author Cavanilles Institute of Biodiversity and Evolutionary Biology
#' @author Calle Catedrático José Beltrán Martínez, nº 2
#' @author 46980 Paterna - Valencia - Spain
#' @author Phone: +34 (9635) 44477
#'
#' @references
#'
#' Batschelet, E. (1981). Circular statistics in biology. Academic press, 111 Fifth Ave., New York, NY 10003, 1981, 388.
#'
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's path:: straightness, sinuosity, or fractal dimension?. Journal of theoretical biology, 229(2), 209-220.
#'
#' Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals' movements using a correlated random walk model. Journal of theoretical biology, 131(4), 419-433.
#'
#' @examples
#' # Example 1:
#' track_param(PaluxyRiver)
#'
#' # Example 2:
#' track_param(MountTom)
#'
#' @importFrom trajr TrajAngles
#' @importFrom trajr TrajDistance
#' @importFrom trajr TrajLength
#' @importFrom trajr TrajStepLengths
#' @importFrom trajr TrajSinuosity2
#' @importFrom trajr TrajStraightness
#'
#' @seealso \code{\link{tps_to_track}}
#'
#' @export


track_param <- function(data) {
  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("Both elements of 'data' must be lists. Ensure that 'Trajectories' and 'Footprints' are provided.")
  }


  ## Code----

  # Extract the first element of the input 'data' list, which is expected to contain the 'Trajectories' list.
  data <- data[[1]]

  # Initialize an empty list to store the results for each trajectory.
  list <- list()

  # Loop through each trajectory in the 'data' list.
  for (i in 1:length(data)) {
    # Initialize an empty sublist to store the calculated parameters for the current trajectory.
    sublist <- list()

    # Calculate the turning angles for the current trajectory in degrees.
    sublist[[1]] <- TrajAngles(data[[i]], compass.direction = 0) * (180 / pi)

    # Calculate the mean turning angle for the current trajectory in degrees.
    sublist[[2]] <- mean(TrajAngles(data[[i]], compass.direction = 0)) * (180 / pi)

    # Calculate the standard deviation of the turning angles for the current trajectory in degrees.
    sublist[[3]] <- sd(TrajAngles(data[[i]], compass.direction = 0)) * (180 / pi)

    # Calculate the total distance traveled for the current trajectory.
    sublist[[4]] <- TrajDistance(data[[i]])

    # Calculate the total length of the trajectory for the current trajectory.
    sublist[[5]] <- TrajLength(data[[i]])

    # Calculate the step lengths (distance between consecutive points) for the current trajectory.
    sublist[[6]] <- TrajStepLengths(data[[i]])

    # Calculate the mean step length for the current trajectory.
    sublist[[7]] <- mean(TrajStepLengths(data[[i]]))

    # Calculate the standard deviation of the step lengths for the current trajectory.
    sublist[[8]] <- sd(TrajStepLengths(data[[i]]))

    # Calculate the sinuosity (a measure of how much a path deviates from a straight line) for the current trajectory.
    sublist[[9]] <- TrajSinuosity2(data[[i]])

    # Calculate the straightness index (a ratio comparing the straight-line distance to the actual path length) for the current trajectory.
    sublist[[10]] <- TrajStraightness(data[[i]])

    # Assign descriptive names to the elements of the sublist for clarity.
    names(sublist) <- c(
      "Turning_angles",
      "Mean_turning_angle",
      "Standard_deviation_turning_angle",
      "Distance",
      "Length",
      "Step_lengths",
      "Mean_step_length",
      "Standard_deviation_step_length",
      "Sinuosity",
      "Straightness"
    )

    # Store the sublist of parameters for the current trajectory in the main list.
    list[[i]] <- sublist
  }

  # Assign descriptive names to each element of the main list, corresponding to each track.
  names(list) <- paste0("Track_", str_pad(1:length(data), nchar(length(data)), pad = "0"), sep = "")

  # Print the final list of track parameters.
  return(list)
}
