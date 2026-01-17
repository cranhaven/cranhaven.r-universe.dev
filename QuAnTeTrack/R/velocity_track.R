#' Calculate velocities and relative stride lengths for tracks
#'
#' \code{velocity_track()} calculates the velocities and relative stride lengths for each step in a series of tracks, based on the step length, height at the hip, and gravity acceleration.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param H A numeric vector representing the height at the hip (in meters) for each track maker. The length of this vector should match the number of tracks in the data.
#' @param G Gravity acceleration (in meters per second squared). Default is \code{9.8}.
#' @param method A character vector specifying the method to calculate velocities for each track. Method \code{"A"} follows the approach from Alexander (1976), while method \code{"B"} is based on Ruiz & Torices (2013). If \code{NULL}, method \code{"A"} will be used for all tracks.
#'
#' @details
#' The \code{velocity_track()} function calculates velocities using two methods:
#'
#' **Method A**: Based on Alexander (1976), with the formula:
#' \deqn{v = 0.25 \cdot \sqrt{G} \cdot S^{1.67} \cdot H^{-1.17}}{v = 0.25 * sqrt(G) * S^1.67 * H^-1.17}
#' - **v**: Velocity of the track-maker (in meters per second).
#' - **G**: Acceleration due to gravity (in meters per second squared), typically \eqn{9.81\ \text{m/s}^2}.
#' - **S**: Stride length, which is the distance between consecutive footprints (in meters).
#' - **H**: Height at the hip of the track-maker (in meters).
#' - The coefficients \eqn{0.25}, \eqn{1.67}, and \eqn{-1.17} are derived from empirical studies. These coefficients adjust the formula to account for different animal sizes and gaits.
#'
#' This method applies to a wide range of terrestrial vertebrates and is used to estimate velocity across different gaits.
#'
#' **Method B**: Based on Ruiz & Torices (2013), with the formula:
#' \deqn{v = 0.226 \cdot \sqrt{G} \cdot S^{1.67} \cdot H^{-1.17}}{v = 0.226 * sqrt(G) * S^1.67 * H^-1.17}
#' - **v**: Velocity of the track-maker (in meters per second).
#' - **G**: Acceleration due to gravity (in meters per second squared), typically \eqn{9.81\ \text{m/s}^2}.
#' - **S**: Stride length (in meters).
#' - **H**: Height at the hip of the track-maker (in meters).
#' - The oefficient \eqn{0.226} in method B is a refinement based on updated data for bipedal locomotion.
#'
#' Based on Thulborn & Wade (1984), it is possible to identify the gaits of track-makers on the basis of relative stride length, as follows:
#' - **Walk**: \eqn{A/H < 2.0}; locomotor performance equivalent to walking in mammals.
#' - **Trot**: \eqn{2.0 \leq A/H \leq 2.9}; locomotor performance equivalent to trotting or racking in mammals.
#' - **Run**: \eqn{A/H > 2.9}; locomotor performance equivalent to cantering, galloping, or sprinting in mammals.
#'
#' @return A \code{track velocity} R object consisting of a list of lists, where each sublist contains the computed parameters for a corresponding track.
#' The parameters included are:
#'   * \code{Step_velocities}: A vector of velocities for each step in the track (in meters per second).
#'   * \code{Mean_velocity}: The mean velocity across all steps in the track (in meters per second).
#'   * \code{Standard_deviation_velocity}: The standard deviation of velocities across all steps in the track (in meters per second).
#'   * \code{Maximum_velocity}: The maximum velocity among all steps in the track (in meters per second).
#'   * \code{Minimum_velocity}: The minimum velocity among all steps in the track (in meters per second).
#'   * \code{Step_relative_stride}: A vector of relative stride lengths for each step in the track (dimensionless).
#'   * \code{Mean_relative_stride}: The mean relative stride length across all steps in the track (dimensionless).
#'   * \code{Standard_deviation_relative_stride}: The standard deviation of relative stride lengths across all steps in the track (dimensionless).
#'   * \code{Maximum_relative_stride}: The maximum relative stride length among all steps in the track (dimensionless).
#'   * \code{Minimum_relative_stride}: The minimum relative stride length among all steps in the track (dimensionless).
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
#' Alexander, R. M. (1976). Estimates of speeds of dinosaurs. Nature, 261(5556), 129-130.
#'
#' Ruiz, J., & Torices, A. (2013). Humans running at stadiums and beaches and the accuracy of speed estimations from fossil trackways. Ichnos, 20(1), 31-35.
#'
#' Thulborn, R. A., & Wade, M. (1984). Dinosaur trackways in the Winton Formation (mid-Cretaceous) of Queensland. Memoirs of the Queensland Museum, 21(2), 413-517.
#'
#' @examples
#' # Example 1: Calculate velocities for the MountTom dataset using default settings.
#' # H_mounttom contains hip heights for each track in the MountTom dataset.
#' # The function will use the default method "A" for all tracks.
#' # Hip heights are inferred as four times the footprint length, following Alexander's approach.
#' H_mounttom <- c(
#'   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
#'   1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
#'   1.648, 1.760, 1.612
#' )
#' velocity_track(MountTom, H = H_mounttom)
#'
#' # Example 2: Calculate velocities for the PaluxyRiver dataset using default settings.
#' # H_paluxyriver contains hip heights for each track in the PaluxyRiver dataset.
#' # The function will use the default method "A" for all tracks.
#' # Hip heights are inferred as four times the footprint length, following Alexander's approach.
#' H_paluxyriver <- c(3.472, 2.200)
#' velocity_track(PaluxyRiver, H = H_paluxyriver)
#'
#' # Example 3: Calculate velocities for the PaluxyRiver dataset using different methods
#' # for velocity calculation. Method "A" is used for sauropods, which is more
#' # appropriate for quadrupedal dinosaurs. Method "B" is used for theropods, which
#' # is more appropriate for bipedal dinosaurs. Hip heights are inferred as four times
#' # the footprint length, following Alexander's approach.
#' H_paluxyriver <- c(3.472, 2.200)
#' Method_paluxyriver <- c("A", "B")
#' velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)
#'
#' @importFrom stringr str_pad
#'
#' @seealso \code{\link{tps_to_track}}
#'
#' @export

velocity_track <- function(data,
                           H, # height at the hip (m)
                           G = NULL, # gravity acceleration (m/s^2)
                           method = NULL # formula to calculate speed for each track
) {

  ## Set default values if arguments are NULL----
  if (is.null(G)) G <- 9.8 # Set default Gravity acceleration if 'G' is NULL
  if (is.null(method)) method <- c(rep("A", length(data[[1]]))) # Set default method to calculate speed if 'method' is NULL

  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check if H is a numeric vector
  if (!is.numeric(H) || any(is.na(H))) {
    stop("Error: 'H' must be a numeric vector with valid values.")
  }

  # Check if H length matches number of tracks in data
  if (length(H) != length(data[[1]])) {
    stop("Error: Length of 'H' must match the number of tracks in 'data'.")
  }

  # Check if G is a positive numeric value
  if (!is.null(G) && (!is.numeric(G) || G <= 0)) {
    stop("Error: 'G' must be a positive numeric value.")
  }

  # Check if 'method' is valid
  if (!is.null(method)) {
    if (!all(method %in% c("A", "B"))) {
      stop("Error: 'method' must be a character vector containing only 'A' or 'B'.")
    }
    if (length(method) != length(data[[1]])) {
      stop("Error: Length of 'method' must match the number of tracks in 'data'.")
    }
  }

  ## Code----

  # Extract the track data (assuming the first element of 'data' is the list of tracks)
  data <- data[[1]]

  # Function to calculate Euclidean distance between two points
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2)^2))

  # Initialize a list to store results for each track
  list <- list()

  # Loop through each track
  for (j in 1:length(data)) {
    veltrack <- c()
    relstride <- c()

    # Loop through each step in the track
    if (method[j] == "A") {
      for (i in 1:(length(data[[j]][, 1]) - 1)) {
        S <- euc.dist(data[[j]][i, 1:2], data[[j]][i + 1, 1:2]) * 2
        veltrack[i] <- 0.25 * (G^0.5) * (S^1.67) * (H[j]^-1.17) # Update veltrack[i]
      }
    }

    if (method[j] == "B") {
      for (i in 1:(length(data[[j]][, 1]) - 1)) {
        S <- euc.dist(data[[j]][i, 1:2], data[[j]][i + 1, 1:2]) * 2
        veltrack[i] <- 0.226 * (G^0.5) * (S^1.67) * (H[j]^-1.17) # Update veltrack[i]
      }
    }

    for (i in 1:(length(data[[j]][, 1]) - 1)) {
      S <- euc.dist(data[[j]][i, 1:2], data[[j]][i + 1, 1:2]) * 2
      relstride[i] <- S / H[j] # Update relstride[i]
    }

    # Store calculated velocities and summary statistics in a sublist
    sublist <- list()
    sublist[[1]] <- veltrack
    sublist[[2]] <- mean(veltrack)
    sublist[[3]] <- sd(veltrack)
    sublist[[4]] <- max(veltrack)
    sublist[[5]] <- min(veltrack)
    sublist[[6]] <- relstride
    sublist[[7]] <- mean(relstride)
    sublist[[8]] <- sd(relstride)
    sublist[[9]] <- max(relstride)
    sublist[[10]] <- min(relstride)
    names(sublist) <- c("Step_velocities", "Mean_velocity", "Standard_deviation_velocity", "Maximum_velocity", "Minimum_velocity", "Step_relative_stride", "Mean_relative_stride", "Standard_deviation_relative_stride", "Maximum_relative_stride", "Minimum_relative_stride")

    # Add the sublist to the main list
    list[[j]] <- sublist
  }

  # Name each element in the main list according to the track number
  names(list) <- paste0("Track_", str_pad(1:length(data), nchar(length(data)), pad = "0"), sep = "")

  # Print the result
  return(list)
}
