#' Cluster tracks based on movement parameters
#'
#' \code{cluster_track()} clusters trajectories based on various movement and velocity parameters calculated for each track.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param veltrack A \code{track velocity} R object consisting of a list of lists, where each sublist contains the computed parameters for a corresponding track.
#' @param variables A character vector specifying the movement parameters to be used in the clustering analysis. Valid parameter names include: \code{"TurnAng"}, \code{"sdTurnAng"}, \code{"Distance"}, \code{"Length"}, \code{"StLength"}, \code{"sdStLength"}, \code{"Sinuosity"}, \code{"Straightness"}, \code{"Velocity"}, \code{"sdVelocity"}, \code{"MaxVelocity"}, \code{"MinVelocity"}.
#'
#' @details
#' The \code{cluster_track()} function performs a model-based clustering analysis on track parameters using the \code{Mclust()} function from the \pkg{mclust} package.
#'
#' The function first filters out tracks with fewer than four steps, as these tracks may not provide reliable movement data.
#' It then calculates various movement parameters for each remaining track, including turning angles, distances, lengths, sinuosity, straightness, and velocities.
#' Finally, the selected movement parameters are used as input for clustering the tracks.
#'
#' If only one parameter is selected, the clustering is performed using equal variance (\code{"E"}) and variable variance (\code{"V"}) Gaussian models.
#' If more than one parameter is selected, all Gaussian models available in \code{mclust.options("emModelNames")} are considered.
#'
#' The following movement parameters can be included in the clustering:
#'    * \code{"TurnAng"}: Turning angles for the track, measured in degrees. This measures how much the direction of movement changes at each step.
#'    * \code{"sdTurnAng"}: The standard deviation of the turning angles, indicating how variable the turning angles are across the track.
#'    * \code{"Distance"}: The total distance covered by the track, calculated as the sum of the straight-line distances between consecutive points (in meters).
#'    * \code{"Length"}: The overall length of the track, a straight-line distance between the starting and ending points (in meters).
#'    * \code{"StLength"}: Step lengths for each step of the track, representing how far the object moved between two consecutive points (in meters).
#'    * \code{"sdStLength"}: The standard deviation of the step lengths, showing how consistent the steps are in length.
#'    * \code{"Sinuosity"}: A measure of the track's winding nature, calculated as the ratio of the actual track length to the straight-line distance (dimensionless).
#'    * \code{"Straightness"}: The straightness of the track, calculated as the straight-line distance divided by the total path length (dimensionless).
#'    * \code{"Velocity"}: The average velocity of the track, calculated as the total distance divided by the time elapsed between the first and last footprint (in meters per second).
#'    * \code{"sdVelocity"}: The standard deviation of the velocity, indicating how much the velocity fluctuates throughout the track.
#'    * \code{"MaxVelocity"}: The maximum velocity achieved during the track, identifying the fastest point (in meters per second).
#'    * \code{"MinVelocity"}: The minimum velocity during the track, identifying the slowest point (in meters per second).
#'
#' The \code{cluster_track()} function has biological relevance in identifying groups of tracks with similar movement parameters,
#' providing insights into ecological and behavioral patterns. By clustering tracks based on characteristics such as sinuosity,
#' velocity, and turning angles, it allows detecting movement patterns associated with specific behaviors. This can help identify tracks
#' potentially made by individuals moving together, which is useful for investigating hypotheses on gregarious behavior, predation
#' strategies, or coordinated movement. Additionally, clustering serves as a preliminary step before similarity tests and
#' simulations, refining track selection and improving hypothesis testing in movement ecology studies.
#'
#' @return
#' A \code{track clustering} R object consisting of a list containing the following elements:
#'   * \code{matrix}: A data frame containing the movement parameters calculated for each track.
#'   * \code{clust}: An \code{Mclust} object containing the results of the model-based clustering analysis. This object provides the optimal (according to BIC) mixture model estimation. The output components are:
#'     * \code{call}: The matched call.
#'     * \code{data}: The input data matrix.
#'     * \code{modelName}: A character string denoting the model at which the optimal BIC occurs.
#'     * \code{n}: The number of observations in the data.
#'     * \code{d}: The dimension of the data.
#'     * \code{G}: The optimal number of mixture components.
#'     * \code{BIC}: All BIC values.
#'     * \code{loglik}: The log-likelihood corresponding to the optimal BIC.
#'     * \code{df}: The number of estimated parameters.
#'     * \code{bic}: BIC value of the selected model.
#'     * \code{icl}: ICL value of the selected model.
#'     * \code{hypvol}: The hypervolume parameter for the noise component if required, otherwise set to NULL.
#'     * \code{parameters}: A list with the following components:
#'       * \code{pro}: A vector whose k\eqn{^{th}} component is the mixing proportion for the k\eqn{^{th}} component of the mixture model. If missing, equal proportions are assumed.
#'       * \code{mean}: The mean for each component. If there is more than one component, this is a matrix whose k\eqn{^{th}} column is the mean of the k\eqn{^{th}} component of the mixture model.
#'     * \code{variance}: A list of variance parameters for the model. The components of this list depend on the model specification. See the help file for mclustVariance for details.
#'     * \code{z}: A matrix whose i,k\eqn{^{th}} entry is the probability that observation i in the test data belongs to the k\eqn{^{th}} class.
#'     * \code{classification}: The classification corresponding to z, i.e., map(z).
#'     * \code{uncertainty}: The uncertainty associated with the classification.
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
#'  Scrucca L., Fop M., Murphy T. B., & Raftery A. E. (2016) mclust 5: clustering, classification and density estimation using Gaussian finite mixture models. The R Journal, 8(1), 289-317.
#'
#' @examples
#' # Example 1: Cluster MountTom tracks using TurnAng and Velocity
#' H_mounttom <- c(
#'   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
#'   1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784,
#'   1.676, 1.872, 1.648, 1.760, 1.612
#' ) # Hip heights for MountTom tracks
#' veltrack_MountTom <- velocity_track(MountTom, H = H_mounttom)
#' result1 <- cluster_track(MountTom, veltrack_MountTom,
#'   variables = c("TurnAng", "Velocity")
#' )
#' result1$clust$classification
#'
#' # Example 2: Cluster MountTom tracks using Sinuosity and Step Length
#' result2 <- cluster_track(MountTom, veltrack_MountTom,
#'   variables = c("Sinuosity", "StLength")
#' )
#' plot(result2$clust)
#'
#' # Example 3: Cluster MountTom tracks using Maximum and Minimum Velocity
#' result3 <- cluster_track(MountTom, veltrack_MountTom,
#'   variables = c("MaxVelocity", "MinVelocity")
#' )
#' result3$clust$classification
#'
#' # Example 4: Cluster MountTom tracks using Straightness
#' result4 <- cluster_track(MountTom, veltrack_MountTom, variables = "Straightness")
#' result4$clust$classification
#'
#' # Example 5: Cluster PaluxyRiver tracks using Distance and Straightness
#' H_paluxyriver <- c(3.472, 2.200) # Hip heights for PaluxyRiver tracks
#' Method_paluxyriver <- c("A", "B") # Different methods for different tracks
#' veltrack_PaluxyRiver <- velocity_track(PaluxyRiver,
#'   H = H_paluxyriver,
#'   method = Method_paluxyriver
#' )
#' result5 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver,
#'   variables = c("Distance", "Straightness")
#' )
#' result5$matrix
#' result5$clust$classification
#'
#' # Example 6: Cluster PaluxyRiver tracks using Length and SD of Velocity
#' result6 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver,
#'   variables = c("Length", "sdVelocity")
#' )
#' plot(result6$clust)
#'
#' # Example 7: Cluster PaluxyRiver tracks using TurnAng and SD of TurnAng
#' result7 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver,
#'   variables = c("TurnAng", "sdTurnAng")
#' )
#' result7$clust$classification
#'
#' # Example 8: Cluster PaluxyRiver tracks using Sinuosity
#' result8 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver,
#'   variables = c("Sinuosity")
#' )
#' result8$clust$classification
#'
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#' @importFrom mclust mclust.options
#'
#' @seealso \code{\link{track_param}}, \code{\link{velocity_track}}, \code{\link[mclust]{Mclust}}
#'
#' @export

cluster_track <- function(data, veltrack, variables) {

  ## Set default values if arguments are NULL----

  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check if 'variables' is a non-empty vector
  if (length(variables) == 0) {
    stop("Error: No movement parameters specified for clustering. Please provide a valid vector of variables.")
  }

  # Check if 'data[[1]]' has any tracks
  if (length(data[[1]]) == 0) {
    stop("Error: No tracks available for clustering. Please check the input data.")
  }

  # Identify tracks with fewer than 4 steps
  list <- sapply(data[[1]], function(track) nrow(track) < 4)

  # Count the discarded tracks
  discarded_tracks <- which(list)

  # Check if any tracks were discarded and issue a warning
  if (length(discarded_tracks) > 0) {
    warning(paste(length(discarded_tracks), "tracks were discarded for having fewer than 4 footprints. Discarded track indices:", paste(discarded_tracks, collapse = ", ")))
  }

  ## Code----

  # Identify tracks with fewer than 4 steps
  list <- sapply(data[[1]], function(track) nrow(track) < 4)

  # Subset data and H using valid tracks
  valid_indices <- which(!list)
  data <- subset_track(data, valid_indices)
  veltrack <- veltrack[valid_indices]

  # Calculate movement parameters and velocity
  data_param <- track_param(data)

  # Create a results matrix
  matrix <- data.frame(matrix(nrow = length(data[[1]]), ncol = 12))
  colnames(matrix) <- c(
    "TurnAng", "sdTurnAng", "Distance", "Length", "StLength", "sdStLength",
    "Sinuosity", "Straightness", "Velocity", "sdVelocity", "MaxVelocity", "MinVelocity"
  )
  rownames(matrix) <- paste0("Track ", valid_indices)

  for (i in 1:length(data[[1]])) {
    matrix[i, 1] <- data_param[[i]]$Mean_turning_angle
    matrix[i, 2] <- data_param[[i]]$Standard_deviation_turning_angle
    matrix[i, 3] <- data_param[[i]]$Distance
    matrix[i, 4] <- data_param[[i]]$Length
    matrix[i, 5] <- data_param[[i]]$Mean_step_length
    matrix[i, 6] <- data_param[[i]]$Standard_deviation_step_length
    matrix[i, 7] <- data_param[[i]]$Sinuosity
    matrix[i, 8] <- data_param[[i]]$Straightness
    matrix[i, 9] <- veltrack[[i]]$Mean_velocity
    matrix[i, 10] <- veltrack[[i]]$Standard_deviation_velocity
    matrix[i, 11] <- veltrack[[i]]$Maximum_velocity
    matrix[i, 12] <- veltrack[[i]]$Minimum_velocity
  }

  # Subset matrix based on selected variables
  matrixsub <- matrix[, colnames(matrix) %in% variables, drop = FALSE]

  # Perform model-based clustering
  if (ncol(matrixsub) == 1) {
    clust <- Mclust(as.matrix(matrixsub), G = 1:length(data[[1]]), modelNames = c("E", "V"))
  } else {
    clust <- Mclust(as.matrix(matrixsub), G = 1:length(data[[1]]), modelNames = mclust.options("emModelNames"))
  }

  # Return results
  return(list(matrix = matrix, clust = clust))
}
