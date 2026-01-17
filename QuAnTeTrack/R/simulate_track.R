#' Simulate tracks using different models
#'
#' \code{simulate_track()} simulates movement trajectories based on an original set of tracks. Three movement models are available for simulation, each reflecting different levels of constraint in movement patterns. These models can represent biological or environmental constraints, such as movement along coastlines, rivers, or towards resources like water or food.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param nsim The number of simulations to run. Defaults to \code{1000} if not specified.
#' @param model The type of movement model to use. Options are \code{"Directed"}, \code{"Constrained"}, or \code{"Unconstrained"}. Defaults to \code{"Unconstrained"} if not provided.
#'
#' @details
#' This function simulates movement trajectories based on the following models:
#'
#' - **Directed**: This model simulates movement that follows a specific direction navigating with a compass (i.e., a directed walk/allothetic directed walk/oriented path) (Cheung et al., 2007, 2008). The trajectory is constrained by both the angular and linear properties of the original track, with minor deviations allowed to reflect natural variability.
#'     - **Angular constraints**: The trajectory closely follows a specific direction, maintaining the overall angular orientation of the original track. Deviations of consecutive steps are governed by the angular standard deviation calculated from the original track using \code{TrajAngles()}.
#'     - **Linear constraints**: Step lengths are constrained to the mean of the original track's step lengths, with variability allowed according to the standard deviation of step lengths computed with \code{TrajStepLengths()}.
#'     - **Starting direction**: Fixed to the original direction (overall angular orientation) of the track.
#'
#' This model is ideal for simulating movement directed toward a specific resource or constrained by natural barriers, with a relatively fixed direction and minor deviations.
#'
#' - **Constrained**: This model simulates movement that correspond to a correllated random walk/idiothetic directed walk (Kareiva & Shigesada, 1983), corresponding to an animal navigating without a compass (Cheung et al., 2008), while still maintaining certain angular and linear characteristics of the original track. It provides more flexibility than the Directed model but is not entirely random like the Unconstrained model.
#'     - **Angular constraints**: The trajectory does not follow a specific direction. Deviations of consecutive steps are governed by the angular standard deviation calculated from the original track using \code{TrajAngles()}.
#'     - **Linear constraints**: Step lengths are constrained to the mean of the original track's step lengths, with variability allowed according to the standard deviation of step lengths computed with \code{TrajStepLengths()}.
#'     - **Starting direction**: Fixed to the original direction (overall angular orientation) of the track.
#'
#' This model is suitable for scenarios where movement is influenced by external constraints but allows for some degree of random exploration.
#'
#' - **Unconstrained**: This model simulates movement that correspond to a correllated random walk/idiothetic directed walk (Kareiva & Shigesada, 1983), corresponding to an animal navigating without a compass (Cheung et al., 2008), while still maintaining certain angular and linear characteristics of the original track.
#'     - **Angular constraints**: The trajectory does not follow a specific direction. Deviations of consecutive steps are governed by the angular standard deviation calculated from the original track using \code{TrajAngles()}.
#'     - **Linear constraints**: Step lengths are constrained to the mean of the original track's step lengths, with variability allowed according to the standard deviation of step lengths computed with \code{TrajStepLengths()}.
#'     - **Starting direction**: Randomly determined.
#'
#' This model is suitable for simulating exploratory or dispersal behavior in open environments, where movement is random and not influenced by specific constraints.
#'
#' Note: Simulations cannot be applied to trajectories with fewer than four steps as the standard deviations of angles and step lengths cannot be computed for such short trajectories. Consider using the \code{subset_track()} function to filter tracks with four or more steps.
#'
#' The function utilizes the \pkg{trajr} package for key calculations:
#'
#' - \code{TrajGenerate()}: Generates a new trajectory based on random or directed movement models, constrained by specified parameters.
#' - \code{TrajStepLengths()}: Calculates the step lengths (distances between consecutive points) of the original trajectory.
#' - \code{TrajAngles()}: Computes the angles between consecutive segments of the trajectory, used to maintain directional movement in constrained models.
#' - \code{TrajRotate()}: Rotates the trajectory by a specified angle to match the original direction or a random angle for unconstrained models.
#' - \code{TrajTranslate()}: Translates the simulated trajectory to start at the same geographic location as the original.
#'
#' The \code{NISTdegTOradian()} function from the \pkg{NISTunits} package is used to convert angles from degrees to radians.
#'
#' @return A \code{track simulation} R object consisting of a list of simulated trajectories stored as \code{track} R objects.
#'
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
#' Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2007). Animal navigation: the difficulty of moving in a straight line. Biological cybernetics, 97, 47-61.
#'
#' Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2008). Animal navigation: general properties of directed walks. Biological cybernetics, 99, 197-217.
#'
#' @examples
#' # Example 1: Simulate tracks using data from the Paluxy River
#' # Default model (Unconstrained movement)
#' simulated_tracks <- simulate_track(PaluxyRiver, nsim = 3)
#'
#' # Example 2: Simulate tracks using the "Directed" model, representing movement
#' # toward a resource (e.g., water source)
#' simulated_tracks_directed <- simulate_track(PaluxyRiver, nsim = 3, model = "Directed")
#'
#' # Example 3: Simulate tracks using the "Constrained" model, representing movement
#' # along a geographic feature (e.g., coastline)
#' simulated_tracks_constrained <- simulate_track(PaluxyRiver, nsim = 3, model = "Constrained")
#'
#' # Example 4: Simulate tracks using the "Unconstrained" model (random exploratory movement)
#' simulated_tracks_unconstrained <- simulate_track(PaluxyRiver, nsim = 3, model = "Unconstrained")
#'
#' # Subsetting trajectories with four or more steps in the MountTom dataset
#' sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
#'
#' # Example 5: Simulate tracks using data from Mount Tom
#' # Default model (Unconstrained movement)
#' simulated_tracks_mt <- simulate_track(sbMountTom, nsim = 3)
#'
#' # Example 6: Simulate tracks using the "Directed" model for Mount Tom, representing
#' # directed movement
#' simulated_tracks_mt_directed <- simulate_track(sbMountTom, nsim = 3, model = "Directed")
#'
#' # Example 7: Simulate tracks using the "Constrained" model for Mount Tom, representing
#' # constrained movement
#' simulated_tracks_mt_constrained <- simulate_track(sbMountTom, nsim = 3, model = "Constrained")
#'
#' # Example 8: Simulate tracks using the "Unconstrained" model for Mount Tom, representing
#' # random exploratory movement
#' simulated_tracks_mt_unconstrained <- simulate_track(sbMountTom, nsim = 3, model = "Unconstrained")
#'
#' @importFrom trajr TrajGenerate TrajStepLengths TrajAngles TrajRotate TrajTranslate
#' @importFrom stats runif sd
#' @importFrom NISTunits NISTdegTOradian
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{plot_sim}}, \code{\link{subset_track}}, \code{\link[trajr]{TrajGenerate}}, \code{\link[trajr]{TrajStepLengths}}, \code{\link[trajr]{TrajAngles}}, \code{\link[trajr]{TrajRotate}}, \code{\link[trajr]{TrajTranslate}}, \code{\link[NISTunits]{NISTdegTOradian}}
#'
#' @export

simulate_track <- function(data, nsim = NULL, model = NULL) {

  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check if nsim is a valid positive integer
  if (is.null(nsim)) {
    warning("`nsim` is NULL. Using default value of 1000.")
  } else if (!is.numeric(nsim) || nsim != as.integer(nsim) || nsim <= 0) {
    stop("Error: `nsim` must be a positive integer.")
  }

  # Validate the model parameter
  valid_models <- c("Directed", "Constrained", "Unconstrained")
  if (is.null(model)) {
    warning("`model` is NULL. Defaulting to 'Unconstrained'.")
    model <- "Unconstrained"
  } else if (!(model %in% valid_models)) {
    stop("Error: Invalid `model` specified.")
  }

  # Check if the dataset contains tracks with fewer than three steps
  check_rows <- sapply(data[[1]], function(df) nrow(df) < 4)
  if (any(check_rows)) {
    indices <- which(check_rows)
    stop(paste("The following tracks have fewer than four steps:", paste(indices, collapse = ", "), ". Simulations cannot be applied to trajectories with fewer than four steps, as standard deviations of angles and step lengths cannot be computed. Consider using the function 'subset_track()' to filter only tracks with four or more steps."))
  }

  ## Set default values if arguments are NULL----
  if (is.null(nsim)) nsim <- 1000 # Default number of simulations set to 1000 if 'nsim' is NULL
  if (is.null(model)) model <- "Unconstrained" # Default model type set to "Unconstrained" if 'model' is NULL

  ## Code----

  # Extract the trajectory data from the input list
  data <- data[[1]]

  # Initialize an empty list to store the results of all simulations
  list2 <- list()

  # Loop through the number of simulations
  for (u in 1:nsim) {
    # Initialize a list to store each simulation's individual trajectories
    list <- list()

    # Loop through each trajectory in the original dataset
    for (i in 1:length(data)) {
      # Calculate the overall direction (angle) of the trajectory using the atan2 function
      # This computes the angle of the line between the first and last point of the trajectory
      double <- atan2(
        data[[i]][, 2][length(data[[i]][, 2])] - data[[i]][, 2][1],
        data[[i]][, 1][length(data[[i]][, 1])] - data[[i]][, 1][1]
      )

      # Simulate a trajectory based on the chosen model
      if (model == "Directed") {
        trajsim <- TrajGenerate(
          n = length(data[[i]][, 1]) - 1,
          random = FALSE,
          stepLength = mean(TrajStepLengths(data[[i]])),
          angularErrorSd = sd(TrajAngles(data[[i]], compass.direction = double)),
          linearErrorSd = sd(TrajStepLengths(data[[i]]))
        )
      }

      if (model == "Constrained") {
        trajsim <- TrajGenerate(
          n = length(data[[i]][, 1]) - 1,
          random = TRUE,
          stepLength = mean(TrajStepLengths(data[[i]])),
          angularErrorSd = sd(TrajAngles(data[[i]])),
          linearErrorSd = sd(TrajStepLengths(data[[i]]))
        )
      }

      if (model == "Unconstrained") {
        trajsim <- TrajGenerate(
          n = length(data[[i]][, 1]) - 1,
          random = TRUE,
          stepLength = mean(TrajStepLengths(data[[i]])),
          angularErrorSd = sd(TrajAngles(data[[i]])),
          linearErrorSd = sd(TrajStepLengths(data[[i]]))
        )
      }

      # Rotate the simulated trajectory to match the original trajectory direction
      if (model == "Directed" || model == "Constrained") {
        trajsim <- TrajRotate(trajsim, angle = double, relative = FALSE)
      }

      # For "Unconstrained", rotate by a random angle between 0 and 360 degrees
      if (model == "Unconstrained") {
        trajsim <- TrajRotate(trajsim, angle = NISTdegTOradian(runif(1, 0.0001, 360)), relative = FALSE)
      }

      # Translate the simulated trajectory to start at the same point as the original
      trajsim <- TrajTranslate(trajsim, data[[i]][1, 1], data[[i]][1, 2], dt = 0)

      # Add metadata to the simulated trajectory: trajectory ID, simulation ID, and a combined ID
      trajsim$Trajectory <- rep(paste("Traj_", i, sep = ""), length(trajsim[, 1]))
      trajsim$Simulation <- rep(paste("Sim_", u, sep = ""), length(trajsim[, 1]))
      trajsim$TrajSim <- rep(paste("TrajSim_", i, "_", u, sep = ""), length(trajsim[, 1]))

      # Store the simulated trajectory in the list for this simulation
      list[[i]] <- trajsim
    }

    # Store all the simulated trajectories for this iteration
    list2[[u]] <- list
  }

  # Return the list of all simulated trajectories
  return(list2)
}
