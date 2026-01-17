#' Similarity metric using Dynamic Time Warping (DTW)
#'
#' \code{simil_DTW_metric()}  computes similarity metrics between two or more trajectories using
#' Dynamic Time Warping (DTW). It allows for different superposition methods
#' to align trajectories before calculating the DTW metric. The function also supports
#' testing with simulations to calculate *p*-values for the DTW distance metrics.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param test Logical; if \code{TRUE}, the function compares the observed DTW distances against
#' simulated trajectories and calculates *p*-values. Default is \code{FALSE}.
#' @param sim A \code{track simulation} R object consisting of a list of simulated trajectories to use for comparison when \code{test = TRUE}.
#' @param superposition A character string indicating the method used to align trajectories.
#' Options are \code{"None"}, \code{"Centroid"}, or \code{"Origin"}. Default is \code{"None"}.
#'
#' @details
#' The \code{simil_DTW_metric()} function calculates the similarity between trajectories using
#' the Dynamic Time Warping (DTW) algorithm from the \pkg{dtw} package. The \code{dtw()} function
#' is used with the \code{dist.method} argument set to \code{"Euclidean"} for computing the local distances
#' between points in the trajectories.
#'
#' DTW aligns two time series by minimizing the cumulative distance between their points, creating an optimal alignment despite
#' variations in length or temporal distortions. The algorithm constructs a distance matrix where each element represents the cost of aligning
#' points between the two series and finds a warping path through this matrix that minimizes the total distance. The warping path is contiguous and monotonic,
#' starting from the bottom-left corner and ending at the top-right corner (Cleasby et al., 2019).
#'
#' DTW measures are non-negative and unbounded, with larger values indicating greater dissimilarity between the time series. This method has been used in various contexts, including ecological studies to analyze and cluster trajectory data (Cleasby et al., 2019).
#'
#' Potential limitations and biases of DTW include sensitivity to noise and outliers, computational complexity, and the need for appropriate distance metrics.
#' Additionally, DTW may not always account for all structural differences between trajectories and can be biased by the chosen alignment constraints.
#' While DTW can handle trajectories of different lengths due to its elastic nature, having trajectories of similar lengths can
#' improve the accuracy and interpretability of the similarity measure. Similar lengths result in a more meaningful alignment and
#' can make the computation more efficient. When trajectories differ significantly in length, preprocessing or normalization might
#' be necessary, and careful analysis is required to understand the alignment path. The function’s flexibility in handling different
#' lengths allows it to be applied in various contexts. However, large differences in trajectory lengths might introduce potential biases that should be
#' considered when interpreting the results.
#'
#' The function offers three different superposition methods to align the trajectories
#' before \code{DTW()} is applied:
#' \itemize{
#'   \item \code{"None"}: No superposition is applied.
#'   \item \code{"Centroid"}: Trajectories are shifted to align based on their centroids.
#'   \item \code{"Origin"}: Trajectories are shifted to align based on their starting point.
#' }
#'
#' If \code{test = TRUE}, the function can compute *p*-values by comparing the observed DTW
#' distances with those generated from a set of simulated trajectories. The *p*-values
#' are calculated for both individual trajectory pairs and for the entire set of trajectories.
#'
#' @return
#' A \code{track similarity} R object consisting ofa list containing the following elements:
#' \item{DTW_distance_metric}{A matrix containing the pairwise DTW distances between trajectories.}
#' \item{DTW_distance_metric_p_values}{(If \code{test} is \code{TRUE}) A matrix containing the *p*-values for the pairwise DTW distances.}
#' \item{DTW_metric_p_values_combined}{(If \code{test} is \code{TRUE)} The overall *p*-value for the combined DTW distances.}
#' \item{DTW_distance_metric_simulations}{(If \code{test} is \code{TRUE)} A list of DTW distance matrices from each simulated dataset.}
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
#' Cleasby, I. R., Wakefield, E. D., Morrissey, B. J., Bodey, T. W., Votier, S. C., Bearhop, S., & Hamer, K. C. (2019). Using time-series similarity measures to compare animal movement trajectories in ecology. Behavioral Ecology and Sociobiology, 73, 1-19.
#'
#' @examples
#' # Example 1: Simulating tracks using the "Directed" model and comparing DTW distance
#' # in the PaluxyRiver dataset
#' s1 <- simulate_track(PaluxyRiver, nsim = 3, model = "Directed")
#' simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
#'
#' # Example 2: Simulating tracks using the "Constrained" model and comparing DTW distance
#' # in the PaluxyRiver dataset
#' s2 <- simulate_track(PaluxyRiver, nsim = 3, model = "Constrained")
#' simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s2, superposition = "None")
#'
#' # Example 3: Simulating tracks using the "Unconstrained" model and comparing DTW distance
#' # in the PaluxyRiver dataset
#' s3 <- simulate_track(PaluxyRiver, nsim = 3, model = "Unconstrained")
#' simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s3, superposition = "None")
#'
#' # Example 4: Simulating and comparing DTW distance in the MountTom dataset using the
#' # "Centroid" superposition method
#' sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
#' s4 <- simulate_track(sbMountTom, nsim = 3)
#' simil_DTW_metric(sbMountTom, test = TRUE, sim = s4, superposition = "Centroid")
#'
#' # Example 5: Simulating and comparing DTW distance in the MountTom dataset using the
#' # "Origin" superposition method
#' sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
#' s5 <- simulate_track(sbMountTom, nsim = 3)
#' simil_DTW_metric(sbMountTom, test = TRUE, sim = s5, superposition = "Origin")
#'
#' @importFrom dtw dtw
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom schoolmath is.real.positive
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{simulate_track}}, \code{\link[dtw]{dtw}}
#'
#' @export


simil_DTW_metric <- function(data, test = NULL, sim = NULL, superposition = NULL) {

  ## Set default values if arguments are NULL----
  if (is.null(test)) test <- FALSE # Set default if 'test' is NULL
  if (is.null(superposition)) superposition <- "None" # Set default superposition method if 'superposition' is NULL


  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Warn if the 'test' argument is not a boolean
  if (!is.logical(test)) {
    stop("'test' argument should be TRUE or FALSE.")
  }

  # Check if 'sim' is provided when test is TRUE
  if (test == TRUE && is.null(sim)) {
    stop("A 'sim' argument must be provided when 'test' is TRUE.")
  }

  # Check if superposition method is valid
  valid_superpositions <- c("None", "Centroid", "Origin")
  if (!superposition %in% valid_superpositions) {
    stop("Invalid 'superposition' argument. One of 'None', 'Centroid', or 'Origin' must be chosen.")
  }

  # If 'sim' is provided, ensure it is a list and has the same structure as 'data'
  if (!is.null(sim)) {
    if (!is.list(sim)) {
      stop("The 'sim' argument must be a list.")
    }

    # Check that 'sim' contains the same number of tracks as 'data'
    if (length(sim[[1]]) != length(data[[1]])) {
      stop("The 'sim' list must have the same number of trajectories as 'data'.")
    }
  }


  ## Code----
  data <- data[[1]]

  # Calculate actual metrics
  Matrixsim <- data.frame(matrix(nrow = length(data), ncol = length(data)))
  colnames(Matrixsim) <- names(data)
  rownames(Matrixsim) <- names(data)
  DTW <- Matrixsim

  ## Superposition
  if (test == TRUE) {
    if (superposition == "Centroid") {
      for (i in 1:length(data)) {
        data[[i]][, 1] <- data[[i]][, 1] - mean(data[[i]][, 1])
        data[[i]][, 2] <- data[[i]][, 2] - mean(data[[i]][, 2])
      }
    }

    if (superposition == "Origin") {
      for (i in 1:length(data)) {
        data[[i]][, 1] <- data[[i]][, 1] - data[[i]][1, 1]
        data[[i]][, 2] <- data[[i]][, 2] - data[[i]][1, 2]
      }
    }

    if (superposition == "None") {
      data <- data
    }
  }


  for (c in 1:length(data)) {
    for (r in 1:length(data)) {
      if (c <= r) next
      DTW[r, c] <- dtw(as.matrix(data[[r]][, 1:2]), as.matrix(data[[c]][, 1:2]), distance.only = TRUE)$distance
    }
  }


  if (test == TRUE) {
    # Tests
    ## Superposition
    if (superposition == "Centroid") {
      for (i in 1:length(sim)) {
        for (j in 1:length(sim[[1]])) {
          sim[[i]][[j]][, 1] <- sim[[i]][[j]][, 1] - mean(sim[[i]][[j]][, 1])
          sim[[i]][[j]][, 2] <- sim[[i]][[j]][, 2] - mean(sim[[i]][[j]][, 2])
        }
      }
    }

    if (superposition == "Origin") {
      for (i in 1:length(sim)) {
        for (j in 1:length(sim[[1]])) {
          sim[[i]][[j]][, 1] <- sim[[i]][[j]][, 1] - sim[[i]][[j]][1, 1]
          sim[[i]][[j]][, 2] <- sim[[i]][[j]][, 2] - sim[[i]][[j]][1, 2]
        }
      }
    }

    if (superposition == "None") {
      sim <- sim
    }

    listSIM <- list()
    for (i in 1:length(sim)) {
      listSIM[[i]] <- bind_rows(sim[[i]])
    }
    sim <- listSIM

    ## Calculate simulated metrics
    nsim <- length(sim)
    DTWsim <- Matrixsim

    listDTW <- list()
    listnegDTW <- c()

    for (i in 1:nsim) {
      sim[[i]]$Trajectory <- as.factor(sim[[1]]$Trajectory)
      levels <- levels(sim[[i]]$Trajectory)

      for (c in 1:length(data)) {
        for (r in 1:length(data)) {
          if (c <= r) next
          DTWsim[r, c] <- dtw(as.matrix(sim[[i]][which(sim[[i]]$Trajectory == levels[r]), 1:2]), as.matrix(sim[[i]][which(sim[[i]]$Trajectory == levels[c]), 1:2]), distance.only = TRUE)$distance
        }
      }
      listDTW[[i]] <- DTWsim

      positive <- c(as.matrix(DTW - listDTW[[i]]))
      positive <- positive[!is.na(positive)]
      listnegDTW[i] <- all(is.real.positive(positive))

      message(paste(Sys.time(), paste("Iteration", i)))
      message(" ")
      message("DTW metric")
      DTWsim
      message("------------------------------------")
      if (i == nsim) {
        message("ANALYSIS COMPLETED")
        message("------------------------------------")
        message(" ")
      }
    }


    ## Calculate p-values
    DTWsim_pval <- Matrixsim


    vector <- c()
    for (c in 1:length(data)) {
      for (r in 1:length(data)) {
        if (c <= r) next
        for (i in 1:nsim) {
          vector[i] <- listDTW[[i]][r, c]
          DTWsim_pval[r, c] <- length(which(vector <= DTW[r, c])) / nsim
        }
      }
    }


    DTW_together_pval <- length(which(listnegDTW == TRUE)) / nsim
  }

  # Value
  if (test == TRUE) {
    list <- list()
    list[[1]] <- DTW
    list[[2]] <- DTWsim_pval
    list[[3]] <- DTW_together_pval
    list[[4]] <- listDTW

    names(list) <- c("DTW_distance_metric", "DTW_distance_metric_p_values", "DTW_metric_p_values_combined", "DTW_distance_metric_simulations")
    return(list)
  }

  if (test == FALSE) {
    list <- list()
    list[[1]] <- DTW
    list[[2]] <- DTW

    names(list) <- c("DTW_distance_metric")
    return(list)
  }
}
