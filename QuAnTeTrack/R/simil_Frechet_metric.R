#' Similarity metric using Fréchet distance
#'
#' \code{simil_Frechet_metric()} computes similarity metrics between two or more trajectories using
#' the Fréchet distance. It allows for different superposition methods
#' to align trajectories before calculating the Fréchet distance  metrics. The function also supports
#' testing with simulations to calculate *p*-values for the Fréchet distance metrics.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param test Logical; if \code{TRUE}, the function compares the observed Fréchet distances against
#' simulated trajectories and calculates *p*-values. Default is \code{FALSE}.
#' @param sim A \code{track simulation} R object consisting of a list of simulated trajectories to use for comparison when \code{test = TRUE}.
#' @param superposition A character string indicating the method used to align trajectories.
#' Options are \code{"None"}, \code{"Centroid"}, or \code{"Origin"}. Default is \code{"None"}.
#'
#' @details
#' The \code{simil_Frechet_metric()} function calculates the similarity between trajectories using
#' the \code{Frechet()} function from the \pkg{SimilarityMeasures} package.
#'
#' The Fréchet distance is a measure of similarity between two curves or continuous trajectories, which takes into account
#' both the order and location of points within the trajectories (Besse et al. 2015). The distance can be described by the
#' analogy of a person walking a dog on an extendable leash (Aronov et al. 2006). Both the person and the dog move along
#' their respective trajectories, with each able to adjust their speed but not retrace their steps. The Fréchet distance is
#' the minimum leash length required to keep the dog connected to the person throughout the walk (Cleasby et al., 2019).
#'
#' Unlike other trajectory comparison techniques, such as Dynamic Time Warping, the Fréchet distance focuses on the overall
#' shape of the trajectories rather than matching specific points. As a result, it is sensitive to noise because all points
#' of the compared trajectories are considered in its calculation. However, it can still be a powerful tool for trajectory
#' clustering and comparison, particularly when shape is the primary concern (Cleasby et al., 2019).
#'
#' Note that when comparing real trajectories that are very disparate or those simulated under an unconstrained method,
#' the resulting trajectories may not be suitable for Fréchet distance calculations. In such cases, the Fréchet distance is
#' returned as -1 to indicate an invalid measurement.
#'
#' The function offers three different superposition methods to align the trajectories
#' before \code{Frechet()} is applied:
#' \itemize{
#'   \item \code{"None"}: No superposition is applied.
#'   \item \code{"Centroid"}: Trajectories are shifted to align based on their centroids.
#'   \item \code{"Origin"}: Trajectories are shifted to align based on their starting point.
#' }
#'
#' If \code{test = TRUE}, the function can compute *p*-values by comparing the observed Fréchet
#' distances with those generated from a set of simulated trajectories. The *p*-values
#' are calculated for both individual trajectory pairs and for the entire set of trajectories.
#'
#' @return
#' A \code{track similarity} R object consisting ofa list containing the following elements:
#' \item{Frechet_distance_metric}{A matrix containing the pairwise Frechet distances between trajectories.}
#' \item{Frechet_distance_metric_p_values}{(If \code{test} is \code{TRUE)} A matrix containing the *p*-values for the pairwise Frechet distances.}
#' \item{Frechet_metric_p_values_combined}{(If \code{test} is \code{TRUE)} The overall *p*-value for the combined Frechet distances.}
#' \item{Frechet_distance_metric_simulations}{(If \code{test} is \code{TRUE)} A list of Frechet distance matrices from each simulated dataset.}
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
#' # Example 1: Simulating tracks using the "Directed" model and comparing Frechet distance
#' # in the PaluxyRiver dataset
#' s1 <- simulate_track(PaluxyRiver, nsim = 3, model = "Directed")
#' simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
#'
#' # Example 2: Simulating tracks using the "Constrained" model and comparing Frechet distance
#' # in the PaluxyRiver dataset  using the "Centroid" superposition method
#' s2 <- simulate_track(PaluxyRiver, nsim = 3, model = "Constrained")
#' simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s2, superposition = "Centroid")
#'
#' # Example 3: Simulating tracks using the "Unconstrained" model and comparing Frechet distance
#' # in the PaluxyRiver dataset using the "Origin" superposition method
#' s3 <- simulate_track(PaluxyRiver, nsim = 3, model = "Unconstrained")
#' simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s3, superposition = "Origin")
#'
#'
#' @importFrom SimilarityMeasures Frechet
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom schoolmath is.real.positive
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{simulate_track}}, \code{\link[SimilarityMeasures]{Frechet}}
#'
#' @export




# Function to calculate track similarity Frechet metric----
simil_Frechet_metric <- function(data, test = FALSE, sim = NULL, superposition = "None") {

  ## Set default values if arguments are NULL----
  if (is.null(test)) test <- FALSE # Set default if 'test' is NULL
  if (is.null(superposition)) superposition <- "None" # Set default superposition method if 'superposition' is NULL

  ## Errors and Warnings ----

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
  Frechet <- Matrixsim

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
      Frechet[r, c] <- Frechet(as.matrix(data[[r]][, 1:2]), as.matrix(data[[c]][, 1:2]))
    }
  }

  if (test == TRUE) {
    # Tests----
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

    ### Calculate simulated metrics----
    nsim <- length(sim)
    Frechetsim <- Matrixsim

    listFrechet <- list()
    listnegFrechet <- c()

    for (i in 1:nsim) {
      sim[[i]]$Trajectory <- as.factor(sim[[1]]$Trajectory)
      levels <- levels(sim[[i]]$Trajectory)

      for (c in 1:length(data)) {
        for (r in 1:length(data)) {
          if (c <= r) next
          Frechetsim[r, c] <- suppressWarnings(Frechet(as.matrix(sim[[i]][which(sim[[i]]$Trajectory == levels[r]), 1:2]), as.matrix(sim[[i]][which(sim[[i]]$Trajectory == levels[c]), 1:2])))
        }
      }
      listFrechet[[i]] <- Frechetsim

      positive <- c(as.matrix(Frechet - listFrechet[[i]]))
      positive <- positive[!is.na(positive)]
      listnegFrechet[i] <- all(is.real.positive(positive))

      message(paste(Sys.time(), paste("Iteration", i)))
      message(" ")
      message("Frechet metric")
      Frechetsim
      message("------------------------------------")
      if (i == nsim) {
        message("ANALYSIS COMPLETED")
        message("------------------------------------")
        message(" ")
      }
    }

    ## Calculate p-values
    Frechetsim_pval <- Matrixsim

    vector <- c()
    for (c in 1:length(data)) {
      for (r in 1:length(data)) {
        if (c <= r) next
        for (i in 1:nsim) {
          vector[i] <- listFrechet[[i]][r, c]
          Frechetsim_pval[r, c] <- length(which(vector <= Frechet[r, c])) / nsim
        }
      }
    }

    Frechet_together_pval <- length(which(listnegFrechet == TRUE)) / nsim
  }

  # Value----
  if (test == TRUE) {
    list <- list()
    list[[1]] <- Frechet
    list[[2]] <- Frechetsim_pval
    list[[3]] <- Frechet_together_pval
    list[[4]] <- listFrechet

    names(list) <- c("Frechet_distance_metric", "Frechet_distance_metric_p_values", "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations")
    return(list)
  }

  if (test == FALSE) {
    list <- list()
    list[[1]] <- Frechet

    names(list) <- c("Frechet_distance_metric")
    return(list)
  }
}
