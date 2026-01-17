#' Calculate combined probabilities of similarity or intersection metrics of tracks
#'
#' \code{combined_prob()} calculates the combined probabilities of similarity and intersection metrics
#' derived from different models. The function uses simulation data to extract *p*-values, providing insight into
#' the significance of combined metrics across various similarity assessments.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param metrics A list of \code{track similarity} and/or \code{track intersection} R objects derived from different tests. All tests must be
#' based on the same number of simulations.
#'
#' @details
#' The \code{combined_prob()} function combines *p*-values derived from multiple similarity metric tests and intersection tests.
#' It calculates the combined *p*-values by assessing the probability of observing the combined metrics across simulated datasets.
#' This function is particularly useful for comparing multiple models and evaluating their collective performance in terms of *p*-values.
#'
#' @return A list containing:
#' \item{P_values (model names)}{A matrix of *p*-values for the combined metrics across all trajectories. Each entry represents
#' the probability of observing the combined metrics between the corresponding pair of trajectories.}
#' \item{P_values_combined (model names)}{A numeric value representing the overall probability of observing the combined metrics,
#' across all pairs of trajectories.}
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
#' @examples
#' # Example 1: "Directed" model and similarity metrics.
#' s1 <- simulate_track(PaluxyRiver, nsim = 3, model = "Directed")
#' DTW1 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
#' Frechet1 <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
#' int1 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s1,
#'   origin.permutation = "None")
#' combined_prob(PaluxyRiver, metrics = list(DTW1, Frechet1, int1))
#'
#' # Example 2: "Constrained" model and similarity metrics.
#' s2 <- simulate_track(PaluxyRiver, nsim = 3, model = "Constrained")
#' DTW2 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s2,
#'   superposition = "None")
#' Frechet2 <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s2,
#'   superposition = "None")
#' int2 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s2,
#'   origin.permutation = "Min.Box")
#' combined_prob(PaluxyRiver, metrics = list(DTW2, Frechet2, int2))
#'
#' # Example 3: "Unconstrained" model and similarity metrics.
#' s3 <- simulate_track(PaluxyRiver, nsim = 3, model = "Unconstrained")
#' DTW3 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s3,
#'   superposition = "None")
#' Frechet3 <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s3,
#'   superposition = "None")
#' int3 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s3,
#'   origin.permutation = "Conv.Hull")
#' combined_prob(PaluxyRiver, metrics = list(DTW3, Frechet3, int3))
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{simulate_track}}, \code{\link{track_intersection}}, \code{\link{simil_DTW_metric}}, \code{\link{simil_Frechet_metric}}
#'
#' @export


combined_prob <- function(data, metrics = NULL) {

  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Error: Check that 'metrics' is provided and is a list
  if (is.null(metrics) || !is.list(metrics)) {
    stop("'metrics' must be provided and should be a list of track similarity or intersection metrics.")
  }

  # Error: Ensure all elements in 'metrics' have the same number of simulations
  nsim_lengths <- sapply(metrics, function(x) length(x[[4]]))
  if (length(unique(nsim_lengths)) != 1) {
    stop("All elements in 'metrics' must have the same number of simulations.")
  }


  ## Code----
  data <- data[[1]]

  Matrixsim <- data.frame(matrix(nrow = length(data), ncol = length(data)))
  colnames(Matrixsim) <- names(data)
  rownames(Matrixsim) <- names(data)

  togetherlist <- Matrixsim
  nsim <- length(metrics[[1]][[4]])

  listc <- list()
  for (j in 1:nsim) {
    listb <- list()
    for (i in 1:length(metrics)) {
      for (c in 1:length(data)) {
        for (r in 1:length(data)) {
          if (c <= r) next
          togetherlist[r, c] <- metrics[[i]][[1]][[r, c]] - metrics[[i]][[4]][[j]][[r, c]]
        }
        listb[[i]] <- togetherlist
      }
    }
    listc[[j]] <- listb
  }

  togetherpvalues <- Matrixsim
  togetherpvaluescombined <- c()

  for (c in 1:length(data)) {
    for (r in 1:length(data)) {
      if (c <= r) next

      vectorpos <- c()
      for (j in 1:nsim) {
        vector <- c()
        for (i in 1:length(metrics)) {
          vector[i] <- listc[[j]][[i]][[r, c]]
        }
        vectorpos[j] <- all(is.real.positive(vector))
      }
      togetherpvalues[[r, c]] <- length(which(vectorpos == TRUE)) / nsim
    }
  }

  for (i in 1:nsim) {
    positive <- do.call(rbind, listc[[i]])
    positive <- positive[!is.na(positive)]
    togetherpvaluescombined[i] <- all(is.real.positive(positive))
  }
  togetherpvaluescombined <- length(which(togetherpvaluescombined == TRUE)) / nsim

  listpvalue <- list()
  listpvalue[[1]] <- togetherpvalues
  listpvalue[[2]] <- togetherpvaluescombined

  nameslist <- c()
  for (i in 1:length(metrics)) {
    nameslist[i] <- names(metrics[[i]][1])
  }
  nameslist <- paste(nameslist, collapse = ", ")

  names(listpvalue) <- c(paste("P_values (", nameslist, ")", sep = ""), paste("P_values_combined (", nameslist, ")", sep = ""))

  return(listpvalue)
}
