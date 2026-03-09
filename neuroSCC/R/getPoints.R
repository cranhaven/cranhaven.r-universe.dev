#' Extract Significant SCC Points from an SCC Comparison Object
#'
#' @description
#' Identifies and extracts coordinates where differences fall outside the simultaneous confidence corridors (SCCs),
#' indicating statistically significant regions. This function processes the results from
#' \code{ImageSCC::scc.image()} and returns the voxel locations that represent either hypo- or hyperactivity.
#'
#' Interpretation depends on the order of inputs in the SCC computation.
#' If SCC was computed as \code{scc.image(Ya = Y_AD, Yb = Y_CN, ...)} (i.e., the Control group is the second argument).
#'
#' \itemize{
#'   \item \code{positivePoints} — Regions where Control minus Pathological is significantly above the SCC.
#'     These correspond to areas where the Pathological group (AD) is \emph{hypoactive} relative to Controls.
#'   \item \code{negativePoints} — Regions where Control minus Pathological is significantly below the SCC.
#'     These correspond to areas where the Pathological group is \emph{hyperactive} relative to Controls.
#' }
#'
#' \strong{Always confirm the order of \code{Ya} and \code{Yb} in the SCC computation}
#' to interpret the directionality correctly.
#'
#' @param sccResult A list of SCC computation results, as returned by \code{ImageSCC::scc.image}.
#' Must include the following components
#' \itemize{
#'   \item \code{Z.band} — A matrix specifying grid positions.
#'   \item \code{ind.inside.cover} — Indices of grid points within the confidence band.
#'   \item \code{scc} — A 3D array containing the computed SCC values.
#' }
#'
#' @return A named list with two elements
#' \itemize{
#'   \item \code{positivePoints} — A data frame with coordinates where the \strong{first group (Ya)} shows
#'         significantly lower activity than the \strong{second group (Yb)}.
#'   \item \code{negativePoints} — A data frame with coordinates where the \strong{first group (Ya)} shows
#'         significantly higher activity than the \strong{second group (Yb)}.
#' }
#'
#' @examples
#' # Load precomputed SCC example
#' data("SCCcomp", package = "neuroSCC")
#'
#' # Extract significant SCC points
#' significantPoints <- getPoints(SCCcomp)
#'
#' # Show extracted points (interpretation depends on SCC setup; see description)
#' head(significantPoints$positivePoints)  # Pathological hypoactive vs. Control
#' head(significantPoints$negativePoints)  # Pathological hyperactive vs. Control
#'
#' @seealso
#' \code{ImageSCC::scc.image} for SCC computation.
#'
#' @export
getPoints <- function(sccResult) {
  # 1. Validate Input
  # ---------------------------
  if (!is.list(sccResult)) {
    stop("'sccResult' must be a list containing SCC comparison results.")
  }

  required_elements <- c("Z.band", "ind.inside.cover", "scc")
  missing_elements <- setdiff(required_elements, names(sccResult))
  if (length(missing_elements) > 0) {
    stop("Missing required elements in SCC object: ", paste(missing_elements, collapse = ", "))
  }

  # Extract required components
  Z.band <- matrix(sccResult$Z.band, ncol = 2)  # Grid positions
  insideCover <- sccResult$ind.inside.cover     # Indices inside the SCC region
  sccValues <- sccResult$scc                    # SCC values (3D array)

  # 2. Ensure SCC Data is Valid
  # ---------------------------
  if (is.null(sccValues) || all(is.na(sccValues))) {
    warning("SCC values are empty or contain only NA values. Returning empty results.")
    return(list(positivePoints = data.frame(x = numeric(), y = numeric()),
                negativePoints = data.frame(x = numeric(), y = numeric())))
  }

  # Get unique grid positions
  z1 <- unique(Z.band[, 1])
  z2 <- unique(Z.band[, 2])
  n1 <- length(z1)
  n2 <- length(z2)

  # 3. Create SCC Matrices
  # ---------------------------
  # Initialize SCC matrices
  scc <- matrix(NA, n1 * n2, 2)

  # Ensure proper alignment when assigning SCC values
  if (length(insideCover) != nrow(sccValues)) {
    stop("Mismatch: 'ind.inside.cover' length does not match SCC matrix size.")
  }

  scc[insideCover, ] <- sccValues[, , 2]  # Assign SCC confidence band values

  # Define SCC thresholds
  sccLower <- matrix(scc[, 1], nrow = n2, ncol = n1)  # Lower bound (negative SCC threshold)
  sccUpper <- matrix(scc[, 2], nrow = n2, ncol = n1)  # Upper bound (positive SCC threshold)

  # Keep only values that fall outside the SCC region:

  # If the lower bound is negative, it means the difference is too high (Control > AD),
  # so we keep only positive values.
  sccLower[sccLower < 0] <- NA  # Retain only significant positive deviations (Control > AD)

  # If the upper bound is positive, it means the difference is too low (AD > Control),
  #so we keep only negative values.
  sccUpper[sccUpper > 0] <- NA  # Retain only significant negative deviations (AD > Control)

  # 4. Identify Significant SCC Points
  # ---------------------------
  posPoints <- which(sccLower > 0, arr.ind = TRUE)  # First group stronger
  negPoints <- which(sccUpper < 0, arr.ind = TRUE)  # Second group stronger

  # 5. Convert Indexes to Coordinates
  # ---------------------------
  positiveCoords <- if (nrow(posPoints) > 0) {
    data.frame(x = z1[posPoints[, 2]], y = z2[posPoints[, 1]])
  } else {
    data.frame(x = numeric(), y = numeric())
  }

  negativeCoords <- if (nrow(negPoints) > 0) {
    data.frame(x = z1[negPoints[, 2]], y = z2[negPoints[, 1]])
  } else {
    data.frame(x = numeric(), y = numeric())
  }

  # Return structured results
  return(list(positivePoints = positiveCoords, negativePoints = negativeCoords))
}
