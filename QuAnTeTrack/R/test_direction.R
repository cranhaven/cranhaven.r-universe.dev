#' Test for differences in direction means with pairwise comparisons
#'
#' \code{test_direction()} evaluates differences in mean direction across different tracks using a specified statistical test. It includes options for ANOVA, Kruskal-Wallis test, and Generalized Linear Models (GLM), and checks for assumptions such as normality and homogeneity of variances. For datasets with more than two tracks, it performs pairwise comparisons to identify specific differences between tracks.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param analysis A character string specifying the type of analysis: \code{"ANOVA"}, \code{"Kruskal-Wallis"}, or \code{"GLM"}. Default is \code{"ANOVA"}.
#'
#' @details
#' The \code{test_direction()} function performs the following operations:
#'
#' - **Condition Testing:**
#'   - **Normality:** Shapiro-Wilk test for normality on step direction data within each track.
#'   - **Homogeneity of Variances:** Levene's test for equal variances across tracks.
#'
#' - **Statistical Analysis:**
#'   - **ANOVA:** Compares step mean directions across tracks, assuming normality and homogeneity of variances. Includes Tukey's HSD post-hoc test for pairwise comparisons.
#'   - **Kruskal-Wallis Test:** Non-parametric alternative to ANOVA for comparing step median directions across tracks when assumptions are violated. Includes Dunn's test for pairwise comparisons.
#'   - **GLM:** Generalized Linear Model with a Gaussian family for comparing step means if ANOVA assumptions are not met. Pairwise comparisons in the GLM are conducted using estimated marginal means (least-squares means) with the \pkg{emmeans} package, which computes differences between group means while adjusting for multiple comparisons using Tukey’s method.
#'
#' - **Direction Measurement:**
#'   - The direction is measured in degrees. The reference direction, or 0 degrees, is considered to be along the positive x-axis. Angles are measured counterclockwise from the positive x-axis, with 0 degrees pointing directly along this axis.
#'
#' @return A list with the results of the statistical analysis and diagnostic tests:
#'   - \code{normality_results}: A matrix of test statistics and *p*-values from the Shapiro-Wilk test for each track, with rows for the test statistic and *p*-value, and columns for each track.
#'   - \code{homogeneity_test}: The result of Levene's test, including the *p*-value for homogeneity of variances.
#'   - \code{ANOVA} (If \code{analysis} is \code{"ANOVA"}): A list containing the ANOVA table and Tukey HSD post-hoc test results.
#'   - \code{Kruskal_Wallis} (If \code{analysis} is \code{"Kruskal-Wallis"}): A list containing the Kruskal-Wallis test result and Dunn's test post-hoc results.
#'   - \code{GLM} (If \code{analysis} is \code{"GLM"}): A summary of the GLM fit and pairwise comparisons.
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
#' # Example 1: Test for Differences in Direction Means with Pairwise Comparisons in MountTom dataset
#' test_direction(MountTom, analysis = "ANOVA")
#'
#' # Example 2: Test for Differences in Direction Means with Pairwise Comparisons in MountTom dataset
#' test_direction(MountTom, analysis = "Kruskal-Wallis")
#'
#' # Example 3: Test for Differences in Direction Means with Pairwise Comparisons in MountTom dataset
#' test_direction(MountTom, analysis = "GLM")
#'
#' # Example 4: Test for Differences in Direction Means with Pairwise Comparisons in PaluxyRiver
#' # dataset
#' test_direction(PaluxyRiver, analysis = "ANOVA")
#'
#' # Example 5: Test for Differences in Direction Means with Pairwise Comparisons in PaluxyRiver
#' # dataset
#' test_direction(PaluxyRiver, analysis = "Kruskal-Wallis")
#'
#' # Example 6: Test for Differences in Direction Means with Pairwise Comparisons in PaluxyRiver
#' # dataset
#' test_direction(PaluxyRiver, analysis = "GLM")
#'
#' @importFrom dplyr mutate summarise group_by n ungroup
#' @importFrom stats aov kruskal.test glm shapiro.test TukeyHSD gaussian
#' @importFrom car leveneTest
#' @importFrom dunn.test dunn.test
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{plot_direction}}
#'
#' @export

test_direction <- function(data, analysis = NULL) {
  ## Set default values if arguments are NULL----
  if (is.null(analysis)) analysis <- "ANOVA" # Default to "ANOVA" if 'analysis' is NULL


  ## Errors and Warnings----

  # Error if the 'data' argument is not a list or has fewer than two elements.
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements: 'Trajectories' and 'Footprints'.")
  }

  # Error if the first two elements of 'data' are not lists.
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("Both elements of 'data' must be lists. Ensure that 'Trajectories' and 'Footprints' are provided.")
  }

  # Error if any element within 'data' is empty.
  if (any(sapply(data, function(x) length(x) == 0))) {
    stop("The elements within 'data' must not be empty. Ensure that both 'Trajectories' and 'Footprints' contain data.")
  }

  # Error if an invalid analysis type is provided.
  if (!analysis %in% c("ANOVA", "Kruskal-Wallis", "GLM")) {
    stop("Invalid 'analysis' argument. Choose from 'ANOVA', 'Kruskal-Wallis', or 'GLM'.")
  }

  ## Code----

  # Extract trajectory parameters from the 'track_param' function
  track_param <- track_param(data)
  data <- data[[1]] # Update 'data' to only contain the first element (typically 'Trajectories')

  # Combine direction data from all tracks into a single vector 'n'
  n <- c(track_param[[1]][[1]], track_param[[1]][[1]][[length(track_param[[1]][[1]])]])

  # If more than one track is present, concatenate their direction data
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      n <- c(n, c(track_param[[i]][[1]], track_param[[i]][[1]][[length(track_param[[i]][[1]])]]))
    }
  }

  # Create a data frame 'M' with direction data and track names
  M <- data.frame(dir = n, track = rep(names(data), sapply(data, nrow)))

  # Replace underscores with spaces in track names to ensure readability
  M$track <- gsub("_", " ", M$track)
  M_original <- M # Keep a copy of the original data frame

  # Identify valid and invalid tracks based on the number of footprints (steps)
  track_counts <- table(M$track)
  valid_tracks <- names(track_counts[track_counts > 3]) # Tracks with more than 3 footprints
  invalid_tracks <- names(track_counts[track_counts <= 3]) # Tracks with 3 or fewer footprints

  # Warning if any tracks are removed from the analysis
  if (length(invalid_tracks) > 0) {
    warning("The following tracks were removed from the analysis due to having 3 or fewer footprints: ", paste(invalid_tracks, collapse = ", "), ".")
  }

  # Error if less than two valid tracks are available for analysis
  if (length(valid_tracks) < 2) {
    stop("Not enough tracks with more than 3 footprints for meaningful analysis. Ensure at least two tracks have more than 3 footprints.")
  }

  # Subset the data to include only valid tracks
  M_analysis <- subset(M, track %in% valid_tracks)

  # Check normality of each track's direction data using the Shapiro-Wilk test
  normality_tests <- lapply(split(M_analysis$dir, M_analysis$track), shapiro.test)
  normality_results <- sapply(normality_tests, function(x) c(statistic = x$statistic, p_value = x$p.value))

  # Warning if any track does not follow a normal distribution
  if (any(normality_results["p_value", ] <= 0.05)) {
    warning("One or more tracks do not follow a normal distribution (p-value <= 0.05). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.")
  }

  # Check homogeneity of variances across tracks using Levene's test
  homogeneity_test <- car::leveneTest(dir ~ as.factor(track), data = M_analysis)

  # Warning if the homogeneity of variances assumption is violated
  if (homogeneity_test$Pr[1] <= 0.05) {
    warning("Homogeneity of variances assumption is violated (Levene's test p-value <= 0.05). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.")
  }

  # Initialize results list to store outputs of the analysis
  results <- list(normality_results = normality_results, homogeneity_test = homogeneity_test)

  # Perform the selected analysis based on the user's input
  if (analysis == "ANOVA") {
    anova_result <- summary(aov(dir ~ track, data = M_analysis))
    tukey_result <- TukeyHSD(aov(dir ~ track, data = M_analysis))
    results$ANOVA <- list(ANOVA = anova_result, Tukey = tukey_result)
  } else if (analysis == "Kruskal-Wallis") {
    # Perform Kruskal-Wallis test if selected
    kruskal_result <- kruskal.test(dir ~ track, data = M_analysis)
    dunn_result <- dunn.test::dunn.test(M_analysis$dir, M_analysis$track, kw = TRUE)
    results$Kruskal_Wallis <- list(Kruskal_Wallis = kruskal_result, Dunn = dunn_result)
  } else if (analysis == "GLM") {
    # Perform GLM if selected
    glm_result <- summary(glm(dir ~ track, data = M_analysis, family = gaussian()))
    results$GLM$GLM <- glm_result
    results$GLM$pairwise_results <- emmeans::emmeans(glm(dir ~ track, data = M_analysis, family = gaussian()), pairwise ~ track, adjust = "tukey")
  }

  return(results) # Return the results list containing the analysis outcomes
}
