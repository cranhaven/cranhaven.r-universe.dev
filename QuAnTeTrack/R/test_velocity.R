#' Test for differences in velocity means with pairwise comparisons
#'
#' \code{test_velocity()} evaluates differences in velocity means across different tracks using a specified statistical test. It includes options for ANOVA, Kruskal-Wallis test, and Generalized Linear Models (GLM), and checks for assumptions such as normality and homogeneity of variances. For datasets with more than two tracks, it performs pairwise comparisons to identify specific differences between tracks.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param trackvel A \code{track velocity} R object consisting of a list where each element corresponds to a track and contains velocity or relative stride length data.
#' @param plot A logical value indicating whether to plot a boxplot of velocities by track (default is \code{FALSE}).
#' @param analysis A character string specifying the type of analysis: \code{"ANOVA"}, \code{"Kruskal-Wallis"}, or \code{"GLM"}. Default is \code{"ANOVA"}.
#'
#' @details
#' The \code{test_velocity} function performs the following operations:
#'
#' - **Condition Testing:**
#'   - **Normality:** Shapiro-Wilk test for normality on velocity data within each track.
#'   - **Homogeneity of Variances:** Levene's test for equal variances across tracks.
#'
#' - **Statistical Analysis:**
#'   - **ANOVA:** Compares mean velocities across tracks, assuming normality and homogeneity of variances. Includes Tukey's HSD post-hoc test for pairwise comparisons.
#'   - **Kruskal-Wallis Test:** Non-parametric alternative to ANOVA for comparing median velocities across tracks when assumptions are violated. Includes Dunn's test for pairwise comparisons.
#'   - **GLM:** Generalized Linear Model with a Gaussian family for comparing means if ANOVA assumptions are not met. Pairwise comparisons in the GLM are conducted using estimated marginal means (least-squares means) with the \pkg{emmeans} package, which computes differences between group means while adjusting for multiple comparisons using Tukey’s method.
#'
#' - **Plotting:**
#'   - If \code{plot} is \code{TRUE}, a boxplot of velocities by track is generated.
#'
#' @return A list with the results of the statistical analysis and diagnostic tests:
#'   - \code{normality_results}: A matrix of test statistics and *p*-values from the Shapiro-Wilk test for each track, with rows for the test statistic and *p*-value, and columns for each track.
#'   - \code{homogeneity_test}: The result of Levene's test, including the *p*-value for homogeneity of variances.
#'   - \code{ANOVA} (If \code{analysis} is \code{"ANOVA"}): A list containing the ANOVA table and Tukey HSD post-hoc test results.
#'   - \code{Kruskal_Wallis} (If \code{analysis} is \code{"Kruskal-Wallis"}): A list containing the Kruskal-Wallis test result and Dunn's test post-hoc results.
#'   - \code{GLM} (If \code{analysis} is \code{"GLM"}): A summary of the GLM fit and pairwise comparisons.
#'   - \code{plot} (If \code{plot} is \code{TRUE}): A boxplot of velocities by track is generated and displayed.
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
#' # Example 1: Test for Differences in Velocity Means with Pairwise Comparisons in Trajectories
#' # in MountTom dataset.
#'
#' # Hip heights for each track in the MountTom dataset
#' H_mounttom <- c(
#'   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
#'   1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
#'   1.648, 1.760, 1.612
#' )
#'
#' # Calculate velocities using the default Method "A"
#' V_mounttom <- velocity_track(MountTom, H = H_mounttom)
#'
#' # Test for Differences in Velocity Means with Pairwise Comparisons
#' test_velocity(MountTom, V_mounttom)
#'
#' # Example 2: Test for Differences in Velocity Means with Pairwise Comparisons in Trajectories
#' # in PaluxyRiver dataset.
#'
#' # Hip heights for each track in the PaluxyRiver dataset
#' H_paluxyriver <- c(3.472, 2.200)
#'
#' # Specify different methods for different tracks
#' Method_paluxyriver <- c("A", "B")
#'
#' # Calculate velocities using specified methods
#' V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)
#'
#' # Test for Differences in Velocity Means with Pairwise Comparisons
#' test_velocity(PaluxyRiver, V_paluxyriver)
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point theme_classic labs position_jitter element_text
#' @importFrom car leveneTest
#' @importFrom dunn.test dunn.test
#' @importFrom emmeans emmeans
#' @importFrom stringr str_pad
#' @importFrom stats aov cor.test kruskal.test glm pnorm shapiro.test TukeyHSD gaussian
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{velocity_track}}
#'
#' @export

test_velocity <- function(data, trackvel, plot = FALSE, analysis = NULL) {
  ## Set default values if arguments are NULL----
  if (is.null(analysis)) analysis <- "ANOVA" # Default to "ANOVA" if 'analysis' is NULL

  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements: 'Trajectories' and 'Footprints'.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check if 'trackvel' is a list
  if (!is.list(trackvel)) {
    stop("The 'trackvel' argument must be a list.")
  }

  # Check if 'analysis' is a valid option
  if (!analysis %in% c("ANOVA", "Kruskal-Wallis", "GLM")) {
    stop("Invalid 'analysis' type. Choose from 'ANOVA', 'Kruskal-Wallis', or 'GLM'.")
  }


  ## Code----


  data <- data[[1]] # Update 'data' to only contain the first element (typically 'Trajectories')

  # Combine velocity data from all tracks into a single vector 'n'
  n <- c(trackvel[[1]][[1]], trackvel[[1]][[1]][[length(trackvel[[1]][[1]])]])

  # If more than one track is present, concatenate their velocity data
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      n <- c(n, c(trackvel[[i]][[1]], trackvel[[i]][[1]][[length(trackvel[[i]][[1]])]]))
    }
  }

  # Create a data frame 'M' with velocity data and track names
  M <- data.frame(matrix(nrow = length(n), ncol = 2))
  colnames(M) <- c("vel", "track")

  M[, 1] <- n

  n <- rep(names(data[1]), length(data[[1]][, 1]))
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      n <- c(n, rep(paste(names(data[i])), length(data[[i]][, 1])))
    }
  }
  M[, 2] <- n

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

  # Check normality of each track's velocity data using the Shapiro-Wilk test
  normality_tests <- lapply(split(M_analysis$vel, M_analysis$track), shapiro.test)
  normality_results <- sapply(normality_tests, function(x) c(statistic = x$statistic, p_value = x$p.value))

  # Warning if any track does not follow a normal distribution
  if (any(normality_results["p_value", ] <= 0.05)) {
    warning("One or more tracks do not follow a normal distribution (p-value <= 0.05). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.")
  }

  # Check homogeneity of variances across tracks using Levene's test
  homogeneity_test <- car::leveneTest(vel ~ as.factor(track), data = M_analysis)

  # Warning if the homogeneity of variances assumption is violated
  if (homogeneity_test$Pr[1] <= 0.05) {
    warning("Homogeneity of variances assumption is violated (Levene's test p-value <= 0.05). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.")
  }

  # Initialize results list to store outputs of the analysis
  results <- list(normality_results = normality_results, homogeneity_test = homogeneity_test)

  # Perform the selected analysis based on the user's input
  if (analysis == "ANOVA") {
    anova_result <- summary(aov(vel ~ track, data = M_analysis))
    tukey_result <- TukeyHSD(aov(vel ~ track, data = M_analysis))
    results$ANOVA <- list(ANOVA = anova_result, Tukey = tukey_result)
  } else if (analysis == "Kruskal-Wallis") {
    # Perform Kruskal-Wallis test if selected
    kruskal_result <- kruskal.test(vel ~ track, data = M_analysis)
    dunn_result <- dunn.test::dunn.test(M_analysis$vel, M_analysis$track, kw = TRUE)
    results$Kruskal_Wallis <- list(Kruskal_Wallis = kruskal_result, Dunn = dunn_result)
  } else if (analysis == "GLM") {
    # Perform GLM if selected
    glm_result <- summary(glm(vel ~ track, data = M_analysis, family = gaussian()))
    results$GLM$GLM <- glm_result
    results$GLM$pairwise_results <- emmeans::emmeans(glm(vel ~ track, data = M_analysis, family = gaussian()), pairwise ~ track, adjust = "tukey")
  }

  # Generate boxplot if requested
  if (plot) {
    p <- ggplot(M_analysis, aes(x = track, y = vel)) +
      geom_boxplot() +
      geom_point(alpha = 0.2, position = position_jitter()) +
      theme_classic() +
      labs(y = "Velocity (m/s)", x = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    print(p)
  }

  return(results)
}
