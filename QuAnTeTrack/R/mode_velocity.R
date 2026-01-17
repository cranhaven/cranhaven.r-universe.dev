#' Test for steady, acceleration, or deceleration along trajectories
#'
#' \code{mode_velocity()} evaluates the trend in velocity along each trajectory by applying Spearman's rank correlation test. The function classifies the trend into "acceleration", "deceleration", or "steady" based on the correlation and the *p*-value.
#'
#' @param trackvel A \code{track velocity} object  where each element corresponds to a track and contains a vector of velocity or relative stride length data.
#'
#' @details
#' The \code{mode_velocity()} function performs the following operations:
#'
#' - **Spearman's Rank Correlation Test:**
#'   - This non-parametric test assesses the strength and direction of a monotonic relationship between two variables. It does not require assumptions about the normality of data or a linear relationship between velocity and step number.
#'   - It uses ranks rather than raw values, making it robust to outliers and suitable for detecting general trends (acceleration or deceleration) in velocity data.
#'
#' - **Function Operation:**
#'   - For each trajectory in the \code{trackvel} list, the function calculates the Spearman correlation coefficient and the associated *p*-value between velocity and step number.
#'   - Based on the *p*-value and correlation coefficient, it classifies the trend as "acceleration", "deceleration", or "steady".
#'   - If a trajectory contains fewer than 3 steps, the function returns a message indicating insufficient data for correlation analysis.
#'
#' - **Advantages:**
#'   - The non-parametric nature allows flexibility with data distributions and reduced sensitivity to outliers compared to parametric tests.
#'   - Effective for detecting monotonic trends (either increasing or decreasing) when the correlation is statistically significant.
#'
#' - **Limitations:**
#'   - May be unreliable with very small sample sizes (e.g., fewer than 3 steps), providing potentially non-informative results.
#'   - Does not capture the magnitude of change or provide detailed insights into the rate of acceleration or deceleration.
#'   - Identifies monotonic trends based on statistical significance but does not distinguish between different types of monotonic relationships (e.g., steady acceleration vs. abrupt changes).
#'
#' **Interpretation of Results:**
#'   - **Acceleration:** If the *p*-value is less than 0.05 and the Spearman correlation coefficient is positive.
#'   - **Deceleration:** If the *p*-value is less than 0.05 and the Spearman correlation coefficient is negative.
#'   - **Steady:** If the *p*-value is greater than or equal to 0.05, indicating no significant monotonic relationship.
#'
#' **Usage Considerations:**
#'   - Ensure that each trajectory in \code{trackvel} has a sufficient number of steps for meaningful analysis.
#'   - For more detailed analysis of velocity trends, consider complementary methods such as linear or non-linear regression, or specialized change point detection techniques.
#'
#' @return A list where each element corresponds to a trajectory from the input \code{trackvel} and contains:
#'   - **correlation:** The result of the Spearman correlation test, including the correlation coefficient and *p*-value.
#'   - **trend:** A classification of the trend as "Acceleration", "Deceleration", or "Steady" based on the *p*-value and the correlation coefficient.
#'   - If a trajectory has fewer than 3 steps, the entry contains the message "Less than three steps."
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
#' # Example 1: Test for Steady, Acceleration, or Deceleration in MountTom dataset.
#'
#' # Hip heights for each track in the MountTom dataset
#' H_mounttom <- c(
#'   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
#'   1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784,
#'   1.676, 1.872, 1.648, 1.760, 1.612
#' )
#'
#' # Calculate velocities using the default Method "A"
#' V_mounttom <- velocity_track(MountTom, H = H_mounttom)
#'
#' # Test for Steady, Acceleration, or Deceleration
#' mode_velocity(V_mounttom)
#'
#' # Example 2: Test for Steady, Acceleration, or Deceleration in PaluxyRiver dataset.
#'
#' # Hip heights for each track in the PaluxyRiver dataset
#' H_paluxyriver <- c(3.472, 2.200)
#'
#' # Specify different methods for different tracks
#' Method_paluxyriver <- c("A", "B")
#'
#' # Calculate velocities using specified methods
#' V_paluxyriver <- velocity_track(PaluxyRiver,
#'   H = H_paluxyriver,
#'   method = Method_paluxyriver
#' )
#'
#' # Test for Steady, Acceleration, or Deceleration
#' mode_velocity(V_paluxyriver)
#'
#' @importFrom stringr str_pad
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{velocity_track}}, \code{\link{plot_velocity}}
#'
#' @export

mode_velocity <- function(trackvel) {

  # Initialize an empty list to store results
  results <- list()

  ## Errors and Warnings----

  # Check if 'trackvel' is a list
  if (!is.list(trackvel)) {
    stop("The 'trackvel' argument must be a list.")
  }

  ## Code----

  # Iterate over each trajectory in the input list
  for (i in seq_along(trackvel)) {
    # Extract velocity data for the current trajectory
    velocity <- trackvel[[i]][[1]]
    steps <- seq_along(velocity)

    # Check if the trajectory has more than 2 steps
    if (length(velocity) > 2) {
      # Perform Spearman's rank correlation test
      corr <- cor.test(x = velocity, y = steps, method = "spearman", exact = FALSE)
      corr_value <- corr$estimate
      p_value <- corr$p.value

      # Classify the trend based on p-value and Spearman correlation coefficient
      if (p_value < 0.05) {
        if (corr_value > 0) {
          trend <- "Acceleration"
        } else {
          trend <- "Deceleration"
        }
      } else {
        trend <- "Steady"
      }

      # Store the results for the current trajectory
      result <- list(
        correlation = corr,
        trend = trend
      )
    } else {
      # If the trajectory has fewer than 3 steps, indicate insufficient data
      result <- "Less than three steps"
    }

    # Store the result in the list with a named entry
    results[[i]] <- result
  }

  # Assign names to the results list based on the trajectory index
  names(results) <- paste0("Track_", str_pad(seq_along(trackvel), nchar(length(trackvel)), pad = "0"))

  return(results)
}
