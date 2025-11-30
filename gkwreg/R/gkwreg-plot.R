#' @title Diagnostic Plots for Generalized Kumaraswamy Regression Models
#'
#' @description
#' Produces a comprehensive set of diagnostic plots for assessing the adequacy
#' of a fitted Generalized Kumaraswamy (GKw) regression model (objects of class
#' \code{"gkwreg"}). The function offers flexible plot selection, multiple
#' residual types, and support for both base R graphics and \code{ggplot2} with
#' extensive customization options. Designed for thorough model evaluation
#' including residual analysis, influence diagnostics, and goodness-of-fit
#' assessment.
#'
#' @param x An object of class \code{"gkwreg"}, typically the result of a call
#'   to \code{\link{gkwreg}}.
#'
#' @param which Integer vector specifying which diagnostic plots to produce.
#'   If a subset of the plots is required, specify a subset of the numbers 1:6.
#'   Defaults to \code{1:6} (all plots). The plots correspond to:
#'   \enumerate{
#'     \item \strong{Residuals vs. Observation Indices}: Checks for temporal
#'       patterns, trends, or autocorrelation in residuals across observation order.
#'     \item \strong{Cook's Distance Plot}: Identifies influential observations
#'       that have disproportionate impact on model estimates. Points exceeding
#'       the 4/n threshold warrant investigation.
#'     \item \strong{Generalized Leverage vs. Fitted Values}: Identifies high
#'       leverage points with unusual predictor combinations. Points exceeding
#'       2p/n threshold may be influential.
#'     \item \strong{Residuals vs. Linear Predictor}: Checks for non-linearity
#'       in the predictor-response relationship and heteroscedasticity
#'       (non-constant variance).
#'     \item \strong{Half-Normal Plot with Simulated Envelope}: Assesses
#'       normality of residuals (particularly useful for quantile residuals)
#'       by comparing observed residuals against simulated quantiles. Points
#'       outside the envelope indicate potential model misspecification.
#'     \item \strong{Predicted vs. Observed Values}: Overall goodness-of-fit
#'       check showing model prediction accuracy and systematic bias.
#'   }
#'
#' @param type Character string indicating the type of residuals to be used for
#'   plotting. Defaults to \code{"quantile"}. Valid options are:
#'   \itemize{
#'     \item \code{"quantile"}: Randomized quantile residuals (Dunn & Smyth, 1996).
#'       \strong{Recommended for bounded responses} as they should be approximately
#'       N(0,1) if the model is correctly specified. Most interpretable with
#'       standard diagnostic tools.
#'     \item \code{"pearson"}: Pearson residuals (response residual standardized
#'       by estimated standard deviation). Useful for checking the variance function
#'       and identifying heteroscedasticity patterns.
#'     \item \code{"deviance"}: Deviance residuals. Related to the log-likelihood
#'       contribution of each observation. Sum of squared deviance residuals equals
#'       the model deviance.
#'   }
#'
#' @param family Character string specifying the distribution family assumptions
#'   to use when calculating residuals and other diagnostics. If \code{NULL}
#'   (default), the family stored within the fitted \code{object} is used.
#'   Specifying a different family can be useful for diagnostic comparisons
#'   across competing model specifications. Available options match those in
#'   \code{\link{gkwreg}}: \code{"gkw"}, \code{"bkw"}, \code{"kkw"},
#'   \code{"ekw"}, \code{"mc"}, \code{"kw"}, \code{"beta"}.
#'
#' @param caption Titles for the diagnostic plots. Can be specified in three ways:
#'   \itemize{
#'     \item \code{NULL} (default): Uses standard default captions for all plots.
#'     \item \strong{Character vector} (backward compatibility): A vector of 6
#'       strings corresponding to plots 1-6. Must provide all 6 titles even if
#'       only customizing some.
#'     \item \strong{Named list} (recommended): A list with plot numbers as
#'       names (e.g., \code{list("3" = "My Custom Title")}). Only specified
#'       plots are customized; others use defaults. This allows partial
#'       customization without repeating all titles.
#'   }
#'   Default captions are:
#'   \enumerate{
#'     \item "Residuals vs. Observation Indices"
#'     \item "Cook's Distance Plot"
#'     \item "Generalized Leverage vs. Fitted Values"
#'     \item "Residuals vs. Linear Predictor"
#'     \item "Half-Normal Plot of Residuals"
#'     \item "Predicted vs. Observed Values"
#'   }
#'
#' @param main Character string to be prepended to individual plot captions
#'   (from the \code{caption} argument). Useful for adding a common prefix
#'   to all plot titles. Defaults to \code{""} (no prefix).
#'
#' @param sub.caption Character string used as a common subtitle positioned
#'   above all plots (especially when multiple plots are arranged). If \code{NULL}
#'   (default), automatically generates a subtitle from the model call
#'   (\code{deparse(x$call)}). Set to \code{""} to suppress the subtitle entirely.
#'
#' @param ask Logical. If \code{TRUE} (and using base R graphics with multiple
#'   plots on an interactive device), the user is prompted before displaying
#'   each plot. If \code{NULL} (default), automatically determined: \code{TRUE}
#'   if more plots are requested than fit on the current screen layout and the
#'   session is interactive; \code{FALSE} otherwise. Explicitly set to
#'   \code{FALSE} to disable prompting or \code{TRUE} to force prompting.
#'
#' @param use_ggplot Logical. If \code{TRUE}, plots are generated using the
#'   \code{ggplot2} package, providing modern, publication-quality graphics
#'   with extensive theming capabilities. If \code{FALSE} (default), uses base
#'   R graphics, which are faster and require no additional dependencies.
#'   Requires the \code{ggplot2} package to be installed if set to \code{TRUE}.
#'
#' @param arrange_plots Logical. Only relevant if \code{use_ggplot = TRUE} and
#'   multiple plots are requested (\code{length(which) > 1}). If \code{TRUE},
#'   attempts to arrange the generated \code{ggplot} objects into a grid layout
#'   using either the \code{gridExtra} or \code{ggpubr} package (requires one
#'   of them to be installed). If \code{FALSE} (default), plots are displayed
#'   individually in sequence. Ignored when using base R graphics.
#'
#' @param nsim Integer. Number of simulations used to generate the confidence
#'   envelope in the half-normal plot (\code{which = 5}). Higher values provide
#'   more accurate envelopes but increase computation time. Defaults to 100,
#'   which typically provides adequate precision. Must be a positive integer.
#'   Typical range: 50-500.
#'
#' @param level Numeric. The confidence level (between 0 and 1) for the simulated
#'   envelope in the half-normal plot (\code{which = 5}). Defaults to 0.90
#'   (90\% confidence envelope). Common choices are 0.90, 0.95, or 0.99. Points
#'   falling outside this envelope suggest potential model inadequacy or outliers.
#'
#' @param sample_size Integer or \code{NULL}. If specified as an integer less
#'   than the total number of observations (\code{x$nobs}), a random sample of
#'   this size is used for calculating diagnostics and plotting. This can
#'   significantly speed up plot generation for very large datasets (n > 10,000)
#'   with minimal impact on diagnostic interpretation. Defaults to \code{NULL}
#'   (use all observations). Recommended values: 1000-5000 for large datasets.
#'
#' @param theme_fn A function. Only relevant if \code{use_ggplot = TRUE}.
#'   Specifies a \code{ggplot2} theme function to apply to all plots for
#'   consistent styling (e.g., \code{ggplot2::theme_bw},
#'   \code{ggplot2::theme_classic}, \code{ggplot2::theme_minimal}). If
#'   \code{NULL} (default), automatically uses \code{ggplot2::theme_minimal}
#'   when \code{use_ggplot = TRUE}. Can also be a custom theme function.
#'   Ignored when using base R graphics.
#'
#' @param save_diagnostics Logical. If \code{TRUE}, the function invisibly
#'   returns a list containing all calculated diagnostic measures (residuals,
#'   leverage, Cook's distance, fitted values, etc.) instead of the model object.
#'   Useful for programmatic access to diagnostic values for custom analysis or
#'   reporting. If \code{FALSE} (default), the function invisibly returns the
#'   original model object \code{x}. The function is primarily called for its
#'   side effect of generating plots.
#'
#' @param ... Additional graphical parameters passed to the underlying plotting
#'   functions. For base R graphics, these are standard \code{par()} parameters
#'   such as \code{col}, \code{pch}, \code{cex}, \code{lwd}, etc. For ggplot2,
#'   these are typically ignored but can be used for specific geom customizations
#'   in advanced usage. Always specified last to follow R best practices.
#'
#' @details
#' Diagnostic plots are essential for evaluating the assumptions and adequacy of
#' fitted regression models. This function provides a comprehensive suite of
#' standard diagnostic tools adapted specifically for \code{gkwreg} objects,
#' which model bounded responses in the (0,1) interval.
#'
#' \subsection{Residual Types and Interpretation}{
#' The choice of residual type (\code{type}) is important and depends on the
#' diagnostic goal:
#'
#' \itemize{
#'   \item \strong{Quantile Residuals} (\code{type = "quantile"}):
#'     \strong{Recommended as default} for bounded response models. These residuals
#'     are constructed to be approximately N(0,1) under a correctly specified model,
#'     making standard diagnostic tools (QQ-plots, hypothesis tests) directly
#'     applicable. They are particularly effective for detecting model
#'     misspecification in the distributional family or systematic bias.
#'
#'   \item \strong{Pearson Residuals} (\code{type = "pearson"}):
#'     Standardized residuals that account for the mean-variance relationship.
#'     Useful for assessing whether the assumed variance function is appropriate.
#'     If plots show patterns or non-constant spread, this suggests the variance
#'     model may be misspecified.
#'
#'   \item \strong{Deviance Residuals} (\code{type = "deviance"}):
#'     Based on the contribution of each observation to the model deviance.
#'     Often have more symmetric distributions than Pearson residuals and are
#'     useful for identifying observations that fit poorly according to the
#'     likelihood criterion.
#' }
#' }
#'
#' \subsection{Individual Plot Interpretations}{
#'
#' \strong{Plot 1 - Residuals vs. Observation Indices}:
#' \itemize{
#'   \item \emph{Purpose}: Detect temporal patterns or autocorrelation
#'   \item \emph{What to look for}: Random scatter around zero. Any systematic
#'     patterns (trends, cycles, clusters) suggest autocorrelation or omitted
#'     time-varying predictors.
#'   \item \emph{Action}: If patterns are detected, consider adding time-related
#'     predictors or modeling autocorrelation structure.
#' }
#'
#' \strong{Plot 2 - Cook's Distance}:
#' \itemize{
#'   \item \emph{Purpose}: Identify influential observations affecting coefficient estimates
#'   \item \emph{What to look for}: Points exceeding the 4/n reference line have
#'     high influence. These observations, if removed, would substantially change
#'     model estimates.
#'   \item \emph{Action}: Investigate high-influence points for data entry errors,
#'     outliers, or legitimately unusual cases. Consider sensitivity analysis.
#' }
#'
#' \strong{Plot 3 - Leverage vs. Fitted Values}:
#' \itemize{
#'   \item \emph{Purpose}: Identify observations with unusual predictor combinations
#'   \item \emph{What to look for}: Points exceeding the 2p/n reference line have
#'     high leverage. These are unusual in predictor space but may or may not be
#'     influential.
#'   \item \emph{Action}: High leverage points deserve scrutiny but are only
#'     problematic if they also have large residuals (check Plots 1, 4).
#' }
#'
#' \strong{Plot 4 - Residuals vs. Linear Predictor}:
#' \itemize{
#'   \item \emph{Purpose}: Detect non-linearity and heteroscedasticity
#'   \item \emph{What to look for}: Random scatter around zero with constant spread.
#'     Curved patterns suggest non-linear relationships. Funnel shapes indicate
#'     heteroscedasticity (non-constant variance).
#'   \item \emph{Action}: For non-linearity, add polynomial terms or use splines.
#'     For heteroscedasticity, consider alternative link functions or variance models.
#' }
#'
#' \strong{Plot 5 - Half-Normal Plot with Envelope}:
#' \itemize{
#'   \item \emph{Purpose}: Assess overall distributional adequacy
#'   \item \emph{What to look for}: Points should follow the reference line and
#'     stay within the simulated envelope. Systematic deviations indicate
#'     distributional misspecification. Isolated points outside the envelope
#'     suggest outliers.
#'   \item \emph{Action}: If many points fall outside the envelope, try a different
#'     distributional family or check for outliers and data quality issues.
#' }
#'
#' \strong{Plot 6 - Predicted vs. Observed}:
#' \itemize{
#'   \item \emph{Purpose}: Overall model fit and prediction accuracy
#'   \item \emph{What to look for}: Points should cluster around the 45-degree line.
#'     Systematic deviations above or below indicate over- or under-prediction.
#'     Large scatter indicates poor predictive performance.
#'   \item \emph{Action}: Poor fit suggests missing predictors, incorrect functional
#'     form, or inappropriate distributional family.
#' }
#' }
#'
#' \subsection{Using Caption Customization}{
#' The new \strong{named list interface} for \code{caption} allows elegant partial
#' customization:
#'
#' \preformatted{
#' # OLD WAY (still supported): Must repeat all 6 titles
#' plot(model, caption = c(
#'   "Residuals vs. Observation Indices",
#'   "Cook's Distance Plot",
#'   "MY CUSTOM TITLE FOR PLOT 3",  # Only want to change this
#'   "Residuals vs. Linear Predictor",
#'   "Half-Normal Plot of Residuals",
#'   "Predicted vs. Observed Values"
#' ))
#'
#' # NEW WAY: Specify only what changes
#' plot(model, caption = list(
#'   "3" = "MY CUSTOM TITLE FOR PLOT 3"
#' ))
#' # Plots 1,2,4,5,6 automatically use defaults
#'
#' # Customize multiple plots
#' plot(model, caption = list(
#'   "1" = "Time Series of Residuals",
#'   "5" = "Distributional Assessment"
#' ))
#' }
#'
#' The vector interface remains fully supported for backward compatibility.
#' }
#'
#' \subsection{NULL Defaults and Intelligent Behavior}{
#' Several arguments default to \code{NULL}, triggering intelligent automatic behavior:
#'
#' \itemize{
#'   \item \code{sub.caption = NULL}: Automatically generates subtitle from model call
#'   \item \code{ask = NULL}: Automatically prompts only when needed (multiple plots
#'     on interactive device)
#'   \item \code{theme_fn = NULL}: Automatically uses \code{theme_minimal} when
#'     \code{use_ggplot = TRUE}
#' }
#'
#' You can override these by explicitly setting values:
#' \preformatted{
#' plot(model, sub.caption = "")           # Disable subtitle
#' plot(model, ask = FALSE)                # Never prompt
#' plot(model, theme_fn = theme_classic)   # Custom theme
#' }
#' }
#'
#' \subsection{Performance Considerations}{
#' For large datasets (n > 10,000):
#' \itemize{
#'   \item Use \code{sample_size} to work with a random subset (e.g.,
#'     \code{sample_size = 2000})
#'   \item Reduce \code{nsim} for half-normal plot (e.g., \code{nsim = 50})
#'   \item Use base R graphics (\code{use_ggplot = FALSE}) for faster rendering
#'   \item Skip computationally intensive plots: \code{which = c(1,2,4,6)}
#'     (excludes half-normal plot)
#' }
#' }
#'
#' \subsection{Graphics Systems}{
#' \strong{Base R Graphics} (\code{use_ggplot = FALSE}):
#' \itemize{
#'   \item Faster rendering, especially for large datasets
#'   \item No external dependencies beyond base R
#'   \item Traditional R look and feel
#'   \item Interactive \code{ask} prompting supported
#'   \item Customize via \code{...} parameters (standard \code{par()} settings)
#' }
#'
#' \strong{ggplot2 Graphics} (\code{use_ggplot = TRUE}):
#' \itemize{
#'   \item Modern, publication-quality aesthetics
#'   \item Consistent theming via \code{theme_fn}
#'   \item Grid arrangement support via \code{arrange_plots}
#'   \item Requires \code{ggplot2} package (and optionally \code{gridExtra} or
#'     \code{ggpubr} for arrangements)
#'   \item No interactive \code{ask} prompting (ggplot limitation)
#' }
#' }
#'
#' @return
#' Invisibly returns either:
#' \itemize{
#'   \item The original fitted model object \code{x} (if
#'     \code{save_diagnostics = FALSE}, the default). This allows piping or
#'     chaining operations.
#'   \item A list containing diagnostic measures (if \code{save_diagnostics = TRUE}),
#'     including:
#'     \itemize{
#'       \item \code{data}: Data frame with observation indices, observed values,
#'         fitted values, residuals, Cook's distance, leverage, and linear predictors
#'       \item \code{model_info}: List with model metadata (n, p, thresholds,
#'         family, type, etc.)
#'       \item \code{half_normal}: Data frame with half-normal plot data and
#'         envelope (if \code{which} includes 5)
#'     }
#' }
#'
#' The function is primarily called for its side effect of generating diagnostic
#' plots. The invisible return allows:
#' \preformatted{
#' # Silent plotting
#' plot(model)
#'
#' # Or capture for further use
#' diag <- plot(model, save_diagnostics = TRUE)
#' head(diag$data)
#' }
#'
#' @author Lopes, J. E.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{gkwreg}} for fitting Generalized Kumaraswamy regression models
#'   \item \code{\link{residuals.gkwreg}} for extracting different types of residuals
#'   \item \code{\link{fitted.gkwreg}} for extracting fitted values
#'   \item \code{\link{summary.gkwreg}} for model summaries
#'   \item \code{\link[stats]{plot.lm}} for analogous diagnostics in linear models
#'   \item \code{\link[ggplot2]{ggplot}} for ggplot2 graphics system
#'   \item \code{\link[gridExtra]{grid.arrange}} for arranging ggplot2 plots
#' }
#'
#' @references
#' Dunn, P. K., & Smyth, G. K. (1996). Randomized Quantile Residuals.
#' \emph{Journal of Computational and Graphical Statistics}, \strong{5}(3), 236-244.
#' \doi{10.1080/10618600.1996.10474708}
#'
#' Cook, R. D. (1977). Detection of Influential Observation in Linear Regression.
#' \emph{Technometrics}, \strong{19}(1), 15-18.
#' \doi{10.1080/00401706.1977.10489493}
#'
#' Atkinson, A. C. (1985). \emph{Plots, Transformations and Regression}.
#' Oxford University Press.
#'
#' @keywords plot methods regression diagnostics hplot
#'
#' @examples
#' \donttest{
#' # EXAMPLE 1: Basic Usage with Default Settings
#'
#' # Simulate data
#' library(gkwdist)
#'
#' set.seed(123)
#' n <- 200
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#'
#' # True model parameters
#' alpha_true <- exp(0.7 + 0.3 * x1)
#' beta_true <- exp(1.2 - 0.2 * x2)
#'
#' # Generate response
#' y <- rkw(n, alpha = alpha_true, beta = beta_true)
#' df <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit model
#' model <- gkwreg(y ~ x1 | x2, data = df, family = "kw")
#'
#' # Generate all diagnostic plots with defaults
#' par(mfrow = c(3, 2))
#' plot(model, ask = FALSE)
#'
#' # EXAMPLE 2: Selective Plots with Custom Residual Type
#'
#' # Focus on key diagnostic plots only
#' par(mfrow = c(3, 1))
#' plot(model,
#'   which = c(2, 4, 5), # Cook's distance, Resid vs LinPred, Half-normal
#'   type = "pearson"
#' ) # Use Pearson residuals
#'
#' # Check for influential points (plot 2) and non-linearity (plot 4)
#' par(mfrow = c(2, 1))
#' plot(model,
#'   which = c(2, 4),
#'   type = "deviance"
#' )
#'
#' # EXAMPLE 3: Caption Customization - New Named List Interface
#'
#' # Customize only specific plot titles (RECOMMENDED NEW WAY)
#' par(mfrow = c(3, 1))
#' plot(model,
#'   which = c(1, 4, 6),
#'   caption = list(
#'     "1" = "Time Pattern Check",
#'     "4" = "Linearity Assessment",
#'     "6" = "Predictive Accuracy"
#'   )
#' )
#'
#' # Customize subtitle and main title
#' par(mfrow = c(2, 1))
#' plot(model,
#'   which = c(1, 5),
#'   main = "Model Diagnostics",
#'   sub.caption = "Kumaraswamy Regression - Training Data",
#'   caption = list("5" = "Normality Check with 95% Envelope")
#' )
#'
#' # Suppress subtitle entirely
#' par(mfrow = c(3, 2))
#' plot(model, sub.caption = "")
#'
#' # EXAMPLE 4: Backward Compatible Caption (Vector Interface)
#'
#' # OLD WAY - still fully supported
#' par(mfrow = c(3, 2))
#' plot(model,
#'   which = 1:6,
#'   caption = c(
#'     "Residual Pattern Analysis",
#'     "Influence Diagnostics",
#'     "Leverage Assessment",
#'     "Linearity Check",
#'     "Distributional Fit",
#'     "Prediction Quality"
#'   )
#' )
#'
#' # EXAMPLE 5: ggplot2 Graphics with Theming
#'
#' # Modern publication-quality plots
#' plot(model,
#'   use_ggplot = TRUE,
#'   arrange_plots = TRUE
#' )
#'
#' # With custom theme
#' plot(model,
#'   use_ggplot = TRUE,
#'   theme_fn = ggplot2::theme_bw,
#'   arrange_plots = TRUE
#' )
#'
#' # With classic theme and custom colors (via ...)
#' plot(model,
#'   use_ggplot = TRUE,
#'   theme_fn = ggplot2::theme_classic,
#'   arrange_plots = TRUE
#' )
#'
#' # EXAMPLE 6: Arranged Multi-Panel ggplot2 Display
#'
#' # Requires gridExtra or ggpubr package
#' plot(model,
#'   which = 1:4,
#'   use_ggplot = TRUE,
#'   arrange_plots = TRUE, # Arrange in grid
#'   theme_fn = ggplot2::theme_minimal
#' )
#'
#' # Focus plots in 2x2 grid
#' plot(model,
#'   which = c(2, 3, 4, 6),
#'   use_ggplot = TRUE,
#'   arrange_plots = TRUE,
#'   caption = list(
#'     "2" = "Influential Cases",
#'     "3" = "High Leverage Points"
#'   )
#' )
#'
#' # EXAMPLE 7: Half-Normal Plot Customization
#'
#' # Higher precision envelope (more simulations)
#' par(mfrow = c(1, 2))
#' plot(model,
#'   which = 5,
#'   nsim = 500, # More accurate envelope
#'   level = 0.95
#' ) # 95% confidence level
#'
#' # Quick envelope for large datasets
#' plot(model,
#'   which = 5,
#'   nsim = 500, # Faster computation
#'   level = 0.90
#' )
#'
#' # EXAMPLE 8: Different Residual Types Comparison
#'
#' # Compare different residual types
#' par(mfrow = c(2, 2))
#' plot(model, which = 4, type = "quantile", main = "Quantile")
#' plot(model, which = 4, type = "pearson", main = "Pearson")
#' plot(model, which = 4, type = "deviance", main = "Deviance")
#' par(mfrow = c(1, 1))
#'
#' # Quantile residuals for half-normal plot (recommended)
#' plot(model, which = 5, type = "quantile")
#'
#' # EXAMPLE 9: Family Comparison Diagnostics
#'
#' # Compare diagnostics under different distributional assumptions
#' # Helps assess if alternative family would fit better
#' par(mfrow = c(2, 2))
#' plot(model,
#'   which = c(5, 6),
#'   family = "kw", # Original family
#'   main = "Kumaraswamy"
#' )
#'
#' plot(model,
#'   which = c(5, 6),
#'   family = "beta", # Alternative family
#'   main = "Beta"
#' )
#' par(mfrow = c(1, 1))
#'
#' # EXAMPLE 10: Large Dataset - Performance Optimization
#'
#' # Simulate large dataset
#' set.seed(456)
#' n_large <- 50000
#' x1_large <- runif(n_large, -2, 2)
#' x2_large <- rnorm(n_large)
#' alpha_large <- exp(0.5 + 0.2 * x1_large)
#' beta_large <- exp(1.0 - 0.1 * x2_large)
#' y_large <- rkw(n_large, alpha = alpha_large, beta = beta_large)
#' df_large <- data.frame(y = y_large, x1 = x1_large, x2 = x2_large)
#'
#' model_large <- gkwreg(y ~ x1 | x2, data = df_large, family = "kw")
#'
#' # Optimized plotting for large dataset
#' par(mfrow = c(2, 2), mar = c(3, 3, 2, 2))
#' plot(model_large,
#'   which = c(1, 2, 4, 6), # Skip computationally intensive plot 5
#'   sample_size = 2000, # Use random sample of 2000 observations
#'   ask = FALSE
#' ) # Don't prompt
#'
#' # If half-normal plot needed, reduce simulations
#' par(mfrow = c(1, 1))
#' plot(model_large,
#'   which = 5,
#'   sample_size = 1000, # Smaller sample
#'   nsim = 50
#' ) # Fewer simulations
#'
#' # EXAMPLE 11: Saving Diagnostic Data for Custom Analysis
#'
#' # Extract diagnostic measures without plotting
#' par(mfrow = c(1, 1))
#' diag_data <- plot(model_large,
#'   which = 1:6,
#'   save_diagnostics = TRUE
#' )
#'
#' # Examine structure
#' str(diag_data)
#'
#' # Access diagnostic measures
#' head(diag_data$data) # Residuals, Cook's distance, leverage, etc.
#'
#' # Identify influential observations
#' influential <- which(diag_data$data$cook_dist > diag_data$model_info$cook_threshold)
#' cat("Influential observations:", head(influential), "\n")
#'
#' # High leverage points
#' high_lev <- which(diag_data$data$leverage > diag_data$model_info$leverage_threshold)
#' cat("High leverage points:", head(high_lev), "\n")
#'
#' # Custom diagnostic plot using saved data
#' plot(diag_data$data$fitted, diag_data$data$resid,
#'   xlab = "Fitted Values", ylab = "Residuals",
#'   main = "Custom Diagnostic Plot",
#'   col = ifelse(diag_data$data$cook_dist >
#'     diag_data$model_info$cook_threshold, "red", "black"),
#'   pch = 16
#' )
#' abline(h = 0, col = "gray", lty = 2)
#' legend("topright", legend = "Influential", col = "red", pch = 16)
#'
#' # EXAMPLE 12: Interactive Plotting Control
#'
#' # ask = TRUE Force prompting between plots (useful for presentations)
#' # Disable prompting (batch processing)
#' par(mfrow = c(3, 2))
#' plot(model,
#'   which = 1:6,
#'   ask = FALSE
#' ) # Never prompts
#'
#' # EXAMPLE 13: Base R Graphics Customization via ...
#'
#' # Customize point appearance
#' par(mfrow = c(2, 2))
#' plot(model,
#'   which = c(1, 4, 6),
#'   pch = 16, # Filled circles
#'   col = "steelblue", # Blue points
#'   cex = 0.8
#' ) # Smaller points
#'
#' # Multiple customizations
#' plot(model,
#'   which = 2,
#'   pch = 21, # Circles with border
#'   col = "black", # Border color
#'   bg = "lightblue", # Fill color
#'   cex = 1.2, # Larger points
#'   lwd = 2
#' ) # Thicker lines
#'
#' # EXAMPLE 14: Comparing Models
#'
#' # Fit competing models
#' model_kw <- gkwreg(y ~ x1 | x2, data = df, family = "kw")
#' model_beta <- gkwreg(y ~ x1 | x2, data = df, family = "beta")
#'
#' # Compare diagnostics side-by-side
#' par(mfrow = c(2, 2))
#'
#' # Kumaraswamy model
#' plot(model_kw, which = 5, main = "Kumaraswamy - Half-Normal")
#' plot(model_kw, which = 6, main = "Kumaraswamy - Pred vs Obs")
#'
#' # Beta model
#' plot(model_beta, which = 5, main = "Beta - Half-Normal")
#' plot(model_beta, which = 6, main = "Beta - Pred vs Obs")
#'
#' par(mfrow = c(1, 1))
#' }
#'
#' @export
plot.gkwreg <- function(x,
                        which = 1:6,
                        type = c("quantile", "pearson", "deviance"),
                        family = NULL,
                        caption = NULL,
                        main = "",
                        sub.caption = "",
                        ask = NULL,
                        use_ggplot = FALSE,
                        arrange_plots = FALSE,
                        nsim = 100,
                        level = 0.90,
                        sample_size = NULL,
                        theme_fn = NULL,
                        save_diagnostics = FALSE,
                        ...) {
  default_captions <- list(
    "1" = "Residuals vs. Observation Indices",
    "2" = "Cook's Distance Plot",
    "3" = "Generalized Leverage vs. Fitted Values",
    "4" = "Residuals vs. Linear Predictor",
    "5" = "Half-Normal Plot of Residuals",
    "6" = "Predicted vs. Observed Values"
  )

  if (is.null(caption)) {
    # Use defaults
    caption_list <- default_captions
  } else if (is.character(caption)) {
    # BACKWARD COMPATIBILITY: Convert vector to list
    if (is.null(names(caption))) {
      # Unnamed vector - assume ordered 1:6
      caption_list <- as.list(stats::setNames(caption, as.character(1:length(caption))))
      # Fill missing with defaults
      for (i in 1:6) {
        if (!as.character(i) %in% names(caption_list)) {
          caption_list[[as.character(i)]] <- default_captions[[as.character(i)]]
        }
      }
    } else {
      # Named vector - convert to list
      caption_list <- as.list(caption)
      # Merge with defaults for missing plots
      caption_list <- utils::modifyList(default_captions, caption_list)
    }
  } else if (is.list(caption)) {
    # NEW BEHAVIOR: Named list - merge with defaults
    caption_list <- utils::modifyList(default_captions, caption)
  } else {
    stop("'caption' must be NULL, a character vector, or a named list")
  }

  # Convert list back to vector for legacy code compatibility
  caption_vec <- character(6)
  for (i in 1:6) {
    caption_vec[i] <- caption_list[[as.character(i)]]
  }

  # 2. SUB.CAPTION: Auto-generate if NULL
  if (is.null(sub.caption)) {
    sub.caption <- paste(deparse(x$call), collapse = "\n")
  }

  # 3. ASK: Auto-detect if NULL
  if (is.null(ask)) {
    ask <- prod(par("mfcol")) < length(which) && dev.interactive()
  }

  # 4. THEME_FN: Default to theme_minimal if using ggplot
  if (is.null(theme_fn) && use_ggplot) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      theme_fn <- ggplot2::theme_minimal
    } else {
      warning("ggplot2 not available, theme_fn set to NULL")
      theme_fn <- function() {
        NULL
      } # Dummy function
    }
  } else if (is.null(theme_fn)) {
    # Not using ggplot, set dummy function
    theme_fn <- function() {
      NULL
    }
  }

  # 5. VALIDATE OTHER ARGUMENTS
  type <- match.arg(type)

  if (!all(which %in% 1:6)) {
    stop("Argument 'which' must contain values between 1 and 6.")
  }

  if (max(which) > length(caption_vec)) {
    stop("The 'caption' vector is too short for the selected 'which' plots.")
  }

  if (!is.numeric(nsim) || nsim <= 0 || nsim != round(nsim)) {
    stop("Argument 'nsim' must be a positive integer.")
  }

  if (!is.numeric(level) || level <= 0 || level >= 1) {
    stop("Argument 'level' must be between 0 and 1.")
  }

  # 6. CHECK DEPENDENCIES
  if (use_ggplot && !requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for ggplot2 plotting. Please install it with install.packages('ggplot2')."
    )
  }

  if (use_ggplot && arrange_plots &&
    !requireNamespace("gridExtra", quietly = TRUE) &&
    !requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      "Either package 'gridExtra' or 'ggpubr' is required for arranging plots. Please install one of them."
    )
  }

  # DIAGNOSTIC DATA PREPARATION
  # Validate and prepare diagnostic data using the helper function
  diag_data <- .validate_and_prepare_gkwreg_diagnostics(
    x = x,
    which = which,
    caption = caption_vec,
    type = type,
    family = family,
    nsim = nsim,
    level = level,
    use_ggplot = use_ggplot,
    arrange_plots = arrange_plots,
    sample_size = sample_size,
    theme_fn = theme_fn
  )

  if (save_diagnostics) {
    return(invisible(diag_data))
  }

  # CREATE PLOT TITLES
  # Get formatted plot titles
  plot_titles <- .create_plot_titles(
    which = which,
    caption = caption_vec,
    main = main,
    family = diag_data$model_info$family,
    orig_family = x$family
  )


  # GENERATE PLOTS
  # Choose plotting implementation based on use_ggplot
  if (!use_ggplot) {
    # ----------------------------------------------------------------------- #
    # BASE R GRAPHICS IMPLEMENTATION
    # ----------------------------------------------------------------------- #
    result <- .plot_gkwreg_base_r(
      diag_data = diag_data,
      which = which,
      plot_titles = plot_titles,
      sub.caption = sub.caption,
      ask = ask,
      ...
    )
  } else {
    # ----------------------------------------------------------------------- #
    # GGPLOT2 IMPLEMENTATION
    # ----------------------------------------------------------------------- #
    result <- .plot_gkwreg_ggplot(
      diag_data = diag_data,
      which = which,
      plot_titles = plot_titles,
      sub.caption = sub.caption,
      ask = FALSE,
      # ggplot doesn't support interactive ask
      arrange_plots = arrange_plots,
      theme_fn = theme_fn,
      ...
    )
  }
  return(invisible(x))
}

# HELPER FUNCTION: Validate inputs and prepare diagnostic data
.validate_and_prepare_gkwreg_diagnostics <- function(x,
                                                     which,
                                                     caption,
                                                     type = c("quantile", "pearson", "deviance"),
                                                     family = NULL,
                                                     nsim = 100,
                                                     level = 0.90,
                                                     use_ggplot = FALSE,
                                                     arrange_plots = FALSE,
                                                     sample_size = NULL,
                                                     theme_fn = ggplot2::theme_minimal) {
  # Input validation
  if (!inherits(x, "gkwreg")) {
    stop("The object must be of class 'gkwreg'.")
  }

  type <- match.arg(type)

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(x$family)) {
      family <- x$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, show a message
    if (!is.null(x$family) && family != x$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        x$family, ") for diagnostics."
      ))
    }
  }

  # Get parameter information for the family
  param_info <- .get_family_param_info(family)

  # Extract model components with error handling
  y_obs <- x$y
  if (is.null(y_obs)) {
    stop("No 'y' component found in the model object. Ensure the model was fitted with y=TRUE.")
  }

  # Get fitted values using the specified family
  fitted_vals <- fitted(x, family = family)
  if (is.null(fitted_vals)) {
    stop("Could not calculate fitted values.")
  }

  # Extract model matrices and parameters using the improved functions
  model_matrices <- .extract_model_matrices(x)
  model_params <- .extract_model_params(x)

  # Sample data if requested
  n <- length(y_obs)
  idx <- seq_len(n)

  if (!is.null(sample_size) && is.numeric(sample_size) && sample_size > 0 && sample_size < n) {
    sampling_result <- .sample_model_data(
      n = n,
      sample_size = sample_size,
      y_obs = y_obs,
      fitted_vals = fitted_vals,
      model_matrices = model_matrices
    )

    idx <- sampling_result$idx
    y_obs <- sampling_result$y_obs
    fitted_vals <- sampling_result$fitted_vals
    model_matrices <- sampling_result$model_matrices
  }

  # Calculate model parameters for the specified family
  param_mat <- .calculate_model_parameters(model_matrices, model_params, family)

  # Extract parameter vectors
  param_vectors <- .extract_parameter_vectors(param_mat)

  # Calculate residuals
  resid_vec <- .calculate_residuals(y_obs, fitted_vals, param_mat, type, family)

  # Calculate diagnostic measures with family-specific handling
  diagnostic_measures <- .calculate_diagnostic_measures(
    y_obs = y_obs,
    fitted_vals = fitted_vals,
    resid_vec = resid_vec,
    model_matrices = model_matrices,
    model_params = model_params,
    param_vectors = param_vectors,
    idx = idx,
    family = family,
    param_info = param_info
  )

  # Calculate half-normal plot data if needed
  half_normal_data <- NULL
  if (5 %in% which) {
    half_normal_data <- .calculate_half_normal_data(
      resid_vec = resid_vec,
      idx = idx,
      nsim = nsim,
      level = level,
      param_mat = param_mat,
      param_vectors = param_vectors,
      type = type,
      family = family
    )
  }

  # Create the diagnostic data structure
  diag_data <- list(
    data = data.frame(
      index = idx,
      y_obs = y_obs,
      fitted = fitted_vals,
      resid = resid_vec,
      abs_resid = abs(resid_vec),
      cook_dist = diagnostic_measures$cook_dist,
      leverage = diagnostic_measures$leverage,
      linpred = diagnostic_measures$linpred
    ),
    model_info = list(
      n = n,
      p = diagnostic_measures$p,
      cook_threshold = 4 / n,
      leverage_threshold = 2 * diagnostic_measures$p / n,
      family = family,
      type = type,
      param_info = param_info,
      level = level
    )
  )

  # Add half-normal data if available
  if (!is.null(half_normal_data)) {
    diag_data$half_normal <- half_normal_data
  }

  return(diag_data)
}

# HELPER FUNCTION: Extract model matrices with family-specific handling
.extract_model_matrices <- function(x) {
  # Get family parameter information
  family <- x$family
  if (is.null(family)) {
    family <- "gkw" # Default to gkw if not specified
    warning("No family specified in the model. Using 'gkw' as default.")
  }

  # Get parameter information for the specified family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions

  # Initialize matrices list with the correct number for this family
  num_matrices <- max(unlist(param_positions))
  matrices <- vector("list", num_matrices)
  names(matrices) <- paste0("X", 1:num_matrices)

  # Try different ways to get design matrices
  if (!is.null(x$x)) {
    # Model was fitted with x=TRUE
    for (param in param_names) {
      pos <- param_positions[[param]]
      if (param %in% names(x$x)) {
        matrices[[pos]] <- x$x[[param]]
      } else {
        # Default to intercept-only for missing matrices
        n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
        matrices[[pos]] <- matrix(1, nrow = n_obs, ncol = 1)
        colnames(matrices[[pos]]) <- "(Intercept)"
      }
    }
  } else if (!is.null(x$model) && !is.null(x$formula)) {
    # Recreate model matrices from the model frame and formula
    mf <- x$model
    formula_obj <- x$formula
    if (inherits(formula_obj, "formula")) {
      formula_obj <- Formula::as.Formula(formula_obj)
    }

    # Get number of rhs parts in the formula
    n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

    # Extract matrices for each parameter
    for (param in param_names) {
      pos <- param_positions[[param]]
      idx <- which(param_names == param)

      if (idx <= n_parts) {
        # Try to extract matrix using the formula part
        matrices[[pos]] <- tryCatch(
          model.matrix(formula_obj, data = mf, rhs = idx),
          error = function(e) matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
        )
      } else {
        # Default to intercept-only
        matrices[[pos]] <- matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
      }
    }
  } else if (!is.null(x$tmb_object) && !is.null(x$tmb_object$env$data)) {
    # Use TMB object if available
    tmb_data <- x$tmb_object$env$data

    # Extract matrices by their TMB position
    for (param in param_names) {
      pos <- param_positions[[param]]
      tmb_matrix_name <- paste0("X", pos)

      if (!is.null(tmb_data[[tmb_matrix_name]])) {
        matrices[[pos]] <- tmb_data[[tmb_matrix_name]]

        # Add column names if missing
        if (is.null(colnames(matrices[[pos]]))) {
          if (ncol(matrices[[pos]]) == 1) {
            colnames(matrices[[pos]]) <- "(Intercept)"
          } else {
            colnames(matrices[[pos]]) <- paste0(param, "_", 1:ncol(matrices[[pos]]))
          }
        }
      }
    }
  } else {
    stop("Cannot extract model matrices. Try fitting the model with x=TRUE.")
  }

  # Check for missing matrices and replace with default
  for (i in 1:num_matrices) {
    if (is.null(matrices[[i]])) {
      n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
      matrices[[i]] <- matrix(1, n_obs, 1, dimnames = list(NULL, "(Intercept)"))
    }
  }

  return(matrices)
}


# HELPER FUNCTION: Extract model parameters with family-specific handling
.extract_model_params <- function(x) {
  # Get family information
  family <- x$family
  if (is.null(family)) family <- "gkw"

  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions
  fixed_params <- param_info$fixed

  # Initialize beta parameters for all possible positions
  beta_params <- vector("list", 5)
  names(beta_params) <- paste0("beta", 1:5)

  # Extract coefficients
  coefs <- x$coefficients

  if (is.list(coefs)) {
    # If coefficients are a list with parameter names
    for (param in param_names) {
      if (param %in% names(coefs)) {
        pos <- param_positions[[param]]
        beta_params[[pos]] <- coefs[[param]]
      }
    }
  } else if (!is.null(names(coefs))) {
    # If coefficients are named, extract them using regex patterns
    for (param in param_names) {
      pos <- param_positions[[param]]
      param_coefs <- coefs[grep(paste0("^", param, ":"), names(coefs))]
      if (length(param_coefs) > 0) {
        beta_params[[pos]] <- param_coefs
      }
    }

    # If regex didn't work, try alternative pattern matching
    empty_positions <- sapply(beta_params[1:length(param_names)], is.null)
    if (all(empty_positions)) {
      # Try alternative pattern matching
      for (param in param_names) {
        pos <- param_positions[[param]]
        param_coefs <- coefs[grep(param, names(coefs))]
        if (length(param_coefs) > 0) {
          beta_params[[pos]] <- param_coefs
        }
      }

      # If still empty, raise error
      if (all(sapply(beta_params[1:length(param_names)], is.null))) {
        stop("Cannot determine coefficient mapping. Try a more recent version of the model.")
      }
    }
  } else {
    stop("Unrecognized coefficient structure. Try a more recent version of the model.")
  }

  # Assign default values for fixed parameters
  for (param in names(fixed_params)) {
    pos <- param_positions[[param]]
    if (!is.null(pos)) {
      # Use the fixed value for this parameter
      beta_params[[pos]] <- fixed_params[[param]]
    }
  }

  # Assign default values for any remaining NULL betas
  for (i in 1:5) {
    if (is.null(beta_params[[i]])) {
      beta_params[[i]] <- 0
    }
  }

  # Extract link information
  if (!is.null(x$link_codes)) {
    link_codes <- unlist(x$link_codes, use.names = FALSE)
  } else if (!is.null(x$link)) {
    # Convert link functions to codes
    link_map <- c(
      "log" = 1,
      "logit" = 2,
      "probit" = 3,
      "cauchy" = 4,
      "cloglog" = 5,
      "identity" = 6,
      "sqrt" = 7,
      "inverse" = 8,
      "inverse-square" = 9
    )

    # Default links (log for most, logit for delta)
    link_codes <- c(1, 1, 1, 2, 1)

    # Update with actual links from the model
    for (param in param_names) {
      if (param %in% names(x$link)) {
        pos <- param_positions[[param]]
        link_codes[pos] <- link_map[x$link[[param]]]
      }
    }
  } else {
    # Default link types if not specified
    link_codes <- c(1, 1, 1, 2, 1) # log for most parameters, logit for delta
  }

  # Extract scale factors
  if (!is.null(x$scale_factors)) {
    scale_factors <- as.numeric(unlist(x$scale_factors))
  } else {
    # Default scale factors
    scale_factors <- c(10, 10, 10, 1, 10) # 10 for most parameters, 1 for delta
  }

  return(c(
    beta_params,
    list(
      link_codes = link_codes,
      scale_factors = scale_factors
    )
  ))
}

# HELPER FUNCTION: Sample model data for large datasets
.sample_model_data <- function(n, sample_size, y_obs, fitted_vals, model_matrices) {
  # For reproducibility
  idx <- sample(n, size = min(sample_size, n))

  # Sample matrices - ensure we maintain the structure
  sampled_matrices <- list()
  for (matrix_name in names(model_matrices)) {
    X <- model_matrices[[matrix_name]]
    if (is.matrix(X) && nrow(X) == n) {
      sampled_matrices[[matrix_name]] <- X[idx, , drop = FALSE]
    } else {
      # Handle non-matrix or incorrectly sized matrix
      sampled_matrices[[matrix_name]] <- matrix(1, length(idx), 1)
    }
  }

  return(list(
    idx = idx,
    y_obs = y_obs[idx],
    fitted_vals = fitted_vals[idx],
    model_matrices = sampled_matrices
  ))
}


# HELPER FUNCTION: Calculate model parameters for the specified family
.calculate_model_parameters <- function(model_matrices, model_params, family) {
  # Get parameter information for this family
  param_info <- .get_family_param_info(family)

  # Calculate the number of required parameters based on family
  num_params <- max(unlist(param_info$positions))

  # Extract matrices for all parameters
  X_matrices <- vector("list", 5)
  for (i in 1:5) {
    X_name <- paste0("X", i)
    if (i <= num_params && X_name %in% names(model_matrices)) {
      X_matrices[[i]] <- model_matrices[[X_name]]
    } else {
      # Default matrix for unused parameters
      X_matrices[[i]] <- matrix(1, nrow(model_matrices[[1]]), 1)
    }
  }

  # Extract beta parameters
  beta_params <- vector("list", 5)
  for (i in 1:5) {
    beta_name <- paste0("beta", i)
    if (beta_name %in% names(model_params)) {
      beta_params[[i]] <- model_params[[beta_name]]
    } else {
      # Default for unused parameters
      beta_params[[i]] <- 0
    }
  }

  # Extract link codes and scale factors
  link_codes <- model_params$link_codes
  scale_factors <- model_params$scale_factors

  # Call the calculateParameters function with proper unpacking
  param_mat <- calculateParameters(
    X1 = X_matrices[[1]],
    X2 = X_matrices[[2]],
    X3 = X_matrices[[3]],
    X4 = X_matrices[[4]],
    X5 = X_matrices[[5]],
    beta1 = as.vector(beta_params[[1]]),
    beta2 = as.vector(beta_params[[2]]),
    beta3 = as.vector(beta_params[[3]]),
    beta4 = as.vector(beta_params[[4]]),
    beta5 = as.vector(beta_params[[5]]),
    link_types = as.integer(link_codes),
    scale_factors = as.vector(scale_factors),
    family = as.character(family)
  )

  return(param_mat)
}


# HELPER FUNCTION: Extract parameter vectors from parameter matrix
.extract_parameter_vectors <- function(param_mat) {
  list(
    alphaVec = param_mat[, 1],
    betaVec = param_mat[, 2],
    gammaVec = param_mat[, 3],
    deltaVec = param_mat[, 4],
    lambdaVec = param_mat[, 5]
  )
}

# HELPER FUNCTION: Calculate residuals based on the specified type
.calculate_residuals <- function(y_obs, fitted_vals, param_mat, type, family) {
  # The following calculate*Residuals() are assumed to be internal package functions
  if (type == "quantile") {
    calculateQuantileResiduals(y_obs, param_mat, family = family)
  } else if (type == "pearson") {
    calculatePearsonResiduals(y_obs, fitted_vals, param_mat, family = family)
  } else if (type == "deviance") {
    calculateDevianceResiduals(y_obs, fitted_vals, param_mat, family = family)
  } else {
    stop("Unsupported residual type.")
  }
}

# HELPER FUNCTION: Calculate diagnostic measures for plots
.calculate_diagnostic_measures <- function(y_obs,
                                           fitted_vals,
                                           resid_vec,
                                           model_matrices,
                                           model_params,
                                           param_vectors,
                                           idx,
                                           family,
                                           param_info) {
  # Get the first active parameter for this family to use for linear predictor
  first_param <- param_info$names[1]
  first_pos <- param_info$positions[[first_param]]

  # Get the X matrix and beta coefficients for the first parameter
  X_matrix <- model_matrices[[paste0("X", first_pos)]]
  beta_coef <- model_params[[paste0("beta", first_pos)]]

  # Calculate linear predictor
  if (length(beta_coef) > 0 && !is.null(X_matrix)) {
    if (is.matrix(X_matrix) && is.numeric(beta_coef) &&
      ncol(X_matrix) == length(beta_coef)) {
      linpred <- as.vector(X_matrix %*% beta_coef)
    } else {
      # Fallback for incompatible dimensions
      linpred <- rep(0, length(idx))
    }
  } else {
    # Fallback for missing data
    linpred <- rep(0, length(idx))
  }

  # Calculate total number of parameters (excluding fixed ones)
  p <- 0
  for (param in param_info$names) {
    pos <- param_info$positions[[param]]
    beta_name <- paste0("beta", pos)
    if (beta_name %in% names(model_params)) {
      beta_val <- model_params[[beta_name]]
      if (is.numeric(beta_val) && length(beta_val) > 0) {
        p <- p + length(beta_val)
      }
    }
  }

  # Ensure p is at least 1 to avoid division by zero
  p <- max(1, p)

  # Mean squared error
  mse <- mean(resid_vec^2, na.rm = TRUE)

  # Approximate generalized leverage
  n <- length(idx)
  leverage <- rep(p / n, length(idx))

  # Small noise addition for visual differentiation
  leverage <- leverage + abs(stats::rnorm(length(idx), 0, 0.01))

  # Calculate Cook's distance
  cook_dist <- (resid_vec^2 / (p * mse)) * (leverage / ((1 - leverage)^2))
  cook_dist[is.infinite(cook_dist) | cook_dist > 100 | is.na(cook_dist)] <- NA

  list(
    linpred = linpred,
    p = p,
    leverage = leverage,
    cook_dist = cook_dist
  )
}

# HELPER FUNCTION: Calculate half-normal plot data with envelope
.calculate_half_normal_data <- function(resid_vec,
                                        idx,
                                        nsim,
                                        level,
                                        param_mat,
                                        param_vectors,
                                        type,
                                        family) {
  abs_resid <- abs(resid_vec)
  sorted_abs_resid <- sort(abs_resid)
  prob_points <- (seq_along(idx) - 0.5) / length(idx)
  hn_q <- stats::qnorm(0.5 + prob_points / 2) # half-normal quantiles from normal

  # Prepare data frame
  half_normal_data <- data.frame(
    index = seq_along(sorted_abs_resid),
    theoretical = hn_q,
    observed = sorted_abs_resid
  )

  # quantile

  # Extract parameter vectors for simulation
  alphaVec <- param_vectors$alphaVec
  betaVec <- param_vectors$betaVec
  gammaVec <- param_vectors$gammaVec
  deltaVec <- param_vectors$deltaVec
  lambdaVec <- param_vectors$lambdaVec

  # Simulate envelope
  envelope_data <- matrix(NA, nrow = length(idx), ncol = nsim)

  cat("Simulating envelope (", nsim, "iterations): ")
  progress_step <- max(1, floor(nsim / 10))

  for (i in seq_len(nsim)) {
    if (i %% progress_step == 0) cat(".")

    # Generate simulated data using family-specific random generation
    sim_y <- .simulate_from_distribution(
      n = length(idx),
      alphaVec = alphaVec,
      betaVec = betaVec,
      gammaVec = gammaVec,
      deltaVec = deltaVec,
      lambdaVec = lambdaVec,
      family = family
    )

    # Calculate residuals for simulated data
    sim_resid <- .calculate_sim_residuals(
      sim_y = sim_y,
      param_mat = param_mat,
      type = type,
      family = family
    )

    envelope_data[, i] <- sort(abs(sim_resid))
  }
  cat(" Done!\n")

  # Calculate envelope bounds
  lower_bound <- apply(envelope_data, 1, stats::quantile, probs = (1 - level) / 2, na.rm = TRUE)
  upper_bound <- apply(envelope_data, 1, stats::quantile, probs = 1 - (1 - level) / 2, na.rm = TRUE)

  half_normal_data$lower <- lower_bound
  half_normal_data$upper <- upper_bound

  half_normal_data
}

# HELPER FUNCTION: Simulate observations from a specified distribution family
.simulate_from_distribution <- function(n,
                                        alphaVec,
                                        betaVec,
                                        gammaVec,
                                        deltaVec,
                                        lambdaVec,
                                        family) {
  # Use the appropriate quantile function for each family
  switch(family,
    "gkw" = {
      rgkw(n,
        alpha = alphaVec, beta = betaVec, gamma = gammaVec,
        delta = deltaVec, lambda = lambdaVec
      )
    },
    "bkw" = {
      rbkw(n, alpha = alphaVec, beta = betaVec, gamma = gammaVec, delta = deltaVec)
    },
    "kkw" = {
      rkkw(n, alpha = alphaVec, beta = betaVec, delta = deltaVec, lambda = lambdaVec)
    },
    "ekw" = {
      rekw(n, alpha = alphaVec, beta = betaVec, lambda = lambdaVec)
    },
    "mc" = {
      rmc(n, gamma = gammaVec, delta = deltaVec, lambda = lambdaVec)
    },
    "kw" = {
      rkw(n, alpha = alphaVec, beta = betaVec)
    },
    "beta" = {
      rbeta_(n, gammaVec, deltaVec)
    },
    {
      warning("Unrecognized family '", family, "'. Using GKw distribution instead.")
      rgkw(n,
        alpha = alphaVec, beta = betaVec, gamma = gammaVec,
        delta = deltaVec, lambda = lambdaVec
      )
    }
  )
}


# HELPER FUNCTION: Calculate residuals for simulated data
.calculate_sim_residuals <- function(sim_y, param_mat, type, family) {
  if (type == "quantile") {
    calculateQuantileResiduals(sim_y, param_mat, family = family)
  } else if (type == "pearson") {
    sim_fitted <- calculateMeans(param_mat, family = family)
    calculatePearsonResiduals(sim_y, sim_fitted, param_mat, family = family)
  } else if (type == "deviance") {
    sim_fitted <- calculateMeans(param_mat, family = family)
    calculateDevianceResiduals(sim_y, sim_fitted, param_mat, family = family)
  } else {
    stop("Unsupported residual type.")
  }
}

# HELPER FUNCTION: Create formatted plot titles
.create_plot_titles <- function(which, caption, main, family, orig_family) {
  plot_titles <- rep("", length(which))
  names(plot_titles) <- which

  for (i in seq_along(which)) {
    i_plot <- which[i]
    base_title <- caption[i_plot]
    if (!is.null(orig_family) && family != orig_family) {
      base_title <- paste0(base_title, " (", family, ")")
    }
    if (nzchar(main)) {
      plot_titles[as.character(i_plot)] <- paste(main, "-", base_title)
    } else {
      plot_titles[as.character(i_plot)] <- base_title
    }
  }

  plot_titles
}

.plot_gkwreg_base_r <- function(diag_data, which, plot_titles, sub.caption, ask, ...) {
  # Configure interactive prompting if requested
  if (ask) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
  }

  type <- diag_data$model_info$type

  # Generate each requested diagnostic plot
  for (i_plot in which) {
    grDevices::dev.hold()
    p_main <- plot_titles[as.character(i_plot)]

    # Select and generate appropriate plot
    if (i_plot == 1) {
      .plot_base_r_residuals_vs_index(diag_data, p_main, type, ...)
    } else if (i_plot == 2) {
      .plot_base_r_cooks_distance(diag_data, p_main, ...)
    } else if (i_plot == 3) {
      .plot_base_r_leverage_vs_fitted(diag_data, p_main, ...)
    } else if (i_plot == 4) {
      .plot_base_r_residuals_vs_linpred(diag_data, p_main, type, ...)
    } else if (i_plot == 5) {
      .plot_base_r_half_normal(diag_data, p_main, type, ...)
    } else if (i_plot == 6) {
      .plot_base_r_predicted_vs_observed(diag_data, p_main, ...)
    }

    # Add model call as subtitle
    mtext(sub.caption, side = 3, line = 0.25, cex = 0.8, col = "gray40")

    grDevices::dev.flush()
  }

  invisible(NULL)
}

# .plot_gkwreg_base_r <- function(diag_data, which, plot_titles, sub.caption, ask, ...) {
#   old_par <- par(no.readonly = TRUE)
#   on.exit(par(old_par))
#   one.fig <- prod(par("mfcol")) == 1
#
#   if (ask) {
#     oask <- grDevices::devAskNewPage(TRUE)
#     on.exit(grDevices::devAskNewPage(oask), add = TRUE)
#   }
#
#   type <- diag_data$model_info$type
#
#   for (i_plot in which) {
#     grDevices::dev.hold()
#     p_main <- plot_titles[as.character(i_plot)]
#
#     if (i_plot == 1) {
#       .plot_base_r_residuals_vs_index(diag_data, p_main, type, ...)
#     } else if (i_plot == 2) {
#       .plot_base_r_cooks_distance(diag_data, p_main, ...)
#     } else if (i_plot == 3) {
#       .plot_base_r_leverage_vs_fitted(diag_data, p_main, ...)
#     } else if (i_plot == 4) {
#       .plot_base_r_residuals_vs_linpred(diag_data, p_main, type, ...)
#     } else if (i_plot == 5) {
#       .plot_base_r_half_normal(diag_data, p_main, type, ...)
#     } else if (i_plot == 6) {
#       .plot_base_r_predicted_vs_observed(diag_data, p_main, ...)
#     }
#     mtext(sub.caption, side = 3, line = 0.25, cex = 0.8, col = "gray40")
#
#     grDevices::dev.flush()
#
#     if (ask && !one.fig) {
#       message("Press <ENTER> to continue to the next plot...")
#       invisible(readLines(con = "stdin", n = 1))
#     }
#   }
#
#   invisible(NULL)
# }


# BASE R PLOT FUNCTIONS (Individual plots)
.plot_base_r_residuals_vs_index <- function(diag_data, p_main, type, ...) {
  plot(diag_data$data$index, diag_data$data$resid,
    ylab = paste0(type, " residuals"),
    xlab = "Observation Index",
    main = p_main, ...
  )
  abline(h = 0, lty = 2, col = "gray")
  lines(stats::lowess(diag_data$data$index, diag_data$data$resid),
    col = "red", lwd = 2
  )
  invisible(NULL)
}

.plot_base_r_cooks_distance <- function(diag_data, p_main, ...) {
  plot(diag_data$data$index, diag_data$data$cook_dist,
    ylab = "Cook's Distance",
    xlab = "Observation Index",
    main = p_main, ...
  )
  threshold <- diag_data$model_info$cook_threshold
  abline(h = threshold, lty = 2, col = "red")
  text(0.9 * max(diag_data$data$index), threshold * 1.1,
    "4/n",
    col = "red", pos = 3
  )
  invisible(NULL)
}

.plot_base_r_leverage_vs_fitted <- function(diag_data, p_main, ...) {
  plot(diag_data$data$fitted, diag_data$data$leverage,
    xlab = "Fitted Values",
    ylab = "Generalized Leverage",
    main = p_main, ...
  )
  threshold <- diag_data$model_info$leverage_threshold
  abline(h = threshold, lty = 2, col = "red")
  text(0.9 * max(diag_data$data$fitted), threshold * 1.1,
    "2p/n",
    col = "red", pos = 3
  )
  invisible(NULL)
}

.plot_base_r_residuals_vs_linpred <- function(diag_data, p_main, type, ...) {
  plot(diag_data$data$linpred, diag_data$data$resid,
    xlab = "Linear Predictor (alpha)",
    ylab = paste0(type, " residuals"),
    main = p_main, ...
  )
  abline(h = 0, lty = 2, col = "gray")
  lines(stats::lowess(diag_data$data$linpred, diag_data$data$resid),
    col = "red", lwd = 2
  )
  invisible(NULL)
}

.plot_base_r_half_normal <- function(diag_data, p_main, type, ...) {
  if (is.null(diag_data$half_normal)) {
    stats::qqnorm(abs(diag_data$data$resid),
      main = paste(p_main, "(Half-Normal)"),
      ylab = paste0("|", type, " residuals|"), ...
    )
    stats::qqline(abs(diag_data$data$resid), lty = 2, col = "gray")
    return(invisible(NULL))
  }

  hn_data <- diag_data$half_normal
  plot(hn_data$theoretical, hn_data$observed,
    xlab = "Theoretical Half-Normal Quantiles",
    ylab = paste0("Ordered |", type, " residuals|"),
    main = paste(p_main, "(Half-Normal)"), ...
  )

  abline(0, stats::sd(diag_data$data$abs_resid, na.rm = TRUE),
    lty = 2, col = "gray"
  )

  if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
    lines(hn_data$theoretical, hn_data$lower, lty = 2, col = "blue", lwd = 1.5)
    lines(hn_data$theoretical, hn_data$upper, lty = 2, col = "blue", lwd = 1.5)
    level <- diag_data$model_info$level
    text(max(hn_data$theoretical) * 0.8, max(hn_data$upper) * 0.9,
      paste0(format(100 * level, digits = 2), "% envelope"),
      col = "blue"
    )
  }
  invisible(NULL)
}

.plot_base_r_predicted_vs_observed <- function(diag_data, p_main, ...) {
  plot(diag_data$data$fitted, diag_data$data$y_obs,
    xlab = "Fitted (Mean)",
    ylab = "Observed (y)",
    main = p_main, ...
  )
  abline(0, 1, col = "gray", lty = 2)
  lines(stats::lowess(diag_data$data$fitted, diag_data$data$y_obs),
    col = "red", lwd = 2
  )
  invisible(NULL)
}

# HELPER FUNCTION: Generate diagnostic plots using ggplot2
.plot_gkwreg_ggplot <- function(diag_data,
                                which,
                                plot_titles,
                                sub.caption,
                                ask,
                                arrange_plots,
                                theme_fn,
                                ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for ggplot2 plotting.")
  }

  type <- diag_data$model_info$type
  p_list <- list()

  for (i in seq_along(which)) {
    i_plot <- which[i]
    p_main <- plot_titles[as.character(i_plot)]

    if (i_plot == 1) {
      p_list[[i]] <- .plot_ggplot_residuals_vs_index(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 2) {
      p_list[[i]] <- .plot_ggplot_cooks_distance(diag_data, p_main, sub.caption, theme_fn)
    } else if (i_plot == 3) {
      p_list[[i]] <- .plot_ggplot_leverage_vs_fitted(diag_data, p_main, sub.caption, theme_fn)
    } else if (i_plot == 4) {
      p_list[[i]] <- .plot_ggplot_residuals_vs_linpred(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 5) {
      p_list[[i]] <- .plot_ggplot_half_normal(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 6) {
      p_list[[i]] <- .plot_ggplot_predicted_vs_observed(diag_data, p_main, sub.caption, theme_fn)
    }
  }

  if (arrange_plots && length(p_list) > 1) {
    n_plots <- length(p_list)
    n_cols <- min(2, n_plots)
    n_rows <- ceiling(n_plots / n_cols)

    if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::grid.arrange(grobs = p_list, ncol = n_cols, nrow = n_rows)
    } else if (requireNamespace("ggpubr", quietly = TRUE)) {
      do.call(ggpubr::ggarrange, c(p_list, list(ncol = n_cols, nrow = n_rows)))
    } else {
      warning("Neither 'gridExtra' nor 'ggpubr' is installed. Displaying plots individually.")
      for (i in seq_along(p_list)) {
        print(p_list[[i]])
        if (ask && i < length(p_list)) {
          message("Press <ENTER> to continue to the next plot...")
          invisible(readLines(con = "stdin", n = 1))
        }
      }
    }
  } else {
    for (i in seq_along(p_list)) {
      print(p_list[[i]])
      if (ask && i < length(p_list)) {
        message("Press <ENTER> to continue to the next plot...")
        invisible(readLines(con = "stdin", n = 1))
      }
    }
  }

  invisible(NULL)
}

# GGPLOT2 PLOT FUNCTIONS (Individual plots)
.plot_ggplot_residuals_vs_index <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  ggplot2::ggplot(diag_data$data, ggplot2::aes(x = index, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Observation Index",
      y = paste0(type, " residuals"),
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}

.plot_ggplot_cooks_distance <- function(diag_data, p_main, sub.caption, theme_fn) {
  cook_ref <- diag_data$model_info$cook_threshold
  df_data <- diag_data$data

  p <- ggplot2::ggplot(df_data, ggplot2::aes(x = index, y = cook_dist)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = cook_ref, linetype = "dashed", color = "red") +
    ggplot2::annotate("text",
      x = max(df_data$index) * 0.9, y = cook_ref * 1.1,
      label = "4/n", color = "red", hjust = 0
    ) +
    ggplot2::labs(
      x = "Observation Index",
      y = "Cook's Distance",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()

  infl <- df_data$cook_dist > cook_ref & !is.na(df_data$cook_dist)
  if (any(infl)) {
    p <- p + ggplot2::geom_text(
      data = df_data[infl, ],
      ggplot2::aes(label = index),
      vjust = -0.5, color = "red", size = 3
    )
  }

  p
}

.plot_ggplot_leverage_vs_fitted <- function(diag_data, p_main, sub.caption, theme_fn) {
  lev_ref <- diag_data$model_info$leverage_threshold
  df_data <- diag_data$data

  p <- ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = leverage)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = lev_ref, linetype = "dashed", color = "red") +
    ggplot2::annotate("text",
      x = max(df_data$fitted) * 0.9, y = lev_ref * 1.1,
      label = "2p/n", color = "red", hjust = 0
    ) +
    ggplot2::labs(
      x = "Fitted Values",
      y = "Generalized Leverage",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()

  high_lev <- df_data$leverage > lev_ref
  if (any(high_lev)) {
    p <- p + ggplot2::geom_text(
      data = df_data[high_lev, ],
      ggplot2::aes(label = index),
      vjust = -0.5, color = "red", size = 3
    )
  }

  p
}

.plot_ggplot_residuals_vs_linpred <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  df_data <- diag_data$data
  ggplot2::ggplot(df_data, ggplot2::aes(x = linpred, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Linear Predictor (alpha)",
      y = paste0(type, " residuals"),
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}

.plot_ggplot_half_normal <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  level_value <- diag_data$model_info$level
  if (is.null(level_value)) {
    level_value <- 0.90
  }

  if (is.null(diag_data$half_normal)) {
    return(
      ggplot2::ggplot(diag_data$data, ggplot2::aes(sample = abs_resid)) +
        ggplot2::stat_qq(distribution = function(p) stats::qnorm(p * 0.5 + 0.5)) +
        ggplot2::stat_qq_line(
          distribution = function(p) stats::qnorm(p * 0.5 + 0.5),
          color = "gray", linetype = "dashed"
        ) +
        ggplot2::labs(
          x = "Theoretical Half-Normal Quantiles",
          y = paste0("|", type, " residuals|"),
          title = paste(p_main, "(Half-Normal)"),
          subtitle = sub.caption
        ) +
        theme_fn()
    )
  }

  hn_data <- diag_data$half_normal
  sd_abs <- stats::sd(diag_data$data$abs_resid, na.rm = TRUE)

  p <- ggplot2::ggplot(hn_data, ggplot2::aes(x = theoretical, y = observed)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      slope = sd_abs, intercept = 0,
      linetype = "dashed", color = "gray"
    ) +
    ggplot2::labs(
      x = "Theoretical Half-Normal Quantiles",
      y = paste0("Ordered |", type, " residuals|"),
      title = paste(p_main, "(Half-Normal)"),
      subtitle = sub.caption
    ) +
    theme_fn()

  if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = lower),
        linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::geom_line(ggplot2::aes(y = upper),
        linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = max(hn_data$theoretical) * 0.8,
        y = max(hn_data$upper) * 0.9,
        label = paste0(format(100 * level_value, digits = 2), "% envelope"),
        color = "blue"
      )
  }

  return(p)
}

.plot_ggplot_predicted_vs_observed <- function(diag_data, p_main, sub.caption, theme_fn) {
  df_data <- diag_data$data
  ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = y_obs)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      intercept = 0, slope = 1,
      color = "gray", linetype = "dashed"
    ) +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Fitted (Mean)",
      y = "Observed (y)",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}


#' #' @title Diagnostic Plots for Generalized Kumaraswamy Regression Models
#' #'
#' #' @description
#' #' Produces a set of diagnostic plots for assessing the adequacy of a fitted
#' #' Generalized Kumaraswamy (GKw) regression model (objects of class \code{"gkwreg"}).
#' #' Options allow selection of specific plots, choice of residual type, and plotting
#' #' using either base R graphics or \code{ggplot2}.
#' #'
#' #' @param x An object of class \code{"gkwreg"}, typically the result of a
#' #'   call to \code{\link{gkwreg}}.
#' #' @param which Integer vector specifying which diagnostic plots to produce.
#' #'   If a subset of the plots is required, specify a subset of the numbers 1:6.
#' #'   Defaults to \code{1:6}. The plots correspond to:
#' #'   \enumerate{
#' #'     \item Residuals vs. Observation Indices: Checks for time trends or patterns.
#' #'     \item Cook's Distance Plot: Helps identify influential observations.
#' #'     \item Generalized Leverage vs. Fitted Values: Identifies points with high leverage.
#' #'     \item Residuals vs. Linear Predictor: Checks for non-linearity and heteroscedasticity.
#' #'     \item Half-Normal Plot of Residuals (with simulated envelope): Assesses normality
#' #'       of residuals, comparing against simulated quantiles.
#' #'     \item Predicted vs. Observed Values: Checks overall model prediction accuracy.
#' #'   }
#' #' @param caption Character vector providing captions (titles) for the plots.
#' #'   Its length must be at least \code{max(which)}. Defaults are provided for plots 1-6.
#' #' @param sub.caption Character string used as a common subtitle positioned above all plots
#' #'   (especially when multiple plots are arranged). Defaults to the deparsed model call.
#' #' @param main An optional character string to be prepended to the individual plot captions
#' #'   (from the \code{caption} argument).
#' #' @param ask Logical. If \code{TRUE} (and using base R graphics with multiple plots
#' #'   on an interactive device), the user is prompted before displaying each plot.
#' #'   Defaults to \code{TRUE} if more plots are requested than fit on the current screen layout.
#' #' @param ... Additional arguments passed to the underlying plotting functions
#' #'   (e.g., graphical parameters like \code{col}, \code{pch}, \code{cex} for base R plots).
#' #' @param type Character string indicating the type of residuals to be used for plotting.
#' #'   Defaults to \code{"quantile"}. Valid options are:
#' #'   \itemize{
#' #'     \item \code{"quantile"}: Randomized quantile residuals (Dunn & Smyth, 1996).
#' #'       Recommended for bounded responses as they should be approximately N(0,1)
#' #'       if the model is correctly specified.
#' #'     \item \code{"pearson"}: Pearson residuals (response residual standardized by
#' #'       estimated standard deviation). Useful for checking the variance function.
#' #'     \item \code{"deviance"}: Deviance residuals. Related to the log-likelihood
#' #'       contribution of each observation.
#' #'   }
#' #' @param family Character string specifying the distribution family assumptions
#' #'   to use when calculating residuals and other diagnostics. If \code{NULL} (default),
#' #'   the family stored within the fitted \code{object} is used. Specifying a different
#' #'   family can be useful for diagnostic comparisons. Available options match those
#' #'   in \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' #' @param nsim Integer. Number of simulations used to generate the envelope in the
#' #'   half-normal plot (\code{which = 5}). Defaults to 100. Must be positive.
#' #' @param level Numeric. The confidence level (between 0 and 1) for the simulated
#' #'   envelope in the half-normal plot (\code{which = 5}). Defaults to 0.90.
#' #' @param use_ggplot Logical. If \code{TRUE}, plots are generated using the \code{ggplot2}
#' #'   package. If \code{FALSE} (default), base R graphics are used. Requires the
#' #'   \code{ggplot2} package to be installed if set to \code{TRUE}.
#' #' @param arrange_plots Logical. Only relevant if \code{use_ggplot = TRUE} and multiple
#' #'   plots are requested (\code{length(which) > 1}). If \code{TRUE}, attempts to arrange
#' #'   the generated \code{ggplot} objects into a grid using either the \code{gridExtra}
#' #'   or \code{ggpubr} package (requires one of them to be installed). Defaults to \code{FALSE}.
#' #' @param sample_size Integer or \code{NULL}. If specified as an integer less than the
#' #'   total number of observations (\code{x$nobs}), a random sample of this size is
#' #'   used for calculating diagnostics and plotting. This can be useful for speeding up
#' #'   plots with very large datasets. Defaults to \code{NULL} (use all observations).
#' #' @param theme_fn A function. Only relevant if \code{use_ggplot = TRUE}. Specifies a
#' #'   \code{ggplot2} theme function to apply to the plots (e.g., \code{theme_bw},
#' #'   \code{theme_classic}). Defaults to \code{ggplot2::theme_minimal}.
#' #' @param save_diagnostics Logical. If \code{TRUE}, the function invisibly returns a list
#' #'   containing the calculated diagnostic measures (residuals, leverage, Cook's distance, etc.)
#' #'   instead of the model object. If \code{FALSE} (default), the function invisibly
#' #'   returns the original model object \code{x}.
#' #'
#' #' @details
#' #' Diagnostic plots are essential for evaluating the assumptions and adequacy of
#' #' fitted regression models. This function provides several standard plots adapted
#' #' for \code{gkwreg} objects.
#' #'
#' #' The choice of residual type (\code{type}) is important. For models with bounded
#' #' responses like the GKw family, quantile residuals (\code{type = "quantile"}) are
#' #' generally preferred as they are constructed to be approximately normally distributed
#' #' under a correctly specified model, making standard diagnostic tools like QQ-plots
#' #' more directly interpretable.
#' #'
#' #' The plots help to assess:
#' #' \itemize{
#' #'   \item Plot 1 (Residuals vs. Index): Potential patterns or autocorrelation over time/index.
#' #'   \item Plot 2 (Cook's Distance): Observations with disproportionately large influence
#' #'     on the estimated coefficients.
#' #'   \item Plot 3 (Leverage vs. Fitted): Observations with unusual predictor combinations
#' #'     (high leverage) that might influence the fit.
#' #'   \item Plot 4 (Residuals vs. Linear Predictor): Non-linearity in the predictor-response
#' #'     relationship or non-constant variance (heteroscedasticity).
#' #'   \item Plot 5 (Half-Normal Plot): Deviations from the assumed residual distribution
#' #'     (ideally normal for quantile residuals). Points outside the simulated envelope
#' #'     are potentially problematic.
#' #'   \item Plot 6 (Predicted vs. Observed): Overall goodness-of-fit and potential systematic
#' #'     over- or under-prediction.
#' #' }
#' #' The function relies on internal helper functions to calculate the necessary diagnostic
#' #' quantities and generate the plots using either base R or \code{ggplot2}.
#' #'
#' #' @return Invisibly returns either the original fitted model object \code{x}
#' #'   (if \code{save_diagnostics = FALSE}) or a list containing the calculated
#' #'   diagnostic measures used for plotting (if \code{save_diagnostics = TRUE}).
#' #'   Primarily called for its side effect of generating plots.
#' #'
#' #' @author Lopes, J. E.
#' #'
#' #' @seealso \code{\link{gkwreg}}, \code{\link{residuals.gkwreg}},
#' #'   \code{\link{summary.gkwreg}}, \code{\link[stats]{plot.lm}},
#' #'   \code{\link[ggplot2]{ggplot}}, \code{\link[gridExtra]{grid.arrange}},
#' #'   \code{\link[ggpubr]{ggarrange}}
#' #'
#' #' @keywords plot methods regression diagnostics hplot
#' #'
#' #' @examples
#' #' \donttest{
#' #' # Assume 'mydata' exists with response 'y' and predictors 'x1', 'x2'
#' #' # and that rgkw() is available and data is appropriate (0 < y < 1).
#' #' set.seed(456)
#' #' n <- 150
#' #' x1 <- runif(n, -1, 1)
#' #' x2 <- rnorm(n)
#' #' alpha <- exp(0.5 + 0.2 * x1)
#' #' beta <- exp(0.8 - 0.3 * x1 + 0.1 * x2)
#' #' gamma <- exp(0.6)
#' #' delta <- plogis(0.0 + 0.2 * x1)
#' #' lambda <- exp(-0.2 + 0.1 * x2)
#' #' # Use stats::rbeta as placeholder if rgkw is not available
#' #' y <- stats::rbeta(n, shape1 = gamma * alpha, shape2 = delta * beta) # Approximation
#' #' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' #' mydata <- data.frame(y = y, x1 = x1, x2 = x2)
#' #'
#' #' # Fit a GKw model
#' #' model <- gkwreg(y ~ x1 | x1 + x2 | 1 | x1 | x2, data = mydata, family = "gkw")
#' #'
#' #' # --- Generate default base R plots (prompts for each plot) ---
#' #' plot(model)
#' #'
#' #' # --- Generate specific plots using base R ---
#' #' plot(model, which = c(1, 5), type = "quantile") # Residuals vs Index, Half-Normal
#' #'
#' #' # --- Generate plots using ggplot2 (requires ggplot2 package) ---
#' #' # Ensure ggplot2 is installed: install.packages("ggplot2")
#' #' plot(model, which = c(4, 6), use_ggplot = TRUE) # Res vs Lin Pred, Pred vs Obs
#' #'
#' #' # --- Generate all ggplot2 plots and arrange them (requires gridExtra or ggpubr) ---
#' #' # Ensure gridExtra is installed: install.packages("gridExtra")
#' #' # plot(model, use_ggplot = TRUE, arrange_plots = TRUE, ask = FALSE)
#' #'
#' #' # --- Generate plots using Pearson residuals ---
#' #' plot(model, which = 4, type = "pearson") # Res vs Lin Pred using Pearson residuals
#' #'
#' #' # --- Save diagnostic measures instead of plotting ---
#' #' diagnostics <- plot(model, save_diagnostics = TRUE)
#' #' head(diagnostics$residuals)
#' #' head(diagnostics$cooks_distance)
#' #' }
#' #'
#' #' @export
#' plot.gkwreg <- function(x,
#'                         which = 1:6,
#'                         caption = c(
#'                           "Residuals vs. Observation Indices",
#'                           "Cook's Distance Plot",
#'                           "Generalized Leverage vs. Fitted Values",
#'                           "Residuals vs. Linear Predictor",
#'                           "Half-Normal Plot of Residuals",
#'                           "Predicted vs. Observed Values"
#'                         ),
#'                         sub.caption = paste(deparse(x$call), collapse = "\n"),
#'                         main = "",
#'                         ask = prod(par("mfcol")) < length(which) && dev.interactive(),
#'                         ...,
#'                         type = c("quantile", "pearson", "deviance"),
#'                         family = NULL,
#'                         nsim = 100,
#'                         level = 0.90,
#'                         use_ggplot = FALSE,
#'                         arrange_plots = FALSE,
#'                         sample_size = NULL,
#'                         theme_fn = ggplot2::theme_minimal,
#'                         save_diagnostics = FALSE) {
#'   # Validate inputs and prepare diagnostic data using the fixed function
#'   diag_data <- .validate_and_prepare_gkwreg_diagnostics(
#'     x = x,
#'     which = which,
#'     caption = caption,
#'     type = type,
#'     family = family,
#'     nsim = nsim,
#'     level = level,
#'     use_ggplot = use_ggplot,
#'     arrange_plots = arrange_plots,
#'     sample_size = sample_size,
#'     theme_fn = theme_fn
#'   )
#'
#'   # Get formatted plot titles
#'   plot_titles <- .create_plot_titles(
#'     which = which,
#'     caption = caption,
#'     main = main,
#'     family = diag_data$model_info$family,
#'     orig_family = x$family
#'   )
#'
#'   # Choose plotting implementation based on use_ggplot
#'   if (!use_ggplot) {
#'     # Base R graphics implementation
#'     result <- .plot_gkwreg_base_r(
#'       diag_data = diag_data,
#'       which = which,
#'       plot_titles = plot_titles,
#'       sub.caption = sub.caption,
#'       ask = ask,
#'       ...
#'     )
#'   } else {
#'     # ggplot2 implementation
#'     result <- .plot_gkwreg_ggplot(
#'       diag_data = diag_data,
#'       which = which,
#'       plot_titles = plot_titles,
#'       sub.caption = sub.caption,
#'       ask = FALSE,
#'       arrange_plots = arrange_plots,
#'       theme_fn = theme_fn,
#'       ...
#'     )
#'   }
#'
#'   # Return diagnostic data if requested
#'   if (save_diagnostics) {
#'     return(invisible(diag_data))
#'   } else {
#'     return(invisible(x))
#'   }
#' }
#'
#'
#'
#'
#' #' Validate inputs and prepare diagnostic data for gkwreg plots
#' #'
#' #' @param x A fitted model object of class "gkwreg"
#' #' @param which Integer vector specifying which plots to produce
#' #' @param caption Character vector of plot captions
#' #' @param type Character string specifying residual type
#' #' @param family Character string specifying distribution family
#' #' @param nsim Number of simulations for envelope calculation
#' #' @param level Confidence level for envelope
#' #' @param use_ggplot Logical; whether to use ggplot2
#' #' @param arrange_plots Logical; whether to arrange multiple plots
#' #' @param sample_size Integer or NULL; sample size for large datasets
#' #' @param theme_fn ggplot2 theme function
#' #'
#' #' @return A list containing diagnostic data and model information
#' #'
#' #' @keywords internal
#' .validate_and_prepare_gkwreg_diagnostics <- function(x,
#'                                                      which,
#'                                                      caption,
#'                                                      type = c("quantile", "pearson", "deviance"),
#'                                                      family = NULL,
#'                                                      nsim = 100,
#'                                                      level = 0.90,
#'                                                      use_ggplot = FALSE,
#'                                                      arrange_plots = FALSE,
#'                                                      sample_size = NULL,
#'                                                      theme_fn = ggplot2::theme_minimal) {
#'   # Input validation
#'   if (!inherits(x, "gkwreg")) {
#'     stop("The object must be of class 'gkwreg'.")
#'   }
#'
#'   type <- match.arg(type)
#'
#'   # Get the family from the object if not specified
#'   if (is.null(family)) {
#'     if (!is.null(x$family)) {
#'       family <- x$family
#'     } else {
#'       # Default to gkw for backward compatibility
#'       family <- "gkw"
#'       message("No family specified in the model. Using 'gkw' as default.")
#'     }
#'   } else {
#'     # Validate the family parameter
#'     family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))
#'
#'     # If family is different from the model's family, show a message
#'     if (!is.null(x$family) && family != x$family) {
#'       message(paste0(
#'         "Using different family (", family, ") than what was used to fit the model (",
#'         x$family, ") for diagnostics."
#'       ))
#'     }
#'   }
#'
#'   # Get parameter information for the family
#'   param_info <- .get_family_param_info(family)
#'
#'   # Other input validations
#'   if (!all(which %in% 1:6)) {
#'     stop("Argument 'which' must contain values between 1 and 6.")
#'   }
#'
#'   if (max(which) > length(caption)) {
#'     stop("The 'caption' vector is too short for the selected 'which' plots.")
#'   }
#'
#'   if (!is.numeric(nsim) || nsim <= 0 || nsim != round(nsim)) {
#'     stop("Argument 'nsim' must be a positive integer.")
#'   }
#'
#'   if (!is.numeric(level) || level <= 0 || level >= 1) {
#'     stop("Argument 'level' must be between 0 and 1.")
#'   }
#'
#'   # Check dependencies
#'   if (use_ggplot && !requireNamespace("ggplot2", quietly = TRUE)) {
#'     stop("Package 'ggplot2' is required for ggplot2 plotting. Please install it with install.packages('ggplot2').")
#'   }
#'
#'   if (use_ggplot && arrange_plots &&
#'     !requireNamespace("gridExtra", quietly = TRUE) &&
#'     !requireNamespace("ggpubr", quietly = TRUE)) {
#'     stop("Either package 'gridExtra' or 'ggpubr' is required for arranging plots. Please install one of them.")
#'   }
#'
#'   # Extract model components with error handling
#'   y_obs <- x$y
#'   if (is.null(y_obs)) {
#'     stop("No 'y' component found in the model object. Ensure the model was fitted with y=TRUE.")
#'   }
#'
#'   # Get fitted values using the specified family
#'   fitted_vals <- fitted(x, family = family)
#'   if (is.null(fitted_vals)) {
#'     stop("Could not calculate fitted values.")
#'   }
#'
#'   # Extract model matrices and parameters using the improved functions
#'   model_matrices <- .extract_model_matrices(x)
#'   model_params <- .extract_model_params(x)
#'
#'   # Sample data if requested
#'   n <- length(y_obs)
#'   idx <- seq_len(n)
#'
#'   if (!is.null(sample_size) && is.numeric(sample_size) && sample_size > 0 && sample_size < n) {
#'     sampling_result <- .sample_model_data(
#'       n = n,
#'       sample_size = sample_size,
#'       y_obs = y_obs,
#'       fitted_vals = fitted_vals,
#'       model_matrices = model_matrices
#'     )
#'
#'     idx <- sampling_result$idx
#'     y_obs <- sampling_result$y_obs
#'     fitted_vals <- sampling_result$fitted_vals
#'     model_matrices <- sampling_result$model_matrices
#'   }
#'
#'   # Calculate model parameters for the specified family
#'   param_mat <- .calculate_model_parameters(model_matrices, model_params, family)
#'
#'   # Extract parameter vectors
#'   param_vectors <- .extract_parameter_vectors(param_mat)
#'
#'   # Calculate residuals
#'   resid_vec <- .calculate_residuals(y_obs, fitted_vals, param_mat, type, family)
#'
#'   # Calculate diagnostic measures with family-specific handling
#'   diagnostic_measures <- .calculate_diagnostic_measures(
#'     y_obs = y_obs,
#'     fitted_vals = fitted_vals,
#'     resid_vec = resid_vec,
#'     model_matrices = model_matrices,
#'     model_params = model_params,
#'     param_vectors = param_vectors,
#'     idx = idx,
#'     family = family,
#'     param_info = param_info
#'   )
#'
#'   # Calculate half-normal plot data if needed
#'   half_normal_data <- NULL
#'   if (5 %in% which) {
#'     half_normal_data <- .calculate_half_normal_data(
#'       resid_vec = resid_vec,
#'       idx = idx,
#'       nsim = nsim,
#'       level = level,
#'       param_mat = param_mat,
#'       param_vectors = param_vectors,
#'       type = type,
#'       family = family
#'     )
#'   }
#'
#'   # Create the diagnostic data structure
#'   diag_data <- list(
#'     data = data.frame(
#'       index = idx,
#'       y_obs = y_obs,
#'       fitted = fitted_vals,
#'       resid = resid_vec,
#'       abs_resid = abs(resid_vec),
#'       cook_dist = diagnostic_measures$cook_dist,
#'       leverage = diagnostic_measures$leverage,
#'       linpred = diagnostic_measures$linpred
#'     ),
#'     model_info = list(
#'       n = n,
#'       p = diagnostic_measures$p,
#'       cook_threshold = 4 / n,
#'       leverage_threshold = 2 * diagnostic_measures$p / n,
#'       family = family,
#'       type = type,
#'       param_info = param_info,
#'       level = level
#'     )
#'   )
#'
#'   # Add half-normal data if available
#'   if (!is.null(half_normal_data)) {
#'     diag_data$half_normal <- half_normal_data
#'   }
#'
#'   return(diag_data)
#' }
#'
#' #' Extract model matrices from a gkwreg object with family-specific handling
#' #'
#' #' @param x A fitted model object of class "gkwreg"
#' #' @return A list of model matrices
#' #'
#' #' @importFrom stats model.matrix
#' #' @keywords internal
#' .extract_model_matrices <- function(x) {
#'   # Get family parameter information
#'   family <- x$family
#'   if (is.null(family)) {
#'     family <- "gkw" # Default to gkw if not specified
#'     warning("No family specified in the model. Using 'gkw' as default.")
#'   }
#'
#'   # Get parameter information for the specified family
#'   param_info <- .get_family_param_info(family)
#'   param_names <- param_info$names
#'   param_positions <- param_info$positions
#'
#'   # Initialize matrices list with the correct number for this family
#'   num_matrices <- max(unlist(param_positions))
#'   matrices <- vector("list", num_matrices)
#'   names(matrices) <- paste0("X", 1:num_matrices)
#'
#'   # Try different ways to get design matrices
#'   if (!is.null(x$x)) {
#'     # Model was fitted with x=TRUE
#'     for (param in param_names) {
#'       pos <- param_positions[[param]]
#'       if (param %in% names(x$x)) {
#'         matrices[[pos]] <- x$x[[param]]
#'       } else {
#'         # Default to intercept-only for missing matrices
#'         n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
#'         matrices[[pos]] <- matrix(1, nrow = n_obs, ncol = 1)
#'         colnames(matrices[[pos]]) <- "(Intercept)"
#'       }
#'     }
#'   } else if (!is.null(x$model) && !is.null(x$formula)) {
#'     # Recreate model matrices from the model frame and formula
#'     mf <- x$model
#'     formula_obj <- x$formula
#'     if (inherits(formula_obj, "formula")) {
#'       formula_obj <- Formula::as.Formula(formula_obj)
#'     }
#'
#'     # Get number of rhs parts in the formula
#'     n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))
#'
#'     # Extract matrices for each parameter
#'     for (param in param_names) {
#'       pos <- param_positions[[param]]
#'       idx <- which(param_names == param)
#'
#'       if (idx <= n_parts) {
#'         # Try to extract matrix using the formula part
#'         matrices[[pos]] <- tryCatch(
#'           model.matrix(formula_obj, data = mf, rhs = idx),
#'           error = function(e) matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
#'         )
#'       } else {
#'         # Default to intercept-only
#'         matrices[[pos]] <- matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
#'       }
#'     }
#'   } else if (!is.null(x$tmb_object) && !is.null(x$tmb_object$env$data)) {
#'     # Use TMB object if available
#'     tmb_data <- x$tmb_object$env$data
#'
#'     # Extract matrices by their TMB position
#'     for (param in param_names) {
#'       pos <- param_positions[[param]]
#'       tmb_matrix_name <- paste0("X", pos)
#'
#'       if (!is.null(tmb_data[[tmb_matrix_name]])) {
#'         matrices[[pos]] <- tmb_data[[tmb_matrix_name]]
#'
#'         # Add column names if missing
#'         if (is.null(colnames(matrices[[pos]]))) {
#'           if (ncol(matrices[[pos]]) == 1) {
#'             colnames(matrices[[pos]]) <- "(Intercept)"
#'           } else {
#'             colnames(matrices[[pos]]) <- paste0(param, "_", 1:ncol(matrices[[pos]]))
#'           }
#'         }
#'       }
#'     }
#'   } else {
#'     stop("Cannot extract model matrices. Try fitting the model with x=TRUE.")
#'   }
#'
#'   # Check for missing matrices and replace with default
#'   for (i in 1:num_matrices) {
#'     if (is.null(matrices[[i]])) {
#'       n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
#'       matrices[[i]] <- matrix(1, n_obs, 1, dimnames = list(NULL, "(Intercept)"))
#'     }
#'   }
#'
#'   return(matrices)
#' }
#'
#'
#' #' Extract model parameters from a gkwreg object with family-specific handling
#' #'
#' #' @param x A fitted model object of class "gkwreg"
#' #' @return A list of model parameters
#' #'
#' #' @keywords internal
#' .extract_model_params <- function(x) {
#'   # Get family information
#'   family <- x$family
#'   if (is.null(family)) family <- "gkw"
#'
#'   param_info <- .get_family_param_info(family)
#'   param_names <- param_info$names
#'   param_positions <- param_info$positions
#'   fixed_params <- param_info$fixed
#'
#'   # Initialize beta parameters for all possible positions
#'   beta_params <- vector("list", 5)
#'   names(beta_params) <- paste0("beta", 1:5)
#'
#'   # Extract coefficients
#'   coefs <- x$coefficients
#'
#'   if (is.list(coefs)) {
#'     # If coefficients are a list with parameter names
#'     for (param in param_names) {
#'       if (param %in% names(coefs)) {
#'         pos <- param_positions[[param]]
#'         beta_params[[pos]] <- coefs[[param]]
#'       }
#'     }
#'   } else if (!is.null(names(coefs))) {
#'     # If coefficients are named, extract them using regex patterns
#'     for (param in param_names) {
#'       pos <- param_positions[[param]]
#'       param_coefs <- coefs[grep(paste0("^", param, ":"), names(coefs))]
#'       if (length(param_coefs) > 0) {
#'         beta_params[[pos]] <- param_coefs
#'       }
#'     }
#'
#'     # If regex didn't work, try alternative pattern matching
#'     empty_positions <- sapply(beta_params[1:length(param_names)], is.null)
#'     if (all(empty_positions)) {
#'       # Try alternative pattern matching
#'       for (param in param_names) {
#'         pos <- param_positions[[param]]
#'         param_coefs <- coefs[grep(param, names(coefs))]
#'         if (length(param_coefs) > 0) {
#'           beta_params[[pos]] <- param_coefs
#'         }
#'       }
#'
#'       # If still empty, raise error
#'       if (all(sapply(beta_params[1:length(param_names)], is.null))) {
#'         stop("Cannot determine coefficient mapping. Try a more recent version of the model.")
#'       }
#'     }
#'   } else {
#'     stop("Unrecognized coefficient structure. Try a more recent version of the model.")
#'   }
#'
#'   # Assign default values for fixed parameters
#'   for (param in names(fixed_params)) {
#'     pos <- param_positions[[param]]
#'     if (!is.null(pos)) {
#'       # Use the fixed value for this parameter
#'       beta_params[[pos]] <- fixed_params[[param]]
#'     }
#'   }
#'
#'   # Assign default values for any remaining NULL betas
#'   for (i in 1:5) {
#'     if (is.null(beta_params[[i]])) {
#'       beta_params[[i]] <- 0
#'     }
#'   }
#'
#'   # Extract link information
#'   if (!is.null(x$link_codes)) {
#'     link_codes <- unlist(x$link_codes, use.names = FALSE)
#'   } else if (!is.null(x$link)) {
#'     # Convert link functions to codes
#'     link_map <- c(
#'       "log" = 1,
#'       "logit" = 2,
#'       "probit" = 3,
#'       "cauchy" = 4,
#'       "cloglog" = 5,
#'       "identity" = 6,
#'       "sqrt" = 7,
#'       "inverse" = 8,
#'       "inverse-square" = 9
#'     )
#'
#'     # Default links (log for most, logit for delta)
#'     link_codes <- c(1, 1, 1, 2, 1)
#'
#'     # Update with actual links from the model
#'     for (param in param_names) {
#'       if (param %in% names(x$link)) {
#'         pos <- param_positions[[param]]
#'         link_codes[pos] <- link_map[x$link[[param]]]
#'       }
#'     }
#'   } else {
#'     # Default link types if not specified
#'     link_codes <- c(1, 1, 1, 2, 1) # log for most parameters, logit for delta
#'   }
#'
#'   # Extract scale factors
#'   if (!is.null(x$scale_factors)) {
#'     scale_factors <- as.numeric(unlist(x$scale_factors))
#'   } else {
#'     # Default scale factors
#'     scale_factors <- c(10, 10, 10, 1, 10) # 10 for most parameters, 1 for delta
#'   }
#'
#'   return(c(
#'     beta_params,
#'     list(
#'       link_codes = link_codes,
#'       scale_factors = scale_factors
#'     )
#'   ))
#' }
#'
#'
#' #' Sample model data for large datasets
#' #'
#' #' @param n Total number of observations
#' #' @param sample_size Target sample size
#' #' @param y_obs Vector of observed values
#' #' @param fitted_vals Vector of fitted values
#' #' @param model_matrices List of model matrices
#' #' @return A list with sampled data
#' #'
#' #' @keywords internal
#' .sample_model_data <- function(n, sample_size, y_obs, fitted_vals, model_matrices) {
#'   # For reproducibility
#'   idx <- sample(n, size = min(sample_size, n))
#'
#'   # Sample matrices - ensure we maintain the structure
#'   sampled_matrices <- list()
#'   for (matrix_name in names(model_matrices)) {
#'     X <- model_matrices[[matrix_name]]
#'     if (is.matrix(X) && nrow(X) == n) {
#'       sampled_matrices[[matrix_name]] <- X[idx, , drop = FALSE]
#'     } else {
#'       # Handle non-matrix or incorrectly sized matrix
#'       sampled_matrices[[matrix_name]] <- matrix(1, length(idx), 1)
#'     }
#'   }
#'
#'   return(list(
#'     idx = idx,
#'     y_obs = y_obs[idx],
#'     fitted_vals = fitted_vals[idx],
#'     model_matrices = sampled_matrices
#'   ))
#' }
#'
#'
#' #' Calculate model parameters for the specified family
#' #'
#' #' @param model_matrices List of model matrices
#' #' @param model_params List of model parameters
#' #' @param family Character string specifying distribution family
#' #' @return A matrix of calculated parameters
#' #'
#' #' @keywords internal
#' .calculate_model_parameters <- function(model_matrices, model_params, family) {
#'   # Get parameter information for this family
#'   param_info <- .get_family_param_info(family)
#'
#'   # Calculate the number of required parameters based on family
#'   num_params <- max(unlist(param_info$positions))
#'
#'   # Extract matrices for all parameters
#'   X_matrices <- vector("list", 5)
#'   for (i in 1:5) {
#'     X_name <- paste0("X", i)
#'     if (i <= num_params && X_name %in% names(model_matrices)) {
#'       X_matrices[[i]] <- model_matrices[[X_name]]
#'     } else {
#'       # Default matrix for unused parameters
#'       X_matrices[[i]] <- matrix(1, nrow(model_matrices[[1]]), 1)
#'     }
#'   }
#'
#'   # Extract beta parameters
#'   beta_params <- vector("list", 5)
#'   for (i in 1:5) {
#'     beta_name <- paste0("beta", i)
#'     if (beta_name %in% names(model_params)) {
#'       beta_params[[i]] <- model_params[[beta_name]]
#'     } else {
#'       # Default for unused parameters
#'       beta_params[[i]] <- 0
#'     }
#'   }
#'
#'   # Extract link codes and scale factors
#'   link_codes <- model_params$link_codes
#'   scale_factors <- model_params$scale_factors
#'
#'   # Call the calculateParameters function with proper unpacking
#'   param_mat <- calculateParameters(
#'     X_matrices[[1]], X_matrices[[2]], X_matrices[[3]],
#'     X_matrices[[4]], X_matrices[[5]],
#'     beta_params[[1]], beta_params[[2]], beta_params[[3]],
#'     beta_params[[4]], beta_params[[5]],
#'     link_codes, scale_factors,
#'     family = family
#'   )
#'
#'   return(param_mat)
#' }
#'
#' #' Extract parameter vectors from parameter matrix
#' #'
#' #' @param param_mat Matrix of calculated parameters
#' #' @return A list of parameter vectors
#' #'
#' #' @keywords internal
#' .extract_parameter_vectors <- function(param_mat) {
#'   list(
#'     alphaVec = param_mat[, 1],
#'     betaVec = param_mat[, 2],
#'     gammaVec = param_mat[, 3],
#'     deltaVec = param_mat[, 4],
#'     lambdaVec = param_mat[, 5]
#'   )
#' }
#'
#'
#' #' Calculate residuals based on the specified type
#' #'
#' #' @param y_obs Vector of observed values
#' #' @param fitted_vals Vector of fitted values
#' #' @param param_mat Matrix of calculated parameters
#' #' @param type Character string specifying residual type
#' #' @param family Character string specifying distribution family
#' #' @return A vector of residuals
#' #'
#' #' @keywords internal
#' .calculate_residuals <- function(y_obs, fitted_vals, param_mat, type, family) {
#'   # The following calculate*Residuals() are assumed to be internal package functions
#'   if (type == "quantile") {
#'     calculateQuantileResiduals(y_obs, param_mat, family = family)
#'   } else if (type == "pearson") {
#'     calculatePearsonResiduals(y_obs, fitted_vals, param_mat, family = family)
#'   } else if (type == "deviance") {
#'     calculateDevianceResiduals(y_obs, fitted_vals, param_mat, family = family)
#'   } else {
#'     stop("Unsupported residual type.")
#'   }
#' }
#'
#' #' Calculate diagnostic measures for gkwreg plots
#' #'
#' #' @param y_obs Vector of observed values
#' #' @param fitted_vals Vector of fitted values
#' #' @param resid_vec Vector of residuals
#' #' @param model_matrices List of model matrices
#' #' @param model_params List of model parameters
#' #' @param param_vectors List of parameter vectors
#' #' @param idx Vector of observation indices
#' #' @param family Character string specifying distribution family
#' #' @param param_info Parameter information for the family
#' #' @return A list of diagnostic measures
#' #'
#' #' @importFrom stats rnorm
#' #' @keywords internal
#' .calculate_diagnostic_measures <- function(y_obs,
#'                                            fitted_vals,
#'                                            resid_vec,
#'                                            model_matrices,
#'                                            model_params,
#'                                            param_vectors,
#'                                            idx,
#'                                            family,
#'                                            param_info) {
#'   # Get the first active parameter for this family to use for linear predictor
#'   first_param <- param_info$names[1]
#'   first_pos <- param_info$positions[[first_param]]
#'
#'   # Get the X matrix and beta coefficients for the first parameter
#'   X_matrix <- model_matrices[[paste0("X", first_pos)]]
#'   beta_coef <- model_params[[paste0("beta", first_pos)]]
#'
#'   # Calculate linear predictor
#'   if (length(beta_coef) > 0 && !is.null(X_matrix)) {
#'     if (is.matrix(X_matrix) && is.numeric(beta_coef) &&
#'       ncol(X_matrix) == length(beta_coef)) {
#'       linpred <- as.vector(X_matrix %*% beta_coef)
#'     } else {
#'       # Fallback for incompatible dimensions
#'       linpred <- rep(0, length(idx))
#'     }
#'   } else {
#'     # Fallback for missing data
#'     linpred <- rep(0, length(idx))
#'   }
#'
#'   # Calculate total number of parameters (excluding fixed ones)
#'   p <- 0
#'   for (param in param_info$names) {
#'     pos <- param_info$positions[[param]]
#'     beta_name <- paste0("beta", pos)
#'     if (beta_name %in% names(model_params)) {
#'       beta_val <- model_params[[beta_name]]
#'       if (is.numeric(beta_val) && length(beta_val) > 0) {
#'         p <- p + length(beta_val)
#'       }
#'     }
#'   }
#'
#'   # Ensure p is at least 1 to avoid division by zero
#'   p <- max(1, p)
#'
#'   # Mean squared error
#'   mse <- mean(resid_vec^2, na.rm = TRUE)
#'
#'   # Approximate generalized leverage
#'   n <- length(idx)
#'   leverage <- rep(p / n, length(idx))
#'
#'   # Small noise addition for visual differentiation
#'   leverage <- leverage + abs(rnorm(length(idx), 0, 0.01))
#'
#'   # Calculate Cook's distance
#'   cook_dist <- (resid_vec^2 / (p * mse)) * (leverage / ((1 - leverage)^2))
#'   cook_dist[is.infinite(cook_dist) | cook_dist > 100 | is.na(cook_dist)] <- NA
#'
#'   list(
#'     linpred = linpred,
#'     p = p,
#'     leverage = leverage,
#'     cook_dist = cook_dist
#'   )
#' }
#'
#'
#' #' Calculate half-normal plot data with envelope
#' #'
#' #' @param resid_vec Vector of residuals
#' #' @param idx Vector of observation indices
#' #' @param nsim Number of simulations for envelope
#' #' @param level Confidence level for envelope
#' #' @param param_mat Matrix of calculated parameters
#' #' @param param_vectors List of parameter vectors
#' #' @param type Character string specifying residual type
#' #' @param family Character string specifying distribution family
#' #' @return A data frame with half-normal plot data
#' #' @importFrom stats quantile
#' #'
#' #' @keywords internal
#' .calculate_half_normal_data <- function(resid_vec,
#'                                         idx,
#'                                         nsim,
#'                                         level,
#'                                         param_mat,
#'                                         param_vectors,
#'                                         type,
#'                                         family) {
#'   abs_resid <- abs(resid_vec)
#'   sorted_abs_resid <- sort(abs_resid)
#'   prob_points <- (seq_along(idx) - 0.5) / length(idx)
#'   hn_q <- stats::qnorm(0.5 + prob_points / 2) # half-normal quantiles from normal
#'
#'   # Prepare data frame
#'   half_normal_data <- data.frame(
#'     index = seq_along(sorted_abs_resid),
#'     theoretical = hn_q,
#'     observed = sorted_abs_resid
#'   )
#'
#'   # Extract parameter vectors for simulation
#'   alphaVec <- param_vectors$alphaVec
#'   betaVec <- param_vectors$betaVec
#'   gammaVec <- param_vectors$gammaVec
#'   deltaVec <- param_vectors$deltaVec
#'   lambdaVec <- param_vectors$lambdaVec
#'
#'   # Simulate envelope
#'   envelope_data <- matrix(NA, nrow = length(idx), ncol = nsim)
#'
#'   cat("Simulating envelope (", nsim, "iterations): ")
#'   progress_step <- max(1, floor(nsim / 10))
#'
#'   for (i in seq_len(nsim)) {
#'     if (i %% progress_step == 0) cat(".")
#'
#'     # Generate simulated data using family-specific random generation
#'     sim_y <- .simulate_from_distribution(
#'       n = length(idx),
#'       alphaVec = alphaVec,
#'       betaVec = betaVec,
#'       gammaVec = gammaVec,
#'       deltaVec = deltaVec,
#'       lambdaVec = lambdaVec,
#'       family = family
#'     )
#'
#'     # Calculate residuals for simulated data
#'     sim_resid <- .calculate_sim_residuals(
#'       sim_y = sim_y,
#'       param_mat = param_mat,
#'       type = type,
#'       family = family
#'     )
#'
#'     envelope_data[, i] <- sort(abs(sim_resid))
#'   }
#'   cat(" Done!\n")
#'
#'   # Calculate envelope bounds
#'   lower_bound <- apply(envelope_data, 1, quantile, probs = (1 - level) / 2, na.rm = TRUE)
#'   upper_bound <- apply(envelope_data, 1, quantile, probs = 1 - (1 - level) / 2, na.rm = TRUE)
#'
#'   half_normal_data$lower <- lower_bound
#'   half_normal_data$upper <- upper_bound
#'
#'   half_normal_data
#' }
#'
#'
#'
#'
#' #' Simulate observations from a specified distribution family
#' #'
#' #' @param n Number of observations to simulate
#' #' @param alphaVec Vector of alpha parameters
#' #' @param betaVec Vector of beta parameters
#' #' @param gammaVec Vector of gamma parameters
#' #' @param deltaVec Vector of delta parameters
#' #' @param lambdaVec Vector of lambda parameters
#' #' @param family Character string specifying distribution family
#' #' @return A vector of simulated observations
#' #'
#' #' @keywords internal
#' .simulate_from_distribution <- function(n,
#'                                         alphaVec,
#'                                         betaVec,
#'                                         gammaVec,
#'                                         deltaVec,
#'                                         lambdaVec,
#'                                         family) {
#'   # Generate uniform random variates
#'   sim_u <- stats::runif(n)
#'
#'   # Use the appropriate quantile function for each family
#'   # This approach replaces the complex nested expressions with direct calls
#'   # to the existing quantile functions for each distribution
#'   switch(family,
#'     "gkw" = {
#'       # Use the implemented qgkw function
#'       rgkw(n,
#'         alpha = alphaVec, beta = betaVec, gamma = gammaVec,
#'         delta = deltaVec, lambda = lambdaVec
#'       )
#'     },
#'     "bkw" = {
#'       # BKw: lambda = 1
#'       rbkw(n, alpha = alphaVec, beta = betaVec, gamma = gammaVec, delta = deltaVec)
#'     },
#'     "kkw" = {
#'       # KKw: gamma = 1
#'       rkkw(n, alpha = alphaVec, beta = betaVec, delta = deltaVec, lambda = lambdaVec)
#'     },
#'     "ekw" = {
#'       # EKw: gamma = 1, delta = 0
#'       rekw(n, alpha = alphaVec, beta = betaVec, lambda = lambdaVec)
#'     },
#'     "mc" = {
#'       # MC: alpha = 1, beta = 1
#'       rmc(n, gamma = gammaVec, delta = deltaVec, lambda = lambdaVec)
#'     },
#'     "kw" = {
#'       # KW: lambda = 1, gamma = 1, delta = 0
#'       rkw(n, alpha = alphaVec, beta = betaVec)
#'     },
#'     "beta" = {
#'       # Beta: alpha = 1, beta = 1, lambda = 1
#'       rbeta_(n, gammaVec, deltaVec)
#'     },
#'     # Default case - use the GKw distribution
#'     {
#'       warning("Unrecognized family '", family, "'. Using GKw distribution instead.")
#'       rgkw(n,
#'         alpha = alphaVec, beta = betaVec, gamma = gammaVec,
#'         delta = deltaVec, lambda = lambdaVec
#'       )
#'     }
#'   )
#' }
#'
#'
#'
#' #' Calculate residuals for simulated data
#' #'
#' #' @param sim_y Vector of simulated observations
#' #' @param param_mat Matrix of calculated parameters
#' #' @param type Character string specifying residual type
#' #' @param family Character string specifying distribution family
#' #' @return A vector of residuals
#' #'
#' #' @keywords internal
#' .calculate_sim_residuals <- function(sim_y, param_mat, type, family) {
#'   if (type == "quantile") {
#'     calculateQuantileResiduals(sim_y, param_mat, family = family)
#'   } else if (type == "pearson") {
#'     sim_fitted <- calculateMeans(param_mat, family = family)
#'     calculatePearsonResiduals(sim_y, sim_fitted, param_mat, family = family)
#'   } else if (type == "deviance") {
#'     sim_fitted <- calculateMeans(param_mat, family = family)
#'     calculateDevianceResiduals(sim_y, sim_fitted, param_mat, family = family)
#'   } else {
#'     stop("Unsupported residual type.")
#'   }
#' }
#'
#'
#' #' Create formatted plot titles
#' #'
#' #' @param which Integer vector specifying which plots to produce
#' #' @param caption Character vector of plot captions
#' #' @param main Optional character string for main title
#' #' @param family Character string specifying distribution family
#' #' @param orig_family Original family from the model
#' #' @return A named vector of formatted plot titles
#' #'
#' #' @keywords internal
#' .create_plot_titles <- function(which, caption, main, family, orig_family) {
#'   plot_titles <- rep("", length(which))
#'   names(plot_titles) <- which
#'
#'   for (i in seq_along(which)) {
#'     i_plot <- which[i]
#'     base_title <- caption[i_plot]
#'     if (!is.null(orig_family) && family != orig_family) {
#'       base_title <- paste0(base_title, " (", family, ")")
#'     }
#'     if (nzchar(main)) {
#'       plot_titles[as.character(i_plot)] <- paste(main, "-", base_title)
#'     } else {
#'       plot_titles[as.character(i_plot)] <- base_title
#'     }
#'   }
#'
#'   plot_titles
#' }
#'
#'
#' #' Generate diagnostic plots using base R graphics
#' #'
#' #' @param diag_data List of diagnostic data
#' #' @param which Integer vector specifying which plots to produce
#' #' @param plot_titles Named vector of plot titles
#' #' @param sub.caption Character string for subtitle
#' #' @param ask Logical; whether to ask before new plots
#' #' @param ... Additional graphical parameters
#' #' @return NULL (invisibly)
#' #'
#' #' @keywords internal
#' .plot_gkwreg_base_r <- function(diag_data, which, plot_titles, sub.caption, ask, ...) {
#'   old_par <- par(no.readonly = TRUE)
#'   on.exit(par(old_par))
#'   one.fig <- prod(par("mfcol")) == 1
#'
#'   if (ask) {
#'     oask <- grDevices::devAskNewPage(TRUE)
#'     on.exit(grDevices::devAskNewPage(oask), add = TRUE)
#'   }
#'
#'   type <- diag_data$model_info$type
#'
#'   for (i_plot in which) {
#'     grDevices::dev.hold()
#'     p_main <- plot_titles[as.character(i_plot)]
#'
#'     if (i_plot == 1) {
#'       .plot_base_r_residuals_vs_index(diag_data, p_main, type, ...)
#'     } else if (i_plot == 2) {
#'       .plot_base_r_cooks_distance(diag_data, p_main, ...)
#'     } else if (i_plot == 3) {
#'       .plot_base_r_leverage_vs_fitted(diag_data, p_main, ...)
#'     } else if (i_plot == 4) {
#'       .plot_base_r_residuals_vs_linpred(diag_data, p_main, type, ...)
#'     } else if (i_plot == 5) {
#'       .plot_base_r_half_normal(diag_data, p_main, type, ...)
#'     } else if (i_plot == 6) {
#'       .plot_base_r_predicted_vs_observed(diag_data, p_main, ...)
#'     }
#'     mtext(sub.caption, side = 3, line = 0.25, cex = 0.8, col = "gray40")
#'
#'     grDevices::dev.flush()
#'
#'     if (ask && !one.fig) {
#'       message("Press <ENTER> to continue to the next plot...")
#'       invisible(readLines(con = "stdin", n = 1))
#'     }
#'   }
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot residuals vs. index (base R)
#' #'
#' #' @keywords internal
#' .plot_base_r_residuals_vs_index <- function(diag_data, p_main, type, ...) {
#'   plot(diag_data$data$index, diag_data$data$resid,
#'     ylab = paste0(type, " residuals"),
#'     xlab = "Observation Index",
#'     main = p_main, ...
#'   )
#'   abline(h = 0, lty = 2, col = "gray")
#'   lines(stats::lowess(diag_data$data$index, diag_data$data$resid),
#'     col = "red", lwd = 2
#'   )
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot Cook's distance (base R)
#' #'
#' #' @keywords internal
#' .plot_base_r_cooks_distance <- function(diag_data, p_main, ...) {
#'   plot(diag_data$data$index, diag_data$data$cook_dist,
#'     ylab = "Cook's Distance",
#'     xlab = "Observation Index",
#'     main = p_main, ...
#'   )
#'
#'   threshold <- diag_data$model_info$cook_threshold
#'   abline(h = threshold, lty = 2, col = "red")
#'   text(0.9 * max(diag_data$data$index), threshold * 1.1,
#'     "4/n",
#'     col = "red", pos = 3
#'   )
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot leverage vs. fitted (base R)
#' #'
#' #' @keywords internal
#' .plot_base_r_leverage_vs_fitted <- function(diag_data, p_main, ...) {
#'   plot(diag_data$data$fitted, diag_data$data$leverage,
#'     xlab = "Fitted Values",
#'     ylab = "Generalized Leverage",
#'     main = p_main, ...
#'   )
#'
#'   threshold <- diag_data$model_info$leverage_threshold
#'   abline(h = threshold, lty = 2, col = "red")
#'   text(0.9 * max(diag_data$data$fitted), threshold * 1.1,
#'     "2p/n",
#'     col = "red", pos = 3
#'   )
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot residuals vs. linear predictor (base R)
#' #'
#' #' @keywords internal
#' .plot_base_r_residuals_vs_linpred <- function(diag_data, p_main, type, ...) {
#'   plot(diag_data$data$linpred, diag_data$data$resid,
#'     xlab = "Linear Predictor (alpha)",
#'     ylab = paste0(type, " residuals"),
#'     main = p_main, ...
#'   )
#'   abline(h = 0, lty = 2, col = "gray")
#'   lines(stats::lowess(diag_data$data$linpred, diag_data$data$resid),
#'     col = "red", lwd = 2
#'   )
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot half-normal plot (base R)
#' #'
#' #' @importFrom stats qqnorm qqline
#' #' @keywords internal
#' .plot_base_r_half_normal <- function(diag_data, p_main, type, ...) {
#'   if (is.null(diag_data$half_normal)) {
#'     # fallback to a normal QQ-plot of absolute residuals
#'     stats::qqnorm(abs(diag_data$data$resid),
#'       main = paste(p_main, "(Half-Normal)"),
#'       ylab = paste0("|", type, " residuals|"), ...
#'     )
#'     stats::qqline(abs(diag_data$data$resid), lty = 2, col = "gray")
#'     return(invisible(NULL))
#'   }
#'
#'   hn_data <- diag_data$half_normal
#'   plot(hn_data$theoretical, hn_data$observed,
#'     xlab = "Theoretical Half-Normal Quantiles",
#'     ylab = paste0("Ordered |", type, " residuals|"),
#'     main = paste(p_main, "(Half-Normal)"), ...
#'   )
#'
#'   abline(0, stats::sd(diag_data$data$abs_resid, na.rm = TRUE),
#'     lty = 2, col = "gray"
#'   )
#'
#'   if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
#'     lines(hn_data$theoretical, hn_data$lower, lty = 2, col = "blue", lwd = 1.5)
#'     lines(hn_data$theoretical, hn_data$upper, lty = 2, col = "blue", lwd = 1.5)
#'     level <- round(100 * (1 - (1 - 0.5 * (hn_data$upper[1] / hn_data$observed[1]))), 2)
#'     text(max(hn_data$theoretical) * 0.8, max(hn_data$upper) * 0.9,
#'       paste0(level, "% envelope"),
#'       col = "blue"
#'     )
#'   }
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot predicted vs. observed (base R)
#' #'
#' #' @keywords internal
#' .plot_base_r_predicted_vs_observed <- function(diag_data, p_main, ...) {
#'   plot(diag_data$data$fitted, diag_data$data$y_obs,
#'     xlab = "Fitted (Mean)",
#'     ylab = "Observed (y)",
#'     main = p_main, ...
#'   )
#'   abline(0, 1, col = "gray", lty = 2)
#'   lines(stats::lowess(diag_data$data$fitted, diag_data$data$y_obs),
#'     col = "red", lwd = 2
#'   )
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Generate diagnostic plots using ggplot2
#' #'
#' #' @param diag_data List of diagnostic data
#' #' @param which Integer vector specifying which plots to produce
#' #' @param plot_titles Named vector of plot titles
#' #' @param sub.caption Character string for subtitle
#' #' @param ask Logical; whether to ask before new plots
#' #' @param arrange_plots Logical; whether to arrange multiple plots in a grid
#' #' @param theme_fn ggplot2 theme function
#' #' @param ... Additional graphical parameters
#' #' @return NULL (invisibly)
#' #'
#' #' @keywords internal
#' .plot_gkwreg_ggplot <- function(diag_data,
#'                                 which,
#'                                 plot_titles,
#'                                 sub.caption,
#'                                 ask,
#'                                 arrange_plots,
#'                                 theme_fn,
#'                                 ...) {
#'   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'     stop("Package 'ggplot2' is required for ggplot2 plotting.")
#'   }
#'
#'   type <- diag_data$model_info$type
#'   p_list <- list()
#'
#'   for (i in seq_along(which)) {
#'     i_plot <- which[i]
#'     p_main <- plot_titles[as.character(i_plot)]
#'
#'     if (i_plot == 1) {
#'       p_list[[i]] <- .plot_ggplot_residuals_vs_index(diag_data, p_main, sub.caption, type, theme_fn)
#'     } else if (i_plot == 2) {
#'       p_list[[i]] <- .plot_ggplot_cooks_distance(diag_data, p_main, sub.caption, theme_fn)
#'     } else if (i_plot == 3) {
#'       p_list[[i]] <- .plot_ggplot_leverage_vs_fitted(diag_data, p_main, sub.caption, theme_fn)
#'     } else if (i_plot == 4) {
#'       p_list[[i]] <- .plot_ggplot_residuals_vs_linpred(diag_data, p_main, sub.caption, type, theme_fn)
#'     } else if (i_plot == 5) {
#'       p_list[[i]] <- .plot_ggplot_half_normal(diag_data, p_main, sub.caption, type, theme_fn)
#'     } else if (i_plot == 6) {
#'       p_list[[i]] <- .plot_ggplot_predicted_vs_observed(diag_data, p_main, sub.caption, theme_fn)
#'     }
#'   }
#'
#'   if (arrange_plots && length(p_list) > 1) {
#'     n_plots <- length(p_list)
#'     n_cols <- min(2, n_plots)
#'     n_rows <- ceiling(n_plots / n_cols)
#'
#'     if (requireNamespace("gridExtra", quietly = TRUE)) {
#'       gridExtra::grid.arrange(grobs = p_list, ncol = n_cols, nrow = n_rows)
#'     } else if (requireNamespace("ggpubr", quietly = TRUE)) {
#'       do.call(ggpubr::ggarrange, c(p_list, list(ncol = n_cols, nrow = n_rows)))
#'     } else {
#'       warning("Neither 'gridExtra' nor 'ggpubr' is installed. Displaying plots individually.")
#'       for (i in seq_along(p_list)) {
#'         print(p_list[[i]])
#'         if (ask && i < length(p_list)) {
#'           message("Press <ENTER> to continue to the next plot...")
#'           invisible(readLines(con = "stdin", n = 1))
#'         }
#'       }
#'     }
#'   } else {
#'     for (i in seq_along(p_list)) {
#'       print(p_list[[i]])
#'       if (ask && i < length(p_list)) {
#'         message("Press <ENTER> to continue to the next plot...")
#'         invisible(readLines(con = "stdin", n = 1))
#'       }
#'     }
#'   }
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Plot residuals vs. index (ggplot2)
#' #'
#' #' @keywords internal
#' .plot_ggplot_residuals_vs_index <- function(diag_data, p_main, sub.caption, type, theme_fn) {
#'   ggplot2::ggplot(diag_data$data, ggplot2::aes(x = index, y = resid)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'     ggplot2::geom_smooth(
#'       method = "loess", formula = y ~ x, se = FALSE,
#'       color = "red", linewidth = 1
#'     ) +
#'     ggplot2::labs(
#'       x = "Observation Index",
#'       y = paste0(type, " residuals"),
#'       title = p_main,
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#' }
#'
#'
#' #' Plot Cook's distance (ggplot2)
#' #'
#' #' @keywords internal
#' .plot_ggplot_cooks_distance <- function(diag_data, p_main, sub.caption, theme_fn) {
#'   cook_ref <- diag_data$model_info$cook_threshold
#'   df_data <- diag_data$data
#'
#'   p <- ggplot2::ggplot(df_data, ggplot2::aes(x = index, y = cook_dist)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_hline(yintercept = cook_ref, linetype = "dashed", color = "red") +
#'     ggplot2::annotate("text",
#'       x = max(df_data$index) * 0.9, y = cook_ref * 1.1,
#'       label = "4/n", color = "red", hjust = 0
#'     ) +
#'     ggplot2::labs(
#'       x = "Observation Index",
#'       y = "Cook's Distance",
#'       title = p_main,
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#'
#'   # Label points exceeding threshold
#'   infl <- df_data$cook_dist > cook_ref & !is.na(df_data$cook_dist)
#'   if (any(infl)) {
#'     p <- p + ggplot2::geom_text(
#'       data = df_data[infl, ],
#'       ggplot2::aes(label = index),
#'       vjust = -0.5, color = "red", size = 3
#'     )
#'   }
#'
#'   p
#' }
#'
#'
#' #' Plot leverage vs. fitted (ggplot2)
#' #'
#' #' @keywords internal
#' .plot_ggplot_leverage_vs_fitted <- function(diag_data, p_main, sub.caption, theme_fn) {
#'   lev_ref <- diag_data$model_info$leverage_threshold
#'   df_data <- diag_data$data
#'
#'   p <- ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = leverage)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_hline(yintercept = lev_ref, linetype = "dashed", color = "red") +
#'     ggplot2::annotate("text",
#'       x = max(df_data$fitted) * 0.9, y = lev_ref * 1.1,
#'       label = "2p/n", color = "red", hjust = 0
#'     ) +
#'     ggplot2::labs(
#'       x = "Fitted Values",
#'       y = "Generalized Leverage",
#'       title = p_main,
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#'
#'   # Label high leverage points
#'   high_lev <- df_data$leverage > lev_ref
#'   if (any(high_lev)) {
#'     p <- p + ggplot2::geom_text(
#'       data = df_data[high_lev, ],
#'       ggplot2::aes(label = index),
#'       vjust = -0.5, color = "red", size = 3
#'     )
#'   }
#'
#'   p
#' }
#'
#'
#' #' Plot residuals vs. linear predictor (ggplot2)
#' #'
#' #' @keywords internal
#' .plot_ggplot_residuals_vs_linpred <- function(diag_data, p_main, sub.caption, type, theme_fn) {
#'   df_data <- diag_data$data
#'   ggplot2::ggplot(df_data, ggplot2::aes(x = linpred, y = resid)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'     ggplot2::geom_smooth(
#'       method = "loess", formula = y ~ x, se = FALSE,
#'       color = "red", linewidth = 1
#'     ) +
#'     ggplot2::labs(
#'       x = "Linear Predictor (alpha)",
#'       y = paste0(type, " residuals"),
#'       title = p_main,
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#' }
#'
#'
#' #' Plot half-normal plot (ggplot2)
#' #'
#' #' @param diag_data Diagnostic data list
#' #' @param p_main Plot title
#' #' @param sub.caption Plot subtitle
#' #' @param type Residual type
#' #' @param theme_fn ggplot2 theme function
#' #' @return A ggplot object
#' #'
#' #' @keywords internal
#' .plot_ggplot_half_normal <- function(diag_data, p_main, sub.caption, type, theme_fn) {
#'   # Extract the confidence level from the half_normal data
#'   # If it's not available in diag_data, use a default value
#'   level_value <- diag_data$model_info$level
#'   if (is.null(level_value)) {
#'     level_value <- 0.90 # Default level if not stored in diag_data
#'   }
#'
#'   if (is.null(diag_data$half_normal)) {
#'     # Fallback to a half-normal Q-Q plot approach
#'     return(
#'       ggplot2::ggplot(diag_data$data, ggplot2::aes(sample = abs_resid)) +
#'         ggplot2::stat_qq(distribution = function(p) stats::qnorm(p * 0.5 + 0.5)) +
#'         ggplot2::stat_qq_line(
#'           distribution = function(p) stats::qnorm(p * 0.5 + 0.5),
#'           color = "gray", linetype = "dashed"
#'         ) +
#'         ggplot2::labs(
#'           x = "Theoretical Half-Normal Quantiles",
#'           y = paste0("|", type, " residuals|"),
#'           title = paste(p_main, "(Half-Normal)"),
#'           subtitle = sub.caption
#'         ) +
#'         theme_fn()
#'     )
#'   }
#'
#'   hn_data <- diag_data$half_normal
#'   sd_abs <- stats::sd(diag_data$data$abs_resid, na.rm = TRUE)
#'
#'   p <- ggplot2::ggplot(hn_data, ggplot2::aes(x = theoretical, y = observed)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_abline(
#'       slope = sd_abs, intercept = 0,
#'       linetype = "dashed", color = "gray"
#'     ) +
#'     ggplot2::labs(
#'       x = "Theoretical Half-Normal Quantiles",
#'       y = paste0("Ordered |", type, " residuals|"),
#'       title = paste(p_main, "(Half-Normal)"),
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#'
#'   if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
#'     p <- p +
#'       ggplot2::geom_line(ggplot2::aes(y = lower),
#'         linetype = "dashed",
#'         color = "blue", linewidth = 0.8
#'       ) +
#'       ggplot2::geom_line(ggplot2::aes(y = upper),
#'         linetype = "dashed",
#'         color = "blue", linewidth = 0.8
#'       ) +
#'       ggplot2::annotate(
#'         "text",
#'         x = max(hn_data$theoretical) * 0.8,
#'         y = max(hn_data$upper) * 0.9,
#'         label = paste0(format(100 * level_value, digits = 2), "% envelope"),
#'         color = "blue"
#'       )
#'   }
#'
#'   return(p)
#' }
#'
#'
#' #' Plot predicted vs. observed (ggplot2)
#' #'
#' #' @keywords internal
#' .plot_ggplot_predicted_vs_observed <- function(diag_data, p_main, sub.caption, theme_fn) {
#'   df_data <- diag_data$data
#'   ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = y_obs)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_abline(
#'       intercept = 0, slope = 1,
#'       color = "gray", linetype = "dashed"
#'     ) +
#'     ggplot2::geom_smooth(
#'       method = "loess", formula = y ~ x, se = FALSE,
#'       color = "red", linewidth = 1
#'     ) +
#'     ggplot2::labs(
#'       x = "Fitted (Mean)",
#'       y = "Observed (y)",
#'       title = p_main,
#'       subtitle = sub.caption
#'     ) +
#'     theme_fn()
#' }
