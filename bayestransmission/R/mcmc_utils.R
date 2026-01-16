#' Convert MCMC Parameters to Data Frame
#'
#' Converts the nested list structure of MCMC parameters from \code{runMCMC}
#' output into a tidy data frame format suitable for analysis and visualization.
#'
#' @param mcmc_results The results object returned by \code{runMCMC()}.
#'   Must contain a \code{Parameters} component and a \code{LogLikelihood} component.
#'
#' @return A data frame with one row per MCMC iteration containing:
#' \itemize{
#'   \item \code{iteration}: The iteration number
#'   \item \code{insitu_*}: In-situ probability parameters
#'   \item \code{surv_test_*}: Surveillance test parameters
#'   \item \code{clin_test_*}: Clinical test parameters and rates
#'   \item \code{outunit_*}: Out of unit infection parameters
#'   \item \code{inunit_*}: In unit LinearAbx model parameters (base, time, mass, freq, colabx, susabx, susever, clr, clrAbx, clrEver)
#'   \item \code{abxrate_*}: Antibiotic rate parameters
#'   \item \code{loglikelihood}: Log likelihood at each iteration
#' }
#'
#' @details
#' The function extracts parameters from the nested list structure and handles
#' missing values gracefully by inserting \code{NA} when a parameter is not present.
#' This is particularly useful for creating trace plots and posterior distributions.
#'
#' @examples
#' results <- runMCMC(data = simulated.data,
#'                    modelParameters = LinearAbxModel(),
#'                    nsims = 10,
#'                    nburn = 0,
#'                    outputparam = TRUE,
#'                    outputfinal = FALSE)
#' param_df <- mcmc_to_dataframe(results)
#' head(param_df)
#' @export
mcmc_to_dataframe <- function(mcmc_results) {
  # Validate input
  if (!is.list(mcmc_results)) {
    stop("mcmc_results must be a list")
  }
  if (!"Parameters" %in% names(mcmc_results)) {
    stop("mcmc_results must contain a 'Parameters' component")
  }
  if (!"LogLikelihood" %in% names(mcmc_results)) {
    stop("mcmc_results must contain a 'LogLikelihood' component")
  }
  
  # Helper function to safely extract parameter by name
  get_param <- function(vec, name, default = NA_real_) {
    if (name %in% names(vec)) {
      return(as.numeric(vec[name]))
    }
    return(default)
  }
  
  # Initialize empty data frame
  param_df <- data.frame()
  
  # Loop through each MCMC iteration
  for (i in seq_along(mcmc_results$Parameters)) {
    iter_params <- mcmc_results$Parameters[[i]]
    
    # Extract key parameters based on actual structure
    row <- data.frame(
      iteration = i,
      
      # In-situ probabilities
      insitu_uncolonized = get_param(iter_params$Insitu, "Insit.P(unc)"),
      insitu_colonized = get_param(iter_params$Insitu, "Insit.P(col)"),
      
      # Surveillance test parameters
      surv_test_uncol_neg = get_param(iter_params$SurveillanceTest, "ATest.P(+|unc-)"),
      surv_test_col_neg = get_param(iter_params$SurveillanceTest, "ATest.P(+|col-)"),
      surv_test_uncol_pos = get_param(iter_params$SurveillanceTest, "ATest.P(+|unc+)"),
      surv_test_col_pos = get_param(iter_params$SurveillanceTest, "ATest.P(+|col+)"),
      
      # Clinical test parameters
      clin_test_uncol = get_param(iter_params$ClinicalTest, "RTest.P(+|unc)"),
      clin_test_col = get_param(iter_params$ClinicalTest, "RTest.P(+|col)"),
      clin_rate_uncol = get_param(iter_params$ClinicalTest, "RTest.rateUnc"),
      clin_rate_col = get_param(iter_params$ClinicalTest, "RTest.rateCol"),
      
      # Out of unit parameters
      outunit_acquisition = get_param(iter_params$OutCol, "Out.acq"),
      outunit_clearance = get_param(iter_params$OutCol, "Out.clr"),
      
      # In unit parameters - LinearAbx model (adjust based on your model)
      inunit_base = get_param(iter_params$InCol, "LABX.base"),
      inunit_time = get_param(iter_params$InCol, "LABX.time"),
      inunit_mass = get_param(iter_params$InCol, "LABX.mass.mx"),
      inunit_freq = get_param(iter_params$InCol, "LABX.freq.mx"),
      inunit_colabx = get_param(iter_params$InCol, "LABX.colabx"),
      inunit_susabx = get_param(iter_params$InCol, "LABX.susabx"),
      inunit_susever = get_param(iter_params$InCol, "LABX.susever"),
      inunit_clr = get_param(iter_params$InCol, "LABX.clr"),
      inunit_clrAbx = get_param(iter_params$InCol, "LABX.clrAbx"),
      inunit_clrEver = get_param(iter_params$InCol, "LABX.clrEver"),
      
      # Antibiotic rate parameters
      abxrate_uncolonized = get_param(iter_params$Abx, "Abx.rateUnc"),
      abxrate_colonized = get_param(iter_params$Abx, "Abx.rateCol"),
      
      # Log likelihood
      loglikelihood = mcmc_results$LogLikelihood[i]
    )
    
    param_df <- rbind(param_df, row)
  }
  
  return(param_df)
}
