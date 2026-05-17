#' srr_stats
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} *Statistical Software lists primary references from published academic literature - documented in each function file.*
#' @srrstats {G1.1} *Statistical Software documents algorithm implementation status - documented in each function file.*
#' @srrstats {G1.2} *Life Cycle Statement is included in README and package documentation.*
#' @srrstats {G1.3} *All statistical terminology is clarified in function documentation.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions - documented in each function file.*
#' @srrstats {G1.4a} *All internal functions are documented with roxygen2 format.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - documented in applicable function files.*
#' @srrstats {G2.0a} *Provides explicit secondary documentation of expectations on lengths of inputs - documented in applicable function files.*
#' @srrstats {G2.1} *Implements assertions on types of inputs - documented in applicable function files.*
#' @srrstats {G2.1a} *Provides explicit secondary documentation of expectations on data types - documented in applicable function files.*
#' @srrstats {G2.2} *Appropriately prohibits multivariate input to univariate parameters - documented in applicable function files.*
#' @srrstats {G2.3a} *Uses explicit validation for character parameters where applicable - documented in eac(), tcpi(), fit_sigmoidal(), predict_sigmoidal().*
#' @srrstats {G2.3b} *Uses tolower() to convert character inputs to lowercase for case-insensitive matching - documented in eac(), tcpi(), fit_sigmoidal(), predict_sigmoidal(), plot_sigmoidal().*
#' @srrstats {G2.4a} *Uses as.integer() for integer comparisons where applicable - documented in cor_matrix() and mcs().*
#' @srrstats {G2.7} *Accepts standard tabular forms (matrix, data.frame) where applicable - documented in dsm.R and sigmoidal.R.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() and is.na() - documented in applicable function files.*
#' @srrstats {G2.14} *All functions provide options for users to specify how to handle missing (NA) data - documented in each function file.*
#' @srrstats {G2.14a} *Errors on missing data with informative messages - documented in applicable function files.*
#' @srrstats {G2.14b} *Ignores missing data where appropriate with proper handling - documented in learning.R for observed causes/risks.*
#' @srrstats {G3.1} *Covariance calculations allow user-specified correlation matrices - documented in mcs.R, sensitivity.R, and smm.R.*
#' @srrstats {G3.1a} *Documentation describes usage of correlation matrices in examples.*
#' @srrstats {G2.15} *All functions implement explicit checks for NaN values via is.nan() with informative error messages - documented in each function file.*
#' @srrstats {G2.16} *All functions implement explicit checks for undefined (Inf, -Inf) values via is.infinite() with informative error messages - documented in each function file.*
#' @srrstats {G5.2} *Error and warning behaviour is explicitly demonstrated through tests using expect_error() for every stop() message in each function.*
#' @srrstats {G5.2a} *Every error message produced by stop() is unique - documented in applicable function files.*
#' @srrstats {G5.2b} *Tests trigger every error message produced by stop() and compare against expected values using expect_error().*
#' @srrstats {G5.3} *Return objects from all functions are tested for absence of NA, NaN, and Inf values.*
#' @srrstats {G5.6} *Parameter recovery tests verify implementations produce expected results given data with known properties - documented in all test files.*
#' @srrstats {G5.6a} *Parameter recovery tests succeed within defined tolerance rather than exact values - documented in all test files.*
#' @srrstats {G5.6b} *Parameter recovery tests run with multiple random seeds when randomness is involved - documented in test-mcs.R, test-sigmoidal.R, test-inference.R.*
#' @srrstats {G5.7} *Algorithm performance tests verify implementations perform correctly as data properties change - documented in test-mcs.R, test-sigmoidal.R, test-inference.R, test-cormat.R.*
#' @srrstats {G5.8} *Edge condition tests verify appropriate behavior with extreme data properties - documented in all test files.*
#' @srrstats {G5.8a} *Zero-length data tests trigger clear errors - documented in all test files.*
#' @srrstats {G5.8b} *Unsupported data type tests trigger clear errors - documented in all test files.*
#' @srrstats {G5.8c} *All-NA and all-identical data tests trigger clear errors or warnings - documented in all test files.*
#' @srrstats {G5.8d} *Out-of-scope data tests verify appropriate behavior - documented in test-dsm.R, test-evm.R, test-sigmoidal.R.*
#' @srrstats {G5.9} *Noise susceptibility tests verify stochastic behavior stability - documented in test-mcs.R, test-sigmoidal.R, test-inference.R, test-cormat.R, test-smm.R.*
#' @srrstats {G5.9a} *Trivial noise tests show results are stable at machine epsilon scale - documented in applicable test files.*
#' @srrstats {G5.9b} *Random seed stability tests show consistent behavior across different seeds - documented in test-mcs.R, test-sigmoidal.R, test-inference.R, test-cormat.R.*
#' @noRd
NULL

#' NA_standards
#'
#' The following standards have been deemed not applicable to this package.
#' Explanations are provided for each standard.
#'
#' @srrstatsNA {G1.5} *Not applicable - package does not make performance claims requiring reproducible results from publications.*
#' @srrstatsNA {G1.6} *Not applicable - this is the first R implementation; no alternative R packages exist for comparison.*
#' @srrstatsNA {G2.4} *Not applicable - package works with numeric inputs directly; conversion between types not required.*
#' @srrstatsNA {G2.4b} *Not applicable - inputs are already numeric; no conversion to continuous needed.*
#' @srrstatsNA {G2.4c} *Not applicable - package does not convert to character type.*
#' @srrstatsNA {G2.4d} *Not applicable - package does not use factor inputs.*
#' @srrstatsNA {G2.4e} *Not applicable - package does not convert from factor type.*
#' @srrstatsNA {G2.5} *Not applicable - package does not use factor inputs.*
#' @srrstatsNA {G2.6} *Not applicable - one-dimensional inputs are appropriately handled as vectors.*
#' @srrstatsNA {G2.8} *Not applicable - functions receive inputs of defined types directly.*
#' @srrstatsNA {G2.9} *Not applicable - package does not perform type conversions that would lose information.*
#' @srrstatsNA {G2.10} *Not applicable - package does not extract columns from tabular inputs in ways requiring special handling.*
#' @srrstatsNA {G2.11} *Not applicable - package does not process data.frame columns with non-standard class attributes.*
#' @srrstatsNA {G2.12} *Not applicable - package does not process data.frames with list columns.*
#' @srrstatsNA {G2.14c} *Not applicable - imputation is not appropriate for this package's statistical methods.*
#' @srrstatsNA {G3.0} *Not applicable - package does not compare floating point numbers for equality.*
#' @srrstatsNA {G4.0} *Not applicable - package does not write outputs to local files.*
#' @srrstatsNA {G5.0} *Consider updating* *Not applicable - package algorithms are validated through documented examples rather than standard reference datasets.*
#' @srrstatsNA {G5.1} *Consider updating* *Not applicable - package uses inline examples rather than separate test datasets.*
#' @srrstatsNA {G5.4} *Consider updating* *Correctness is validated through documented examples with known inputs and expected outputs.*
#' @srrstatsNA {G5.4a} *Not applicable - methods are implementations of established algorithms.*
#' @srrstatsNA {G5.4b} *Consider updating* *Not applicable - this is the first R implementation of these methods.*
#' @srrstatsNA {G5.4c} *Consider updating* *Correctness is validated against formulas from referenced academic publications.*
#' @srrstatsNA {G5.5} *Consider updating* *Examples use deterministic inputs; stochastic functions use consistent parameters.*
#' @srrstatsNA {G5.10} *Not applicable - no extended tests requiring special environment variables.*
#' @srrstatsNA {G5.11} *Not applicable - no large datasets required for testing.*
#' @srrstatsNA {G5.11a} *Not applicable - no external downloads required for tests.*
#' @srrstatsNA {G5.12} *Not applicable - no special conditions required for extended tests.*
#' @noRd
NULL
