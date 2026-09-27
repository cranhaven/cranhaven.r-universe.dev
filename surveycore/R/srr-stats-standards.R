#' srr_stats
#'
#' Standards addressed or explicitly deferred in this file. Tags may be moved
#' to any source file; those placed here apply package-wide.
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.4} Software uses roxygen2 throughout; RoxygenNote is set in
#'   DESCRIPTION and all exported functions carry roxygen2 documentation.
#'
#' @srrstats {G1.6} Numerical correctness tests compare surveycore estimates
#'   against the survey package for means, totals, proportions, and quantiles
#'   using NHANES and ACS PUMS datasets (see test-variance-taylor.R,
#'   test-variance-replicate.R).
#'
#' @srrstats {G2.0a} All @param entries for column-selecting arguments
#'   explicitly state whether a single column or multiple columns are expected.
#'
#' @srrstats {G2.2} Parameters expected to be univariate (type, fpctype, mse,
#'   nest) are validated via match.arg() or logical coercion, preventing
#'   multi-valued submission.
#'
#' @srrstats {G2.3} Univariate character parameters (type, fpctype, method)
#'   use match.arg(), ensuring only permitted values are accepted.
#'
#' @srrstats {G2.3a} match.arg() is used for all character-valued control
#'   parameters in constructors and analysis functions (e.g. type in
#'   as_survey_replicate(), method in get_corr()).
#'
#' @srrstats {G2.4} Surveycore validates input types at construction time and
#'   errors with informative cli_abort() messages when types are incompatible,
#'   rather than silently coercing. Explicit factor conversion is applied
#'   where appropriate (see G2.4d).
#'
#' @srrstats {G2.4d} Analysis functions convert grouped output columns to
#'   factor when the grouping variable carries value labels and
#'   label_values = TRUE, using explicit factor() conversion.
#'
#' @srrstats {G2.6} Constructor pre-processing via tidyselect handles
#'   one-dimensional column inputs (bare names, c(), starts_with(), etc.)
#'   consistently regardless of how they are specified.
#'
#' @srrstats {G2.7} Constructors accept any data.frame-like tabular input
#'   including data.frame, tibble, and data.table objects, as these all
#'   satisfy the S7::class_data.frame property constraint.
#'
#' @srrstats {G2.8} The S7 class system provides dispatch and type
#'   enforcement: all analysis functions check S7::S7_inherits(x, survey_base)
#'   before proceeding, ensuring a single defined class is propagated through
#'   sub-functions.
#'
#' @srrstats {G2.10} Column extraction throughout the package uses [[
#'   consistently, never [, ensuring single-column extraction returns a
#'   vector regardless of the underlying tabular class.
#'
#' @srrstats {G2.14} Analysis functions provide an na.rm parameter (default
#'   TRUE) giving users explicit control over missing-value handling in
#'   analytic computations.
#'
#' @srrstats {G2.14b} Analysis functions default to na.rm = TRUE, excluding
#'   missing outcome values from computations; behaviour is documented in
#'   @param na.rm.
#'
#' @srrstats {G5.0} Standard datasets with known properties (nhanes_2017 and
#'   acs_pums_wy) are used in numerical correctness tests.
#'
#' @srrstats {G5.1} Both nhanes_2017 and acs_pums_wy are exported datasets,
#'   making them available for users to independently reproduce test results.
#'
#' @srrstats {G5.2} Appropriate error and warning behaviour is explicitly
#'   demonstrated in tests: every cli_abort() / cli_warn() call has a
#'   corresponding expect_error(class = ...) / expect_warning(class = ...)
#'   test, plus snapshot tests for message text.
#'
#' @srrstats {G5.2a} Every error and warning is assigned a unique class=
#'   argument (convention: surveycore_error_* / surveycore_warning_*),
#'   ensuring uniqueness of all generated messages.
#'
#' @srrstats {G5.2b} Each error class documented in plans/error-messages.md
#'   has a corresponding test that triggers it and confirms the class string,
#'   along with a snapshot test for the rendered message text.
#'
#' @srrstats {G5.4} Correctness is tested by comparing surveycore point
#'   estimates and standard errors against the survey package on real survey
#'   datasets, using tolerances of 1e-10 (point estimates) and 1e-8 (SEs).
#'
#' @srrstats {G5.4b} All variance methods (Taylor series, replicate weights,
#'   two-phase) are tested against equivalent survey::svydesign() objects to
#'   confirm numerical equivalence with the existing reference implementation.
#'
#' @srrstats {G5.5} Correctness tests that use synthetic data call
#'   make_survey_data(seed = N) with a fixed seed, ensuring reproducibility.
#'
#' @srrstats {G1.0} Primary academic references are listed in inst/CITATION:
#'   Lumley (2004) JSS article on analysis of complex survey samples, and
#'   Lumley (2010) book. These are the definitive sources for the Taylor
#'   linearization and replicate-weight variance methods vendored in this
#'   package. References also appear in README.md.
#'
#' @srrstats {G1.1} surveycore implements established survey variance
#'   estimation algorithms (Taylor linearization, BRR, jackknife, bootstrap,
#'   two-phase) that were originally implemented in Thomas Lumley's survey
#'   package. surveycore is an improvement on the existing R implementation:
#'   it provides a modern S7 class system, a tidy-select interface, automatic
#'   haven label preservation, and a consistent tibble-based output API that
#'   survey and srvyr do not offer. Full comparison documented in
#'   vignette("surveycore-vs-survey").
#'
#' @srrstats {G1.2} A Life Cycle Statement is included in README.md under
#'   the "Development status" section, showing the completed phases (0–2),
#'   current stability of the API, and the next phase (3 polish/CRAN).
#'   NEWS.md documents the full changelog.
#'
#' @srrstats {G1.3} Statistical terminology (complex survey design, primary
#'   sampling unit, finite population correction, Taylor linearization,
#'   replicate weights, two-phase design, design-based inference, Binder
#'   sandwich estimator) is defined and illustrated in the package vignettes:
#'   vignette("creating-survey-objects") and vignette("getting-started").
#'
#' @srrstats {G1.4a} All internal (non-exported) functions in R/ carry a
#'   final @noRd roxygen2 tag, including all 19 helpers in
#'   R/analysis-helpers.R and all internal helpers in R/glm.R,
#'   R/core-validators.R, and R/variance-*.R files.
#'
#' @srrstats {G2.3b} All character parameters that use match.arg() are
#'   explicitly documented as case-sensitive in their @param entries:
#'   type and fpctype in as_survey_replicate(), method in
#'   as_survey_twophase(), format in get_corr(), scale in get_diffs().
#' @srrstats {G2.9} surveycore does not perform implicit type conversions that
#'   lose information. Design variable types (weights, FPC) are validated at
#'   construction time — non-numeric types cause informative errors rather than
#'   silent coercion. The one coercion that does occur (character → factor in
#'   survey_glm() via stats::model.matrix()) is documented in survey_glm()
#'   @details ("Predictor variable types") and @param family.
#'
#' @srrstats {G2.11} All design variables (weights, FPC, strata IDs, PSU IDs)
#'   are validated at construction time; columns with non-standard class
#'   attributes produce informative errors (surveycore_error_weights_not_numeric
#'   etc.). Non-design columns with unusual types are passed through unchanged
#'   and only produce errors if used in analysis — in which case the error
#'   originates from the statistical computation (e.g., stats::model.matrix()
#'   for non-formula-compatible types). Tested in test-srr-compliance.R (G5.8b).
#'
#' @srrstats {G2.12} surveycore does not accept or process list columns as
#'   design variables (weights, FPC, strata, IDs). Passing a list column where
#'   a numeric is expected produces a surveycore_error_weights_not_numeric or
#'   equivalent error. List columns in non-design positions are stored as-is
#'   and rejected with clear errors if used in analysis. Tested in
#'   test-srr-compliance.R (G5.8b).
#'
#' @srrstats {G2.16} All analysis functions (get_means(), get_totals(),
#'   get_freqs(), get_quantiles(), get_corr(), get_ratios(), get_diffs())
#'   accept a na.rm argument that controls how NA (and NaN, since is.na(NaN)
#'   is TRUE in R) values in outcome variables are handled. survey_glm()
#'   provides na.action to handle undefined values in formula variables.
#'   Design variables (weights, FPC) that contain NA, NaN, or Inf produce
#'   informative errors at construction time (surveycore_error_weights_na,
#'   surveycore_error_fpc_na, etc.). Inf in outcome variables propagates
#'   through the variance estimator as expected by standard R behaviour.
#'
#' @srrstats {G3.0} All statistical computations in surveycore use integer
#'   equality (with integer literals, e.g. == 0L, == 1L) or tolerance-based
#'   comparisons (e.g. abs(x) < .Machine$double.eps, isTRUE(all.equal(x,y))).
#'   The only floating-point == 0 comparisons are display-level guards in
#'   get_diffs() (pct_change column), where dividing by zero would produce
#'   Inf in a formatted output column — these are intentional guards, not
#'   algorithmic equality tests. All algorithmic thresholds (e.g. separation
#'   detection in survey_glm()) use .Machine$double.eps-based bounds.
#'
#' @srrstats {G5.6} Parameter recovery tests compare surveycore estimates
#'   against the survey reference implementation (oracle tests in
#'   test-glm-numerical.R, test-variance-taylor.R, test-variance-replicate.R,
#'   test-variance-twophase.R, test-variance-srs.R). Direct analytical
#'   recovery from deterministic data is tested in test-srr-compliance.R
#'   (G5.6/G5.6a block).
#'
#' @srrstats {G5.6a} All parameter recovery tests use tolerance-based
#'   comparisons: coef oracle tests use tolerance = 1e-10, SE tests use
#'   1e-8, CI tests use 1e-6. Analytical recovery tests in
#'   test-srr-compliance.R use tolerance = 0.05 (finite-sample variance
#'   of the recovery estimate with n = 1000).
#'
#' @srrstats {EA1.0} Target audiences are identified in README.md ("Who is
#'   this for?" section): survey researchers and methodologists, social
#'   scientists / epidemiologists / public health researchers, and R users
#'   seeking a tidyverse-compatible alternative to survey and srvyr.
#'
#' @srrstats {EA1.1} README.md ("Who is this for?" section) identifies the
#'   kinds of data the software analyses: rectangular survey microdata with
#'   complex probability sampling designs (stratified, clustered, replicate-
#'   weight, two-phase), including data with haven-style variable and value
#'   labels. Supported input formats: data.frame, tibble, data.table.
#'
#' @srrstats {EA1.2} README.md ("Who is this for?" section) identifies the
#'   kinds of questions: weighted means, totals, frequencies, quantiles,
#'   correlations, ratios, survey-weighted regression, and subgroup
#'   comparisons. The package is designed for inferential questions that
#'   require design-consistent variance estimates.
#'
#' @srrstats {EA1.3} README.md ("Who is this for?" section) includes a table
#'   documenting the accepted input types for each get_*() function:
#'   get_freqs() (categorical), get_means()/get_totals()/get_quantiles()
#'   (numeric), get_corr() (pairs of numeric), get_ratios() (two numeric),
#'   get_diffs() (categorical + numeric), survey_glm() (numeric/binary
#'   response, numeric/categorical predictors).
#' @noRd
NULL

#' NA_standards
#'
#' Standards deemed not applicable to surveycore, with justifications.
#'
#' @srrstatsNA {G1.5} surveycore does not make quantitative performance claims
#'   in associated academic publications. Correctness is validated against the
#'   survey reference implementation (see G5.4b), not via benchmarks reported
#'   in papers.
#'
#' @srrstatsNA {G2.4a} surveycore does not perform explicit integer conversion.
#'   Where integer columns are required (e.g., PSU IDs), the package errors if
#'   the column is not numeric, rather than silently coercing.
#'
#' @srrstatsNA {G2.4b} Explicit numeric conversion is not applied. The package
#'   validates that weight and FPC columns are already numeric and errors
#'   otherwise.
#'
#' @srrstatsNA {G2.4c} Explicit character conversion is not applied. Column
#'   names are always character (enforced by tidyselect), and no other
#'   character conversions are performed.
#'
#' @srrstatsNA {G2.4e} Conversion from factor is not performed. Factors passed
#'   as outcome variables in analysis functions are accepted as-is; factors
#'   used as design variables are accepted via their underlying values.
#'
#' @srrstatsNA {G2.5} surveycore does not require factor-type inputs for any
#'   parameter. Ordered vs. unordered factor distinction is irrelevant for
#'   survey variance estimation.
#'
#' @srrstatsNA {G2.14c} Imputation of missing values is outside the scope of
#'   surveycore. Survey design objects represent observed sample data, and
#'   imputing design-level variables would invalidate the design specification.
#'
#' @srrstatsNA {G3.1} Survey variance estimation methods (Taylor series
#'   linearization, BRR, jackknife, bootstrap) are fixed by the survey design
#'   type chosen by the user. There is no user-selectable covariance algorithm
#'   within a given design type.
#'
#' @srrstatsNA {G3.1a} Follows from G3.1: there is no user-selectable
#'   covariance algorithm to document; the variance method is the design type
#'   itself.
#'
#' @srrstatsNA {G4.0} surveycore does not enable outputs to be written to
#'   local files. All output is returned as R objects.
#'
#' @srrstatsNA {G5.4a} surveycore does not implement novel statistical
#'   algorithms. It implements established survey variance estimation methods
#'   from the survey statistics literature, with a reference implementation
#'   available in the survey package.
#'
#' @srrstatsNA {G5.4c} The reference implementation (survey package by Lumley)
#'   is directly available in R and used in correctness tests. Published paper
#'   outputs are not needed as a substitute comparison source.
#'
#' @srrstatsNA {G5.6b} Survey variance algorithms are deterministic: no random
#'   component is introduced by surveycore's estimation routines. Multiple
#'   random seeds for parameter recovery tests are therefore not applicable.
#'
#' @srrstatsNA {G5.7} Algorithm performance (scaling) is inherited entirely
#'   from the vendored survey package variance routines. surveycore adds no
#'   novel algorithmic complexity beyond S7 class overhead.
#'
#' @srrstatsNA {G5.9b} Running under different random seeds does not apply:
#'   all variance estimation algorithms in surveycore are deterministic given
#'   the same design and data.
#'
#' @srrstatsNA {G5.11} Extended tests do not require downloads of large
#'   external datasets. The standard test datasets (nhanes_2017, acs_pums_wy)
#'   are bundled within the package.
#'
#' @srrstatsNA {G5.11a} Follows from G5.11: no external data downloads are
#'   required, so skip-on-download-failure logic is not needed.
#'
#' @srrstatsNA {EA2.0} surveycore does not implement or rely on an
#'   index-column system for table operations. Survey design objects use an S7
#'   class that stores the full data frame; row identity is tracked by
#'   position, consistent with how the survey package and base R modelling
#'   functions treat data frames.
#'
#' @srrstatsNA {EA2.1} Follows from EA2.0: no index columns are used.
#'
#' @srrstatsNA {EA2.2} Follows from EA2.0: no index columns are used.
#'
#' @srrstatsNA {EA2.2a} Follows from EA2.0: no index columns are used.
#'
#' @srrstatsNA {EA2.2b} Follows from EA2.0: no index columns are used.
#'
#' @srrstatsNA {EA2.3} surveycore does not perform table join operations. All
#'   analysis is performed on the single data frame stored in the survey design
#'   object.
#'
#' @srrstatsNA {EA2.5} Follows from EA2.0: no index columns are used.
#'
#' @srrstatsNA {EA5.0} surveycore produces no graphical output. All results are
#'   returned as tibbles or model objects.
#'
#' @srrstatsNA {EA5.0a} Follows from EA5.0: no graphical output.
#'
#' @srrstatsNA {EA5.0b} Follows from EA5.0: no graphical output.
#'
#' @srrstatsNA {EA5.1} Follows from EA5.0: no graphical output.
#'
#' @srrstatsNA {EA5.4} Follows from EA5.0: no graphical output.
#'
#' @srrstatsNA {EA5.5} Follows from EA5.0: no graphical output.
#'
#' @srrstatsNA {EA5.6} Follows from EA5.0: no graphical output and no bundled
#'   visualisation libraries.
#'
#' @srrstatsNA {EA6.1} Follows from EA5.0: no graphical output to test.
#'
#' @srrstatsNA {RE2.3} Centring and offsetting of predictors are not
#'   surveycore concerns. Offset terms may be specified in the formula via
#'   offset(). Automatic centring would alter the interpretation of
#'   survey-weighted regression coefficients and is not standard practice.
#'
#' @srrstatsNA {RE4.1} survey_glm() always fits the model immediately;
#'   generating a model specification without fitting is not supported.
#'   Lazy fitting would complicate the design-based variance calculation.
#'
#' @srrstatsNA {RE4.12} No explicit input transformation functions are applied
#'   beyond what the formula specifies. Link function transformations are
#'   handled by the family argument (via standard R family objects) and are not
#'   surveycore-owned functions requiring inverse transforms.
#'
#' @srrstatsNA {RE4.14} surveycore is not a forecasting package. Prediction
#'   generates fitted values for the observed design frame; out-of-sample
#'   extrapolation errors are not relevant.
#'
#' @srrstatsNA {RE4.15} Follows from RE4.14: no forecast horizons.
#'
#' @srrstatsNA {RE4.16} Survey regression estimates population parameters for
#'   the sampled population's group structure; submitting entirely new
#'   categorical groups not present in the design frame is outside the scope
#'   of design-based inference.
#'
#' @srrstatsNA {RE6.3} Follows from RE4.14: no forecast vs. modelled
#'   distinction exists in surveycore outputs.
#'
#' @srrstatsNA {RE7.0a} survey_glm() does not reject noiseless (perfectly
#'   collinear) input data as an error; such data is handled by stats::glm(),
#'   which drops aliased columns.
#'
#' @srrstatsNA {RE7.1a} Speed comparison between noiseless and noisy fits is
#'   not applicable: surveycore delegates all IRLS fitting to stats::glm(),
#'   adding only sandwich variance computation whose cost does not depend on
#'   data noisiness.
#'
#' @srrstatsNA {RE7.4} Follows from RE4.14: no forecast error tests.
#' @noRd
NULL
