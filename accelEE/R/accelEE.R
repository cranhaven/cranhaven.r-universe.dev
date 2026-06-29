#' Predict energy expenditure for accelerometry data
#' @aliases wrap_2RM crouter15 hildebrand_linear hildebrand_nonlinear
#'   montoye sojourn staudenmayer
#'
#'
#' @usage
#'
#'
#' ## Wrapper function:
#'
#'   accelEE(
#'     d, method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012", "Crouter 2015",
#'       "Hibbing 2018", "Hildebrand Linear", "Hildebrand Non-Linear",
#'       "Montoye 2017", "SIP", "Sojourn 1x", "Sojourn 3x",
#'       "Staudenmayer Linear", "Staudenmayer Random Forest"
#'     ), time_var = "Timestamp", output_epoch = "default",
#'     warn_high_low = TRUE, met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE, combine = TRUE,
#'     ee_vars = c("METs", "VO2", "kcal"), verbose = FALSE, ...
#'   )
#'
#'
#' ## Internal applicator functions called by the wrapper, based on
#' ## the value of the `method` argument (external functions listed
#' ## under 'See Also'):
#'
#'   wrap_2RM(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     max_mets = 20, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE, verbose = FALSE,
#'     method = c(
#'       "Crouter 2006", "Crouter 2010",
#'       "Crouter 2012", "Hibbing 2018"
#'     ), ..., met_name = "METs", tag = ""
#'   )
#'
#'   crouter15(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     min_mets = 1, max_mets = 20, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     shrink_output = TRUE, verbose = FALSE,
#'     model = c("VA", "VM"), movement_var = "Axis1",
#'     ...
#'   )
#'
#'   hildebrand_linear(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     min_vo2_mlkgmin = 3, max_vo2_mlkgmin = 70, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE, verbose = FALSE,
#'     age = c("youth", "adult"), monitor = c("ActiGraph", "GENEActiv"),
#'     location = c("hip", "wrist"), enmo_name = "ENMO", ...
#'   )
#'
#'   hildebrand_nonlinear(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     min_vo2_mlkgmin = 3, max_vo2_mlkgmin = 70, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE,
#'     verbose = FALSE, enmo_name = "ENMO", ...
#'   )
#'
#'   montoye(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     min_mets = 1, max_mets = 20, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE,
#'     verbose = FALSE, side = c("left", "right"),
#'     ...
#'   )
#'
#'   sojourn(
#'     d,  time_var = "Timestamp", output_epoch = "default",
#'     min_mets = 1, max_mets = 20, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     shrink_output = TRUE, verbose = FALSE,
#'     axis1 = "Axis1", axis2 = "Axis2", axis3 = "Axis3",
#'     vector.magnitude = "Vector.Magnitude",
#'     method = c("SIP", "Sojourn 1x", "Sojourn 3x"),
#'     ..., met_name = "METs", tag = ""
#'   )
#'
#'   staudenmayer(
#'     d, time_var = "Timestamp", output_epoch = "default",
#'     min_mets = 1, max_mets = 20, warn_high_low = TRUE,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     feature_calc = TRUE, shrink_output = TRUE,
#'     verbose = FALSE, ..., select = c("METs_lm", "METs_rf")
#'   )
#'
#'
#' @param d data frame of data to use for generating predictions
#' @param method the method(s) to use
#' @param time_var character. Name of the column containing
#'   POSIX-formatted timestamps
#' @param output_epoch character. The desired epoch length of output. Acceptable
#'   options are \code{"default"} or else a setting appropriate for the
#'   \code{unit} argument of \code{lubridate::floor_date()}
#' @param warn_high_low logical. Issue warnings when corrections are applied to
#'   values that are out of range?
#' @param met_mlkgmin conversion factor for transforming oxygen consumption (in
#'   ml/kg/min) into metabolic equivalents (METs)
#' @param RER the respiratory exchange ratio. Used for determining conversion
#'   factors when calculating caloric expenditure from oxygen consumption
#' @param feature_calc logical. Calculate features for the selected method(s)?
#'   If \code{FALSE}, the assumption is that features have already been
#'   calculated
#' @param shrink_output logical. Reduce the number of columns in output by
#'   removing calculated feature columns? Default is \code{TRUE}. May only have
#'   an impact on output in certain cases, particularly when setting \cr
#'   \code{output_epoch = "default"} \verb{   } and/or \verb{   }
#'   \code{method = c("Montoye 2017", ...)}
#' @param combine logical. Combine results from each method into a single
#'   data frame? If \code{TRUE} (the default), the results will all be collapsed
#'   to a commonly-compatible epoch length, which may override
#'   \code{output_epoch} if a suitable selection is not given
#' @param ee_vars character vector indicating which energy expenditure variables
#'   to return. Choose one or more of \code{"METs"}, \code{"VO2"}, and
#'   \code{"kcal"} (case insensitive)
#' @param verbose logical. Print updates to console?
#' @param ... arguments passed to specific applicators and beyond. See details
#' @param min_mets minimum allowable metabolic equivalent (MET) value. Values
#'   lower than this (if any) will be rounded up to it
#' @param max_mets maximum allowable metabolic equivalent (MET) value. Values
#'   higher than this (if any) will be rounded down to it
#' @param min_vo2_mlkgmin minimum allowable oxygen consumption value (in
#'   ml/kg/min). Values lower than this (if any) will be rounded up to it
#' @param max_vo2_mlkgmin maximum allowable oxygen consumption value (in
#'   ml/kg/min). Values higher than this (if any) will be rounded down to it
#' @param model character. Can be \code{"VA"} and/or \code{"VM"}, specifying
#'   which of the Crouter 2015 model(s) to use
#' @param movement_var character. name of the variable(s) on which the Crouter
#'   2015 method should be applied. Length must match the length of
#'   \code{model}. It is assumed that the first elements of \code{movement_var}
#'   and \code{model} will correspond with one another, and the same for the
#'   second elements (if applicable)
#' @param age the age group(s) of desired Hildebrand equation(s) to apply
#' @param monitor the monitor being worn by the participant
#' @param location the placement of the monitor on the body
#' @param enmo_name name of the variable containing Euclidian Norm
#'   Minus One (ENMO) values
#' @param side character vector or scalar indicating which side-specific wrist
#'   model(s) to implement for \code{"Montoye 2017"}. Can be \code{"left"},
#'   \code{"right"}, or \code{c("left", "right")}
#' @param axis1 for \code{Sojourn 1x} and \code{Sojourn 3x}, the name of the
#'   variable in \code{d} containing vertical axis activity counts
#' @param axis2 for \code{Sojourn 3x}, the name of the variable in \code{d}
#'   containing horizontal axis activity counts
#' @param axis3 for \code{Sojourn 3x}, the name of the variable in \code{d}
#'   containing lateral axis activity counts
#' @param vector.magnitude for \code{Sojourn 3x}, the name of the variable in
#'   \code{d} containing vector magnitude activity counts
#' @param met_name [for internal use] A character scalar giving the name of the
#'   column containing metabolic equivalent values (METs)
#' @param tag [for internal use] A character scalar giving an informative tag to
#'   add when naming variables
#' @param select [for internal use] A character scalar or vector indicating
#'   which \code{Staudenmayer} model(s) to run
#'
#' @details This is a wrapper and aggregator for applying different energy
#'   expenditure prediction methods. Depending on the value(s) specified in the
#'   \code{method} argument, calls are made to applicator functions (one per
#'   method). Most applicators require values to be passed in for additional
#'   variables. Thus, the signature for each applicator function is included
#'   above, in the \code{usage section}.
#'
#'   For \code{TwoRegression} methods, a customized internal wrapper
#'   (\code{wrap_2RM}) is used around
#'   \code{\link[TwoRegression]{TwoRegression}}. Additional arguments can be
#'   passed to that function directly through this one. Similarly for
#'   \code{Sojourn} methods, additional arguments can be passed directly to the
#'   corresponding functions from the \code{Sojourn} package. Links to those are
#'   below.
#'
#'   For \code{Staudenmayer} and \code{Montoye} methods, values can be passed
#'   directly to \code{\link{staudenmayer_features}} and
#'   \code{\link{montoye_features}}, respectively (if feature calculation is
#'   requested via the \code{feature_calc} argument).
#'
#' @return A data frame appended with new columns containing energy
#'     expenditure predictions
#'
#' @note
#'
#' Some things to be aware of:
#'
#' 1. Oxygen consumption values are converted to kcal using factors from the
#' Lusk table (by default, 4.862 kcal/L, corresponding to RER of 0.85; see
#' `References` below).
#'
#' 2. Not all methods can necessarily be combined through a single call.
#' This capability is dependent on the desired settings and format of the
#' output. There are too many possibilities and contingencies to
#' list in a single documentation file. Options and adaptations can be
#' discussed on \href{https://github.com/paulhibbing/accelEE/issues}{GitHub}.
#'
#' 3. The \code{wrap_2RM} applicator does not have a formal \code{min_mets}
#' argument because the default minima differ depending on the method being
#' implemented. However, you can still pass a value for \code{min_mets}, and
#' it will get forwarded to the \code{TwoRegression} package and applied as expected.
#'
#' @references
#'
#' Crouter et al. (2006). \doi{10.1152/japplphysiol.00818.2005}
#'
#' Crouter et al. (2010). \doi{10.1249/MSS.0b013e3181c37458}
#'
#' Crouter et al. (2012), \doi{10.1249/MSS.0b013e3182447825}
#'
#' Crouter et al. (2015). \doi{10.1249/MSS.0000000000000502}
#'
#' Hibbing et al. (2018). \doi{10.1249/MSS.0000000000001532}
#'
#' Hildebrand et al. (2014). \doi{10.1249/MSS.0000000000000289}
#'
#' Hildebrand et al. (2017). \doi{10.1111/sms.12795}
#'
#' Ellingson et al. (2017). \doi{10.1088/1361-6579/aa6d00}
#'
#' Montoye et al. (2017). \doi{10.1080/1091367X.2017.1337638}
#'
#' Lyden et al. (2014). \doi{10.1249/MSS.0b013e3182a42a2d}
#'
#' Ellingson et al. (2016). \doi{10.1249/MSS.0000000000000915}
#'
#' Staudenmayer et al. (2015). \doi{10.1152/japplphysiol.00026.2015}
#'
#'
#' @seealso
#'
#' Lusk, G. (1924). Analysis of the oxidation of mixtures of carbohydrate and
#' fat: a correction. \emph{Journal of Biological Chemistry}, 59, 41-42.
#'
#' \code{\link[TwoRegression]{TwoRegression}}
#'
#' \code{\link[Sojourn]{sojourn_3x_SIP}}
#'
#' \code{\link[Sojourn]{soj_1x_original}}
#'
#' \code{\link[Sojourn]{soj_3x_original}}
#'
#' @examples
#'
#' #### Below, note the variations throughout the examples,
#' #### showing different ways you can customize the output
#'
#'
#'
#' ## Raw acceleration examples:
#'
#' if (isTRUE(requireNamespace("read.gt3x", quietly = TRUE))) {
#'
#'   f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'   d <- stats::setNames(
#'     read.gt3x::read.gt3x(f, asDataFrame = TRUE, imputeZeroes = TRUE),
#'     c("Timestamp", "Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
#'   )[1:30000, ]
#'
#'   utils::head(
#'     accelEE(
#'       d, "Hibbing 2018", algorithm = 1,
#'       site = c("Left Wrist", "Right Wrist"),
#'       warn_high_low = FALSE, shrink_output = FALSE
#'     )
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       d, c("Hildebrand Linear", "Hildebrand Non-Linear"), age = "adult",
#'       monitor = "ActiGraph", location = "Wrist", warn_high_low = FALSE,
#'       ee_vars = c("METs", "kcal"), output_epoch = "60 sec"
#'     )
#'   )
#'
#'   accelEE(
#'     d, c(
#'       "Montoye 2017", "Staudenmayer Linear",
#'       "Staudenmayer Random Forest"
#'     ), side = "left", ee_vars = "VO2", combine = FALSE
#'   )
#'
#' }
#'
#'
#' ## Activity count examples:
#'
#' if (isTRUE(requireNamespace("TwoRegression", quietly = TRUE))) {
#'
#'   data(count_data, package = "TwoRegression")
#'
#'   utils::head(
#'     accelEE(
#'       count_data, c("Crouter 2006", "Crouter 2010"),
#'       movement_var = "Axis1", time_var = "time"
#'     )
#'   )
#'
#' }
#'
#'
#' \donttest{if (isTRUE(requireNamespace("Sojourn", quietly = TRUE))) {
#'
#'   # Sojourn methods can't be implemented in a single call,
#'   # but you can chain them together, particularly with
#'   # `magrittr` piping although that is not shown below
#'
#'   data(SIP_ag, package = "Sojourn")
#'   data(SIP_ap, package = "Sojourn")
#'   d <- Sojourn::enhance_actigraph(SIP_ag, SIP_ap)
#'
#'   soj_results <- accelEE(d, "SIP", time_var = "Time", warn_high_low = FALSE)
#'   #^^Note that the SIP method causes a `Timestamp` variable to be
#'   #  silently populated, whereas the input data frame must have a column
#'   #  named `Time`. In general, the Sojourn methods (especially SIP) are
#'   #  currently coded somewhat inflexibly, often requiring specific
#'   #  variable names for the input. Your best bet for getting them to run
#'   #  is to copy, paste, and execute the package examples, then format your
#'   #  data to match the example data exactly.
#'
#'   soj_results <- accelEE(
#'     soj_results, "Sojourn 1x", axis1 = "counts", time_var = "Time"
#'   )
#'
#'   soj_results <- accelEE(
#'     soj_results, "Sojourn 3x", axis1 = "counts", axis2 = "axis2",
#'     axis3 = "axis3", vector.magnitude = "vm", output_epoch = "60 sec"
#'   )
#'   #^^Note that this collapses everything to one-minute epochs
#'
#'   utils::head(soj_results)
#'
#' }}
#'
#'
#' @name accelEE-function
#' @export
#'
accelEE <- function(
  d,
  method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Crouter 2015",
    "Hibbing 2018", "Hildebrand Linear", "Hildebrand Non-Linear",
    "Montoye 2017", "SIP", "Sojourn 1x", "Sojourn 3x",
    "Staudenmayer Linear", "Staudenmayer Random Forest"
  ),
  time_var = "Timestamp",
  output_epoch = "default",
  warn_high_low = TRUE,
  met_mlkgmin = 3.5,
  RER = 0.85,
  feature_calc = TRUE,
  shrink_output = TRUE,
  combine = TRUE,
  ee_vars = c("METs", "VO2", "kcal"),
  verbose = FALSE,
  ...
) {


  ## Setup
  timer <- PAutilities::manage_procedure(
    "Start", "\n\nInitiating the `accelEE` process",
    verbose = verbose
  )
  d %<>% check_data_format(.)
  method %<>% check_method_format(.)
  output_epoch %<>% get_compatible_epoch(d, method, time_var, combine)
  ee_vars %<>% get_ee_vars(.)


  ## Apply the methods
  ee_values <-
    method %>%
    sapply(
      switch,
      "Crouter 2006" = wrap_2RM(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, method = "Crouter 2006",
        ..., met_name = "METs", tag = "crouter06"
      ),
      "Crouter 2010" = wrap_2RM(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, method = "Crouter 2010",
        ..., met_name = "METs", tag = "crouter10"
      ),
      "Crouter 2012" = wrap_2RM(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, method = "Crouter 2012",
        ..., met_name = "METs", tag = "crouter12"
      ),
      "Crouter 2015" = crouter15(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        shrink_output = shrink_output, verbose = verbose,
        ...
      ),
      "Hibbing 2018" = wrap_2RM(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, method = "Hibbing 2018",
        ..., met_name = "METs", tag = "hibbing18"
      ),
      "Hildebrand Linear" = hildebrand_linear(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose,
        ...
      ),
      "Hildebrand Non-Linear" = hildebrand_nonlinear(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose,
        ...
      ),
      "Montoye 2017" = montoye(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose,
        ...
      ),
      "SIP" = sojourn(
        d,  time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        shrink_output = shrink_output, verbose = verbose,
        method = "SIP", ..., met_name = "METs", tag = "SIP"
      ),
      "Sojourn 1x" = sojourn(
        d,  time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        shrink_output = shrink_output, verbose = verbose,
        method = "Sojourn 1x", ..., met_name = "METs", tag = "soj_1x"
      ),
      "Sojourn 3x" = sojourn(
        d,  time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        shrink_output = shrink_output, verbose = verbose,
        method = "Sojourn 3x", ..., met_name = "METs", tag = "soj_3x"
      ),
      "Staudenmayer Linear" = staudenmayer(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, ..., select = "METs_lm"
      ),
      "Staudenmayer Random Forest" = staudenmayer(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, ..., select = "METs_rf"
      ),
      "Staudenmayer Both" = staudenmayer(
        d, time_var, output_epoch,
        warn_high_low = warn_high_low,
        met_mlkgmin = met_mlkgmin, RER = RER,
        feature_calc = feature_calc,
        shrink_output = shrink_output,
        verbose = verbose, ..., select = c("METs_lm", "METs_rf")
      ),
      stop(
        "Invalid value passed for `method` argument:",
        " see ?args(accelEE::accelEE) for options",
        call. = FALSE
      ),
      simplify = FALSE
    ) %>%
    stats::setNames(., gsub("^Staudenmayer Both$", "Staudenmayer", names(.)))


  ## Keep desired variables
  removals <- setdiff(c("METs", "vo2", "kcal"), ee_vars)
  if (length(removals) > 0) ee_values %<>% lapply(
    dplyr::select,
    !dplyr::matches(removals)
  )

  ## Navigate return formatting
  if (!combine) {

    output <- ee_values

    if (length(method) == 1) output %<>% .[[1]]

  } else {

    if (verbose) cat("\n...Assembling output")

    is_sip <- "SIP" %in% method

    if (is_sip & length(method) > 1) stop(
      "SIP cannot currently be combined",
      " with other methods via a single call"
    )

    if (is_default(output_epoch)) {

      if (is_sip) time_var <- "Timestamp"

      output_epoch <-
        sapply(ee_values, epoch_length, time_var) %>%
        unique(.) %T>%
        {stopifnot(length(.) == 1)} %>%
        lubridate::period(.)

      if (is_sip) time_var <- "Time"

    }

    output <-
      collapse_EE(d, time_var, output_epoch, verbose = FALSE) %>%
      join_EE(ee_values) %>%
      dplyr::relocate(!dplyr::matches("^crouter06")) %>%
      dplyr::relocate(!dplyr::matches("^crouter10")) %>%
      dplyr::relocate(!dplyr::matches("^crouter12")) %>%
      dplyr::relocate(!dplyr::matches("^crouter15")) %>%
      dplyr::relocate(!dplyr::matches("^hibbing18")) %>%
      dplyr::relocate(!dplyr::matches(".+_hildebrand_linear_.+")) %>%
      dplyr::relocate(!dplyr::matches("hildebrand_nonlinear$")) %>%
      dplyr::relocate(!dplyr::matches("^montoye*left")) %>%
      dplyr::relocate(!dplyr::matches("^montoye*right")) %>%
      dplyr::relocate(!dplyr::matches("^SIP_")) %>%
      dplyr::relocate(!dplyr::matches("^soj_1x")) %>%
      dplyr::relocate(!dplyr::matches("^soj_3x")) %>%
      dplyr::relocate(!dplyr::matches("^staudenmayer*lm")) %>%
      dplyr::relocate(!dplyr::matches("^staudenmayer*rf")) %>%
      dplyr::relocate(!dplyr::matches("METs?", TRUE)) %>%
      dplyr::relocate(!dplyr::matches(("vo2_mlkgmin"))) %>%
      dplyr::relocate(!dplyr::matches("kcal_kgmin"))

  }

  output  %T>%
  {PAutilities::manage_procedure(
    "End", "\nProcess complete. Elapsed time",
    PAutilities::get_duration(timer), "mins\n\n",
    verbose = verbose
  )}

}
