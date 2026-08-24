#' Do factor-analysis, logging warnings and errors
#'
#' @param ... The arguments for `fa` in `psych`.
#' @param n.repeatOnWarning How often to repeat on warnings (in the
#' hopes of getting a run without warnings).
#' @param warningTolerance How many warnings are accepted.
#' @param silentRepeatOnWarning Whether to be chatty or silent when
#' repeating after warnings.
#' @param showWarnings Whether to show the warnings.
#'
#' @return A list with the `fa` object and a `warnings` and an `errors` object.
#' @export
#'
fa_failsafe <- function(...,
                        n.repeatOnWarning = 50,
                        warningTolerance = 2,
                        silentRepeatOnWarning = FALSE,
                        showWarnings = TRUE) {

  ### Set warnings to appear as messages (first save original value)
  oldWarn <- getOption("warn", 0);
  options(warn=1);

  ### First time, so 'zeroeth' repetition
  n.repeatOnWarning_repetitions <- 0;

  if (is.null(n.repeatOnWarning)) {
    n.repeatOnWarning <- 0;
  }

  ### (Re)set logs to empty vector
  options(ufs.CIM.warnings = c());
  options(ufs.CIM.errors = c());

  ### First run to get initial results
  tryCatch(
    withCallingHandlers(
      moreCapturedWarnings <-
        utils::capture.output(
          res <- psych::fa(...)
        ),
      warning = function(w) {
        options(ufs.CIM.warnings = c(getOption("ufs.CIM.warnings"),
                                     w$message));
        invokeRestart("muffleWarning");
      }
    ),
    error = function(e) {
      options(ufs.CIM.errors = c(getOption("ufs.CIM.errors"),
                                 e$message));
      stop("Error encountered in EFA: ", e$message);
    }
  );

  ### Potentially repeat multiple times
  while (((length(getOption("ufs.CIM.warnings")) +
           length(getOption("ufs.CIM.errors"))) > warningTolerance) &&
         (n.repeatOnWarning_repetitions <= n.repeatOnWarning)) {

    if (showWarnings) {
      print(getOption("ufs.CIM.warnings"));
    }

    n.repeatOnWarning_repetitions <-
      n.repeatOnWarning_repetitions + 1;

    if (!silentRepeatOnWarning) {
      cat0("\nEncountered ",
           length(getOption("ufs.CIM.warnings")),
           " warnings and ",
           length(getOption("ufs.CIM.errors")),
           " errors during EFA; trying again. ",
           "This was run ", n.repeatOnWarning_repetitions,
           " of ", n.repeatOnWarning, ".\n");
    }

    ### (Re)set logs to empty vector
    options(ufs.CIM.warnings = c());
    options(ufs.CIM.errors = c());

    ### First run to get initial results
    tryCatch(
      withCallingHandlers(
        moreCapturedWarnings <-
          utils::capture.output(
            res <- psych::fa(...)
          ),
        warning = function(w) {
          options(ufs.CIM.warnings = c(getOption("ufs.CIM.warnings"),
                                       w$message));
          invokeRestart("muffleWarning");
        }
      ),
      error = function(e) {
        options(ufs.CIM.errors = c(getOption("ufs.CIM.errors"),
                                   e$message));
        stop("Error encountered in EFA: ", e$message);
      }
    );

  }

  ### Set warning level back
  options(warn = oldWarn);

  return(list(fa = res,
              warnings = getOption("ufs.CIM.warnings"),
              errors = getOption("ufs.CIM.errors")));

}
