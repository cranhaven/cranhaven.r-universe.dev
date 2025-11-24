#' Fit the GAM part of a sspm model
#'
#' Once formulas have been mapped onto a sspm discrete object, the GAMs can be
#' fitted with this function. Arguments can be passed onto `bam`.
#'
#' @inheritParams spm_smooth
#' @param sspm_formula **\[sspm_formula\]** The formula specifying the the
#'     smoothing model.
#' @inheritParams mgcv::bam
#' @inheritDotParams mgcv::bam
#'
#' @return
#' A object of the class [`sspm_fit`][sspm_fit-class].
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fit_smooths(boundaries = boundaries, keep_fit = TRUE, ...)
#' fit_spm(sspm_object = sspm_object, sspm_formula = sspm_formula, ...)
#' }

#' @rdname fit
setGeneric(name = "fit_smooths",
           def = function(sspm_object,
                          boundaries,
                          keep_fit = TRUE,
                          predict = TRUE,
                          family = mgcv::tw,
                          drop.unused.levels = F,
                          method = "fREML",
                          ...) {
             standardGeneric("fit_smooths")
           }
)

#' @rdname fit
setGeneric(name = "fit_spm",
           def = function(sspm_object,
                          sspm_formula,
                          keep_fit = TRUE,
                          family = mgcv::scat,
                          drop.unused.levels = F,
                          select = TRUE,
                          method = "REML",
                          ...) {
             standardGeneric("fit_spm")
           }
)

# Methods -----------------------------------------------------------------

#' @rdname fit
#' @keywords internal
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm_dataset",
                    boundaries = "sspm_discrete_boundary"),
          function(sspm_object, boundaries,
                   keep_fit, predict,
                   family, drop.unused.levels, method, ...) {

            # Get data
            the_data <- units::drop_units(spm_data(sspm_object))
            time_col <- spm_time(sspm_object)
            boundaries <- spm_boundaries(sspm_object)
            patches <- spm_patches(boundaries)

            # Initialize/collect smoothed_vars/fit
            full_smoothed_vars <- spm_smoothed_vars(sspm_object)
            if (is.null(full_smoothed_vars)) {
              full_smoothed_vars <- c()
            }

            tmp_fit <- spm_smoothed_fit(sspm_object)
            if (length(tmp_fit) == 0){
              tmp_fit <- list()
            } else {
              tmp_fit <- spm_smoothed_fit(sspm_object)
            }

            # Get the length of the formula set
            formulas <- spm_formulas(sspm_object)
            formula_length <- length(formulas)

            # Fit the formulas into a list
            tmp_smoothed <-
              vector(mode = "list", length = formula_length)

            # For loop, no lapply, because the for loop exits for each formula
            # which has already been fitted and we only really fit one model
            # per call to this function
            for (form_id in seq_len(length.out = formula_length)) {

              # Index formula
              form <- formulas[[form_id]]

              if (is_fitted(form)) {
                next
              }

              response <- spm_response(form)
              form_vars <- formula_vars(form)

              # Print info
              cli::cli_alert_info(
                paste0(" Fitting formula: ",
                       cli::col_yellow(format_formula(raw_formula(form))),
                       " for dataset ", cli::col_cyan(paste0("'", spm_name(sspm_object), "'"))))

              # Modify formula env
              form_env <- attr(translated_formula(form), ".Environment")
              for (var in names(form_vars)) {
                assign(x = var, value = form_vars[[var]], envir = form_env)
              }

              # Fit the formula, important to attach the vars
              tmp_fit[[response]] <- tryCatch({
                mgcv::bam(formula = translated_formula(form),
                          data = the_data,
                          family = family,
                          drop.unused.levels = drop.unused.levels,
                          method = method,
                          ...)
              }, error = function(e) {

                # Catch potential issue with by variables
                if (e$message == "Can't find by variable") {
                  cli::cli_alert_danger(" mgcv failed to fit 'by' smooths")
                  cli::cli_alert_info(" Please ensure that all 'by = ...' variables are encoded as factors")
                  stop("mgcv failed to fit 'by' smooths", call. = FALSE)
                } else {
                  stop(e)
                }

              })
            }

            # Note as fitted
            is_fitted(formulas[[form_id]]) <- TRUE
            spm_formulas(sspm_object) <- formulas

            # Store results at dataset level
            if (keep_fit) {
              spm_smoothed_fit(sspm_object) <- tmp_fit
            }

            full_smoothed_vars <- c(full_smoothed_vars, response)
            spm_smoothed_vars(sspm_object) <- full_smoothed_vars

            return(sspm_object)
          }
)

#' @rdname fit
setMethod(f = "fit_spm",
          signature(sspm_object = "sspm",
                    sspm_formula = "sspm_formula"),
          function(sspm_object, sspm_formula,
                   family, drop.unused.levels, select, method, ...) {

            # Here we fit the full spm
            # Get train dataset
            all_data <- units::drop_units(spm_smoothed_data(sspm_object))
            the_data <- all_data %>%
              dplyr::filter(.data$train_test == TRUE)

            # Get/Initializa vars of use
            time_col_name <- spm_time(sspm_object)
            the_fit <- NULL

            # Index formula
            form_vars <- process_formula_vars(formula_vars(sspm_formula),
                                              all_data)

            # Print info
            cli::cli_alert_info(
              paste0(" Fitting SPM formula: ",
                     cli::col_yellow(format_formula(raw_formula(sspm_formula)))))

            # Modify formula env, best solution for now
            form_env <- attr(translated_formula(sspm_formula), ".Environment")
            for (var in names(form_vars)) {
              assign(x = var, value = form_vars[[var]], envir = form_env)
            }

            # Fit the formula, important to attach the vars
            tmp_fit <-
              mgcv::bam(formula = translated_formula(sspm_formula),
                        data = c(as.list(the_data),
                                 form_vars),
                        family = family,
                        select = select,
                        method = method,
                        ...)

            # For now return fit
            return(tmp_fit)

          }
)

# Helpers -----------------------------------------------------------------

# Process the variables into a list for proper predictions
process_formula_vars <- function(vars, the_data, select = TRUE) {

  checkmate::assert_list(vars)
  train_IDs <- which(the_data$train_test == select)

  for (var_name in names(vars)) {

    if (var_name %in% c("lag_matrix", "by_matrix")) {

      vars[[var_name]] <- vars[[var_name]][train_IDs, ]

    }

  }

  return(vars)
}
