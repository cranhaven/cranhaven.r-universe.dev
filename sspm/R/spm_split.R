#' Split data in test and train sets
#'
#' Split data before fitting spm.
#'
#' @param sspm_object **\[sspm\]** An object of class [sspm][sspm-class].
#' @param ... **\[expression\]** Expression to evaluate to split data.
#'
#' @return
#' The updated sspm object.
#'
#' @examples
#' \dontrun{
#' sspm_model <- sspm_model %>%
#'     spm_split(year_f %in% c(1990:2017))
#' }
#'
#' @export
setGeneric(name = "spm_split",
           def = function(sspm_object,
                          ...) {
             standardGeneric("spm_split")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_split
setMethod(f = "spm_split",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...) {

            the_data <- spm_smoothed_data(sspm_object)
            time_col <- spm_time(sspm_object)

            # Need to check whether time is a factor
            is_factor <- FALSE
            if (is.factor(the_data[[time_col]])) {
              is_factor <- TRUE
              the_data <- the_data %>%
                dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))
            }

            the_expr <- (match.call(expand.dots = FALSE)$`...`)[[1]]
            selection <- rlang::eval_tidy(the_expr,
                                          data = the_data)

            # selection <- rlang::eval_tidy(str2lang(predicate),
            #                               data = the_data)

            the_data$train_test <- selection
            is_split(sspm_object) <- TRUE

            # Re-establish factor
            if (is_factor) {
              the_data <- the_data %>%
                dplyr::mutate(!!time_col := as.factor(.data[[time_col]]))
            }

            # Relocate for nicer printing
            spm_smoothed_data(sspm_object) <- the_data %>%
              dplyr::relocate("train_test", .after = "row_ID")

            return(sspm_object)
          }
)
