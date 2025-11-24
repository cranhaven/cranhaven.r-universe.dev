#' Aggregate a dataset or fit data variable based on a boundary
#'
#' Aggregate the data contained in a dataset or fit  based on the discretized
#' boundaries, using a function and a filling value.
#'
#' @param sspm_object **\[sspm_dataset or sspm_fit\]** The dataset object.
#' @param boundaries **\[sspm_discrete_boundary\]** The boundaries object
#'     (optionnal).
#' @param level **\[character\]** The aggregation level, "patch" or
#'     "boundary".
#' @param type **\[character\]** The targeted type of aggregation, one of
#'     "data" for base data or "smoothed" for smoothed data.
#' @param variable **\[character\]** Variable to aggregate (ignored in case
#'     `apply_to_df` is `TRUE`).
#' @param fun **\[function\]** Function to use to aggregate data.
#' @param group_by **\[character\]** One of `time`, `space` and `spacetime`.
#' @param fill **\[logical OR numeric OR function\]** Whether to complete the
#'     incomplete cases, default to `FALSE` for no completion.
#' @param apply_to_df **\[logical\]** Wether `fun` applied to  the data frame
#'     group or to `variable`, default to `FALSE`.
#' @param ... More arguments passed onto `fun`
#'
#' @return
#' Updated `sspm_dataset` or `sspm_fit`.
#'
#' @examples
#' \dontrun{
#' spm_aggregate(sspm_object = catch,
#'               boundaries = spm_boundaries(biomass),
#'               variable = catch_variable,
#'               fun = fun, group_by = group_by,
#'               fill = fill, apply_to_df = apply_to_df,
#'               na.rm = TRUE, ...)
#' }
#'
#' @export
#' @rdname spm_aggregate
setGeneric(name = "spm_aggregate",
           def = function(sspm_object,
                          boundaries,
                          level = "patch",
                          type = "data",
                          variable,
                          fun,
                          group_by = "spacetime",
                          fill = FALSE,
                          apply_to_df = FALSE,
                          ...){

             standardGeneric("spm_aggregate")
           }
)

# Methods -----------------------------------------------------------------
#' @export
#' @rdname spm_aggregate
setMethod(f = "spm_aggregate",
          signature(sspm_object = "sspm_dataset",
                    boundaries = "missing"),
          function(sspm_object,
                   boundaries,
                   level,
                   type,
                   variable,
                   fun,
                   group_by,
                   fill,
                   apply_to_df,
                   ...){

            if (is_mapped(sspm_object)) {
              boundaries <- spm_boundaries(sspm_object)
            } else {
              cli::cli_alert_danger("no boundaries provided to aggregate an un-mapped dataset")
              stop(call. = FALSE)
            }

            spm_aggregate(sspm_object, boundaries, level, type, variable, fun,
                          group_by, fill, apply_to_df, ...)

          }
)

#' @export
#' @rdname spm_aggregate
setMethod(f = "spm_aggregate",
          signature(sspm_object = "sspm_dataset",
                    boundaries = "sspm_discrete_boundary"),
          function(sspm_object,
                   boundaries,
                   level,
                   type,
                   variable,
                   fun,
                   group_by,
                   fill,
                   apply_to_df,
                   ...){

            # Check info
            checkmate::assert_character(variable)
            checkmate::assert_function(fun)
            checkmate::assert_choice(group_by, spm_aggregation_choices())
            checkmate::assert_choice(level, spm_aggregation_levels_choices())
            checkmate::assert_choice(type, spm_aggregation_types_choices())

            # Get data
            bounds <- spm_boundaries(sspm_object)
            boundary <- spm_boundary(bounds)
            time_col <- spm_time(sspm_object)

            if (!is_mapped(sspm_object)) { # Need to map dataset
              sspm_object <- join_datasets(sspm_object, boundaries)
            }

            if (type == "data"){

              dataset_data <- spm_data(sspm_object)
              assert_column(dataset_data, variable)
              spm_data(sspm_object) <-
                spm_aggregate_routine(dataset_data, boundaries, group_by, level,
                                      time_col, boundary, variable, fun, fill,
                                      apply_to_df, ...)

            } else if (type == "smoothed"){

              dataset_data <- spm_smoothed_data(sspm_object)
              assert_column(dataset_data, variable)
              spm_smoothed_data(sspm_object) <-
                spm_aggregate_routine(dataset_data, boundaries, group_by, level,
                                      time_col, boundary, variable, fun, fill,
                                      apply_to_df, ...)

            }

            return(sspm_object)

          }
)

# # @export
# # @rdname spm_aggregate
# setMethod(f = "spm_aggregate",
#           signature(sspm_object = "sspm_fit"),
#           function(sspm_object,
#                    variable,
#                    fun,
#                    group_by,
#                    fill,
#                    apply_to_df,
#                    ...){
#
#             # Check info
#             checkmate::assert_character(variable)
#             checkmate::assert_function(fun)
#             checkmate::assert_choice(group_by, spm_aggregation_choices())
#
#             # Get info
#             dataset_data <- spm_smoothed_data(sspm_object)
#             bounds <- spm_boundaries(sspm_object)
#             boundary <- spm_boundary(bounds)
#             time_col <- spm_time(sspm_object)
#
#             # Verify column
#             assert_column(dataset_data, variable)
#
#             spm_smoothed_data(sspm_object) <-
#               spm_aggregate_routine(dataset_data, bounds, group_by, "boundary",
#                                     time_col, boundary, variable, fun, fill,
#                                     apply_to_df, ...)
#
#             return(sspm_object)
#
#           }
# )
