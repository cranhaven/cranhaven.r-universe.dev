#' Update biomass value from catch adta
#'
#' Aggregate the catch data contained in a catch dataset and update the biomass
#' dataset with the subtracted catch.
#'
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param catch **\[sspm_dataset\]** The dataset containing the catch variable.
#' @param biomass_variable **\[character\]** The biomass variab of `biomass`.
#' @param catch_variable **\[character\]** The catch column of `catch`.
#' @param corrections **\[data.frame\]** Optional landings corrections.
#' @inheritParams spm_aggregate
#'
#' @return
#' Updated `sspm_dataset`.
#'
#' @examples
#' \dontrun{
#' spm_aggregate_catch(biomass = biomass_smooth, catch = catch_dataset,
#'                     biomass_variable = "weight_per_km2",
#'                     catch_variable = "catch",
#'                     fill = mean)
#' }
#'
#' @export
setGeneric(name = "spm_aggregate_catch",
           def = function(biomass,
                          catch,
                          biomass_variable,
                          catch_variable,
                          corrections = NULL,
                          fun = sum,
                          group_by = "spacetime",
                          fill,
                          apply_to_df = FALSE,
                          ...){
             standardGeneric("spm_aggregate_catch")
           }
)

#' @rdname spm_aggregate_catch
#' @export
setMethod(f = "spm_aggregate_catch",
          signature(biomass = "sspm_dataset",
                    catch = "sspm_dataset",
                    biomass_variable = "character",
                    catch_variable = "character"),
          function(biomass,
                   catch,
                   biomass_variable,
                   catch_variable,
                   corrections = NULL,
                   fun = sum,
                   group_by = "spacetime",
                   fill,
                   apply_to_df = FALSE,
                   ...){

            # Run some checks
            run_catch_aggregation_checks(biomass_variable, biomass,
                                         catch_variable, catch)

            # If successful, print useful message
            info_message <-
              paste0(" Offsetting biomass with catch data using columns: ",
                     paste(cli::col_green(c(biomass_variable, catch_variable)), collapse = ", "))
            cli::cli_alert_info(info_message)

            # Get the right columns
            biomass_time_col <- spm_time(biomass)
            catch_time_col <- spm_time(catch)
            boundary_col <- spm_boundary(spm_boundaries(biomass))
            area_col <- spm_patches_area(spm_boundaries(biomass))

            # Aggregate the catch
            catch <- spm_aggregate(sspm_object = catch,
                                   boundaries = spm_boundaries(biomass),
                                   variable = catch_variable,
                                   fun = fun, group_by = group_by,
                                   fill = fill, apply_to_df = apply_to_df,
                                   na.rm = TRUE, ...)

            catch_data <- spm_data(catch) %>%
              dplyr::rename(!!biomass_time_col := dplyr::all_of(catch_time_col))

            # First, join data
            full_smoothed_data <- spm_smoothed_data(biomass) %>%
              dplyr::mutate(!!biomass_time_col :=
                              as.factor(.data[[biomass_time_col]])) %>%
              dplyr::left_join(catch_data,
                               by = sapply(c(biomass_time_col, "patch_id"),
                                           rlang::as_string))

            # Use corrections if need be
            if(!is.null(corrections)){
              full_smoothed_data <- apply_corrections(full_smoothed_data, corrections,
                                                      catch_variable)
            }

            # Make correct names
            catch_name <- paste(c(biomass_variable, spm_name(biomass),
                                  "with_catch"), collapse = "_")

            # Calculate productivity
            full_smoothed_data <- full_smoothed_data %>%

              dplyr::rename(catch = dplyr::all_of(catch_variable)) %>%

              dplyr::group_by(.data[["patch_id"]], .data[[boundary_col]]) %>%
              dplyr::mutate(catch_density = .data$catch / .data[[area_col]]) %>%

              dplyr::mutate(
                !!catch_name := (.data[[biomass_variable]] + .data$catch_density)) %>%
              dplyr::mutate(
                log_productivity = log(.data[[catch_name]]) -
                  log(dplyr::lag(.data[[biomass_variable]], default = NA)),
                # Make sure to drop units, as the backticks heuristic units uses
                # does not work in this case
                log_productivity = units::drop_units(.data$log_productivity),
                productivity = exp(.data$log_productivity)) %>%

              dplyr::ungroup()

            # Re-arrange things
            full_smoothed_data <-  full_smoothed_data %>%
              dplyr::relocate(dplyr::starts_with(biomass_variable),
                              .after = "row_ID") %>%
              dplyr::relocate(c("log_productivity", "productivity"),
                              .after = "row_ID") %>%
              # TODO this might not be needed anymore
              dplyr::mutate(!!biomass_time_col :=
                              as.numeric(as.character(.data[[biomass_time_col]])))

            spm_smoothed_data(biomass) <- full_smoothed_data

            return(biomass)

          }
)

# Helpers -----------------------------------------------------------------

# This functions operates a few checks necessary before running the aggregation
run_catch_aggregation_checks <- function(biomass_variable, biomass,
                                         catch_variable, catch){

  assert_column(spm_smoothed_data(biomass), biomass_variable)
  assert_column(spm_data(catch), catch_variable)

  # TODO check that catch is a biomass var!
  if (!is_biomass(spm_data(catch)[[catch_variable]])) {
    stop("`catch_variable` must have units of type biomass",
         call. = FALSE)
  }

}

# This function applies catch corrections
apply_corrections <- function(full_data, corrections, catch_var){
  # TODO: add verification for time and boundary cols
  nrow_data <- nrow(full_data)

  stopifnot("catch_adjustment" %in% names(corrections))

  full_data <- full_data %>%
    dplyr::left_join(corrections) %>%
    dplyr::mutate(catch = .data$catch_adjustment *
                    .data[[!!catch_var]]) %>%
    dplyr::select(-.data$catch_adjustment)

  checkmate::assert_true(nrow(full_data) == nrow_data)

  return(full_data)
}
