#' Create a `sspm` model object
#'
#' Create a sspm_model object.
#'
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param predictors **\[list  OF sspm_dataset (smoothed)\]** The list of predictor
#'     datasets.
# @param biomass_var **\[character\]** The biomass variable.
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @examples
#' \dontrun{
#' sspm_model <- sspm(biomass = biomass_smooth_w_catch,
#'                    predictors = predator_smooth)
#' }
#'
#' @rdname sspm-constructor
#' @export
setGeneric(name = "sspm",
           def = function(biomass,
                          predictors) {
             standardGeneric("sspm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "missing"),
          function(biomass, predictors) {

            new_sspm <- new("sspm",
                            datasets = list(biomass = biomass),
                            time = spm_time(biomass),
                            uniqueID = "row_ID",
                            boundaries = spm_boundaries(biomass),
                            smoothed_data = spm_smoothed_data(biomass),
                            smoothed_vars = spm_smoothed_vars(biomass),
                            is_split = FALSE)

          }
)


#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "sspm_dataset"),
          function(biomass, predictors) {

            # Turn predictors to list
            predictors <- list(predictors)
            names(predictors) <- sapply(predictors, spm_name)
            sspm(biomass, predictors)

          }
)

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "list"),
          function(biomass, predictors) {

            # 1. Check all predictors in list are sspm_dataset
            if (any(!is_sspm_dataset(predictors))) {
              cli::cli_alert_danger("Some predictors are not of class sspm_dataset")

            } else {

              # 2. Check boundaries
              biomass_boundaries <- spm_boundaries(biomass)
              biomass_patches <- spm_patches(biomass_boundaries) %>%
                dplyr::select("patch_id")
              biomass_boundary <- spm_boundary(spm_boundaries(biomass))
              biomass_time <- spm_time(biomass)

              patch_area <- spm_patches_area(biomass_boundaries)
              predictors_boundaries <- lapply(predictors, spm_boundaries)
              all_boundaries <- unname(append(list(biomass_boundaries),
                                              predictors_boundaries))
              check_identical_boundaries(all_boundaries)

              # 3. combine the full_smoothed_data/vars
              info_message <- paste0(" Joining smoothed data from all datasets")
              cli::cli_alert_info(info_message)

              biomass_clean <- clean_data_for_joining(spm_smoothed_data(biomass))
              joining_vars <- c("patch_id", biomass_boundary, biomass_time)

              if (patch_area %in% names(biomass_clean)) {
                joining_vars <- c(joining_vars, patch_area)
              }

              full_smoothed_data <- biomass_clean
              full_smoothed_vars <- spm_smoothed_vars(biomass)

              # This for loop cannot easily be a function, as it joins things
              # consecutively to the biomass dataset
              for (predictor in predictors) {

                the_suffix <- c(paste0("_", spm_name(biomass)),
                                paste0("_", spm_name(predictor)))

                dataset <- predictor %>%
                  spm_smoothed_data() %>%
                  clean_data_for_joining() %>%
                  dplyr::rename(!!spm_time(biomass) := spm_time(predictor))

                full_smoothed_data <- full_smoothed_data %>%
                  dplyr::left_join(dataset,
                                   by = joining_vars,
                                   suffix = the_suffix)

                full_smoothed_vars <- sort_out_smoothed_vars(full_smoothed_vars,
                                                             predictor,
                                                             the_suffix)
              }

              # Join to patches
              full_smoothed_data <- full_smoothed_data %>%
                dplyr::left_join(biomass_patches, by = "patch_id") %>%
                sf::st_as_sf() %>%
                tibble::rowid_to_column("row_ID")

              # 4. create and return object
              biomass_name <- spm_name(biomass)
              all_data <- append(list(biomass = biomass),
                                 predictors)
              names(all_data)[1] <- biomass_name

              new_sspm <- new("sspm",
                              datasets = all_data,
                              time = spm_time(biomass),
                              uniqueID = "row_ID",
                              boundaries = spm_boundaries(biomass),
                              smoothed_data = full_smoothed_data,
                              smoothed_vars = full_smoothed_vars,
                              is_split = FALSE)

            }
          }
)

# -------------------------------------------------------------------------

# Check if boundaries are identical
check_identical_boundaries <- function(boundaries) {
  boundaries <- lapply(boundaries, spm_boundaries)

  for (bound in seq_len(length(boundaries) - 1)){

    bound_check <- identical(boundaries[bound],
                             boundaries[bound + 1])

    if(!bound_check){
      cli::cli_alert_danger("not all datasets have the same boundaries object")
      stop(call. = FALSE)
    }

  }

}

# Check whether of sspm_dataset class of a list of objects
is_sspm_dataset <- function(list_of_datasets) {
  checkmate::assert_list(list_of_datasets)
  checks <- sapply(list_of_datasets,
                   function(x) {checkmate::test_class(x, "sspm_dataset")})
  return(checks)
}

# Clean up data frame before we can join
clean_data_for_joining <- function(dataset) {
  dataset %>% dplyr::select(-"row_ID") %>% sf::st_drop_geometry()
}

# Sort out the naming of smoothed variables
sort_out_smoothed_vars <- function(full_smoothed_vars, predictor,
                                   the_suffix){

  # Get the predictor smoothed vars
  predictor_smoothed_vars <- spm_smoothed_vars(predictor)

  # Check which vars are the same vars
  same_vars <-
    full_smoothed_vars[full_smoothed_vars %in% predictor_smoothed_vars]

  if (length(same_vars) > 0){

    predictor_smoothed_vars <-
      predictor_smoothed_vars[!(predictor_smoothed_vars %in% same_vars)]
    full_smoothed_vars <-
      full_smoothed_vars[!(full_smoothed_vars %in% same_vars)]

    same_vars <- paste0(same_vars, the_suffix)

  }

  full_smoothed_vars <- c(full_smoothed_vars, predictor_smoothed_vars,
                          same_vars)

}
