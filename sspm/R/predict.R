#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @param object **\[sspm_fit\]** Fit object to predict from.
#' @param new_data **\[data.frame\]** New data to predict with.
#' @param discrete **\[logical\]** If `new_data` is NULL, whether to predict
#'    based on a discrete prediction matrix (default to TRUE).
#' @param biomass **\[character\]** Biomass variable.
#' @param aggregate **\[logical\]** For biomass predictions only, whether to
#'    aggregate the data to the boundary level. Default to FALSE.
#' @param interval **\[logical\]** Whether or not to calculate confidence, and
#'    when possible, prediction intervals.
#' @param next_ts **\[logical\]** For biomass, predict next timestep.
#' @inheritParams mgcv::predict.bam
#'
#' @return
#' A `dataframe` of predictions.
#'
#' @examples
#' \dontrun{
#' # Predictions for a model fit (usually, productivity)
#' predict(sspm_model_fit)
#' # To get biomass predictions, provide the variable name
#' predict(sspm_model_fit, biomass = "weight_per_km2_borealis")
#' # To get the next timestep predictions
#' predict(sspm_model_fit, biomass = "weight_per_km2_borealis", next_ts = TRUE)
#' }
#'
#' @export
#' @name predict
#' @aliases predict.sspm

NULL

#' @rdname predict
setMethod(f = "predict",
          signature(object = "sspm_fit"),
          function(object, new_data = NULL, biomass = NULL, aggregate = FALSE,
                   interval = FALSE, next_ts = FALSE, type = "response") {

            # Get data
            bounds <- spm_boundaries(object)
            bounds_col <- spm_boundary(bounds)
            patch_area_col <- spm_patches_area(bounds)
            time_col <- spm_time(object)
            patches <- spm_patches(bounds)
            patch_area_col <- spm_patches_area(bounds)

            # If no new data, take the smoothed data and the vars from the
            # formula. In both cases, turn into a list
            if (is.null(new_data)){

              new_data <- append(as.list(spm_smoothed_data(object)),
                                 formula_vars(spm_formulas(object)))

            } else {

              if(!is.null(biomass)){
                warning("predicting biomass, new_data argument ignored")
              }

              # Otherwise, make some checks
              checkmate::assert_class(new_data, "data.frame")
              new_data <- as.list(new_data)

            }

            # If biomass variable is not provided, we are predicting productivity
            if(is.null(biomass)){

              preds_df <- predict_productivity(object, new_data, type, interval)

            } else { # Else we are predicting biomass

              preds_df <- predict_biomass(object, new_data, biomass, next_ts,
                                          interval, aggregate)

            }

            return(preds_df)
          }
)

#' @rdname predict
setMethod(f = "predict",
          signature(object = "sspm_dataset"),
          function(object, new_data = NULL, discrete = TRUE, type = "response",
                   interval = FALSE) {

            # Gather variables
            time_col <- spm_time(object)
            the_fit <- spm_smoothed_fit(object)
            the_formulas <- spm_formulas(object)
            responses <- sapply(the_formulas, spm_response)

            # Check if there is a fit
            if (is.null(spm_smoothed_fit(object))) {
              stop("fit is missing, refit model with keep_fit = TRUE")
            }

            # Check or constract new_data. The "discrete" argument makes sure
            # predictions are made from the prediction matrix to generate
            # smoothed data.
            if (!is.null(new_data)){

              checkmate::assert_class(new_data, "data.frame")
              new_data <- as.list(new_data)

            } else {
              if (discrete) {

                new_data <-
                  make_prediction_matrix(the_data = spm_data(object),
                                         time_col = spm_time(object),
                                         patches = spm_patches(spm_boundaries(object)))

              } else {

                new_data <- spm_data(object)

              }

            }

            # Predict all
            preds <- as.data.frame(lapply(the_fit, mgcv::predict.bam,
                                          newdata = new_data,
                                          type = type))
            names(preds) <- responses

            # If we predicted based on prediction matrix, add that
            if (discrete) {
              preds <- new_data %>%
                dplyr::bind_cols(preds)
            }

            if (interval) {

              CIs <- lapply(the_fit, sspm::predict_intervals,
                            new_data = new_data, PI=FALSE)
              CIs <- mapply(CIs, names(CIs),
                            FUN = function(x,y){
                              names(x) <- paste0(y,"_",names(x));return(x)},
                            SIMPLIFY = F)
              if (discrete) {
                preds <- preds %>%
                  dplyr::bind_cols(CIs)
              }
            }

            # Make sure biomass and density variables are giveb the
            # proper units
            biomass_vars <- c(spm_biomass_vars(object))
            biomass_density_vars <- c(spm_density_vars(object))

            for (var in names(preds)){
              if (var %in% biomass_vars){
                preds[[var]] <- set_biomass(preds[[var]])
              } else if (var %in% biomass_density_vars){
                preds[[var]] <- set_biomass_density(preds[[var]])
              }
            }

            return(preds)
          }
)
