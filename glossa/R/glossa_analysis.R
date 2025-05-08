#' Main Analysis Function for GLOSSA Package
#'
#' This function wraps all the analysis that the GLOSSA package performs. It processes presence-absence data,
#' environmental covariates, and performs species distribution modeling and projections under past and future scenarios.
#'
#' @param pa_data A list of data frames containing presence-absence data.
#' @param fit_layers A SpatRaster stack containing model fitting environmental layers.
#' @param proj_files A list of file paths containing environmental layers for projection scenarios.
#' @param study_area_poly A spatial polygon defining the study area.
#' @param predictor_variables A list of predictor variables to be used in the analysis.
#' @param decimal_digits An integer specifying the number of decimal places to which coordinates should be rounded.
#' @param scale_layers Logical; if TRUE, covariate layers will be scaled based on fit layers.
#' @param buffer Buffer value or distance in decimal degrees (arc_degrees).
#' @param native_range A vector of scenarios ('fit_layers', 'projections') where native range modeling should be performed.
#' @param suitable_habitat A vector of scenarios ('fit_layers', 'projections') where habitat suitability modeling should be performed.
#' @param other_analysis A vector of additional analyses to perform (e.g., 'variable_importance', 'functional_responses', 'cross_validation').
#' @param seed Optional; an integer seed for reproducibility of results.
#' @param waiter Optional; a waiter instance to update progress in a Shiny application.
#'
#' @return A list containing structured outputs from each major section of the analysis, including model data, projections,
#' variable importance scores, and habitat suitability assessments.
#'
#' @export
glossa_analysis <- function(
    pa_data = NULL, fit_layers = NULL, proj_files = NULL,
    study_area_poly = NULL, predictor_variables = NULL,
    decimal_digits = NULL, scale_layers = FALSE, buffer = NULL,
    native_range = NULL, suitable_habitat = NULL, other_analysis = NULL,
    seed = NA, waiter = NULL) {

  start_time <- Sys.time()
  message(paste("Start time:", start_time))

  predict.bart <- utils::getFromNamespace("predict.bart", "dbarts")

  #=========================================================#
  # 0. Check inputs and load necessary data ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Initializing objects")))}
  message("Initializing objects...")

  # Set seed
  if (is.na(seed)){
    seed <- NULL
  }

  # Check format of study area mask
  sf::sf_use_s2(FALSE)
  if (!is.null(study_area_poly)){
    stopifnot(inherits(study_area_poly, "sf") || inherits(study_area_poly, "sfc"))
    non_study_area_poly <- invert_polygon(study_area_poly)
  }

  # Initialize empty output
  presence_absence_list <- list(raw_pa = NULL, clean_pa = NULL, model_pa = NULL)
  covariate_list <- list(fit_layers = NULL, projections = NULL)
  projections_results <- list(fit_layers = NULL, projections = NULL)
  other_results <- list(variable_importance = NULL, response_curve = NULL, cross_validation = NULL, model_diagnostic = NULL)
  pa_cutoff <- list(native_range = NULL, suitable_habitat = NULL)
  habitat_suitability <- list(fit_layers = NULL, projections = NULL)

  #=========================================================#
  # 1. Load presence(/absence) data and environmental layers ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Loading input data")))}
  message("Loading input data...")

  # * Load presence(/absence) data ----
  presence_absence_list$raw_pa <- pa_data
  sp_names <- names(presence_absence_list$raw_pa)
  long_lat_cols <- colnames(presence_absence_list$raw_pa[[1]])[c(1,2)]

  # * Load covariate layers ----
  # Fit layers
  covariate_list$fit_layers <- read_layers_zip(
    fit_layers,
    extend = ifelse(is.null(study_area_poly), FALSE, TRUE)
  )
  cov_names <- names(covariate_list$fit_layers[[1]])

  # projections layers
  if ("projections" %in% native_range | "projections" %in% suitable_habitat){
    if (length(proj_files) <= 0){
      stop("Error: No projections layers provided.")
    }
    covariate_list$projections <- lapply(proj_files, function(x, extend){
      read_layers_zip(x, extend)},
      extend = ifelse(is.null(study_area_poly), FALSE, TRUE))
    pred_scenario <- names(covariate_list$projections)


    # Check for same layers for fitting and projections
    same_fit_pred_layers <- all(sapply(covariate_list$projections, function(x){
      all(names(x[[1]]) == cov_names)
    }))
    if (!same_fit_pred_layers){
      stop("Error: projection layers differ in the covariate names from fit layers.")
    }
  }

  # * Load extent polygon ----
  # Apply buffer to polygon if requested
  if (!is.null(buffer) & !is.null(study_area_poly)){
    if (buffer != 0) study_area_poly <- buffer_polygon(study_area_poly, buffer)
  }

  # * Select predictor variables ----
  if (is.null(predictor_variables)){
    predictor_variables <- lapply(seq_along(presence_absence_list$raw_pa), function(x) cov_names)
  }
  names(predictor_variables) <- sp_names


  load_data_time <- Sys.time()
  message(paste("Load data execution time:", difftime(load_data_time, start_time, units = "secs"), "secs"))

  #=========================================================#
  # 2. Clean coordinates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Processing P/A coordinates")))}
  message("Processing P/A coordinates...")

  presence_absence_list$clean_pa <- lapply(presence_absence_list$raw_pa, function(x){
    clean_coordinates(
      df = x,
      study_area = study_area_poly,
      overlapping = FALSE,
      decimal_digits = decimal_digits,
      coords = long_lat_cols,
      by_timestamp = TRUE,
      seed = seed
    )
  })

  for (i in names(presence_absence_list$clean_pa)){
    if (nrow(presence_absence_list$clean_pa[[i]]) == 0){
      stop(paste("Error: Check input of species", i, "as after processing it has no remaining points for the analysis."))
    }
  }

  clean_coords_time <- Sys.time()
  message(paste("Clean coordinates execution time:", difftime(clean_coords_time, load_data_time, units = "secs"), "secs"))

  #=========================================================#
  # 3. Covariate layer processing ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Processing covariate layers")))}
  message("Processing covariate layers...")

  # * Process environmental layers for model fitting ----
  if (!is.null(study_area_poly)){
    covariate_list$fit_layers <- lapply(covariate_list$fit_layers, function(x){
      layer_mask(layers = x, study_area = study_area_poly)
    })
  }

  if (scale_layers) {
    # Compute mean and sd of the fit_layers model fitting layers for each environmental variable
    fit_layers_mean <- lapply(cov_names, function(i){
      mean(
        unlist(lapply(covariate_list$fit_layers, function(x){as.vector(x[i])})),
        na.rm = TRUE
      )
    })
    names(fit_layers_mean) <- cov_names
    fit_layers_mean <- unlist(fit_layers_mean)

    fit_layers_sd <- lapply(cov_names, function(i){
      sd(
        unlist(lapply(covariate_list$fit_layers, function(x){as.vector(x[i])})),
        na.rm = TRUE
      )
    })
    names(fit_layers_sd) <- cov_names
    fit_layers_sd <- unlist(fit_layers_sd)

    # Scale fit layers with fit_layers mean and sd
    covariate_list$fit_layers <- lapply(covariate_list$fit_layers, function(x){
      terra::scale(x, center = fit_layers_mean, scale = fit_layers_sd)
    })
  }

  # * Process projections environmental layers ----
  if ("projections" %in% native_range | "projections" %in% suitable_habitat) {
    # Mask polygon if provided
    if (!is.null(study_area_poly)){
      covariate_list$projections <- lapply(covariate_list$projections, function(scenario){
        scenario <- lapply(scenario, function(x){
          layer_mask(layers = x, study_area = study_area_poly)
        })
      })
    }

    # Scale layers with fit_layers mean and sd
    if (scale_layers){
      covariate_list$projections <- lapply(covariate_list$projections, function(scenario){
        scenario <- lapply(scenario, function(x){
          terra::scale(x, center = fit_layers_mean, scale = fit_layers_sd)
        })
      })
    }
  }

  process_layers_time <- Sys.time()
  message(paste("Layer processing execution time:", difftime(process_layers_time, clean_coords_time, units = "secs"), "secs"))

  #=========================================================#
  # 4. Extract environmental variable values and remove presences/absences with NA values in covariates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Building model matrix")))}
  message("Building model matrix...")

  # Remove points with NA values in any environmental variable
  presence_absence_list$model_pa <- lapply(seq_along(presence_absence_list$clean_pa), function(i){
    x <- presence_absence_list$clean_pa[[i]][, c(long_lat_cols, "timestamp", "pa")]
    fit_points <- extract_noNA_cov_values(x, covariate_list$fit_layers, predictor_variables[[i]])
    return(fit_points)
  })
  names(presence_absence_list$model_pa) <- names(presence_absence_list$clean_pa)

  #=========================================================#
  # 5. Randomly generate balanced pseudoabsences ----
  #=========================================================#

  # If only occurrences were provided generate balanced random pseudoabsences
  set.seed(seed)
  presence_absence_list$model_pa <- lapply(seq_along(presence_absence_list$model_pa), function(i) {
    x <- presence_absence_list$model_pa[[i]]
    if (all(x[, "pa"] == 1)){
      x <- generate_pseudo_absences(x, study_area_poly, covariate_list$fit_layers, predictor_variables = predictor_variables[[i]], coords = long_lat_cols, decimal_digits = decimal_digits)
    }
    return(x)
  })
  names(presence_absence_list$model_pa) <- names(presence_absence_list$clean_pa)

  # * Aggregate (mean) timestamps of fitting layers for prediction - compute the mean of the layers
  covariate_list$fit_layers <- lapply(cov_names, function(i){
    single_cov_layers <- lapply(covariate_list$fit_layers, function(x) {x[i]})
    mean_cov_layers <- terra::mean(terra::rast(single_cov_layers), na.rm = TRUE)
  })
  covariate_list$fit_layers  <- terra::rast(covariate_list$fit_layers )
  names(covariate_list$fit_layers) <- cov_names

  model_matrix_time <- Sys.time()
  message(paste("Build model matrix execution time:", difftime(model_matrix_time, process_layers_time, units = "secs"), "secs"))

  #=========================================================#
  # 6. Native range  ----
  #=========================================================#

  # If Native Ranges include longitude and latitude for native ranges modeling
  if (!is.null(native_range)){
    start_nr_time <- Sys.time()

    # Create layer with longitude and latitude values
    coords_layer <- create_coords_layer(covariate_list$fit_layers, study_area_poly, scale_layers = scale_layers)
    names(coords_layer) <- c("grid_long", "grid_lat")

    # Extract values for each observation
    presence_absence_list$model_pa <- lapply(presence_absence_list$model_pa, function(x){
      x <- cbind(x, terra::extract(coords_layer, x[, long_lat_cols]))
      x[,colnames(x) != "ID"]
    })

    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Fitting native range models")))}
    message("Fitting native range models...")

    models_native_range <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        y = presence_absence_list$model_pa[[i]][, "pa"],
        x = presence_absence_list$model_pa[[i]][, c(predictor_variables[[i]], names(coords_layer)), drop = FALSE],
        seed = seed
      )
    })
    names(models_native_range) <- names(presence_absence_list$model_pa)

    fit_nr_time <- Sys.time()
    message(paste("Fit native range model execution time:", difftime(fit_nr_time, start_nr_time, units = "mins"), "mins"))

    # * Optimal cutoff ----
    pa_cutoff$native_range <- lapply(names(models_native_range), function(sp) {
      pa_optimal_cutoff(
        y = presence_absence_list$model_pa[[sp]][, "pa"],
        x = presence_absence_list$model_pa[[sp]][, c(predictor_variables[[sp]], names(coords_layer)), drop = FALSE],
        models_native_range[[sp]]
      )
    })
    names(pa_cutoff$native_range) <- names(models_native_range)

    pa_cutoff_nr_time <- Sys.time()
    message(paste("P/A cutoff execution time:", difftime(pa_cutoff_nr_time, fit_nr_time, units = "mins"), "mins"))


    # * Variable importance ----
    if ("variable_importance" %in% other_analysis){
      other_results$variable_importance$native_range <- lapply(names(models_native_range), function(sp){
        variable_importance(
          models_native_range[[sp]],
          cutoff = pa_cutoff$native_range[[sp]],
          seed = seed
        )
      })
      names(other_results$variable_importance$native_range) <- names(models_native_range)

      var_imp_nr_time <- Sys.time()
      message(paste("Variable importance execution time:", difftime(var_imp_nr_time, pa_cutoff_nr_time, units = "mins"), "mins"))

    } else {
      var_imp_nr_time <- Sys.time()
    }

    # * fit_layers projections ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting native range for fit layer")))}
    projections_results$fit_layers$native_range <- lapply(names(models_native_range), function(sp) {
      predict_bart(
        models_native_range[[sp]],
        c(covariate_list$fit_layers[[predictor_variables[[sp]]]], coords_layer),
        pa_cutoff$native_range[[sp]]
      )
    })
    names(projections_results$fit_layers$native_range) <- names(models_native_range)


    hist_nr_time <- Sys.time()
    message(paste("projections on fit layers execution time:", difftime(hist_nr_time, pa_cutoff_nr_time, units = "mins"), "mins"))

    # * Spatial projections to new scenarios/times ----
    if ("projections" %in% native_range){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting other secenarios native range")))}
      projections_results$projections$native_range <- lapply(names(models_native_range), function(sp) {
        projections <- lapply(covariate_list$projections, function(scenario){
          projections_scenario <- lapply(scenario, function(pred_layers){
            #  Covariates by year
            pred_layers <- c(pred_layers[[predictor_variables[[sp]]]], coords_layer)

            predict_bart(models_native_range[[sp]], pred_layers, pa_cutoff$native_range[[sp]])
          })
          return(projections_scenario)
        })
      })
      names(projections_results$projections$native_range) <- names(models_native_range)
    }

    pred_nr_time <- Sys.time()
    message(paste("Native range projections execution time:", difftime(pred_nr_time, hist_nr_time, units = "mins"), "mins"))

    end_nr_time <- Sys.time()
    message(paste("Native range execution time:", difftime(end_nr_time, start_nr_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 7. Suitable habitat  ----
  #=========================================================#

  if (!is.null(suitable_habitat)){
    start_sh_time <- Sys.time()

    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Fitting suitable habitat models")))}
    message("Fitting suitable habitat models...")

    models_suitable_habitat <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        y = presence_absence_list$model_pa[[i]][, "pa"],
        x = presence_absence_list$model_pa[[i]][, predictor_variables[[i]], drop = FALSE],
        seed = seed
      )
    })
    names(models_suitable_habitat) <- names(presence_absence_list$model_pa)

    fit_sh_time <- Sys.time()
    message(paste("Fit suitable habitat model execution time:", difftime(fit_sh_time, start_sh_time, units = "mins"), "mins"))

    # * Optimal cutoff ----
    pa_cutoff$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
      pa_optimal_cutoff(
        y = presence_absence_list$model_pa[[sp]][, "pa"],
        x = presence_absence_list$model_pa[[sp]][, predictor_variables[[sp]], drop = FALSE],
        models_suitable_habitat[[sp]]
      )
    })
    names(pa_cutoff$suitable_habitat) <- names(models_suitable_habitat)

    pa_cutoff_sh_time <- Sys.time()
    message(paste("P/A cutoff execution time:", difftime(pa_cutoff_sh_time, fit_sh_time, units = "mins"), "mins"))

    # * Variable importance ----
    if ("variable_importance" %in% other_analysis){
      other_results$variable_importance$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        variable_importance(
          models_suitable_habitat[[sp]],
          cutoff = pa_cutoff$suitable_habitat[[sp]],
          seed = seed
        )
      })
      names(other_results$variable_importance$suitable_habitat) <- names(models_suitable_habitat)

      var_imp_sh_time <- Sys.time()
      message(paste("Variable importance execution time:", difftime(var_imp_sh_time, pa_cutoff_sh_time, units = "mins"), "mins"))

    } else {
      var_imp_sh_time <- Sys.time()
    }

    # * fit_layers projections ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting suitable habitat for fit layers")))}
    projections_results$fit_layers$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
      predict_bart(
        models_suitable_habitat[[sp]],
        covariate_list$fit_layers[[predictor_variables[[sp]]]],
        pa_cutoff$suitable_habitat[[sp]]
      )
    })
    names(projections_results$fit_layers$suitable_habitat) <- names(models_suitable_habitat)

    hist_sh_time <- Sys.time()
    message(paste("projections on fit layers execution time:", difftime(hist_sh_time, pa_cutoff_sh_time, units = "mins"), "mins"))

    # * Spatial projections to new scenarios/times ----
    if ("projections" %in% suitable_habitat){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting other scenarios suitable habitat")))}
      projections_results$projections$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        projections <- lapply(covariate_list$projections, function(scenario){
          projections_scenario <- lapply(scenario, function(pred_layers){
            pred_layers <- pred_layers[[predictor_variables[[sp]]]]
            predict_bart(models_suitable_habitat[[sp]], pred_layers, pa_cutoff$suitable_habitat[[sp]])
          })
          return(projections_scenario)
        })
      })
      names(projections_results$projections$suitable_habitat) <- names(models_suitable_habitat)
    }

    pred_sh_time <- Sys.time()
    message(paste("Suitable habitat projections execution time:", difftime(pred_sh_time, hist_sh_time, units = "mins"), "mins"))

    # * Habitat suitability change ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Computing habitat suitability change")))}
    # Covered area
    habitat_suitability$fit_layers$covered_area <- lapply(names(models_suitable_habitat), function(sp){
      layer <- projections_results$fit_layers$suitable_habitat[[sp]]["mean"]
      area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
      area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
      return(area)
    })
    names(habitat_suitability$fit_layers$covered_area) <- names(models_suitable_habitat)

    # Mean suitability probability
    habitat_suitability$fit_layers$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
      layer <- projections_results$fit_layers$suitable_habitat[[sp]]
      prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
      return(as.numeric(prob))
    })
    names(habitat_suitability$fit_layers$suit_prob) <- names(models_suitable_habitat)

    if ("projections" %in% suitable_habitat) {
      # Covered area
      habitat_suitability$projections$covered_area <- lapply(names(models_suitable_habitat), function(sp){
        covered_area_scenarios <- lapply(projections_results$projections$suitable_habitat[[sp]], function(scenario) {
          covered_area <- sapply(scenario, function(layer) {
            layer <- layer["mean"]
            area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
            area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
            return(area)
          })
        })
      })
      names(habitat_suitability$projections$covered_area) <- names(models_suitable_habitat)

      # Mean suitability probability
      habitat_suitability$projections$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
        suit_prob_scenarios <- lapply(projections_results$projections$suitable_habitat[[sp]], function(scenario) {
          suit_prob <- sapply(scenario, function(layer) {
            prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
            return(as.numeric(prob))
          })
        })
      })
      names(habitat_suitability$projections$suit_prob) <- names(models_suitable_habitat)
    }

    hab_sh_time <- Sys.time()
    message(paste("Habitat suitability execution time:", difftime(hab_sh_time, pred_sh_time, units = "mins"), "mins"))

    end_sh_time <- Sys.time()
    message(paste("Suitable habitat execution time:", difftime(end_sh_time, start_sh_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 8. Functional responses ----
  #=========================================================#

  if ("functional_responses" %in% other_analysis){
    start_fr_time <- Sys.time()
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Computing functional responses")))}
    message("Computing functional responses...")

    other_results$response_curve <- lapply(names(presence_absence_list$model_pa), function(i){
      if (scale_layers | is.null(suitable_habitat)) {
        # Variable values in original scale (inverse of z-score value)
        x_original_scale <- lapply(predictor_variables[[i]], function(j){
          fit_layers_mean[j] + (presence_absence_list$model_pa[[i]][, j] * fit_layers_sd[j])
        })
        x_original_scale <- as.data.frame(do.call(cbind, x_original_scale))
        colnames(x_original_scale) <- predictor_variables[[i]]

        # Fit new model with variables without scaling
        bart_model <- fit_bart_model(
          y = presence_absence_list$model_pa[[i]][, "pa"],
          x = x_original_scale,
          seed = seed
        )
      } else {
        bart_model <- models_suitable_habitat[[i]]
        x_original_scale <- presence_absence_list$model_pa[[i]][, predictor_variables[[i]], drop = FALSE]
      }

      fr <- response_curve_bart(bart_model = bart_model,
                                data = x_original_scale,
                                predictor_names = predictor_variables[[i]])
      names(fr) <- predictor_variables[[i]]
      return(fr)
    })
    names(other_results$response_curve) <- names(presence_absence_list$model_pa)

    end_fr_time <- Sys.time()
    message(paste("Functional responses execution time:", difftime(end_fr_time, start_fr_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 9. Cross-validation ----
  #=========================================================#

  if ("cross_validation" %in% other_analysis){
    start_cv_time <- Sys.time()
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Performing cross-validation")))}
    message("Performing cross-validation...")

    if (!is.null(native_range)){
      other_results$cross_validation$native_range <- lapply(names(presence_absence_list$model_pa), function(i){
        cv_bart(data = presence_absence_list$model_pa[[i]][, c("pa", predictor_variables[[i]], names(coords_layer))],
                k = 10,
                seed = seed)
      })
      names(other_results$cross_validation$native_range) <- names(presence_absence_list$model_pa)
    }

    if (!is.null(suitable_habitat)){
      other_results$cross_validation$suitable_habitat <- lapply(names(presence_absence_list$model_pa), function(i){
        cv_bart(data = presence_absence_list$model_pa[[i]][, c("pa", predictor_variables[[i]])],
                k = 10,
                seed = seed)
      })
      names(other_results$cross_validation$suitable_habitat) <- names(presence_absence_list$model_pa)
    }

    end_cv_time <- Sys.time()
    message(paste("Cross-validation execution time:", difftime(end_cv_time, start_cv_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 10. Model summary ----
  #=========================================================#
  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Model diagnostic")))}
  start_diag_time <- Sys.time()

  if (!is.null(native_range)){
    other_results$model_diagnostic$native_range <- lapply(names(models_native_range), function(sp){
      model <- models_native_range[[sp]]
      y <- model$fit$data@y
      x <- as.data.frame(model$fit$data@x)
      df <- data.frame(
        observed = y,
        probability = colMeans(predict.bart(model, x))
      )
      temp_cutoff <- pa_cutoff$native_range[[sp]]
      df$predicted <- ifelse(df$probability >= temp_cutoff, 1, 0)
      return(df)
    })
    names(other_results$model_diagnostic$native_range) <- names(models_native_range)
  }

  if (!is.null(suitable_habitat)){
    other_results$model_diagnostic$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp){
      model <- models_suitable_habitat[[sp]]
      y <- model$fit$data@y
      x <- as.data.frame(model$fit$data@x)
      df <- data.frame(
        observed = y,
        probability = colMeans(predict.bart(model, x))
      )
      temp_cutoff <- pa_cutoff$suitable_habitat[[sp]]
      df$predicted <- ifelse(df$probability >= temp_cutoff, 1, 0)
      return(df)
    })
    names(other_results$model_diagnostic$suitable_habitat) <- names(models_suitable_habitat)
  }

  end_diag_time <- Sys.time()
  message(paste("Model summary execution time:", difftime(end_diag_time, start_diag_time, units = "mins"), "mins"))

  #=========================================================#
  # 11. Finalizing -----
  #=========================================================#
  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Finalizing")))}

  end_time <- Sys.time()
  message(paste("GLOSSA analysis execution time:", difftime(end_time, start_time, units = "mins"), "mins"))

  # Return results to Shiny server
  return(list(
    presence_absence_list = presence_absence_list,
    covariate_list = covariate_list,
    projections_results = projections_results,
    other_results = other_results,
    pa_cutoff = pa_cutoff,
    habitat_suitability = habitat_suitability
  ))
}
