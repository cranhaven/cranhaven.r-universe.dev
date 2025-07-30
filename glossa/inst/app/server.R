function(input, output, session) {
  #=========================================================#
  # Initialization ----
  #=========================================================#

  # * Reactive values ----
  # Inputs
  pa_data <- reactiveVal()
  pa_validation_table <- reactiveVal()
  fit_layers_path <- reactiveVal()
  fit_layers_previs <- reactiveVal()
  fit_layers_validation_table <- reactiveVal()
  proj_layers_path <- reactiveVal()
  proj_validation_table <- reactiveVal()
  study_area_poly <- reactiveVal()
  study_area_poly_buff <- reactiveVal()
  extent_validation_table <- reactiveVal()
  raster_timestamp <- reactiveVal()
  raster_timestamp_validation_table <- reactiveVal()
  target_group_points <- reactiveVal()

  # Analysis results
  presence_absence_list <- reactiveVal()
  covariate_list <- reactiveVal()
  projections_results <- reactiveVal()
  other_results <- reactiveVal()
  pa_cutoff <- reactiveVal()
  habitat_suitability <- reactiveVal()
  config <- reactiveVal()

  #=========================================================#
  # Reactive expressions ----
  #=========================================================#

  # * File inputs ----
  pa_files_input <- glossa::file_input_area_server("pa_files")
  fit_layers_input <- glossa::file_input_area_server("fit_layers")
  proj_layers_input <- glossa::file_input_area_server("proj_layers")
  study_area_poly_input <- glossa::file_input_area_server("study_area_poly")

  # Get long lat coordinate colnames
  long_lat_cols <- reactive({
    req(pa_data())
    colnames(pa_data()[[1]])[c(1,2)]
  })

  # Invert study area polygon for plotting
  inv_study_area_poly <- reactive({
    if(is.null(study_area_poly()) | is.null(extent_validation_table())){
      NULL
    } else {
      if(extent_validation_table()[, "validation"] == FALSE){
        NULL
      } else {
        glossa::invert_polygon(study_area_poly())
      }
    }
  })

  # Get file names of species occurrences for picker inputs
  species_files_names <- reactive({
    data <- pa_files_input()
    if (!is.null(data)) {
      data$name
    } else{
      NULL
    }
  })

  # name of uploaded predictor variables (name of files)
  predictor_variables <- reactive({
    req(fit_layers_previs())
    return(names(fit_layers_previs()))
  })

  #=========================================================#
  # Observers ----
  #=========================================================#

  # * Header server ----
  observeEvent(input$new_analysis_header, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })

  observeEvent(input$new_analysis_home, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })

  # show help modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Welcome to the GLOSSA App!",
      tags$div(
        tags$ol(
          tags$li(tags$strong("Get started: "), "If you're new, start by reading our quickstart guide to see how GLOSSA works. Click the 'Get started' button to beggin."),
          tags$li(tags$strong("New analysis: "), "Start by uploading your species occurrences and environmental layers. Then, tune the analysis options to customize your model."),
          tags$li(tags$strong("Reports: "), "After running the analysis, view the results in the Reports tab. Here, you can explore predictions of species suitable habitat and other insights."),
          tags$li(tags$strong("Export: "), "Once you're satisfied with the results, head over to the Export tab to save your findings."),
          tags$li(tags$strong("Documentation: "), "Access detailed documentation and user guides in the Documentation tab."),
          tags$li(tags$strong("How to cite: "), "Find information on how to cite GLOSSA in your publications in the How to cite tab."),
        ),
        tags$p("Need more help? Feel free to reach out to us directly via the Contact tab."),
        tags$p("Happy modeling!")
      ),
      easyClose = TRUE
    ))
  })

  # * New Analysis server ----
  # Open advanced options sidebar
  observeEvent(input$toggle_advanced_options, {
    updateControlbar(id = "advanced_options_sidebar", session = session)
  })

  # leaflet previsualization plots selectizers
  observe({
    req(pa_data())
    updatePickerInput(session, "previsualization_plot_species", choices = c(names(pa_data()[!sapply(pa_data(), is.null)]), "None"))
  })

  observe({
    req(fit_layers_previs())
    updatePickerInput(session, "previsualization_plot_layer", choices = c(names(fit_layers_previs()), "None"))
  })

  # * Validate inputs ----
  load_pa_data <- function() {
    data <- apply(pa_files_input(), 1, function(x){
      glossa::read_presences_absences_csv(x["datapath"], x["name"], show_modal = TRUE, timestamp_mapping = raster_timestamp())
    })
    if (is.null(data)){
      data <- list(data)
    }
    names(data) <- sub(" ", "_", sub("([^.]+)\\.[[:alnum:]]+$", "\\1", pa_files_input()[, "name"]))
    return(data)
  }


  observeEvent(pa_files_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(pa_files_input())){
      pa_data(NULL)
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read and validate files
      pa_data(load_pa_data())
      w$hide()
    }
  })

  observeEvent(fit_layers_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(fit_layers_input())){
      fit_layers_path(NULL)
      fit_layers_previs(NULL)
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Validate layers
      is_valid <- glossa::validate_layers_zip(fit_layers_input()[, "datapath"], timestamp_mapping = raster_timestamp(), show_modal = TRUE)

      if (is_valid == TRUE){
        # Save path
        fit_layers_path(fit_layers_input()[, "datapath"])

        # Read last layer for previsualization
        fit_layers_previs(glossa::read_layers_zip(fit_layers_input()[, "datapath"], first_layer = TRUE)[[1]])
      } else {
        fit_layers_path(NULL)
      }

      w$hide()
    }
  })

  observeEvent(proj_layers_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(proj_layers_input())){
      proj_layers_path(NULL)
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Validate layers and safe file path
      layers <- apply(proj_layers_input(), 1, function(x){
        if (glossa::validate_layers_zip(x["datapath"], show_modal = TRUE)){
          return(x["datapath"])
        } else {
          return(NULL)
        }
      })
      if (is.null(layers)){
        layers <- list(layers)
      }
      names(layers) <- sub("\\.zip$", "", proj_layers_input()[,"name"])

      proj_layers_path(layers)
      w$hide()
    }
  })

  observeEvent(study_area_poly_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(study_area_poly_input())){
      study_area_poly(NULL)
      study_area_poly_buff(NULL)
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read files
      study_area_poly(glossa::read_extent_polygon(study_area_poly_input()["datapath"], show_modal = TRUE))
      study_area_poly_buff(study_area_poly())
      w$hide()
    }
  })

  observeEvent(input$raster_timestamp_file, {
    # Check is not null and it was possible to upload the data
    if (is.null(input$raster_timestamp_file)){
      raster_timestamp(NULL)
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read file
      raster_timestamp_vector <- tryCatch({
        read.table(input$raster_timestamp_file$datapath, header = FALSE, stringsAsFactors = FALSE)[, 1]
      }, error = function(e) {
        showNotification("Error reading raster timestamp file. Please check the file format.", type = "error")
        return(NULL)
      })
      raster_timestamp(raster_timestamp_vector)

      if (!is.null(pa_files_input())) {
        # Read and validate files
        pa_data(load_pa_data())
      }

      if (!is.null(fit_layers_input())) {
        # Validate layers
        is_valid <- glossa::validate_layers_zip(fit_layers_input()[, "datapath"], timestamp_mapping = raster_timestamp(), show_modal = TRUE)

        if (is_valid == TRUE){
          fit_layers_path(fit_layers_input()[, "datapath"])
        } else {
          fit_layers_path(NULL)
        }

      }
      w$hide()
    }
  })

  observeEvent(input$target_group_occ, {
    if (is.null(input$target_group_occ)) {
      target_group_points(NULL)
    } else {
      w <- waiter::Waiter$new(
        id = "target_group_occ",
        html = tagList(
          img(src = "logo_glossa.gif", height = "200px"),
          h4("Loading target-group data...")
        ),
        color = waiter::transparent(0.8)
      )
      w$show()

      tryCatch({
        target_group_points(glossa::read_presences_absences_csv(
          input$target_group_occ$datapath,
          input$target_group_occ$name,
          show_modal = TRUE,
          timestamp_mapping = raster_timestamp()
          ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Target Group Upload Error",
          paste("An error occurred while reading the file:", e$message),
          easyClose = TRUE
        ))
        target_group_points(NULL)
      })

      w$hide()
    }
  })

  observeEvent(c(pa_data(), fit_layers_path()), {
    if (is.null(pa_files_input()) | is.null(pa_data())){
      pa_validation_table(NULL)
    } else {
      # Check which ones where properly loaded
      validation_table <- as.data.frame(pa_files_input())

      if(!is.null(fit_layers_path())){
        validation_table[, "validation"] <- !sapply(pa_data(), is.null) & sapply(pa_data(), function(x){glossa::validate_pa_fit_time(x, fit_layers_path(), show_modal = TRUE)})
      } else {
        validation_table[, "validation"] <- !sapply(pa_data(), is.null)
      }

      validation_table[, "name"] <- paste(as.character(icon("map-location-dot",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      pa_validation_table(validation_table)
    }
  })

  observeEvent(fit_layers_path(), {
    if (is.null(fit_layers_input())){
      fit_layers_validation_table(NULL)
    } else {
      validation_table <- as.data.frame(fit_layers_input())
      validation_table[, "name"] <- paste(as.character(icon("layer-group", style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      if (is.null(fit_layers_path())){
        validation_table[, "validation"] <- FALSE
      } else {
        validation_table[, "validation"] <- !is.null(fit_layers_path())
      }
      fit_layers_validation_table(validation_table)
    }
  })

  observeEvent(c(proj_layers_path(),  fit_layers_path()), {
    if (is.null(proj_layers_input()) | is.null(proj_layers_path())){
      proj_validation_table(NULL)
    } else {
      # Check which ones where properly loaded
      validation_table <- as.data.frame(proj_layers_input())
      validation_table[, "name"] <- paste(as.character(icon("forward",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])

      if(!is.null(fit_layers_input())){
        validation_table[, "validation"] <- !sapply(proj_layers_path(), is.null) & sapply(proj_layers_path(), function(x){glossa::validate_fit_projection_layers(fit_layers_path(), x, show_modal = TRUE)})
      } else {
        validation_table[, "validation"] <- !sapply(proj_layers_path(), is.null)
      }
      proj_validation_table(validation_table)
    }
  })

  observeEvent(study_area_poly(), {
    if (is.null(study_area_poly_input()) | is.null(study_area_poly())){
      extent_validation_table(NULL)
    } else {
      validation_table <- as.data.frame(study_area_poly_input())
      validation_table[, "name"] <- paste(as.character(icon("crop",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      if (is.null(study_area_poly())){
        validation_table[, "validation"] <- FALSE
      } else {
        validation_table[, "validation"] <- !is.null(study_area_poly())
      }
      extent_validation_table(validation_table)
    }
  })

  observeEvent(raster_timestamp(), {
    if (is.null(raster_timestamp())){
      raster_timestamp_validation_table(NULL)
    } else {
      validation_table <- as.data.frame(input$raster_timestamp_file[, c("name", "size", "type", "datapath")])
      validation_table[, "date"] <- format(Sys.time())
      validation_table[, "name"] <- paste(as.character(icon("calendar-days",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      if (is.null(raster_timestamp())){
        validation_table[, "validation"] <- FALSE
      } else {
        validation_table[, "validation"] <- !is.null(raster_timestamp())
      }
      raster_timestamp_validation_table(validation_table)
    }
  })

  # * Polygon preprocessing ----
  observeEvent(input$preview_buff_poly, {
    req(input$buff_poly)
    req(study_area_poly())

    # Create waiter
    w <- waiter::Waiter$new(id = "data_upload",
                            html = tagList(
                              img(src = "logo_glossa.gif", height = "200px")
                            ),
                            color = waiter::transparent(0.8)
    )
    w$show()

    study_area_poly_buff(buffer_polygon(study_area_poly(), input$buff_poly))

    # Update previsualization plot
    updatePrettySwitch(inputId = "previsualization_plot_extent", value = FALSE)
    updatePrettySwitch(inputId = "previsualization_plot_extent", value = TRUE)

    # Hide water
    w$hide()
  })

  # * Info buttons ----
  observe({
    bs4Dash::addPopover(
      id = "data_upload_info",
      options = list(
        content = "1) Occurrences: Upload a TSV file containing latitude and longitude columns for species occurrences, with an optional column 'pa' indicating presence (1) or absence (0), and 'timestamp' with an index of the time period.
        2) Environmental data: Upload a ZIP file containing raster layers of environmental variables.
        3) Projection layers: Upload a ZIP file containing layers for projecting species distribution. Multiple ZIP files can be uploaded.
        4) Study area: Upload a polygon defining the study area if you want to delimit the extent (GPKG, KML or GeoJSON).",
        title = "Data upload",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "raster_timestamp_info",
      options = list(
        content = "Upload a simple text file with one column listing the actual timestamps that correspond to your ordered raster layers This ensures correct alignment between your occurrence timestamps and the raster stack.",
        title = "Raster timestamp mapping (optional)",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_options_info",
      options = list(
        content = "1) Model fitting: Fit the model and perform spatial prediction in the model fitting layers.
        2) Model projection: Predict in new layers (projection layers).",
        title = "Model options",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_nr_info",
      options = list(
        content = "Model incorporating environmental data and spatial smoothing, with latitude and longitude as covariates.",
        title = "Native range",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_sh_info",
      options = list(
        content = "Model estimating spatial probability of niche based solely on environmental variables.",
        title = "Suitable habitat",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_others_info",
      options = list(
        content = "Computing these options may take some time.
        1) Functional responses: Estimate the response curve of the probability for each value of the environmental variable using partial dependence plots.
        2) Variable importance: Computes the variable importance using permutation method.
        3) Cross-validation: Perform k-fold, spatial block, and temporal block cross-validation.",
        title = "Others",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "predictor_variables_info",
      options = list(
        content = "In this section, you can select which variables to include in the model for each species.",
        title = "Predictor variables",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "bart_k_info",
      options = list(
        content = "Controls the amount of shrinkage in the node parameters, allowing you to regulate the degree of overfitting. Enter a positive value for k representing the number of standard deviations; higher values produce more conservative fits. Leave unset to use the default prior, chi(1.25, Inf), which works well in many cases. See the GLOSSA documentation or the dbarts manual for further details.",
        title = "End-node shrinkage prior k",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  # * Reset button ----
  observeEvent(input$reset_input, {
    # Reset data upload
    pa_data(NULL)
    pa_validation_table(NULL)
    fit_layers_path(NULL)
    fit_layers_previs(NULL)
    fit_layers_validation_table(NULL)
    proj_layers_path(NULL)
    proj_validation_table(NULL)
    study_area_poly(NULL)
    study_area_poly_buff(NULL)
    extent_validation_table(NULL)
    raster_timestamp_validation_table(NULL)

    # Reset analysis options
    updatePrettyCheckboxGroup(inputId = "analysis_options_nr", selected = character(0))
    updatePrettyCheckboxGroup(inputId = "analysis_options_sh", selected = character(0))
    updatePrettyCheckboxGroup(inputId = "analysis_options_other", selected = character(0))
    updatePrettySwitch(inputId = "scale_layers", value = FALSE)
    updatePickerInput(inputId = "thinning_method", selected = "None")
    updateNumericInput(inputId = "buff_poly", value = numeric(0))
    updateNumericInput(inputId = "seed", value = numeric(0))

    # Reset preview plot
    previsualization_plot %>%
      leaflet::clearMarkers() %>%
      leaflet::clearImages() %>%
      leaflet::clearShapes()
  })

  # * Run GLOSSA analysis ----
  # Confirmation dialog
  observeEvent(input$run_button, {
    shinyWidgets::ask_confirmation(
      inputId = "run_button_confirmation",
      type = "question",
      title = "Want to confirm?",
      text = "GLOSSA analysis may require some time. Please double-check all inputs before proceeding.",
      btn_labels = c("Cancel", "Confirm"),
    )
  })

  observeEvent(input$run_button_confirmation, {

    req(input$run_button_confirmation == TRUE)

    # Validate input
    # Messages
    if (is.null(pa_files_input())) {
      showNotification("Please upload a P/A file", type = "error")
    }
    if (!all(pa_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid P/A files", type = "error")
    }

    if (is.null(fit_layers_input())) {
      showNotification("Please upload model fit layers", type = "error")
    }
    if (!all(fit_layers_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid model fit layers", type = "error")
    }

    if ("projections" %in% input$analysis_options_nr | "projections" %in% input$analysis_options_sh){
      if (is.null(proj_layers_input())) {
        showNotification("Please upload projection layers", type = "error")
      }
      if (!all(proj_validation_table()[, "validation"] == TRUE)) {
        showNotification("Please upload valid projection layers", type = "error")
      }
    }

    if (is.null(c(input$analysis_options_nr, input$analysis_options_sh))){
      showNotification("Select at least one option to compute from Native range and/or Suitable habitat", type = "error")
    }

    # Req
    req(pa_files_input(), all(pa_validation_table()[, "validation"] == TRUE))
    req(fit_layers_input(), all(fit_layers_validation_table()[, "validation"] == TRUE))
    if ("projections" %in% input$analysis_options_nr | "projections" %in% input$analysis_options_sh) {
      req(proj_layers_input(), all(proj_validation_table()[, "validation"] == TRUE))
    }
    req(c(input$analysis_options_nr, input$analysis_options_sh))

    # Create waiter
    w <- waiter::Waiter$new(
      html = tagList(
        img(src = "logo_glossa.gif", height = "200px"),
        h4("")
      )
    )
    w$show()

    # Get predictor variables for each sp
    predictor_variables <- lapply(seq_len(length(species_files_names())), function(i){
      input[[paste0("pred_vars_", i)]]
    })

    # Model tuning
    model_args <- list(ntree = input$bart_ntrees, k = as.numeric(input$bart_k))
    model_args <- model_args[!sapply(model_args, function(x) is.null(x) || length(x) == 0 || is.na(x))]

    # Pseudo-absence settings
    pseudoabsence_method <- input$pseudoabsence_method
    pa_ratio <- input$pa_ratio
    pa_buffer_distance <- if (pseudoabsence_method == "buffer_out") input$pa_buffer_distance else NULL
    target_group_points <- if (pseudoabsence_method == "target_group") read_target_group() else NULL

    # Cross-validation settings
    cv_active <- "cross_validation" %in% input$analysis_options_other
    cv_methods <- if (cv_active) {input$cv_methods} else {NULL}
    cv_folds <- if (cv_active) {input$cv_folds} else {NULL}
    cv_block_source <- if (cv_active && "spatial_blocks" %in% input$cv_methods) {input$cv_block_source} else {NULL}
    cv_block_size <- if (!is.null(cv_block_source) && cv_block_source == "manual") {input$cv_block_size} else {NULL}

    # Set up log files
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    config_snapshot <- list(
      timestamp = timestamp,
      seed = input$seed,
      species = names(pa_data()),
      file_inputs = list(
        pa_data = pa_files_input()[["name"]],
        fit_layers = fit_layers_input()$name,
        projections = proj_layers_input()[["name"]],
        study_area_poly = study_area_poly_input()[["name"]],
        raster_timestamp_file = input$raster_timestamp_file[["name"]]
      ),
      preprocessing = list(
        thinning_method = input$thinning_method,
        thinning_value = switch (input$thinning_method,
          "precision" = input$thin_precision,
          "grid" = input$thin_grid_size,
          "distance" = input$thin_distance,
          NULL
        ),
        scale_layers = input$scale_layers,
        buffer = input$buff_poly
      ),
      pseudo_absences = list(
        pseudoabsence_method = pseudoabsence_method,
        pa_ratio = pa_ratio,
        pa_buffer_distance = pa_buffer_distance,
        target_group_points = input$target_group_occ$name
      ),
      modeling = list(
        predictor_variables = predictor_variables,
        native_range = input$analysis_options_nr,
        suitable_habitat = input$analysis_options_sh,
        other_analysis = input$analysis_options_other,
        model_args = model_args
      ),
      cross_validation = list(
        active = "cross_validation" %in% input$analysis_options_other,
        methods = cv_methods,
        folds = cv_folds,
        block_source = cv_block_source,
        block_size = cv_block_size
      )
    )

    # Run GLOSSA analysis
    glossa_results <- tryCatch({
      glossa::glossa_analysis(
        pa_data = pa_data(),
        fit_layers = fit_layers_path(),
        proj_files = proj_layers_path(),
        study_area_poly = study_area_poly_buff(),
        predictor_variables = predictor_variables,
        thinning_method = input$thinning_method,
        thinning_value = switch (input$thinning_method, "precision" = input$thin_precision, "grid" = input$thin_grid_size, "distance" = input$thin_distance, NULL),
        pseudoabsence_method = pseudoabsence_method,
        pa_ratio = pa_ratio,
        pa_buffer_distance = pa_buffer_distance,
        target_group_points = target_group_points,
        scale_layers = input$scale_layers,
        buffer = switch(is.na(input$buff_poly) + 1, input$buff_poly, NULL),
        native_range = input$analysis_options_nr,
        suitable_habitat = input$analysis_options_sh,
        other_analysis = input$analysis_options_other,
        model_args = model_args,
        cv_methods = cv_methods,
        cv_folds = cv_folds,
        cv_block_source = cv_block_source,
        cv_block_size = cv_block_size,
        seed = input$seed,
        waiter = w
      )
    },
    error = function(e) {
      print(e)
      return(NULL)
    })

    # Save user input settings
    if (!is.null(glossa_results)) {
      glossa_results$config <- config_snapshot
    }

    if (!is.null(glossa_results)){
      # Move to Reports tab
      bs4Dash::updateTabItems(session, "sidebar_menu", "reports")
    }

    # Save to global environment for safety
    if (!isTRUE(getOption("glossa.clear_env_on_exit", FALSE))) {
      assign("glossa_autosave", glossa_results, envir = .GlobalEnv)
      message("Results have been autosaved as 'glossa_autosave' in your R environment.")
    } else {
      message("Results not autosaved to R environment because clear_global_env = TRUE")
    }

    # Hide waiter
    w$hide()

    if (!is.null(glossa_results)){
      showNotification("GLOSSA analysis done!", duration = NULL, closeButton = TRUE, type = "message")
    } else {
      showNotification("An error has occurred during the GLOSSA analysis. Check your files and if it still does not work, please contact us.", duration = NULL, closeButton = TRUE, type = "error")
    }

    presence_absence_list(glossa_results$presence_absence_list)
    covariate_list(glossa_results$covariate_list)
    projections_results(glossa_results$projections_results)
    other_results(glossa_results$other_results)
    pa_cutoff(glossa_results$pa_cutoff)
    habitat_suitability(glossa_results$habitat_suitability)
    config(glossa_results$config)

  })

  # * Reports server ----
  # Sp names report selectizer
  observe({
    req(presence_absence_list())
    updatePickerInput(session, "sp", label = NULL, choices = names(presence_absence_list()$model_pa), choicesOpt = list(icon = rep("fa-solid fa-globe", length(names(presence_absence_list()$model_pa)))))
  })

  # ** Prediction plot selectizers ----
  # Update pred_plot_layers picker
  observe({
    req(projections_results())
    updatePickerInput(session, "pred_plot_layers", choices = names(projections_results()[!unlist(lapply(projections_results(),is.null))]))
  })

  # Update pred_plot_model picker
  observe({
    req(input$pred_plot_layers)
    display_choices <- names(projections_results()[[input$pred_plot_layers]])
    current_selection <- isolate(input$pred_plot_model)
    if (!is.null(current_selection) && current_selection %in% display_choices) {
      selected_model <- current_selection
    } else {
      selected_model <- display_choices[[1]]
    }

    updatePickerInput(session, "pred_plot_model", choices = display_choices, selected = selected_model)
  })

  # Update pred_plot_value picker
  observe({
    req(input$pred_plot_model)

    if (input$pred_plot_layers != "projections") {
      value_choices <- names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]])
    } else {
      req(input$pred_plot_scenario)
      req(input$pred_plot_year)
      req(input$pred_plot_year <= length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
      value_choices <- names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]][[input$pred_plot_year]])
    }

    current_selection <- isolate(input$pred_plot_value)
    if (!is.null(current_selection) && current_selection %in% value_choices) {
      selected_value <- current_selection
    } else {
      selected_value <- value_choices[[1]]
    }

    updatePickerInput(session, "pred_plot_value", choices = value_choices, selected = selected_value)
  })

  # ** Layers plot selectizer ----
  observe({
    req(covariate_list())
    updatePickerInput(session, "layers_plot_mode", choices = names(covariate_list()[!unlist(lapply(covariate_list(),is.null))]))
  })

  observe({
    req(input$layers_plot_mode)
    if (input$layers_plot_mode == "fit_layers") {
      cov_choices <- names(covariate_list()[[input$layers_plot_mode]])
    } else if (input$layers_plot_mode == "projections") {
      req(input$layers_plot_scenario)
      req(input$layers_plot_year)
      req(input$layers_plot_year <= length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
      cov_choices <- names(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]][[input$layers_plot_year]])
    }

    current_selection <- isolate(input$layers_plot_cov)
    if (!is.null(current_selection) && current_selection %in% cov_choices) {
      selected_cov <- current_selection
    } else {
      selected_cov <- cov_choices[[1]]
    }

    updatePickerInput(session, "layers_plot_cov", choices = cov_choices, selected = selected_cov)
  })

  # ** Functional responses plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["response_curve"]])
    req(input$sp)

    #Extract covariate names
    covariate_choices <- names(other_results()[["response_curve"]][[input$sp]])
    # Preserve current selection if still valid
    current_selection <- isolate(input$fr_plot_cov)
    if (!is.null(current_selection) && current_selection %in% covariate_choices) {
       selected_cov <- current_selection
    } else {
      selected_cov <- covariate_choices[[1]]
    }

    #updatePickerInput(session, "fr_plot_cov", choices = names(other_results()[["response_curve"]][[input$sp]]))
    updatePickerInput(
      session, "fr_plot_cov",
      choices = covariate_choices,
      selected = selected_cov
    )
  })

  # ** Variable importance plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["variable_importance"]])
    updatePickerInput(session, "varimp_plot_mode", choices = names(other_results()[["variable_importance"]]))
  })

  # ** Cross-validation plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["cross_validation"]])
    req(input$sp)
    updatePickerInput(session, "cv_plot_model", choices = names(other_results()[["cross_validation"]]))
  })

  # ** Fitted values plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["model_diagnostic"]])
    req(input$sp)
    updatePickerInput(session, "fv_plot_mode", choices = names(other_results()[["model_diagnostic"]]))
  })

  # ** Classified values plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["model_diagnostic"]])
    req(input$sp)
    updatePickerInput(session, "class_val_plot_mode", choices = names(other_results()[["model_diagnostic"]]))
  })

  # ** ROC plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["model_diagnostic"]])
    req(input$sp)
    updatePickerInput(session, "roc_plot_mode", choices = names(other_results()[["model_diagnostic"]]))
  })

  # * Exports server ----
  # Update selectizers
  observe({
    req(presence_absence_list())
    req(projections_results())
    updateSelectInput(session, "export_sp", choices = names(presence_absence_list()$model_pa))
    export_layer_results <- names(projections_results()[!unlist(lapply(projections_results(),is.null))])
    updateSelectInput(session, "export_results", choices = export_layer_results)
    updateSelectInput(session, "export_models", choices = unique(as.vector((unlist((sapply(projections_results()[export_layer_results], names)))))))
    updateSelectInput(session, "export_values", choices = c("mean", "median", "sd", "q0.025", "q0.975", "diff", "potential_presences"))
    updateSelectInput(session, "export_layer_format", choices = c("tif", "asc", "nc"))
  })

  observeEvent(input$export_all, {
    req(presence_absence_list())
    req(projections_results())
    updateSelectInput(session, "export_sp", selected = names(presence_absence_list()$model_pa))
    export_layer_results <- names(projections_results()[!unlist(lapply(projections_results(),is.null))])
    updateSelectInput(session, "export_results", selected = export_layer_results)
    updateSelectInput(session, "export_models", selected = unique(as.vector((unlist((sapply(projections_results()[export_layer_results], names)))))))
    updateSelectInput(session, "export_values", selected = c("mean", "median", "sd", "q0.025", "q0.975", "diff", "potential_presences"))
    shinyWidgets::updatePrettySwitch(inputId = "export_model_data", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_mod_summary", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_var_imp", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_fr", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_cv", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_pa_cutoff", value = TRUE)
  })

  #=========================================================#
  # Outputs ----
  #=========================================================#

  # * Render selectizers ----
  # Select predictor variable for each species in new analysis tab
  output$predictor_selector <- renderUI({
    if (is.null(species_files_names()) | is.null(predictor_variables())) {
      validate("Upload species ocurrences and fit layers")
    }

    lapply(seq_len(length(species_files_names())), function(i){
      selectInput(inputId = paste0("pred_vars_", i), label = species_files_names()[i], choices = predictor_variables(), selected = predictor_variables(), multiple = TRUE)
    })
  })

  # Update pred_plot_scenario picker
  output$pred_plot_scenario_picker <- renderUI({
    req(input$pred_plot_layers == "projections")
    pickerInput("pred_plot_scenario", label = NULL, width = "90%", choices = names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]]))
  })

  # Update pred_plot_year picker
  output$pred_plot_year_slider <- renderUI({
    req(input$pred_plot_layers == "projections")
    req(input$pred_plot_scenario)
    sliderInput(inputId = "pred_plot_year", label = "Timestamp", value = 1, step = 1, round = TRUE, min = 1, max = length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
  })

  # Update layers plot selectizers
  output$layers_plot_scenario_picker <- renderUI({
    req(input$layers_plot_mode == "projections")
    pickerInput("layers_plot_scenario", label = NULL, width = "90%", choices = names(covariate_list()[[input$layers_plot_mode]]))
  })

  output$layers_plot_year_slider <- renderUI({
    req(input$layers_plot_mode == "projections")
    req(input$layers_plot_scenario)
    sliderInput(inputId = "layers_plot_year", label = "Timestamp", round = TRUE, step = 1, width = "90%", value = 1, min = 1, max = length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
  })

  # * Input validation table ----
  # Render uploaded files as a DT table
  output$uploaded_files <- DT::renderDT(
    if (is.null(rbind(pa_validation_table(), fit_layers_validation_table(), proj_validation_table(), extent_validation_table(), raster_timestamp_validation_table()))) {
      DT::datatable(NULL)
    } else {
      rbind(pa_validation_table(), fit_layers_validation_table(), proj_validation_table(), extent_validation_table(), raster_timestamp_validation_table()) %>%
        dplyr::select(name, size, validation) %>%
        dplyr::mutate(validation = ifelse(
          validation,
          as.character(icon("circle-check", class = "fa-solid", style = "font-size:2rem;color:#418B24")),
          as.character(icon("circle-xmark", class = "fa-solid", style = "font-size:2rem;color:#E90C00"))
        )) %>%
        DT::datatable(
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE,
            searching = FALSE
          ),
          selection = "none",
          class = 'row-border',
          escape = FALSE,
          colnames = c("File name", "File size", "Format Validation"),
          rownames = TRUE,
          filter = "none",
          width = 500
        )
    }
  )

  # * Previsualization of input ----
  # base plot
  output$previsualization_plot <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(0, 0, zoom = 1)
  })

  # plot proxy to update leaflet visualization
  previsualization_plot <- leaflet::leafletProxy("previsualization_plot", session)

  # add presence/absence points
  observeEvent(input$previsualization_plot_species, {
    req(pa_data())
    if(input$previsualization_plot_species != "None"){
      previsualization_plot %>%
        leaflet::setView(0, 0, zoom = 1) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(data = pa_data()[[input$previsualization_plot_species]],
                                  lng = pa_data()[[input$previsualization_plot_species]][, long_lat_cols()[1]],
                                  lat = pa_data()[[input$previsualization_plot_species]][, long_lat_cols()[2]],
                                  color = ~ifelse(pa == 1, "green", "black"), radius = 5)
    } else {
      previsualization_plot %>%
        leaflet::clearMarkers()
    }
  })

  # add environmental raster layers
  observeEvent(input$previsualization_plot_layer, {
    req(fit_layers_previs())
    if (input$previsualization_plot_layer != "None"){
      previsualization_plot %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(terra::crop(fit_layers_previs()[input$previsualization_plot_layer], ext(-180, 180, -87, 87)),
                                colors = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"), opacity = 0.5)
    } else {
      previsualization_plot %>%
        leaflet::clearImages()
    }
  })

  # add study area polygon
  observeEvent(input$previsualization_plot_extent, {
    req(study_area_poly_buff())

    if(!is.null(study_area_poly_buff()[[1]]) & input$previsualization_plot_extent){
      previsualization_plot %>%
        leaflet::clearShapes() %>%
        addPolygons(data = study_area_poly_buff(), color = "#353839", fill = TRUE, fillColor = "353839", fillOpacity = 0.25)
    } else{
      previsualization_plot %>%
        leaflet::clearShapes()
    }
  })
  observeEvent(study_area_poly_buff(), {
    req(study_area_poly_buff())
    shinyWidgets::updatePrettySwitch(inputId = "previsualization_plot_extent", value = TRUE)
  })


  # * Sparkline plots box ----
  output$spark_boxes <- renderUI({
    if (is.null(input$sp)){
      sparkline_data1 <- rep(0, 30)
      sparkline_data2 <- rep(0, 30)
      sparkline_data3 <-  c(0, 0)
      description1_2 <- "%"
    } else {
      # Define default values
      sparkline_data1 <- rep(0, 30)
      sparkline_data2 <- rep(0, 30)
      description1_2 <- "%"

      if (input$pred_plot_layers == "projections" & !is.null(habitat_suitability()[[input$pred_plot_layers]]) & !is.null(input$pred_plot_scenario)){
        # Check if future data along with input scenario are available
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_layers]][["covered_area"]][[input$sp]][[input$pred_plot_scenario]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_layers]][["suit_prob"]][[input$sp]][[input$pred_plot_scenario]]
        description1_2 <- paste("%", input$pred_plot_scenario)
      } else if (input$pred_plot_layers != "projections" & !is.null(habitat_suitability()[[input$pred_plot_layers]])) {
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_layers]][["covered_area"]][[input$sp]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_layers]][["suit_prob"]][[input$sp]]
        description1_2 <- paste("%", input$pred_plot_scenario)
      }

      sparkline_data3 <-  c(
        sum(presence_absence_list()[["model_pa"]][[input$sp]][, "pa"] == 1),
        sum(presence_absence_list()[["model_pa"]][[input$sp]][, "pa"] == 0)
      )
    }

    fluidRow(
      glossa::sparkvalueBox(
        title = "Potential suitable area (km2)",
        sparkline_data = sparkline_data1,
        description = description1_2,
        elevation = 2
      ),

      glossa::sparkvalueBox(
        title = "Mean suitable probability",
        sparkline_data = sparkline_data2,
        description = description1_2,
        elevation = 2
      ),

      glossa::sparkvalueBox(
        title = "Presences/Absences",
        sparkline_data = sparkline_data3,
        description = "ratio P/A",
        elevation = 2,
        type = "bar"
      )
    )
  })
  outputOptions(output, "spark_boxes", suspendWhenHidden = FALSE) # To render before showing the tab

  # * Prediction plot ----
  prediction_plot <- reactive({
    prediction_layer <- NULL
    pa_points <- NULL
    legend_label <- NULL

    if (!is.null(input$pred_plot_value)) {
      if (input$pred_plot_layers == "fit_layers") {
        prediction_layer <- projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_value]]
      } else if (input$pred_plot_layers == "projections") {
        req(input$pred_plot_year)
        req(input$pred_plot_year <= length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
        prediction_layer <- projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]][[input$pred_plot_year]][[input$pred_plot_value]]
      }
      legend_label <- input$pred_plot_value
    }

    if (!is.null(input$sp) & input$pa_points) {
      pa_points <- presence_absence_list()$model_pa[[input$sp]]
    }

    glossa::generate_prediction_plot(prediction_layer, pa_points, legend_label, inv_study_area_poly(), coords = long_lat_cols())
  })
  output$prediction_plot <- renderPlot({
    prediction_plot()
  })

  # * Layers plot ----
  cov_layers_plot <- reactive({
    p <- ggplot2::ggplot()

    if (!is.null(input$layers_plot_cov)){
      legend_label <- input$layers_plot_cov
      if (input$layers_plot_mode == "fit_layers") {
        plot_layers <- covariate_list()[[input$layers_plot_mode]][[input$layers_plot_cov]]

        p <- p + geom_spatraster(data = plot_layers)

        if (is.factor(plot_layers)){
          p <- p + scale_fill_discrete(type = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"),
                                        na.value = "white", name = legend_label)
        } else {
          p <- p + scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"),
                                        na.value = "white", name = legend_label)
        }

      } else if (input$layers_plot_mode == "projections") {
        req(input$layers_plot_year)
        req(input$layers_plot_year <= length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
        plot_layers <- covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]][[input$layers_plot_year]][[input$layers_plot_cov]]

        p <- p+ geom_spatraster(data = plot_layers)

        if (is.factor(plot_layers)){
          p <- p + scale_fill_discrete(type = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"),
                                       na.value = "white", name = legend_label)
        } else {
          p <- p + scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"),
                                        na.value = "white", name = legend_label)
        }
      }
    }

    if (!is.null(inv_study_area_poly())){
      p <- p +
        geom_sf(data = inv_study_area_poly(), color = "#353839", fill = "antiquewhite")
    }

    p +
      theme(
        panel.grid.major = element_line(
          color = gray(.5),
          linetype = "dashed",
          linewidth = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank()
      )
  })
  output$cov_layers_plot<-renderPlot({
    cov_layers_plot()
  })

  # * Observations plot ----
  observations_plot <- reactive({
    p <- ggplot2::ggplot()

    if (!is.null(inv_study_area_poly())){
      p <- p +
        geom_sf(data = inv_study_area_poly(), color = "#353839", fill = "antiquewhite")
    }

    if (!is.null(input$sp)) {
      model_points <- presence_absence_list()$model_pa[[input$sp]]
      model_points <- model_points[model_points[, "pa"] == 1, c(long_lat_cols(), "pa")]
      model_points$type <- "kept"

      raw_points <- presence_absence_list()$raw_pa[[input$sp]]
      raw_points <- raw_points[raw_points[, "pa"] == 1, c(long_lat_cols(), "pa")]
      raw_points <- dplyr::anti_join(raw_points, model_points, by = long_lat_cols())
      if (nrow(raw_points) > 0){
        raw_points$type <- "discarded"
      }

      tmp_data <- rbind(raw_points, model_points)

      p <- p +
        geom_point(data = tmp_data, ggplot2::aes(x = tmp_data[, long_lat_cols()[1]], y = tmp_data[, long_lat_cols()[2]], color = type)) +
        scale_color_manual(values = c("kept" = "#65c4d8", "discarded" = "#f67d33")) +
        ggplot2::coord_sf(datum = sf::st_crs(4326))
    }

    p +
      theme(
        panel.grid.major = element_line(
          color = gray(.5),
          linetype = "dashed",
          linewidth = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank()
      )
  })
  output$observations_plot<-renderPlot({
    observations_plot()
  })


  # * Functional responses plot ----
  fr_plot <- reactive({
    p <- ggplot2::ggplot(data = data.frame(y = 0:1), ggplot2::aes(y = y))

    req(input$sp)
    req(input$fr_plot_cov)

    data_list <- other_results()[["response_curve"]][[input$sp]]
    # Check input$fr_plot_cov is in the list
    validate(
      need(input$fr_plot_cov %in% names(data_list), "Invalid covariate selected.")
    )

    dat <- data_list[[input$fr_plot_cov]]
    req(dat)

    if (is.character(dat$value) || is.factor(dat$value)) {
      p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
        geom_point(ggplot2::aes(y = mean), color = "#004172") +
        geom_errorbar(ggplot2::aes(ymin = q25, ymax = q975), width = 0.2, color = "#65c4d8") +
        xlab(input$fr_plot_cov)
    } else {
      p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
        geom_ribbon(ggplot2::aes(ymin = q25, ymax = q975), fill = "#65c4d8", alpha = 0.3) +
        geom_line(ggplot2::aes(y = mean), color = "#004172", linewidth = 1) +
        xlab(input$fr_plot_cov)
    }

    p +
      ylab("Probability") +
      theme_minimal()
  })
  output$fr_plot<-renderPlot({
    fr_plot()
  })

  # * Variable importance plot ----
  varimp_plot <- reactive({
    p <- ggplot2::ggplot(data.frame(x = paste("var", 1:3), y = c(0, 0, 0)), ggplot2::aes(x = x, y = y))

    if (!is.null(input$varimp_plot_mode)) {
      data <- other_results()[["variable_importance"]][[input$varimp_plot_mode]][[input$sp]]
      data <- utils::stack(data)
      data$ind <- reorder(data$ind, data$values, median, na.rm = TRUE, decreasing = TRUE)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(ind), y = values))
    }

    p +
      geom_boxplot(fill="#007bff") +
      ylab("F-score decrease") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  })
  output$varimp_plot <-renderPlot({
    varimp_plot()
  })

  # * Cross-validation plot ----
  cv_plot <- reactive({
    req(input$cv_plot_model, input$cv_plot_method, input$cv_plot_type, input$sp)

    plot_data <- other_results()$cross_validation[[input$cv_plot_model]][[input$sp]][[input$cv_plot_method]]

    validate(need(!is.null(plot_data), "Please select valid CV options"))

    if (input$cv_plot_type == "Metrics") {
      plot_cv_metrics(plot_data$metrics)
    } else {
      if (!is.null(inv_study_area_poly())){
        plot_cv_folds_points(plot_data$predictions, polygon = inv_study_area_poly())
      } else {
        plot_cv_folds_points(plot_data$predictions)
      }
    }
  })
  output$cv_plot<-renderPlot({
    cv_plot()
  })

  # * Fitted values plot ----
  fv_plot <- reactive({
    if (!is.null(input$fv_plot_mode)){
      x <- other_results()[["model_diagnostic"]][[input$fv_plot_mode]][[input$sp]][["data"]]
    } else {
      x <- data.frame(matrix(ncol = 1, nrow = 0))
      colnames(x) <- "probability"
    }
    ggplot(x, aes(probability )) +
      geom_histogram(stat = "bin", binwidth = 0.05, fill = "#007bff", color = "white") +
      ylab("Count of train data") +
      xlab("Predicted probability") +
      theme_minimal(base_size = 15)
  })
  output$fv_plot<-renderPlot({
    fv_plot()
  })

  # * Classified values plot ----
  class_val_plot <- reactive({
    if (!is.null(input$class_val_plot_mode)){
      x <- other_results()[["model_diagnostic"]][[input$class_val_plot_mode]][[input$sp]][["data"]]
      tmp_cutoff <- pa_cutoff()[[input$class_val_plot_mode]][[input$sp]]
    } else {
      x <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(x) <- c("observed", "probability", "predicted")
      tmp_cutoff <- 0.5
    }
    ggplot(x, aes(x = probability,
                  y = factor(observed),
                  fill = factor(predicted),
                  color = factor(predicted))) +
      geom_jitter(height = 0.2, size = 2, alpha = 0.7, width = 0.05) +
      xlab("Predicted probability") +
      ylab("Observed") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "none") +
      geom_vline(xintercept = tmp_cutoff, col = 'black', linetype = "dashed", linewidth = 1) +
      ggplot2::annotate("text", x = tmp_cutoff, y = 1.5, label = paste("Cutoff =", round(tmp_cutoff,3)),
                        color = "black", vjust = -1, size = 4)
  })
  output$class_val_plot<-renderPlot({
    class_val_plot()
  })

  # * ROC plot ----
  roc_plot <- reactive({
    if (!is.null(input$roc_plot_mode)){
      mod_diag <- other_results()[["model_diagnostic"]][[input$roc_plot_mode]][[input$sp]]
      x <- pROC::roc(mod_diag[["data"]]$observed, mod_diag[["data"]]$probability, levels = c(0, 1), direction = "<")
      auc <- round(mod_diag[["metrics"]]["AUC"] ,4)
      tss <- round(mod_diag[["metrics"]]["TSS"] ,4)
      f_score <- round(mod_diag[["metrics"]]["Fscore"] ,4)
      cbi <- round(mod_diag[["metrics"]]["CBI"] ,4)

      p <- pROC::ggroc(x, colour = "#007bff", linewidth = 1.5)
    } else {
      auc <- 0
      tss <- 0
      f_score <- 0
      cbi <- 0
      p <- ggplot() + xlim(c(1, 0)) + ylim(c(0, 1))
    }
    p +
      geom_abline(intercept=1,slope=1,col="grey", linetype = "dashed", linewidth = 1.5) +
      xlab("False Positive Rate (FPR)") +
      ylab("True Positive Rate (TPR)") +
      ggplot2::annotate("text", x = 0.25, y = 0.45, label = paste("CBI =", cbi),
                        color = "black", vjust = -1, size = 4) +
      ggplot2::annotate("text", x = 0.25, y = 0.35, label = paste("AUC =", auc),
                        color = "black", vjust = -1, size = 4) +
      ggplot2::annotate("text", x = 0.25, y = 0.25, label = paste("TSS =", tss),
                        color = "black", vjust = -1, size = 4) +
      ggplot2::annotate("text", x = 0.25, y = 0.15, label = paste("F-score =", f_score),
                        color = "black", vjust = -1, size = 4) +
      theme_minimal(base_size = 15)
  })
  output$roc_plot<-renderPlot({
    roc_plot()
  })

  #=========================================================#
  # Exports ----
  #=========================================================#

  # * Export plots ----
  glossa::export_plot_server("export_pred_plot", prediction_plot)
  glossa::export_plot_server("export_layers_plot", cov_layers_plot)
  glossa::export_plot_server("export_observations_plot", observations_plot)
  glossa::export_plot_server("export_fr_plot", fr_plot)
  glossa::export_plot_server("export_varimp_plot", varimp_plot)
  glossa::export_plot_server("export_cv_plot", cv_plot)
  glossa::export_plot_server("export_fv_plot", fv_plot)
  glossa::export_plot_server("export_class_val_plot", class_val_plot)
  glossa::export_plot_server("export_roc_plot", roc_plot)

  # * Export results downloadHandler ----
  output$export_button <- downloadHandler(
    filename = function() { paste("glossa_", format(Sys.time(), "%D_%X"), ".zip", sep="") },
    content = function(file) {
      w <- waiter::Waiter$new(html = tagList(
        img(src = "logo_glossa.gif", height = "200px")
      ),
      color = waiter::transparent(0.8)
      )
      w$show()
      export_files <- glossa_export(species = input$export_sp, models = input$export_models,
                                    layer_results = input$export_results, fields = input$export_values,
                                    model_data = input$export_model_data, model_summary = input$export_mod_summary, fr = input$export_fr,
                                    prob_cut = input$export_pa_cutoff, varimp = input$export_var_imp,
                                    cross_val = input$export_cv, layer_format = input$export_layer_format,
                                    projections_results = projections_results(),
                                    presence_absence_list = presence_absence_list(),
                                    other_results = other_results(), pa_cutoff = pa_cutoff(),
                                    config_snapshot = config())

      zip::zip(zipfile = file, files = export_files, mode = "cherry-pick")
      w$hide()
    }
  )

  #=========================================================#
  # Stop app ----
  #=========================================================#
  observeEvent(input$stop_app, {
    shinyWidgets::ask_confirmation(
      inputId = "stop_app_confirmation",
      title = "Do you want to close the app?",
      text = "By selecting 'Confirm', the GLOSSA app will close.",
      type = "warning",
      btn_labels = c("Cancel", "Confirm")
    )
  })

  observeEvent(input$stop_app_confirmation, {
    if (isTRUE(input$stop_app_confirmation)) {
      session$sendCustomMessage("closeWindow", list())
      stopApp()
    }
  })

  #observeEvent(input$stop_app_confirmation, {
  #  req(input$stop_app_confirmation == TRUE)
  #  session$sendCustomMessage("closeWindow", TRUE)
  #  stopApp()
  #})
}
