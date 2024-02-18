#' @rdname rave-ui-preset
#' @export
presets_baseline_choices <- function(
  id = "baseline_choices", varname = "baseline",
  label = "Baseline Settings",
  pipeline_repository = "repository",
  baseline_choices = c("Decibel", "% Change Power", "% Change Amplitude",
                       "z-score Power", "z-score Amplitude"),
  baseline_along_choices = c("Per frequency, trial, and electrode", "Across electrode",
                             "Across trial", "Across trial and electrode")
) {
  force(label)
  force(baseline_choices)
  force(baseline_along_choices)

  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$no_save <- ""
  comp$repository_name <- pipeline_repository

  unit_of_analysis_str <- "unit_of_analysis"
  global_baseline_choice_str <- "global_baseline_choice"
  baseline_windows_str <- "windows"

  # comp$no_save <- c(reset_str, category_choices_str, selected_electrode_text_str)

  # repository_name <- "repository"
  get_repo <- function(){
    if(!comp$container$data[['@has']](pipeline_repository)) {
      repository <- raveio::pipeline_read(var_names = pipeline_repository,
                                          pipe_dir = comp$container$pipeline_path)
      comp$container$data[[pipeline_repository]] <- repository
    } else {
      repository <- comp$container$data[[pipeline_repository]]
    }
    if(!inherits(repository, "rave_repository")) {
      return(NULL)
    }
    repository
  }

  get_default <- function(sub_id, missing = NULL, use_cache = TRUE, constraint = NULL) {
    vname <- comp$get_sub_element_varnames(sub_id)
    repo <- get_repo()
    if(inherits(repo, "rave_repository")) {
      subject <- repo$subject
      missing <- subject$get_default(vname,
                                     default_if_missing = missing, simplify = TRUE)
    }
    comp$get_settings_value(use_cache = use_cache, default = missing,
                            key = vname, constraint = constraint)
  }

  # component_container$add_components(comp)

  comp$ui_func <- function(id, value, depends){

    input_card(
      class_header = "shidashi-anchor",
      title = label,
      class_body = "padding-5",
      href = card_href(label, type = "input",
                       module_id = comp$container$module_id),

      shidashi::flex_container(
        shidashi::flex_item(
          shiny::selectInput(
            inputId = comp$get_sub_element_id(unit_of_analysis_str, with_namespace = TRUE),
            label = "Electrode unit of analysis",
            choices = baseline_choices
          ),
          shiny::selectInput(
            inputId = comp$get_sub_element_id(global_baseline_choice_str, with_namespace = TRUE),
            label = "Baseline method",
            choices = baseline_along_choices
          )
        ),
        shidashi::flex_break(),
        shidashi::flex_item(
          shiny::tags$label("Baseline windows"),
          dipsaus::compoundInput2(
            inputId = comp$get_sub_element_id(baseline_windows_str, with_namespace = TRUE),
            label = NULL,
            initial_ncomp = 1L, min_ncomp = 1L,
            label_color = gray_label_color,
            components = {
              shiny::sliderInput(
                inputId = "window_interval",
                label = NULL, width = "100%",
                min = 0, max = 1, value = c(0, 0),
                round = -2, post = " s", step = 0.01
              )
            }
          )
        )
      )
    )
  }

  comp$server_func <- function(input, output, session){

    comp$ready_to_collect <- function(){
      shiny::isolate(isFALSE(watch_loader_opened()))
    }

    # get pipeline's default, or subject's default, or program default
    reset <- function(...){
      logger("Reset {id}", level = "trace", use_glue = TRUE)
      repo <- get_repo()
      if(is.null(repo)) { return() }

      time_range <- range(repo$time_points)
      if(time_range[1] <= 0){
        default_baseline_windows <- c(time_range[1], 0)
      } else {
        default_baseline_windows <- c(time_range[1], time_range[1])
      }

      # get Electrode unit of analysis
      unit_of_analysis <- get_default(unit_of_analysis_str,
                                      constraint = baseline_choices)
      global_baseline_choice <- get_default(global_baseline_choice_str,
                                            constraint = baseline_along_choices)
      baseline_windows <- get_default(baseline_windows_str,
                                      missing = c(time_range[1], max(time_range[1], 0)))
      baseline_windows <- lapply(baseline_windows, function(x){
        unlist(x$window_interval)
      })
      baseline_windows <- tryCatch({
        raveio::validate_time_window(baseline_windows)
      }, error = function(e){
        list(c(time_range[1], max(time_range[1], 0)))
      })

      # update
      shiny::updateSelectInput(session = session,
                        inputId = comp$get_sub_element_id(unit_of_analysis_str, FALSE),
                        selected = unit_of_analysis)
      shiny::updateSelectInput(session = session,
                        inputId = comp$get_sub_element_id(global_baseline_choice_str, FALSE),
                        selected = global_baseline_choice)
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = comp$get_sub_element_id(baseline_windows_str, FALSE),
        ncomp = length(baseline_windows),
        initialization = list(
          window_interval = list(
            min = time_range[[1]],
            max = time_range[[2]]
          )
        ),
        value = lapply(baseline_windows, function(x){
          list(window_interval = x)
        })
      )


    }

    initialize_with_new_data_reactive <- function(){
      shidashi::clear_notifications(
        class = "_presets_baseline_choices_error_",
        session = session)
      repository <- get_repo()
      if(is.null(repository)){
        shidashi::show_notification(
          title = "Initialization Error",
          message = c(
            "Unable to initialize preset input `",
            id, "`. The container repository has not been set up yet. ",
            "This is a module error. Please contact the module author to ",
            "fix this issue."
          ),
          type = "warning", close = TRUE, autohide = FALSE,
          collapse = "", session = session,
          class = "_presets_baseline_choices_error_"
        )
        return()
      }
      reset()
    }

    # observe({
    #   reset_electrode_selectors()
    # }) |>
    #   shiny::bindEvent(
    #     comp$get_sub_element_input(reset_str),
    #     ignoreNULL = TRUE, ignoreInit = TRUE
      # )

    comp$set_tool("reset", reset, server_needed = TRUE)
    comp$set_tool("initialize_with_new_data", function(){
      shiny::isolate(initialize_with_new_data_reactive())
    }, server_needed = TRUE)

    comp

  }

  comp


}

