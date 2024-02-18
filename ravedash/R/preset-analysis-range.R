#' @rdname rave-ui-preset
#' @export
presets_analysis_ranges <- function(
  id = "analysis_ranges", varname = "analysis_ranges",
  label = "Configure Analysis",
  pipeline_repository = "repository",
  max_components = 2
) {
  max_components <- as.integer(max_components)
  if(max_components < 1L){ max_components <- 1L }
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$repository_name <- pipeline_repository

  # component_container$add_components(comp)
  analysis_lock_str <- "lock"
  analysis_lock_choices <- c("Unlocked", "Lock frequency", "Lock time")

  if(max_components == 1L) {
    comp$no_save <- analysis_lock_str
  }

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

  get_subject <- function(){
    repo <- get_repo()
    if(inherits(repo, "rave_repository")) {
      subject <- repo$subject
      return(subject)
    }
    return(NULL)
  }

  get_default <- function(sub_id, missing = NULL, use_cache = TRUE, constraint = NULL) {
    vname <- comp$get_sub_element_varnames(sub_id)
    subject <- get_subject()
    if(inherits(subject, "RAVESubject")) {
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
      href = card_href(label, module_id = comp$container$module_id),
      shidashi::flex_container(
        shidashi::flex_item(
          class = ifelse(max_components > 1, "", "soft-hidden"),
          shinyWidgets::radioGroupButtons(
            inputId = comp$get_sub_element_id(analysis_lock_str, TRUE),
            label = "Lock range selector",
            choices = analysis_lock_choices,
            justified = TRUE,
            checkIcon = list(
              yes = shiny_icons$check
            ),
            width = "100%",
            size = "sm"
          )
        ),
        shidashi::flex_break(),
        shidashi::flex_item(
          dipsaus::compoundInput2(
            inputId = comp$get_sub_element_id(with_namespace = TRUE),
            label = "Analysis range",
            initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = max_components,
            label_color = gray_label_color,
            components = shiny::div(
              shiny::sliderInput(inputId = "frequency", label = "Frequency",
                                 min = 0, max = 150, value = c(75, 150),
                                 step = 0.5, round = -1, post = " Hz"),
              shiny::sliderInput(inputId = "time", label = "Time range",
                                 min = 0, max = 1, value = c(0, 1),
                                 step = 0.01, round = -2, post = " s")
            )
          )
        )

      )



    )
  }

  comp$server_func <- function(input, output, session){

    # get pipeline's default, or subject's default, or program default
    reset <- function(...){
      logger("Reset {id}", level = "trace", use_glue = TRUE)
      repo <- get_repo()
      if(is.null(repo)) { return() }

      time_range <- range(repo$time_points)
      freq_range <- range(repo$frequency)
      default_analysis_ranges <- list(
        list(
          frequency = c(75, 150),
          time = c(max(0, time_range[[1]]), time_range[[2]])
        )
      )

      analysis_lock <- get_default(sub_id = analysis_lock_str, constraint = analysis_lock_choices)
      analysis_ranges <- get_default(sub_id = NULL, missing = default_analysis_ranges)

      if(!length(analysis_ranges)){
        analysis_ranges <- default_analysis_ranges
      } else if(length(analysis_ranges) > max_components){
        analysis_ranges <- analysis_ranges[seq_len(max_components)]
      }

      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = comp$get_sub_element_id(analysis_lock_str, with_namespace = FALSE),
        selected = analysis_lock
      )
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = comp$get_sub_element_id(with_namespace = FALSE),
        ncomp = length(analysis_ranges),
        initialization = list(
          frequency = list(min = freq_range[[1]], max = freq_range[[2]]),
          time = list(min = time_range[[1]], max = time_range[[2]])
        ),
        value = analysis_ranges
      )

    }

    initialize_with_new_data_reactive <- function(){
      shidashi::clear_notifications(
        class = "_presets_analysis_ranges_error_",
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
          class = "_presets_analysis_ranges_error_"
        )
        return()
      }
      reset()
    }

    # sync analysis ranges
    base_id <- comp$get_sub_element_id(with_namespace = FALSE)
    shiny::bindEvent(
      observe({
        analysis_lock <- which(analysis_lock_choices %in% comp$get_sub_element_input(analysis_lock_str))
        if(!length(analysis_lock) || analysis_lock == 1){ return() }
        if(analysis_lock == 2){
          value <- comp$current_value[[1]]$frequency
          lapply(seq_len(max_components), function(ii){
            shiny::updateSliderInput(
              session = session, inputId = sprintf("%s_frequency_%d", base_id, ii),
              value = value
            )
          })
        } else if(analysis_lock == 3){
          value <- comp$current_value[[1]]$time
          lapply(seq_len(max_components), function(ii){
            shiny::updateSliderInput(
              session = session, inputId = sprintf("%s_time_%d", base_id, ii),
              value = value
            )
          })
        }
      }),
      watch_data_loaded(),
      comp$get_sub_element_input(analysis_lock_str),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    dipsaus::sync_shiny_inputs(
      input = input, session = session,
      inputIds = sprintf("%s_frequency_%d",
                         base_id,
                         seq_len(max_components)),
      uniform = as.list(rep("I", max_components)),
      updates = lapply(seq_len(max_components), function(ii){
        function(value){
          if(isTRUE(comp$get_sub_element_input(analysis_lock_str) == analysis_lock_choices[[2]])){
            shiny::updateSliderInput(
              session = session, inputId = sprintf("%s_frequency_%d", base_id, ii),
              value = value
            )
          }
        }
      })
    )
    dipsaus::sync_shiny_inputs(
      input = input, session = session,
      inputIds = sprintf("%s_time_%d", base_id, seq_len(max_components)),
      uniform = as.list(rep("I", max_components)),
      updates = lapply(seq_len(max_components), function(ii){
        function(value){
          if(isTRUE(comp$get_sub_element_input(analysis_lock_str) == analysis_lock_choices[[3]])){
            shiny::updateSliderInput(
              session = session, inputId = sprintf("%s_time_%d", base_id, ii),
              value = value
            )
          }
        }
      })
    )

    comp$set_tool("reset", reset, server_needed = TRUE)
    comp$set_tool("initialize_with_new_data", function(){
      shiny::isolate(initialize_with_new_data_reactive())
    }, server_needed = TRUE)

    comp

  }

  comp


}

