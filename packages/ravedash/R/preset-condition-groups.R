#' @rdname rave-ui-preset
#' @export
presets_condition_groups <- function(
  id = "condition_groups", varname = "condition_groups",
  label = "Create Condition Contrast",
  pipeline_repository = "repository"
) {
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$repository_name <- pipeline_repository

  # component_container$add_components(comp)

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
      dipsaus::compoundInput2(
        inputId = comp$get_sub_element_id(with_namespace = TRUE),
        label = "Group",
        initial_ncomp = 1L,
        min_ncomp = 1L,
        max_ncomp = 40L,
        label_color = gray_label_color,
        components = shiny::div(
          shiny::textInput(inputId = "group_name", label = "Name"),
          # shiny::selectInput(inputId = "group_conditions", label = NULL, choices = "", multiple = TRUE)
          shiny::selectInput(
            inputId = "group_conditions", label = NULL,
            choices = "", multiple = TRUE
            # options = list(
            #   "live-search" = TRUE,
            #   "actions-box" = TRUE,
            #   "size" = 4
            # )
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

      cond_cont <- table(repo$epoch$table$Condition)
      cond_cont <- cond_cont[order(names(cond_cont))]
      conditions <- names(cond_cont)
      default <- list(list(
        group_name = "All Conditions",
        group_conditions = conditions
      ))

      value <- get_default(sub_id = NULL, missing = NULL)
      if(!length(value) || !is.list(value) || !all(value$group_conditions %in% conditions)){
        value <- default
      }

      # update
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = comp$id,
        initialization = list(
          group_conditions = list(
            choices = conditions
            # choicesOpt = list(
            #   subtext = sprintf("(n = %d)", cond_cont)
            # )
          )
        ),
        value = value,
        ncomp = length(value)
      )

    }

    initialize_with_new_data_reactive <- function(){
      shidashi::clear_notifications(
        class = "_presets_condition_groups_error_",
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
          class = "_presets_condition_groups_error_"
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

