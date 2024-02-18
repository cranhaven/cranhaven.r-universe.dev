

#' @rdname rave-ui-preset
#' @export
presets_loader_epoch <- function(
  id = "loader_epoch_name",
  varname = "epoch_choice",
  label = "Epoch and Trial Duration",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- "default"

  # pre_id <- paste0(id, "_pre")
  # post_id <- paste0(id, "_post")
  # check_id <- paste0(id, "_default")

  pre_varname <- comp$get_sub_element_varnames("trial_starts")
  post_varname <- comp$get_sub_element_varnames("trial_ends")

  comp$ui_func <- function(id, value, depends){

    pre <- comp$get_settings_value(key = pre_varname, default = -1)
    post <- comp$get_settings_value(key = post_varname, default = 2)

    ravedash::flex_group_box(
      title = label,
      shidashi::flex_item(
        size = 2,
        shiny::selectInput(
          inputId = id,
          label = "Epoch name",
          choices = c(value, ""),
          selected = value,
          multiple = FALSE
        )
      ),
      shidashi::flex_item(
        shiny::numericInput(
          inputId = comp$get_sub_element_id("trial_starts", with_namespace = TRUE),
          label = "Pre",
          min = 0,
          value = pre
        )
      ),
      shidashi::flex_item(
        shiny::numericInput(
          inputId = comp$get_sub_element_id("trial_ends", with_namespace = TRUE),
          label = "Post",
          min = 0,
          value = post
        )
      ),
      shidashi::flex_break(),
      shidashi::flex_item(
        shinyWidgets::prettyCheckbox(
          inputId = comp$get_sub_element_id("default", with_namespace = TRUE),
          label = "Set as the default",
          status = "success",
          shape = "square",
          animation = "smooth"
        )
      )
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    get_subject <- loader_subject$get_tool("get_subject")

    get_time_window <- function(){
      subject <- get_subject()
      if(inherits(subject, "RAVESubject")) {
        pre <- comp$get_settings_value(key = pre_varname, default = {
          subject$get_default(pre_varname, default_if_missing = -1)
        })
        post <- comp$get_settings_value(key = post_varname, default = {
          subject$get_default(post_varname, default_if_missing = 2)
        })
      } else {
        pre <- comp$get_settings_value(key = pre_varname, default = -1)
        post <- comp$get_settings_value(key = post_varname, default = 2)
      }
      raveio::validate_time_window(as.vector(rbind(pre, post)))
    }

    shiny::bindEvent(
      observe({
        open_loader <- watch_loader_opened(session = session)
        if(!open_loader){ return() }
        if(!loader_subject$sv$is_valid()){ return() }
        subject <- get_subject()
        epoch_choices <- subject$epoch_names

        default_epochname <- subject$get_default(id)
        if(length(default_epochname)){
          default_epochname <- default_epochname[[1]]
          shinyWidgets::updatePrettyCheckbox(
            session, inputId = comp$get_sub_element_id("default", FALSE),
            label = sprintf("Set as the default (current: %s)", default_epochname)
          )
        } else {
          shinyWidgets::updatePrettyCheckbox(
            session, inputId = comp$get_sub_element_id("default", FALSE),
            label = "Set as the default"
          )
        }
        epoch_name <- comp$get_settings_value(
          default = default_epochname,
          constraint = epoch_choices,
          use_cache = TRUE)

        shiny::updateSelectInput(
          session = session,
          inputId = id,
          choices = epoch_choices,
          selected = epoch_name
        )
      }),
      loader_project$current_value,
      loader_subject$current_value,
      watch_loader_opened(session = session),
      ignoreNULL = TRUE
    )

  }

  comp

}
