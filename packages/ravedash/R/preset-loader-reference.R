
#' @rdname rave-ui-preset
#' @export
presets_loader_reference <- function(
  id = "loader_reference_name",
  varname = "reference_name",
  label = "Reference name",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  mode = c("default", "create")
){
  mode <- match.arg(mode)
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- "default"

  if(mode == "default") {
    comp$ui_func <- function(id, value, depends){
      shiny::tagList(shidashi::flex_item(
        shiny::selectInput(
          inputId = id,
          label = label,
          choices = '',
          selected = NULL,
          multiple = FALSE
        ),
        shinyWidgets::prettyCheckbox(
          inputId = comp$get_sub_element_id("default", with_namespace = TRUE),
          label = "Set as the default",
          status = "success",
          shape = "square",
          animation = "smooth"
        )
      ))
    }
  } else {
    comp$ui_func <- function(id, value, depends){
      shiny::tagList(shidashi::flex_item(
        shiny::selectInput(
          inputId = id,
          label = label,
          choices = "[Blank profile]",
          multiple = FALSE
        )
      ))
    }
  }

  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    get_subject <- loader_subject$get_tool("get_subject")

    shiny::bindEvent(
      observe({
        open_loader <- watch_loader_opened(session = session)
        if(!open_loader){ return() }
        if(!loader_subject$sv$is_valid()){ return() }

        subject <- get_subject()
        ref_choices <- subject$reference_names

        if(mode == "default") {

          default_refname <- subject$get_default("reference_name")
          if(length(default_refname)){
            default_refname <- default_refname[[1]]
            shinyWidgets::updatePrettyCheckbox(
              session, inputId = comp$get_sub_element_id("default", FALSE),
              label = sprintf("Set as the default (current: %s)", default_refname)
            )
          } else {
            shinyWidgets::updatePrettyCheckbox(
              session, inputId = comp$get_sub_element_id("default", FALSE),
              label = "Set as the default"
            )
          }
        } else {
          default_refname <- comp$get_sub_element_input()
          ref_choices <- c("[Blank profile]", ref_choices)
        }

        reference_name <-
          comp$get_settings_value(
            default = default_refname,
            constraint = ref_choices,
            use_cache = TRUE
          )
        shiny::updateSelectInput(
          session = session,
          inputId = id,
          choices = ref_choices,
          selected = reference_name
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
