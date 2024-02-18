#' @rdname rave-ui-preset
#' @export
presets_import_export_subject_pipeline <- function(
  id = "im_ex_pipeline",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  pipeline_repository = "repository",
  settings_entries = c(
    "loaded_electrodes",
    "epoch_choice",
    "epoch_choice__trial_starts",
    "epoch_choice__trial_ends",
    "reference_name"
  ),
  fork_mode = c("exclude", "include")
) {
  force(pipeline_repository)
  force(settings_entries)
  fork_mode <- match.arg(fork_mode)

  # `varname` is not useful
  comp <- RAVEShinyComponent$new(id = id, varname = id)
  comp$no_save <- TRUE
  comp$repository_name <- pipeline_repository
  comp$depends <- c(loader_project_id, loader_subject_id)

  # component_container$add_components(comp)

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

  comp$ui_func <- function(id, value, depends) {}


  comp$server_func <- function(input, output, session){

    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    pipeline_name <- comp$container$pipeline_name
    pipeline_path <- comp$container$pipeline_path
    settings_filename <- basename(comp$container$settings_path)

    shiny::bindEvent(
      observe({
        tryCatch({
          repository <- get_repo()
          if(is.null(repository)){
            stop("There is no repository found.")
          }
          subject_id <- repository$subject$subject_id
          if(length(subject_id)){
            shidashi::show_notification(
              title = "Saving pipeline",
              type = "info",
              message = as.character(
                shiny::tagList(shiny::p(
                  raveio::glue("Saving pipeline [{pipeline_name}] to subject [{subject_id}]. Please name your pipeline below:")
                ),
                shidashi::flex_container(
                  direction = "column",
                  shiny::textInput(
                    inputId = comp$get_sub_element_id(
                      sub_id = "save_name",
                      with_namespace = TRUE
                    ),
                    label = "Pipeline name",
                    value = sprintf("%s-%s", pipeline_name, strftime(Sys.time(), "%y_%m_%d-%H_%M_%S"))),
                  shiny::actionButton(
                    comp$get_sub_element_id(
                      sub_id = "save_btn",
                      with_namespace = TRUE
                    ), label = "Confirm", width = "100%"
                  )
                ))

              ),
              close = TRUE, autohide = FALSE, fixed = TRUE, class = "rave-notification-save-pipeline"
            )
          }

        }, error = function(e){
          shidashi::show_notification(
            title = "Saving pipeline",
            type = "danger",
            message = c(
              "Error found while trying to analyze the pipeline tree:",
              e$message
            ),
            close = TRUE, autohide = TRUE, fixed = TRUE,
            class = "rave-notification-save-pipeline",
            collapse = "\n"
          )
        })

        # # Save current pipeline
        # # load current subject information
        # repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
        # file.path(repository$subject$pipeline_path, pipeline_name, )
        # raveio::pipeline_fork(src = pipeline_path, dest = )
      }),
      ravedash::get_rave_event("save_pipeline"),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )


    shiny::bindEvent(
      observe({
        tryCatch({
          repository <- get_repo()
          if(is.null(repository)){
            stop("There is no repository found.")
          }
          name <- comp$get_sub_element_input(sub_id = "save_name")
          name <- gsub("[^a-zA-Z0-9_-]", "_", name)
          name <- gsub("[_]+", "_", name)
          name <- gsub("[\\-]+", "-", name)
          dest <- file.path(repository$subject$pipeline_path, pipeline_name, name)
          if(dir.exists(dest)){
            stop("A pipeline with this name has already existed. Please choose another name.")
          }

          shidashi::clear_notifications(class = "rave-notification-save-pipeline")
          dipsaus::shiny_alert2(
            title = "Saving in progress",
            icon = "info",
            text = sprintf("Saving pipeline %s", name),
            auto_close = FALSE,
            buttons = FALSE
          )

          tryCatch({
            raveio::pipeline_fork(src = pipeline_path, dest = dest, activate = FALSE)
            dipsaus::close_alert2()
            dipsaus::shiny_alert2(
              title = "Saved!",
              icon = "success",
              auto_close = TRUE,
              buttons = list("Dismiss" = TRUE)
            )
          }, error = function(e){
            dipsaus::close_alert2()
            dipsaus::shiny_alert2(
              title = "Error!",
              icon = "error",
              auto_close = TRUE,
              buttons = list("Dismiss" = TRUE),
              danger_mode = TRUE,
              text = raveio::glue("An error found while saving the pipeline. Reason: \n  {e$message}")
            )
          })

        }, error = function(e){
          shidashi::show_notification(
            title = "Saving pipeline",
            type = "danger",
            message = c(
              "Error while saving the pipeline:",
              e$message
            ),
            close = TRUE, autohide = TRUE, fixed = TRUE,
            class = "rave-notification-save-pipeline",
            collapse = "\n"
          )
        })

      }),
      comp$get_sub_element_input("save_btn"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


    shiny::bindEvent(
      observe({
        tryCatch({
          repository <- get_repo()
          if(inherits(repository, "rave_repository")){
            project <- repository$project
            subject_code <- repository$subject$subject_code
          } else {
            project_name <- loader_project$current_value
            subject_code <- loader_subject$current_value
            if(!length(project_name)){
              stop("Cannot get valid project name from currect context")
            }
            project <- raveio::as_rave_project(project_name)
            if(!length(subject_code)) {
              subject_code <- NA
            }
          }

          pnames <- character(0L)
          dirs <- character(0L)
          if(isTRUE(project$has_subject(subject_code))) {
            subject <- raveio::RAVESubject$new(project_name = project$name,
                                               subject_code = subject_code,
                                               strict = FALSE)
            dirs <- list.dirs(file.path(subject$pipeline_path, pipeline_name),
                              full.names = FALSE, recursive = FALSE)
            if(length(dirs)){
              dirs <- sort(dirs, decreasing = TRUE)
              pnames <- dirs[[1]]
            }
          }

          # if(!length(dirs)){
          #   dirs <- character(0L)
          # } else {
          #   dirs <- sort(dirs, decreasing = TRUE)
          # }

          # print(subject_code)

          shidashi::show_notification(
            title = "Load pipeline",
            type = "info",
            message = as.character(shiny::fluidRow(
              shiny::column(
                width = 12L,
                shiny::p("Please choose a pipeline to load:"),
                # shinyWidgets::pickerInput(ns("load_pipeline"), label = NULL, choices = dirs,
                #                           options = list(
                #                             "live-search" = TRUE
                #                           )),
                shiny::selectInput(comp$get_sub_element_id("load_subject", TRUE),
                                   label = "Subject", choices = project$subjects(),
                                   selected = subject_code),
                shiny::selectInput(comp$get_sub_element_id("load_name", TRUE),
                                   label = NULL, choices = dirs,
                                   selected = pnames,
                                   selectize = FALSE, width = "100%",
                                   size = 10),
                shiny::actionButton(comp$get_sub_element_id("load_btn", TRUE),
                                    label = "Confirm", width = "100%")
              )
            )),
            autohide = FALSE, close = TRUE, class = "rave-notification-load-pipeline"
          )

        }, error = function(e){
          shidashi::show_notification(
            title = "Loading pipeline",
            type = "danger",
            message = c(
              "Error found while trying to analyze the subject pipeline tree:",
              e$message
            ),
            close = TRUE, autohide = TRUE, fixed = TRUE,
            class = "rave-notification-load-pipeline",
            collapse = "\n"
          )
        })
      }),

      ravedash::get_rave_event("load_pipeline"),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )

    shiny::bindEvent(
      observe({
        subject_code <- comp$get_sub_element_input("load_subject")

        repository <- get_repo()
        if(inherits(repository, "rave_repository")){
          project <- repository$project
          project_name <- project$name
        } else {
          project_name <- loader_project$current_value
          project <- raveio::as_rave_project(project_name, strict = FALSE)
        }

        # print(subject_code)
        if(!isTRUE(!project$has_subject(subject_code))) {
          shiny::updateSelectInput(
            session = session,
            inputId = comp$get_sub_element_id("load_name", FALSE),
            choices = character(0L),
            selected = character(0L)
          )
        }

        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code,
                                           strict = FALSE)

        dirs <- list.dirs(file.path(subject$pipeline_path, pipeline_name),
                          full.names = FALSE, recursive = FALSE)
        selected <- character(0L)

        if(length(dirs)){
          dirs <- sort(dirs, decreasing = TRUE)
          selected <- dirs[[1]]
        }

        shiny::updateSelectInput(
          session = session,
          inputId = comp$get_sub_element_id("load_name", FALSE),
          choices = dirs,
          selected = selected
        )

      }),
      comp$get_sub_element_input("load_subject"),
      ignoreInit = TRUE, ignoreNULL = TRUE
    )



    shiny::bindEvent(
      observe({
        shidashi::clear_notifications(class = "rave-notification-load-pipeline")

        tryCatch({

          # get pipeline subject (might be different than the existing loaded subject)
          remote_subject_code <- comp$get_sub_element_input("load_subject")
          name <- comp$get_sub_element_input("load_name")

          repository <- get_repo()
          if(inherits(repository, "rave_repository")){
            project <- repository$project
            project_name <- project$name
            subject <- repository$subject
            subject_code <- subject$subject_code
          } else {
            project_name <- loader_project$current_value
            project <- raveio::as_rave_project(project_name, strict = FALSE)
            subject_code <- loader_subject$current_value
            if(!isTRUE(project$has_subject(subject_code))){
              project_name <- loader_project$get_settings_value()
              subject_code <- loader_subject$get_settings_value()
            }
            subject <- raveio::RAVESubject$new(project_name = project_name,
                                               subject_code = subject_code,
                                               strict = FALSE)
            project <- subject$project
          }


          remote_subject <- raveio::RAVESubject$new(project_name = project_name,
                                                    subject_code = remote_subject_code,
                                                    strict = FALSE)


          remote_pipeline_path <- file.path(remote_subject$pipeline_path, pipeline_name, name)
          if(!length(remote_pipeline_path)){
            stop("No pipeline has been chosen")
          }
          if(!isTRUE(dir.exists(remote_pipeline_path))){
            stop("Hmm... I can't find the pipeline: \n ", remote_pipeline_path)
          }

          remote_settings_path <- file.path(remote_pipeline_path, settings_filename)
          current_settings_path <- file.path(pipeline_path, settings_filename)

          settings <- raveio::load_yaml(current_settings_path)
          remote_settings <- raveio::load_yaml(remote_settings_path)
          nms <- names(remote_settings)

          if(fork_mode == "exclude") {
            nms <- nms[!nms %in% c(settings_entries, loader_project$varname,
                                   loader_subject$varname, "")]
          } else {
            nms <- nms[
              nms %in% settings_entries &
                !nms %in% c(loader_project$varname, loader_subject$varname, "")
            ]
          }

          shiny::showModal(
            shiny::modalDialog(
              title = "Import Pipeline", size = 'l', easyClose = FALSE,
              shiny::p(
                "The pipeline is located. Please choose variables to import from the settings file. Leave it blank to import all."
              ),
              shinyWidgets::pickerInput(
                inputId = comp$get_sub_element_id(
                  "import_varnames",
                  with_namespace = TRUE
                ), label = NULL,
                choices = sort(nms), multiple = TRUE, selected = character(),
                options = list(
                  "live-search" = TRUE,
                  "actions-box" = TRUE
                )
              ),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                dipsaus::actionButtonStyled(
                  comp$get_sub_element_id(
                    "import_confirmed",
                    with_namespace = TRUE
                  ),
                  label = "Confirm"
                )
              )
            )
          )

        }, error = function(e){
          shidashi::show_notification(
            title = "Loading pipeline",
            type = "danger",
            message = c(
              "Error found while trying to load the pipeline:",
              e$message
            ),
            close = TRUE, autohide = TRUE, fixed = TRUE,
            class = "rave-notification-load-pipeline",
            collapse = "\n"
          )
        })

      }),
      comp$get_sub_element_input("load_btn"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


    shiny::bindEvent(
      observe({
        shidashi::clear_notifications(class = "rave-notification-load-pipeline")
        tryCatch({
          # get pipeline subject (might be different than the existing loaded subject)
          remote_subject_code <- comp$get_sub_element_input("load_subject")
          name <- comp$get_sub_element_input("load_name")
          repository <- get_repo()
          if(inherits(repository, "rave_repository")){
            project <- repository$project
            project_name <- project$name
            subject <- repository$subject
            subject_code <- subject$subject_code
          } else {
            project_name <- loader_project$current_value
            project <- raveio::as_rave_project(project_name, strict = FALSE)
            subject_code <- loader_subject$current_value
            if(!isTRUE(project$has_subject(subject_code))){
              project_name <- loader_project$get_settings_value()
              subject_code <- loader_subject$get_settings_value()
            }
            subject <- raveio::RAVESubject$new(project_name = project_name,
                                               subject_code = subject_code,
                                               strict = FALSE)
            project <- subject$project
          }


          remote_subject <- raveio::RAVESubject$new(project_name = project_name,
                                                    subject_code = remote_subject_code,
                                                    strict = FALSE)


          remote_pipeline_path <- file.path(remote_subject$pipeline_path, pipeline_name, name)
          if(!length(remote_pipeline_path)){
            stop("No pipeline has been chosen")
          }
          if(!isTRUE(dir.exists(remote_pipeline_path))){
            stop("Hmm... I can't find the pipeline: \n ", remote_pipeline_path)
          }

          remote_settings_path <- file.path(remote_pipeline_path, settings_filename)
          current_settings_path <- file.path(pipeline_path, settings_filename)

          settings <- raveio::load_yaml(current_settings_path)
          remote_settings <- raveio::load_yaml(remote_settings_path)
          nms <- names(remote_settings)
          selected <- comp$get_sub_element_input("import_varnames")
          if(!length(selected)){
            if(fork_mode == "exclude") {
              nms <- nms[!nms %in% c(settings_entries, loader_project$varname,
                                     loader_subject$varname, "")]
            } else {
              nms <- nms[
                nms %in% settings_entries &
                  !nms %in% c(loader_project$varname, loader_subject$varname, "")
              ]
            }
          } else {
            nms <- nms[nms %in% selected]
          }

          dipsaus::shiny_alert2(
            title = "Loading pipeline", icon = "info", danger_mode = FALSE,
            auto_close = FALSE, buttons = FALSE, session = session,
            text = "Trying to import the pipeline settings and prepare the repository. This may take a while, depending on your hard-drive speed and whether the data has been cached."
          )
          if(length(nms)) {
            dipsaus::list_to_fastmap2(remote_settings[nms], map = settings)
          }
          raveio::save_yaml(settings, current_settings_path)
          logger("Pipeline imported with settings file:\n",
                 paste(readLines(current_settings_path), collapse = "\n"),
                 level = "debug")

          comp$container$reset_data()

          # try to get project and subject from the pipeline settings
          logger("Re-run pipeline... This shouldn't take long if there is no bug...", level = "trace")
          results <- raveio::pipeline_run(
            pipe_dir = pipeline_path,
            scheduler = "none", type = "smart",
            names = pipeline_repository, async = FALSE
          )

          results$promise$then(
            onFulfilled = function(...){
              new_repository <- raveio::pipeline_read(pipeline_repository, pipe_dir = pipeline_path)
              comp$container$data$repository <- new_repository

              ravedash::fire_rave_event('data_loaded', list(
                force = TRUE,
                timestamp = Sys.time()
              ))

              shiny::removeModal()

              dipsaus::close_alert2()
              dipsaus::shiny_alert2(
                title = "Loaded!", icon = "success", danger_mode = FALSE,
                auto_close = TRUE, buttons = NULL,
                text = "The pipeline repository has been loaded and re-generated."
              )
            },
            onRejected = function(e){
              dipsaus::close_alert2()
              dipsaus::shiny_alert2(
                title = "Loading error!", icon = "error", danger_mode = TRUE,
                auto_close = TRUE, buttons = NULL,
                text = paste(
                  "An error found while trying to load the pipeline repository:\n",
                  e$message
                )
              )
              ravedash::fire_rave_event('data_loaded', FALSE)
              logger_error_condition(e)
            }
          )

        }, error = function(e){
          shidashi::show_notification(
            title = "Loading pipeline",
            type = "danger",
            message = c(
              "Error found while trying to load the pipeline:",
              e$message
            ),
            close = TRUE, autohide = TRUE, fixed = TRUE,
            class = "rave-notification-load-pipeline",
            collapse = "\n"
          )
        })
      }),
      comp$get_sub_element_input("import_confirmed"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


  }

  comp
}
