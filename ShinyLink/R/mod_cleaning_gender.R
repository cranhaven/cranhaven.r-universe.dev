#' cleaning_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Gender Race Recoding",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      helpText(
        "Recode the gender and race in sample data set or matching data set."
      )
    ),
    ## Box for Sample data Recoding
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample Recoding",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(6, fileInput(ns("load_config_dfA"), NULL, buttonLabel = "Load Settings", placeholder = "Select configuration file to proceed"), accept = ".json"),
          column(6, downloadButton(ns("save_config_dfA"), "Download Current Settings as JSON format"))
        ),
        helpText("Assign values for Gender"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_male_a"),
              label = "Male Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_female_a"),
              label = "Female Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_transgender_a"),
              label = "Transgender Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        helpText("Assign values for Race/Ethnicity"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_cauc_a"),
              label = "Cauc. Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_afric_a"),
              label = "Afric Amer Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_hisp_a"),
              label = "Hispanic Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_other_a"),
              label = "Other Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_asian_a"),
              label = "Asian Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_native_a"),
              label = "Native Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_mid_a"),
              label = "Mid East Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        )
      )
    ),
    ## Box for Matching Data Recoding
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data Recoding",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(6, fileInput(ns("load_config_dfB"), NULL, buttonLabel = "Load Settings", placeholder = "Select configuration file to proceed"), accept = ".json"),
          column(6, downloadButton(ns("save_config_dfB"), "Download Current Settings as JSON format"))
        ),
        helpText("Assign values for Gender"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_male_b"),
              label = "Male Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_female_b"),
              label = "Female Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_transgender_b"),
              label = "Transgender Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        helpText("Assign values for Race/Ethnicity"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_cauc_b"),
              label = "Cauc. Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_afric_b"),
              label = "Afric Amer Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_hisp_b"),
              label = "Hispanic Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_other_b"),
              label = "Other Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_asian_b"),
              label = "Asian Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_native_b"),
              label = "Native Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_mid_b"),
              label = "Mid East Values",
              choices =NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        )
      )
    )),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(ns('gender_dfA'), width = "100%"))
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(ns('gender_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_assignment"),
          label = "Previous: Assign Variables",
          style = "simple",
          color = "primary",
          icon = icon("arrow-left"),
          size = "sm"
        ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        width = 6,
        actionBttn(
          inputId = ns("next_date_format"),
          label = "Next: Format Dates",
          style = "simple",
          color = "primary",
          icon = icon("arrow-right"),
          size = "sm"
        ),
        align = "right",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      style = "margin-left: 0px;",
      style = "margin-right: 0px;"
    )
  )
}

#' cleaning_gender Server Functions
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom jsonlite toJSON read_json
#' @noRd
mod_cleaning_gender_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

   # Save User configs -------------------------------------------------------
    config_data_dfA <- reactive(
      list(
        recoding_male_a = input$recoding_male_a,
        recoding_female_a = input$recoding_female_a,
        recoding_transgender_a = input$recoding_transgender_a,

        recoding_cauc_a = input$recoding_cauc_a,
        recoding_afric_a = input$recoding_afric_a,
        recoding_hisp_a = input$recoding_hisp_a,
        recoding_asian_a = input$recoding_asian_a,
        recoding_native_a = input$recoding_native_a,
        recoding_mid_a = input$recoding_mid_a,
        recoding_other_a = input$recoding_other_a
      ))

    config_data_dfB <- reactive(
      list(
        recoding_male_b = input$recoding_male_b,
        recoding_female_b = input$recoding_female_b,
        recoding_transgender_b = input$recoding_transgender_b,

        recoding_cauc_b = input$recoding_cauc_b,
        recoding_afric_b = input$recoding_afric_b,
        recoding_hisp_b = input$recoding_hisp_b,
        recoding_asian_b = input$recoding_asian_b,
        recoding_native_b = input$recoding_native_b,
        recoding_mid_b = input$recoding_mid_b,
        recoding_other_b = input$recoding_other_b
      ))

    output$save_config_dfA <- downloadHandler(
      filename = function() {
        paste0("sample-gender-config-", Sys.time(), ".json")
      },
      content = function(file) {
        write(toJSON(config_data_dfA()), file)
      }
    )

    output$save_config_dfB <- downloadHandler(
      filename = function() {
        paste0("matching-gender-config-", Sys.time(), ".json")
      },
      content = function(file) {
        write(toJSON(config_data_dfB()), file)
      }
    )

    # Load User configs -------------------------------------------------------
    config_dfA <- reactive({
      infile <- input$load_config_dfA
      ext <- tools::file_ext(infile$datapath)
      if (is.null(infile)) {
        return(NULL)
      }
      if (ext == "json") {
        return(read_json(infile$datapath))
        # Notification
        showNotification("User defined configuration file loaded for the Sample Data Set",
                         type = "message")
      } else {
        # If the file type is not valid, show an error message
        showModal(modalDialog(
          title = "Error",
          "Please upload a valid JSON configuration file.",
          easyClose = TRUE
        ))
      }
    })

    config_dfB <- reactive({
      infile <- input$load_config_dfB
      ext <- tools::file_ext(infile$datapath)

      if (is.null(infile)) {
        return(NULL)
      }
      if (ext == "json") {
        return(read_json(infile$datapath))
        # Notification
        showNotification("User defined configuration file loaded for the Matching Data Set",
                         type = "message")
      } else {
        # If the file type is not valid, show an error message
        showModal(modalDialog(
          title = "Error",
          "Please upload a valid JSON configuration file.",
          easyClose = TRUE
        ))
      }
    })

    observeEvent(input$load_config_dfA, {
      req(state$dfA_cleaned_assignment)
      loaded_config_data <- config_dfA()

      if (req("sex" %in% colnames(state$dfA_cleaned_assignment))) {
        gender_A <- gender_A_items()
        updateSelectInput(session, "recoding_male_a",
                          choices = c('', gender_A),
                          selected = loaded_config_data[["recoding_male_a"]])
        updateSelectInput(session, "recoding_female_a",
                          choices = c('', gender_A),
                          selected = loaded_config_data[["recoding_female_a"]])
        updateSelectInput(session, "recoding_transgender_a",
                          choices = c('', gender_A),
                          selected = loaded_config_data[["recoding_transgender_a"]])
      }
      if (req("race" %in% colnames(state$dfA_cleaned_assignment))) {
        race_A <- race_A_items()
        updateSelectInput(session, "recoding_cauc_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_cauc_a"]])
        updateSelectInput(session, "recoding_afric_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_afric_a"]])
        updateSelectInput(session, "recoding_hisp_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_hisp_a"]])
        updateSelectInput(session, "recoding_asian_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_asian_a"]])
        updateSelectInput(session, "recoding_native_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_native_a"]])
        updateSelectInput(session, "recoding_mid_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_mid_a"]])
        updateSelectInput(session, "recoding_other_a",
                          choices = c('', race_A),
                          selected = loaded_config_data[["recoding_other_a"]])
      }
    })

    observeEvent(input$load_config_dfB, {
      req(state$dfB_cleaned_assignment)
      loaded_config_data <- config_dfB()

      if (req("sex" %in% colnames(state$dfB_cleaned_assignment))) {
        gender_B <- gender_B_items()
        updateSelectInput(session, "recoding_male_b",
                          choices = c('', gender_B),
                          selected = loaded_config_data[["recoding_male_b"]])
        updateSelectInput(session, "recoding_female_b",
                          choices = c('', gender_B),
                          selected = loaded_config_data[["recoding_female_b"]])
        updateSelectInput(session, "recoding_transgender_b",
                          choices = c('', gender_B),
                          selected = loaded_config_data[["recoding_transgender_b"]])
      }
      if (req("race" %in% colnames(state$dfB_cleaned_assignment))) {
        race_B <- race_B_items()
        updateSelectInput(session, "recoding_cauc_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_cauc_b"]])
        updateSelectInput(session, "recoding_afric_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_afric_b"]])
        updateSelectInput(session, "recoding_hisp_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_hisp_b"]])
        updateSelectInput(session, "recoding_asian_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_asian_b"]])
        updateSelectInput(session, "recoding_native_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_native_b"]])
        updateSelectInput(session, "recoding_mid_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_mid_b"]])
        updateSelectInput(session, "recoding_other_b",
                          choices = c('', race_B),
                          selected = loaded_config_data[["recoding_other_b"]])
      }
    })




    # Create reactive dropdown options ----------------------------------------
    gender_A_items <- reactive({ sort(unique(state$dfA_cleaned_assignment$sex)) })
    race_A_items <- reactive({ sort(unique(state$dfA_cleaned_assignment$race)) })

    gender_B_items <- reactive({ sort(unique(state$dfB_cleaned_assignment$sex)) })
    race_B_items <- reactive({ sort(unique(state$dfB_cleaned_assignment$race)) })

    # Observe dataset and update dropdown menus when contains column " --------
    # "sex" and "race" --------------------------------------------------------
    observe({
      if (req("sex" %in% colnames(state$dfA_cleaned_assignment))) {
        gender_A <- gender_A_items()
        updateSelectInput(session, "recoding_male_a",
                          choices = c('', gender_A))
        updateSelectInput(session, "recoding_female_a",
                          choices = c('', gender_A))
        updateSelectInput(session, "recoding_transgender_a",
                          choices = c('', gender_A))
      }
      if (req("race" %in% colnames(state$dfA_cleaned_assignment))) {
        race_A <- race_A_items()
        updateSelectInput(session, "recoding_cauc_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_afric_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_hisp_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_asian_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_native_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_mid_a",
                          choices = c('', race_A))
        updateSelectInput(session, "recoding_other_a",
                          choices = c('', race_A))
      }
      if (req("sex" %in% colnames(state$dfB_cleaned_assignment))) {
        gender_B <- gender_B_items()
        updateSelectInput(session, "recoding_male_b",
                          choices = c('', gender_B))
        updateSelectInput(session, "recoding_female_b",
                          choices = c('', gender_B))
        updateSelectInput(session, "recoding_transgender_b",
                          choices = c('', gender_B))
      }
      if (req("race" %in% colnames(state$dfB_cleaned_assignment))) {
        race_B <- race_B_items()
        updateSelectInput(session, "recoding_cauc_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_afric_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_hisp_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_asian_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_native_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_mid_b",
                          choices = c('', race_B))
        updateSelectInput(session, "recoding_other_b",
                          choices = c('', race_B))
      }
    })

    # Reactive value storing selected options ---------------------------------
    gender_A_items_selected <- reactive({
      c(input[["recoding_male_a"]],
        input[["recoding_female_a"]],
        input[["recoding_transgender_a"]])
    })
    race_A_items_selected <- reactive({
      c(input[["recoding_cauc_a"]],
        input[["recoding_afric_a"]],
        input[["recoding_hisp_a"]],
        input[["recoding_asian_a"]],
        input[["recoding_native_a"]],
        input[["recoding_mid_a"]],
        input[["recoding_other_a"]])
    })
    gender_B_items_selected <- reactive({
      c(input[["recoding_male_b"]],
        input[["recoding_female_b"]],
        input[["recoding_transgender_b"]])
    })
    race_B_items_selected <- reactive({
      c(input[["recoding_cauc_b"]],
        input[["recoding_afric_b"]],
        input[["recoding_hisp_b"]],
        input[["recoding_asian_b"]],
        input[["recoding_native_b"]],
        input[["recoding_mid_b"]],
        input[["recoding_other_b"]])
    })

    # Function to update gender selection inputs ------------------------------
    update_select_input_dfA_sex <- function() {
      gender_A <- gender_A_items()
      gender_A <- gender_A[!gender_A %in% gender_A_items_selected()]
      updateSelectInput(
        session,
        "recoding_male_a",
        choices = c('', gender_A, input[["recoding_male_a"]]),
        selected = input[["recoding_male_a"]]
      )
      updateSelectInput(
        session,
        "recoding_female_a",
        choices = c('', gender_A, input[["recoding_female_a"]]),
        selected = input[["recoding_female_a"]]
      )
      updateSelectInput(
        session,
        "recoding_transgender_a",
        choices = c('', gender_A, input[["recoding_transgender_a"]]),
        selected = input[["recoding_transgender_a"]]
      )
    }

    # Function to update race selection inputs --------------------------------
    update_select_input_dfA_race <- function() {
      race_A <- race_A_items()
      race_A <- race_A[!race_A %in% race_A_items_selected()]
      updateSelectInput(
        session,
        "recoding_cauc_a",
        choices = c('', race_A, input[["recoding_cauc_a"]]),
        selected = input[["recoding_cauc_a"]]
      )
      updateSelectInput(
        session,
        "recoding_afric_a",
        choices = c('', race_A, input[["recoding_afric_a"]]),
        selected = input[["recoding_afric_a"]]
      )
      updateSelectInput(
        session,
        "recoding_hisp_a",
        choices = c('', race_A, input[["recoding_hisp_a"]]),
        selected = input[["recoding_hisp_a"]]
      )
      updateSelectInput(
        session,
        "recoding_asian_a",
        choices = c('', race_A, input[["recoding_asian_a"]]),
        selected = input[["recoding_asian_a"]]
      )
      updateSelectInput(
        session,
        "recoding_native_a",
        choices = c('', race_A, input[["recoding_native_a"]]),
        selected = input[["recoding_native_a"]]
      )
      updateSelectInput(
        session,
        "recoding_mid_a",
        choices = c('', race_A, input[["recoding_mid_a"]]),
        selected = input[["recoding_mid_a"]]
      )
      updateSelectInput(
        session,
        "recoding_other_a",
        choices = c('', race_A, input[["recoding_other_a"]]),
        selected = input[["recoding_other_a"]]
      )
    }

    # Trigger the update select input function when dropdown selection --------
    # changes
    observeEvent(input[["recoding_male_a"]], {
      update_select_input_dfA_sex()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_female_a"]], {
      update_select_input_dfA_sex()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_transgender_a"]], {
      update_select_input_dfA_sex()
    }, ignoreNULL = FALSE)

    observeEvent(input[["recoding_cauc_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_afric_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_hisp_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_asian_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_native_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_mid_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_other_a"]], {
      update_select_input_dfA_race()
    }, ignoreNULL = FALSE)

    # Function to update gender selection inputs ------------------------------
    update_select_input_dfB_sex <- function() {
      gender_B <- gender_B_items()
      gender_B <- gender_B[!gender_B %in% gender_B_items_selected()]
      updateSelectInput(
        session,
        "recoding_male_b",
        choices = c('', gender_B, input[["recoding_male_b"]]),
        selected = input[["recoding_male_b"]]
      )
      updateSelectInput(
        session,
        "recoding_female_b",
        choices = c('', gender_B, input[["recoding_female_b"]]),
        selected = input[["recoding_female_b"]]
      )
      updateSelectInput(
        session,
        "recoding_transgender_b",
        choices = c('', gender_B, input[["recoding_transgender_b"]]),
        selected = input[["recoding_transgender_b"]]
      )
    }
    # Function to update race selection inputs --------------------------------
    update_select_input_dfB_race <- function() {
      race_B <- race_B_items()
      race_B <- race_B[!race_B %in% race_B_items_selected()]
      updateSelectInput(
        session,
        "recoding_cauc_b",
        choices = c('', race_B, input[["recoding_cauc_b"]]),
        selected = input[["recoding_cauc_b"]]
      )
      updateSelectInput(
        session,
        "recoding_afric_b",
        choices = c('', race_B, input[["recoding_afric_b"]]),
        selected = input[["recoding_afric_b"]]
      )
      updateSelectInput(
        session,
        "recoding_hisp_b",
        choices = c('', race_B, input[["recoding_hisp_b"]]),
        selected = input[["recoding_hisp_b"]]
      )
      updateSelectInput(
        session,
        "recoding_asian_b",
        choices = c('', race_B, input[["recoding_asian_b"]]),
        selected = input[["recoding_asian_b"]]
      )
      updateSelectInput(
        session,
        "recoding_native_b",
        choices = c('', race_B, input[["recoding_native_b"]]),
        selected = input[["recoding_native_b"]]
      )
      updateSelectInput(
        session,
        "recoding_mid_b",
        choices = c('', race_B, input[["recoding_mid_b"]]),
        selected = input[["recoding_mid_b"]]
      )
      updateSelectInput(
        session,
        "recoding_other_b",
        choices = c('', race_B, input[["recoding_other_b"]]),
        selected = input[["recoding_other_b"]]
      )
    }

    # Trigger the update select input function when dropdown selection --------
    # changes
    observeEvent(input[["recoding_male_b"]], {
      update_select_input_dfB_sex()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_female_b"]], {
      update_select_input_dfB_sex()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_transgender_b"]], {
      update_select_input_dfB_sex()
    }, ignoreNULL = FALSE)

    observeEvent(input[["recoding_cauc_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_afric_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_hisp_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_asian_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_native_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_mid_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)
    observeEvent(input[["recoding_other_b"]], {
      update_select_input_dfB_race()
    }, ignoreNULL = FALSE)


    gender_dataset_a <- reactive({
      data <- state$dfA_cleaned_assignment

      if (req("sex" %in% colnames(state$dfA_cleaned_assignment))) {
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_male_a, "male"))
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_female_a, "female"))
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_transgender_a, "Transgender"))
      }
      if (req("race" %in% colnames(state$dfA_cleaned_assignment))) {
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_cauc_a, "Caucasian"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_afric_a, "African American"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_hisp_a, "Hispanic"))

        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_asian_a, "Asian"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_native_a, "Native"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_mid_a, "Mid East"))

        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_other_a, "Other"))
      }

      state$dfA_cleaned_gender <- data # update state

      return(data)
    })

    gender_dataset_b <- reactive({
      data <- state$dfB_cleaned_assignment

      if (req("sex" %in% colnames(state$dfB_cleaned_assignment))) {
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_male_b, "male"))
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_female_b, "female"))
        data <- data %>%
          dplyr::mutate(sex = replace(sex, sex %in% input$recoding_transgender_b, "Transgender"))
      }
      if (req("race" %in% colnames(state$dfB_cleaned_assignment))) {
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_cauc_b, "Caucasian"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_afric_b, "African American"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_hisp_b, "Hispanic"))

        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_asian_b, "Asian"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_native_b, "Native"))
        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_mid_b, "Mid East"))

        data <- data %>%
          dplyr::mutate(race = replace(race, race %in% input$recoding_other_b, "Other"))

      }

      state$dfB_cleaned_gender <- data # update state

      return(data)
    })


    output$gender_dfA <- DT::renderDataTable(
      gender_dataset_a(),
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list('copy', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "Gender Race Recoded Sample Data"),
            list(extend = 'excel', filename = "Gender Race Recoded Sample Data")),
          text = 'Download'
        ))
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$gender_dfB <- DT::renderDataTable(
      gender_dataset_b(),
      caption = 'Data in the Matching data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list('copy', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "Gender Race Recoded Matching Data"),
            list(extend = 'excel', filename = "Gender Race Recoded Matching Data")),
          text = 'Download'
        ))
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Previous page button redirection
    observeEvent(input$previous_assignment, {
      updateTabItems(session = parent, "tabs", "assignment")
    })

    # Next page button redirection
    observeEvent(input$next_date_format, {
      updateTabItems(session = parent, "tabs", "date_format")
    })
  })
}

## To be copied in the UI
# mod_cleaning_gender_ui("cleaning_gender_1")

## To be copied in the server
# mod_cleaning_gender_server("cleaning_gender_1")

utils::globalVariables(c("sex", "race"))

