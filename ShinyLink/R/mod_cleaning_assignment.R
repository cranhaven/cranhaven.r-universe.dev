#' cleaning_assignment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_assignment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Assign Variables",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      helpText(
        "Identify which variables correspond to each piece of information"
      )
    ),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(6, fileInput(ns("load_config_dfA"), NULL, buttonLabel = "Load Settings", placeholder = "Select configuration file to proceed"), accept = ".json"),
          column(6, downloadButton(ns("save_config_dfA"), "Download Current Settings as JSON format"))
        ),
        fluidRow(
          column(
            width = 6,
            fluidRow(
              column(width = 7, selectInput(
                ns("firstname_dfA"),
                "Firstname:",
                choices = NULL
              )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("firstname_dfA_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            fluidRow(
              column(width = 7, selectInput(
                ns("middlename_dfA"),
                "Middlename",
                choices = NULL
              )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("middlename_dfA_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            fluidRow(
              column(width = 7,
                     selectInput(
                       ns("lastname_dfA"),
                       "Lastname",
                       choices = NULL
                     )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("lastname_dfA_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            selectInput(ns("birthday_dfA"),
                        "Birthday",
                        choices = NULL),
            selectInput(ns("race_dfA"),
                        "Race",
                        choices = NULL)
          ),
          column(
            width = 6,
            selectInput(ns("sex_dfA"),
                        "Sex",
                        choices = NULL),
            selectInput(ns("housenum_dfA"),
                        "Housenum",
                        choices = NULL),
            selectInput(ns("streetname_dfA"),
                        "Streetname",
                        choices = NULL),
            selectInput(ns("city_dfA"),
                        "City",
                        choices = NULL),
            selectInput(ns("SSN_dfA"),
                        "SSN",
                        choices = NULL)
          )
        )
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(6, fileInput(ns("load_config_dfB"), NULL, buttonLabel = "Load Settings", placeholder = "Select configuration file to proceed"), accept = ".json"),
          column(6, downloadButton(ns("save_config_dfB"), "Download Current Settings as JSON format"))
        ),
        fluidRow(
          column(
            width = 6,
            fluidRow(
              column(width = 7, selectInput(
                ns("firstname_dfB"),
                "Firstname:",
                choices = NULL
              )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("firstname_dfB_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            fluidRow(
              column(width = 7, selectInput(
                ns("middlename_dfB"),
                "Middlename",
                choices = NULL
              )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("middlename_dfB_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            fluidRow(
              column(width = 7,
                     selectInput(
                       ns("lastname_dfB"),
                       "Lastname",
                       choices = NULL
                     )),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = ns("lastname_dfB_format"),
                  label = "Format",
                  choices = c("D", "U", "L"),
                  status = "primary"
                )
              )
            ),
            selectInput(ns("birthday_dfB"),
                        "Birthday",
                        choices = NULL),
            selectInput(ns("race_dfB"),
                        "Race",
                        choices = NULL)
          ),
          column(
            width = 6,
            selectInput(ns("sex_dfB"),
                        "Sex",
                        choices = NULL),
            selectInput(ns("housenum_dfB"),
                        "Housenum",
                        choices = NULL),
            selectInput(ns("streetname_dfB"),
                        "Streetname",
                        choices = NULL),
            selectInput(ns("city_dfB"),
                        "City",
                        choices = NULL),
            selectInput(ns("SSN_dfB"),
                        "SSN",
                        choices = NULL)
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
        column(12, DT::dataTableOutput(ns('assigned_dfA'), width = "100%"))
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
        column(12, DT::dataTableOutput(ns('assigned_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_duplicate"),
          label = "Previous: Remove Duplicate Rows",
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
          inputId = ns("next_gender_race"),
          label = "Next: Recode Race & Gender",
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

#' cleaning_assignment Server Functions
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom jsonlite toJSON read_json
#'
#' @noRd
mod_cleaning_assignment_server <- function(id, state, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # library(magrittr)
    # pipe operator friendly set column names function

    # load_config_dfA
    # save_config_dfA

    # Save User configs -------------------------------------------------------
    config_data_dfA <- reactive(
      list(
        firstname_dfA = input$firstname_dfA,
        middlename_dfA = input$middlename_dfA,
        lastname_dfA = input$lastname_dfA,
        housenum_dfA = input$housenum_dfA,
        streetname_dfA = input$streetname_dfA,
        city_dfA = input$city_dfA,
        SSN_dfA = input$SSN_dfA,
        birthday_dfA = input$birthday_dfA,
        race_dfA = input$race_dfA,
        sex_dfA = input$sex_dfA,
        firstname_dfA_format = input$firstname_dfA_format,
        middlename_dfA_format = input$middlename_dfA_format,
        lastname_dfA_format = input$lastname_dfA_format
      ))

    config_data_dfB <- reactive(
      list(
        firstname_dfB = input$firstname_dfB,
        middlename_dfB = input$middlename_dfB,
        lastname_dfB = input$lastname_dfB,
        housenum_dfB = input$housenum_dfB,
        streetname_dfB = input$streetname_dfB,
        city_dfB = input$city_dfB,
        SSN_dfB = input$SSN_dfB,
        birthday_dfB = input$birthday_dfB,
        race_dfB = input$race_dfB,
        sex_dfB = input$sex_dfB,
        firstname_dfB_format = input$firstname_dfB_format,
        middlename_dfB_format = input$middlename_dfB_format,
        lastname_dfB_format = input$lastname_dfB_format
      ))

    output$save_config_dfA <- downloadHandler(
      filename = function() {
        paste0("sample-assign-var-config-", Sys.time(), ".json")
      },
      content = function(file) {
        write(toJSON(config_data_dfA()), file)
      }
    )

    output$save_config_dfB <- downloadHandler(
      filename = function() {
        paste0("matching-assign-var-config-", Sys.time(), ".json")
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
      req(state$dfA_cleaned_duplicate)
      loaded_config_data <- config_dfA()

      data_dfA <- state$dfA_cleaned_duplicate

      colnames_dfA <- colnames(data_dfA)
      # set the label and select items
      updateSelectInput(session, "firstname_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['firstname_dfA']])
      updateSelectInput(session, "middlename_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['middlename_dfA']])
      updateSelectInput(session, "lastname_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['lastname_dfA']])
      updateSelectInput(session, "housenum_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['housenum_dfA']])
      updateSelectInput(session, "streetname_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['streetname_dfA']])
      updateSelectInput(session, "city_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['city_dfA']])
      updateSelectInput(session, "SSN_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['SSN_dfA']])
      updateSelectInput(session, "birthday_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['birthday_dfA']])
      updateSelectInput(session, "race_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['race_dfA']])
      updateSelectInput(session, "sex_dfA",
                        choices = c('', colnames_dfA),
                        selected = loaded_config_data[['sex_dfA']])


      updateRadioGroupButtons(
        session = session,
        inputId = "firstname_dfA_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['firstname_dfA_format']],
        status = "primary"
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "middlename_dfA_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['middlename_dfA_format']],
        status = "primary"
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "lastname_dfA_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['lastname_dfA_format']],
        status = "primary"
      )

    })

    observeEvent(input$load_config_dfB, {
      req(state$dfB_cleaned_duplicate)
      loaded_config_data <- config_dfB()

      data_dfB <- state$dfB_cleaned_duplicate

      colnames_dfB <- colnames(data_dfB)
      # set the label and select items
      updateSelectInput(session, "firstname_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['firstname_dfB']])
      updateSelectInput(session, "middlename_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['middlename_dfB']])
      updateSelectInput(session, "lastname_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['lastname_dfB']])
      updateSelectInput(session, "housenum_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['housenum_dfB']])
      updateSelectInput(session, "streetname_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['streetname_dfB']])
      updateSelectInput(session, "city_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['city_dfB']])
      updateSelectInput(session, "SSN_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['SSN_dfB']])
      updateSelectInput(session, "birthday_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['birthday_dfB']])
      updateSelectInput(session, "race_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['race_dfB']])
      updateSelectInput(session, "sex_dfB",
                        choices = c('', colnames_dfB),
                        selected = loaded_config_data[['sex_dfB']])

      updateRadioGroupButtons(
        session = session,
        inputId = "firstname_dfB_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['firstname_dfB_format']],
        status = "primary"
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "middlename_dfB_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['middlename_dfB_format']],
        status = "primary"
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "lastname_dfB_format",
        label = "Format",
        choices = c("D", "U", "L"),
        selected = loaded_config_data[['lastname_dfB_format']],
        status = "primary"
      )


      # sendSweetAlert(
      #   session = session,
      #   title = "Success!",
      #   text = "Now assign values for columns using the dropdown menus in each field",
      #   type = "success"
      # )
    })

    # Function to assign variable names ---------------------------------------
    set_col_names <- function(.data,
                              firstname = NULL,
                              middlename = NULL,
                              lastname = NULL,
                              housenum = NULL,
                              streetname = NULL,
                              city = NULL,
                              SSN = NULL,
                              birthday = NULL,
                              race = NULL,
                              sex = NULL) {
      # message(str(.data))
      if (!is.null(firstname) && firstname != "") {
        .data <- dplyr::rename(.data, firstname = firstname)
      }
      if (!is.null(middlename) && middlename != "") {
        .data <- dplyr::rename(.data, middlename = middlename)
      }
      if (!is.null(lastname) && lastname != "") {
        .data <- dplyr::rename(.data, lastname = lastname)
      }
      if (!is.null(housenum) && housenum != "") {
        .data <- dplyr::rename(.data, housenum = housenum)
      }
      if (!is.null(streetname) && streetname != "") {
        .data <- dplyr::rename(.data, streetname = streetname)
      }
      if (!is.null(city) && city != "") {
        .data <- dplyr::rename(.data, city = city)
      }
      if (!is.null(SSN) && SSN != "") {
        .data <- dplyr::rename(.data, SSN = SSN)
      }
      if (!is.null(birthday) && birthday != "") {
        .data <- dplyr::rename(.data, birthday = birthday)
      }
      if (!is.null(race) && race != "") {
        .data <- dplyr::rename(.data, race = race)
      }
      if (!is.null(sex) && sex != "") {
        .data <- dplyr::rename(.data, sex = sex)
      }
      .data
    }

    observe({
      req(state$dfA_cleaned_duplicate)
      req(state$dfB_cleaned_duplicate)

      data_dfA <- state$dfA_cleaned_duplicate

      colnames_dfA <- colnames(data_dfA)
      # set the label and select items
      updateSelectInput(session, "firstname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "middlename_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "lastname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "housenum_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "streetname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "city_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "SSN_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "birthday_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "race_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "sex_dfA",
                        choices = c('', colnames_dfA))

      data_dfB <- state$dfB_cleaned_duplicate

      colnames_dfB <- colnames(data_dfB)
      # set the label and select items
      updateSelectInput(session, "firstname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "middlename_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "lastname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "housenum_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "streetname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "city_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "SSN_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "birthday_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "race_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "sex_dfB",
                        choices = c('', colnames_dfB))

      # sendSweetAlert(
      #   session = session,
      #   title = "Success!",
      #   text = "Now assign values for columns using the dropdown menus in each field",
      #   type = "success"
      # )
    })

    assigned_dataset_a <- reactive({
      req(state$dfA_cleaned_duplicate)

      data <- state$dfA_cleaned_duplicate %>% set_col_names(
        firstname = input$firstname_dfA,
        middlename = input$middlename_dfA,
        lastname = input$lastname_dfA,
        housenum = input$housenum_dfA,
        streetname = input$streetname_dfA,
        city = input$city_dfA,
        SSN = input$SSN_dfA,
        birthday = input$birthday_dfA,
        race = input$race_dfA,
        sex = input$sex_dfA
      )


      if (input$firstname_dfA %not_in% c(NULL, "")) {
        if (input$firstname_dfA_format == "U") {
          data$firstname <- toupper(data$firstname)
        } else if (input$firstname_dfA_format == "L") {
          data$firstname <- tolower(data$firstname)
        }
      }
      if (input$middlename_dfA %not_in% c(NULL, "")) {
        if (input$middlename_dfA_format == "U") {
          data$middlename <- toupper(data$middlename)
        } else if (input$middlename_dfA_format == "L") {
          data$middlename <- tolower(data$middlename)
        }
      }
      if (input$lastname_dfA %not_in% c(NULL, "")) {
        if (input$lastname_dfA_format == "U") {
          data$lastname <- toupper(data$lastname)
        } else if (input$lastname_dfA_format == "L") {
          data$lastname <- tolower(data$lastname)
        }
      }

      state$dfA_cleaned_assignment <- data # update state
      state$state_dfA <- data # update state

      return(data)
    })

    assigned_dataset_b <- reactive({
      req(state$dfB_cleaned_duplicate)

      data <- state$dfB_cleaned_duplicate %>% set_col_names(
        firstname = input$firstname_dfB,
        middlename = input$middlename_dfB,
        lastname = input$lastname_dfB,
        housenum = input$housenum_dfB,
        streetname = input$streetname_dfB,
        city = input$city_dfB,
        SSN = input$SSN_dfB,
        birthday = input$birthday_dfB,
        race = input$race_dfB,
        sex = input$sex_dfB
      )

      if (input$firstname_dfB %not_in% c(NULL, "")) {
        if (input$firstname_dfB_format == "U") {
          data$firstname <- toupper(data$firstname)
        } else if (input$firstname_dfB_format == "L") {
          data$firstname <- tolower(data$firstname)
        }
      }

      if (input$middlename_dfB %not_in% c(NULL, "")) {
        if (input$middlename_dfB_format == "U") {
          data$middlename <- toupper(data$middlename)
        } else if (input$middlename_dfB_format == "L") {
          data$middlename <- tolower(data$middlename)
        }
      }

      if (input$lastname_dfB %not_in% c(NULL, "")) {
        if (input$lastname_dfB_format == "U") {
          data$lastname <- toupper(data$lastname)
        } else if (input$lastname_dfB_format == "L") {
          data$lastname <- tolower(data$lastname)
        }
      }

      state$dfB_cleaned_assignment <- data # update state
      state$state_dfB <- data # update state

      return(data)
    })


    output$assigned_dfA <- DT::renderDataTable(
      assigned_dataset_a(),
      #  caption = '',
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
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Assigned Sample Data"),
              list(extend = 'excel', filename = "Assigned Sample Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$assigned_dfB <- DT::renderDataTable(
      assigned_dataset_b(),
      #  caption = '',
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
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Assigned Matching Data"),
              list(extend = 'excel', filename = "Assigned Matching Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Previous page button redirection
    observeEvent(input$previous_duplicate, {
      updateTabItems(session = parent, "tabs", "duplicate")
    })

    # Next page button redirection
    observeEvent(input$next_gender_race, {
      updateTabItems(session = parent, "tabs", "gender_race")
    })
  })
}

## To be copied in the UI
# mod_cleaning_assignment_ui("cleaning_assignment_1")

## To be copied in the server
# mod_cleaning_assignment_server("cleaning_assignment_1")
