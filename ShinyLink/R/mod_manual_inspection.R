#' manual_inspection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manual_inspection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Manual Inspection",
      status = "success",
      actionButton(ns("show"), "Show Manual Inspection dialog", class = "btn-primary")
    ),

    box(
      width = 8,
      title = "Data Zone",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      fluidRow(
        column(
          width = 6,
          textInput(ns("level"), "Level of Match:"),

          textInput(ns("firstname_dfA"), "Firstname:"),
          textInput(ns("firstname_dfB"), NULL),

          textInput(ns("middlename_dfA"), "Middlename"),
          textInput(ns("middlename_dfB"), NULL),

          textInput(ns("lastname_dfA"), "Lastname"),
          textInput(ns("lastname_dfB"), NULL),

          textInput(ns("birthday_dfA"), "Birthday"),
          textInput(ns("birthday_dfB"), NULL),

          textInput(ns("race_dfA"), "Race"),
          textInput(ns("race_dfB"), NULL),

        ),
        column(
          width = 6,
          textInput(ns("decision"), "Decision:"),

          textInput(ns("sex_dfA"), "Sex"),
          textInput(ns("sex_dfB"), NULL),

          textInput(ns("housenum_dfA"), "Housenum"),
          textInput(ns("housenum_dfB"), NULL),

          textInput(ns("streetname_dfA"), "Streetname"),
          textInput(ns("streetname_dfB"), NULL),

          textInput(ns("city_dfA"), "City"),
          textInput(ns("city_dfB"), NULL),

          textInput(ns("SSN_dfA"), "SSN"),
          textInput(ns("SSN_dfB"), NULL)
        )
      )
    ),
    box(
      width = 4,
      title = "Control Zone",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      htmlOutput(ns("current_review_text")),
      br(),
      actionButton(ns("jump_to"), "Jump to Page", icon = icon("compass"), class = "btn-info", width = "150px"),
      numericInput(ns("page_number"), "", 1, width = "150px"),


      br(),
      strong('Make your decision:'),
      br(),
      br(),

      fluidRow(
        column(
        width = 4,
        actionButton(ns("diff_person"),"Different Person", icon = icon("minus"), class = "btn-danger", width = "150px"),
        br(),
        br(),

        actionButton(ns("same_person"),"Same Person", icon = icon("plus"), class = "btn-success", width = "150px"),
        br(),
        br(),

        actionButton(ns("undecided_person"),"Undecided", icon = icon("question"), class = "btn-warning", width = "150px")
        )),
      br(),
      strong('Your decision for this possible match will be saved in the variable called "Decision"'),
      br(),
      br(),

      fluidRow(
        column(width = 6,
               actionButton(ns("previous_row"),"Previous Row", icon = icon("chevron-left"), class = "btn-info", width = "150px")
        ),
        column(width = 6,
               actionButton(ns("next_row"),"Next Row", icon = icon("chevron-right"), class = "btn-info", width = "150px")
        )
      )
    ),

    box(
      width = 12,
      title = "Uncertain Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('uncertainty_matches')))
    ),
    box(
      width = 12,
      title = "Manually Confirmed Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('confirmed_matches')))
    ),
    box(
      width = 12,
      title = "Manually Confirmed Non-Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('confirmed_non_matches')))
    ),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_simple_details"),
          label = "Previous: Simple Match Details",
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
        downloadBttn(
          outputId = ns("next_download_all"),
          label = "Next: Download All Files",
          style = "simple",
          color = "success",
          icon = shiny::icon("download"),
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

#' manual_inspection Server Functions
#'
#' @noRd
mod_manual_inspection_server <- function(id, state, parent) {
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("Package 'shinyjs' is required. Please install it.")
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    update_review_data <- function(level = NULL, decision = NULL,
                                   firstname_dfA = NULL, firstname_dfB = NULL,
                                   middlename_dfA = NULL, middlename_dfB = NULL,
                                   lastname_dfA = NULL, lastname_dfB = NULL,
                                   birthday_dfA = NULL, birthday_dfB = NULL,
                                   race_dfA = NULL, race_dfB = NULL,
                                   sex_dfA = NULL, sex_dfB = NULL,
                                   housenum_dfA = NULL, housenum_dfB = NULL,
                                   streetname_dfA = NULL, streetname_dfB = NULL,
                                   city_dfA = NULL, city_dfB = NULL,
                                   SSN_dfA = NULL, SSN_dfB = NULL) {

      updateTextInput(session, "level", "Level of Match:", level)
      updateTextInput(session, "decision", "Decision:", decision)

      updateTextInput(session, "firstname_dfA", "Firstname", firstname_dfA)
      updateTextInput(session, "firstname_dfB", NULL, firstname_dfB)
      if (!is.null(firstname_dfA) && !is.null(firstname_dfB)) {
        if (
          !is.na(firstname_dfA) &&
          !is.na(firstname_dfB) &&
          firstname_dfA != firstname_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(firstname_dfA) && !is.na(firstname_dfB)) | (!is.na(firstname_dfA) && is.na(firstname_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-firstname_dfB').style.backgroundColor = ''"))
        }
      }

      updateTextInput(session, "middlename_dfA", "Middlename", middlename_dfA)
      updateTextInput(session, "middlename_dfB", NULL, middlename_dfB)

      if (!is.null(middlename_dfA) && !is.null(middlename_dfB)){
        if (
          !is.na(middlename_dfA) &&
          !is.na(middlename_dfB) &&
          middlename_dfA != middlename_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(middlename_dfA) && !is.na(middlename_dfB)) | (!is.na(middlename_dfA) && is.na(middlename_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-middlename_dfB').style.backgroundColor = ''"))
        }
      }


      updateTextInput(session, "lastname_dfA", "Lastname", lastname_dfA)
      updateTextInput(session, "lastname_dfB", NULL, lastname_dfB)
        if(!is.null(lastname_dfA) && !is.null(lastname_dfB)) {
          if (!is.na(lastname_dfA) &&
              !is.na(lastname_dfB) &&
              lastname_dfA != lastname_dfB) {
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfA').style.backgroundColor = 'yellow'"))
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfB').style.backgroundColor = 'yellow'"))
          } else if ((is.na(lastname_dfA) && !is.na(lastname_dfB)) | (!is.na(lastname_dfA) && is.na(lastname_dfB))) {
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfA').style.backgroundColor = 'yellow'"))
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfB').style.backgroundColor = 'yellow'"))
          } else {
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfA').style.backgroundColor = ''"))
            shinyjs::runjs(paste0("document.getElementById('manual_inspection-lastname_dfB').style.backgroundColor = ''"))
          }
        }


      updateTextInput(session, "birthday_dfA", "Birthday", birthday_dfA)
      updateTextInput(session, "birthday_dfB", NULL, birthday_dfB)
      if (!is.null(birthday_dfA) && !is.null(birthday_dfB)) {
        if (
          !is.na(birthday_dfA) &&
          !is.na(birthday_dfB) &&
          birthday_dfA != birthday_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(birthday_dfA) && !is.na(birthday_dfB)) | (!is.na(birthday_dfA) && is.na(birthday_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-birthday_dfB').style.backgroundColor = ''"))
        }
      }


      updateTextInput(session, "race_dfA", "Race", race_dfA)
      updateTextInput(session, "race_dfB", NULL, race_dfB)
      if (!is.null(race_dfA) && !is.null(race_dfB)) {
        if (
          !is.na(race_dfA) &&
          !is.na(race_dfB) &&
          race_dfA != race_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(race_dfA) && !is.na(race_dfB)) | (!is.na(race_dfA) && is.na(race_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-race_dfB').style.backgroundColor = ''"))
        }
      }


      updateTextInput(session, "sex_dfA", "Sex", sex_dfA)
      updateTextInput(session, "sex_dfB", NULL, sex_dfB)
      if (!is.null(sex_dfA) && !is.null(sex_dfB)) {
        if (
          !is.na(sex_dfA) &&
          !is.na(sex_dfB) &&
          sex_dfA != sex_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(sex_dfA) && !is.na(sex_dfB)) | (!is.na(sex_dfA) && is.na(sex_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-sex_dfB').style.backgroundColor = ''"))
        }
      }


      updateTextInput(session, "housenum_dfA", "Housenum", housenum_dfA)
      updateTextInput(session, "housenum_dfB", NULL, housenum_dfB)
      if (!is.null(housenum_dfA) && !is.null(housenum_dfB)) {
        if (
          !is.na(housenum_dfA) &&
          !is.na(housenum_dfB) &&
          housenum_dfA != housenum_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(housenum_dfA) && !is.na(housenum_dfB)) | (!is.na(housenum_dfA) && is.na(housenum_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-housenum_dfB').style.backgroundColor = ''"))
        }
      }

      updateTextInput(session, "streetname_dfA", "Streetname", streetname_dfA)
      updateTextInput(session, "streetname_dfB", NULL, streetname_dfB)

      if (!is.null(streetname_dfA) && !is.null(streetname_dfB)) {
        if (
          !is.na(streetname_dfA) &&
          !is.na(streetname_dfB) &&
          streetname_dfA != streetname_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(streetname_dfA) && !is.na(streetname_dfB)) | (!is.na(streetname_dfA) && is.na(streetname_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-streetname_dfB').style.backgroundColor = ''"))
        }
      }

      updateTextInput(session, "city_dfA", "City", city_dfA)
      updateTextInput(session, "city_dfB", NULL, city_dfB)

      if (!is.null(city_dfA) && !is.null(city_dfB)) {
        if (
          !is.na(city_dfA) &&
          !is.na(city_dfB) &&
          city_dfA != city_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(city_dfA) && !is.na(city_dfB)) | (!is.na(city_dfA) && is.na(city_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-city_dfB').style.backgroundColor = ''"))
        }
      }

      updateTextInput(session, "SSN_dfA", "SSN", SSN_dfA)
      updateTextInput(session, "SSN_dfB", NULL, SSN_dfB)
      if (!is.null(SSN_dfA) && !is.null(SSN_dfB)) {
        if (
          !is.na(SSN_dfA) &&
          !is.na(SSN_dfB) &&
          SSN_dfA != SSN_dfB) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfB').style.backgroundColor = 'yellow'"))
        } else if ((is.na(SSN_dfA) && !is.na(SSN_dfB)) | (!is.na(SSN_dfA) && is.na(SSN_dfB))) {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfA').style.backgroundColor = 'yellow'"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfB').style.backgroundColor = 'yellow'"))
        } else {
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfA').style.backgroundColor = ''"))
          shinyjs::runjs(paste0("document.getElementById('manual_inspection-SSN_dfB').style.backgroundColor = ''"))
        }
      }


      if (decision == "Undecided") {
        color <- "solid orange"
      }
      if (decision == "Same Person") {
        color <- "solid green"
      }
      if (decision == "Different Person") {
        color <- "solid red"
      }
      shinyjs::runjs(paste0("document.getElementById('manual_inspection-decision').style.border = '", color, "'"))

    }

    # reactiveValues object for storing current data
    vals <- reactiveValues(current_reviewing = 1,
                           choose_level = 3,
                           uncertain_dfs = NULL, # 7
                           certain_dfs = NULL, # 30
                           color = "")

    # Update Decision color based on value
    observe({
      # In `testServer()` runs, the UI may not be initialized, so `input$decision`
      # can be NULL. Guard to avoid "argument is of length zero" in `if()`.
      if (is.null(input$decision) || identical(input$decision, "")) {
        return()
      }
      if (input$decision == "Undecided") {
        vals$color <- "solid yellow"
      }
      if (input$decision == "Same Person") {
        vals$color <- "solid green"
      }
      if (input$decision == "Different Person") {
        vals$color <- "solid red"
      }
    })

    output$current_review_text <- renderText({
      current_review_text <-
        paste0(
          "Match #",
          vals$current_reviewing,
          " of ",
          nrow(vals$uncertain_dfs),
          " to review"
        )
      strong_text <- paste0("<b><span style='color:black;'>", current_review_text, "</span></b>")
      HTML(strong_text)
    })

    level_statistics <- reactive({
      # Production mode
      matched_dfs_review <- state$matched_results$matched_dfs_review

      tibble::tibble(
        `Level 1` = sum(matched_dfs_review$match_level == 1),
        `Level 2` = sum(matched_dfs_review$match_level == 2),
        `Level 3` = sum(matched_dfs_review$match_level == 3),
        `Level 4` = sum(matched_dfs_review$match_level == 4),
        `Level 5` = sum(matched_dfs_review$match_level == 5),
        `Level 6` = sum(matched_dfs_review$match_level == 6)
      )
    })

    output$n_levels_table <- renderTable({
      level_statistics()
    }, align = "c", striped = TRUE, spacing = "s")

    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {

      # load the namespace
      ns <- session$ns

      # build the modal
      modalDialog(

        # make sure to wrap all id tokens in ns()
        # textInput(ns("dataset"), "Choose data set",
        #           placeholder = 'Try "mtcars" or "abc"'),

        hr(),

        strong("Here are the numbers of matches found in each level:"),

        tableOutput(ns("n_levels_table")),

        hr(),

        radioButtons(ns("choose_level"), "Up to which level would you like to review?",
                     c("Level 1 Certainty of Match: EXACT MATCH" = 1,
                       "Level 2 Certainty of Match: VERY HIGH" = 2,
                       "Level 3 Certainty of Match: HIGH" = 3,
                       "Level 4 Certainty of Match: FAIRLY-HIGH" = 4,
                       "Level 5 Certainty of Match: MODERATE" = 5,
                       "Level 6 Certainty of Match: LOW" = 6),
                     2),
        hr(),

        radioButtons(ns("choose_order"), "In what order would you like to review?",
                     c("LOW to HIGH" = "ascend",
                       "HIGH to LOW" = "descend")),

        # span('(Try the name of a valid data object like "mtcars", ',
        #      'then a name of a non-existent object like "abc")'),

        if (failed)
          div(tags$b("ZERO records to review. Try selecting a higher match level.", style = "color: red;")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK") # wrapped in ns()
        )

      )
    }

    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(dataModal())
    })

    observeEvent(input$ok, {

      # print(input$dataset)

      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      # Check that data object exists and is data frame.
      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {

        vals$choose_level <- input$choose_level

        vals$uncertain_dfs <-
          matched_dfs_review %>% dplyr::filter(match_level >= vals$choose_level)
        vals$uncertain_dfs[["Decision"]] <- "Undecided"

        vals$certain_dfs <-
          matched_dfs_review %>% dplyr::filter(match_level < vals$choose_level)
        vals$certain_dfs[["Decision"]] <- "Same Person"

        vals$current_reviewing <- 1

        updateNumericInput(
          session,
          "page_number",
          label = "",
          value = 1,
          min = 1,
          max = nrow(vals$uncertain_dfs),
          step = 1
        )

        if(!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing], ]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing], ]

          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]])

          removeModal()
        } else {
          showModal(dataModal(failed = TRUE))
        }
      }
    })


    observeEvent(input$diff_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$Decision[vals$current_reviewing] <- "Different Person"

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {
            vals$current_reviewing <- vals$current_reviewing + 1
          }

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]
          # TODO after development remove this
          print(currentA)
          print(currentB)
          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]]
          )


          # dfA.unmatch_review is the fixed value from match result, will only be changed if your re-run the match.
          # dfA.unmatch is the dynamic value for display, will be dynamically changed based on manual review.
          # Same as dfB

          # Update the display version of global variable dfA.unmatch after each click
          state$matched_results[['dfA.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfA.unmatch_review']], dfA[confirmed_non_matches()$inds.a, ])
          # Update the display version of global variable dfB.unmatch after each click
          state$matched_results[['dfB.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfB.unmatch_review']], dfB[confirmed_non_matches()$inds.b, ])
          # Update the display version of global variable matched_intersect after each click
          state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
            vals$uncertain_dfs %>% dplyr::filter(Decision == "Same Person"), vals$certain_dfs)
          # Update the display version of global variable matched_union after each click
          state$matched_results[['matched_union']] <-
            dplyr::bind_rows(state$matched_results[['matched_intersect']],
                             state$matched_results[['dfA.unmatch']],
                             state$matched_results[['dfB.unmatch']])
        }
      }
    })

    observeEvent(input$same_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$Decision[vals$current_reviewing] <- "Same Person"

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {
            vals$current_reviewing <- vals$current_reviewing + 1
          }

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]]
          )

          state$matched_results[['dfA.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfA.unmatch_review']], dfA[confirmed_non_matches()$inds.a, ])
          state$matched_results[['dfB.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfB.unmatch_review']], dfB[confirmed_non_matches()$inds.b, ])

          state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
            vals$uncertain_dfs %>% dplyr::filter(Decision == "Same Person"), vals$certain_dfs)

          state$matched_results[['matched_union']] <-
            dplyr::bind_rows(state$matched_results[['matched_intersect']],
                             state$matched_results[['dfA.unmatch']],
                             state$matched_results[['dfB.unmatch']])
        }
      }
    })

    observeEvent(input$undecided_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$Decision[vals$current_reviewing] <- "Undecided"

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {
            vals$current_reviewing <- vals$current_reviewing + 1
          }

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]]
          )

          state$matched_results[['dfA.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfA.unmatch_review']], dfA[confirmed_non_matches()$inds.a, ])
          state$matched_results[['dfB.unmatch']] <-
            dplyr::bind_rows(state$matched_results[['dfB.unmatch_review']], dfB[confirmed_non_matches()$inds.b, ])

          state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
            vals$uncertain_dfs %>% dplyr::filter(Decision == "Same Person"), vals$certain_dfs)

          state$matched_results[['matched_union']] <-
            dplyr::bind_rows(state$matched_results[['matched_intersect']],
                             state$matched_results[['dfA.unmatch']],
                             state$matched_results[['dfB.unmatch']])
        }
      }
    })

    observeEvent(input$previous_row, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {
          if (vals$current_reviewing > 1) {

            vals$current_reviewing <- vals$current_reviewing - 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )
          }
        }
      }
    })

    observeEvent(input$next_row, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {
          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {

            vals$current_reviewing <- vals$current_reviewing + 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )
          }
        }
      }
    })

    observeEvent(input$jump_to, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs_review <- state$matched_results$matched_dfs_review

      if (!is.null(matched_dfs_review) && tibble::is_tibble(matched_dfs_review)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {
          vals$current_reviewing <- input$page_number

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$Decision[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]]
          )
        }
      }
    })
    uncertainty_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(Decision == "Undecided")
      } else {NULL}
    })

    output[["uncertainty_matches"]] <- DT::renderDataTable(
      uncertainty_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "uncertainty_matches"),
              list(extend = 'excel', filename = "uncertainty_matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    confirmed_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(Decision == "Same Person")
      } else {NULL}
    })

    output[["confirmed_matches"]] <- DT::renderDataTable(
      confirmed_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "confirmed_matches"),
              list(extend = 'excel', filename = "confirmed_matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    confirmed_non_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(Decision == "Different Person")
      } else {NULL}
    })

    output[["confirmed_non_matches"]] <- DT::renderDataTable(
      confirmed_non_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "confirmed_non_matches"),
              list(extend = 'excel', filename = "confirmed_non_matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Previous page button redirection
    observeEvent(input$previous_simple_details, {
      updateTabItems(session = parent, "tabs", "simple_details")
    })

    # Download all
    output$next_download_all <- downloadHandler(
      filename = function() {
        paste('All-Results-', sub(" ", "-", gsub(":", "-", Sys.time())), '.zip', sep='')
      },
      content = function(file) {

        write.csv(state$matched_results[['dfA.unmatch']], "Unique in Sample Data Set.csv", na = "")
        write.csv(state$matched_results[['dfB.unmatch']], "Unique in Matching Data Set.csv", na = "")
        write.csv(state$matched_results[['matched_intersect']], "Confirmed Matches.csv", na = "")
        write.csv(state$matched_results[['matched_union']], "Linked Data.csv", na = "")
        write.csv(uncertainty_matches(), "Uncertain Matches.csv", na = "")
        write.csv(confirmed_matches(), "Manually Confirmed Matches.csv", na = "")
        write.csv(confirmed_non_matches(), "Manually Confirmed Non-Matches.csv", na = "")

        if (!requireNamespace("zip", quietly = TRUE)) {
          stop("Package 'zip' is required for file compression. Please install it.")
        }
        zip::zip(
          file,
          files = c(
            'Unique in Sample Data Set.csv',
            'Unique in Matching Data Set.csv',
            'Confirmed Matches.csv',
            'Linked Data.csv',
            'Uncertain Matches.csv',
            'Manually Confirmed Matches.csv',
            'Manually Confirmed Non-Matches.csv'
          )
        )
      }
    )

  })
}

## To be copied in the UI
# mod_manual_inspection_ui("manual_inspection_1")

## To be copied in the server
# mod_manual_inspection_server("manual_inspection_1")

utils::globalVariables(c("match_level", "Decision"))
