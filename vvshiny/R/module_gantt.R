#' Gantt Chart Shiny App
#'
#' @param df Data frame for Gantt chart
#' @param df_config_gantt Config data frame for Gantt chart
#' @param id Module ID for Gantt chart
#'
#' @return Shiny app object
#' @export
#' @examples
#' df <- dplyr::tribble( ~OPL_Onderdeel_CROHO_examen, ~OPL_Onderdeel_CROHO_instroom,
#' ~OPL_CBS_Label_rubriek_examen, ~OPL_CBS_Label_rubriek_instroom, "GEDRAG EN MAATSCHAPPIJ",
#' "GEZONDHEIDSZORG", "sociale geografie", "(huis)arts, specialist, geneeskunde",
#' "GEDRAG EN MAATSCHAPPIJ", "GEDRAG EN MAATSCHAPPIJ", "sociale geografie", "sociale geografie",
#' "GEDRAG EN MAATSCHAPPIJ", "RECHT", "sociale geografie", "notariaat", "RECHT", "RECHT",
#' "notariaat", "notariaat", "TAAL EN CULTUUR", "RECHT", "niet westerse talen en culturen",
#' "notariaat")
#'
#' df_config_gantt <- dplyr::tribble( ~Categorie, ~Veldnaam, ~Veldnaam_gebruiker, ~input_var,
#' ~target_var, ~title_start, ~title_end, ~position_y_label, "Doorstroom vanuit B ",
#' "OPL_Onderdeel_CROHO_examen",	"B Croho sector", "OPL_Onderdeel_CROHO_examen",
#' "OPL_Onderdeel_CROHO_instroom",	"Waar stromen", "Bachelor gediplomeerden naar toe?",	"right",
#' "Doorstroom vanuit B ",	"OPL_CBS_Label_rubriek_examen",	"B ISCED-F Rubriek",
#' "OPL_CBS_Label_rubriek_examen",	"OPL_CBS_Label_rubriek_instroom",	"Waar stromen",
#' "Bachelor gediplomeerden naar toe?",	"right", "Instroom bij M", "OPL_Onderdeel_CROHO_instroom",
#' "M Croho sector", "OPL_Onderdeel_CROHO_instroom", "OPL_Onderdeel_CROHO_examen",
#' "Waarvandaan stromen ", " Master studenten in?", "left", "Instroom bij M",
#' "OPL_CBS_Label_rubriek_instroom", "M ISCED-F Rubriek", "OPL_CBS_Label_rubriek_instroom",
#' "OPL_CBS_Label_rubriek_examen", "Waarvandaan stromen ", " Master studenten in?", "left" )
gantt_app <- function(df, df_config_gantt, id = "gantt") {
  if(!exists("request")) {
    request <- NULL
  }

  #tabItems <- list(
  #  shinydashboard::tabItem(tabName = id, module_gantt_ind_ui(id, df_config_gantt))
  #)
  #test <- shinydashboard::tabItems(tabItems)

  ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_gantt_ind_ui(id, df_config_gantt)))

  server <- function(input, output, session) {
    module_gantt_ind_server(id, df, df_config_gantt)
  }

  shiny::shinyApp(ui, server)
}

## Server function for gantt chart module
##
## @param id Module id
## @param df Data frame
## @param df_config_gantt Config data frame for gantt chart
##
## @return Shiny module server function
module_gantt_ind_server <- function(id, df, df_config_gantt) {
  shiny::moduleServer(id, function(input, output, session) {

    input_var <- position_y_label <- n <- flow_perc <- flow_perc <- flow_end_perc <- NULL
    flow_start_perc <- NULL

    ## Maak UI om te dplyr::filteren op geselecteerde variabele
    output$filter_values <- shiny::renderUI({

      shiny::req(input$input_var)

      htmltools::tagList(
        pickerGanttValues(id, input$input_var, df)
      )
    })

    ## Maak UI om te dplyr::filteren op geselecteerde variabele
    output$target_var <- shiny::renderUI({

      shiny::req(input$input_var)

      htmltools::tagList(
        pickerGanttVar(id, "target_var", df_config_gantt, input$input_var)
      )
    })

    output$gantt <- plotly::renderPlotly({

      ## Haal vereiste variables op. De input_var update ook de filter_value, dus vandaar de
      ## req voor dplyr::filter en isolate voor input
      shiny::req(input$filter, input$target_var)

      input_var_value <- shiny::isolate(input$input_var)
      filter_value <- input$filter


      split_var <- input$target_var
      position_label_y <- df_config_gantt %>% dplyr::filter(input_var == input_var_value) %>% dplyr::pull(position_y_label) %>% dplyr::first()
      title_start = df_config_gantt %>% dplyr::filter(input_var == input_var_value) %>% dplyr::pull(title_start) %>% dplyr::first()
      title_end = df_config_gantt %>% dplyr::filter(input_var == input_var_value) %>% dplyr::pull(title_end) %>% dplyr::first()
      title = paste0(title_start, filter_value, title_end)

      ## TODO Functie
      ## dplyr::filter gantt based on selections
      df_gantt <- df %>%
        dplyr::filter(!!rlang::sym(input_var_value) == filter_value) %>%
        dplyr::group_by(!!rlang::sym(input_var_value), !!rlang::sym(split_var)) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        ## Zet rijen op volgorde en geef ze vervolgens begin en eind percentage de verschillende
        ## beginnen waar de ander stopt
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(
          flow_perc = n / sum(n),
          flow_end_perc = cumsum(flow_perc),
          flow_start_perc = dplyr::lag(flow_end_perc),
          flow_start_perc = ifelse(is.na(flow_start_perc), 0, flow_start_perc)
        ) %>%
        ## Limit number of values
        limit_n_values_gantt(split_var)

      x <- "flow_start_perc"
      xend <- "flow_end_perc"

      gantt_plot(df_gantt, x, xend, split_var, title, position_label_y)

    })
  })
}


## Limit number of values for Gantt chart
##
## This function limits the number of values displayed in a Gantt chart. If the number of distinct
## values in the specified variable is less than the limit, the function returns the original dataframe.
##
## @param df A dataframe to be processed
## @param split_var A character vector specifying the variable to be split
## @param n_values An integer specifying the maximum number of values (default is 12)
##
## @return A dataframe with a limited number of values for the Gantt chart
limit_n_values_gantt <- function(df, split_var, n_values = 12) {
  split_var_placeholder <- NULL

  if (n_values >= dplyr::n_distinct(df[[split_var]])) {
    return(df)
  }


  values_to_keep <- df[[split_var]][1:(n_values - 1)]

  ## All variables from manipulation before gantt
  n <- sum(df$n[(n_values):nrow(df)])
  flow_perc <- sum(df$flow_perc[n_values:nrow(df)])
  flow_end_perc <- 1
  flow_start_perc <- df$flow_start_perc[n_values]


  df_mutated <- df %>%
    dplyr::filter(!!rlang::sym(split_var) %in% values_to_keep) %>%
    dplyr::bind_rows(data.frame( # !!rlang::sym(split_var) := "other",
      split_var_placeholder = "Anders",
      n = n,
      flow_perc = flow_perc,
      flow_end_perc = flow_end_perc,
      flow_start_perc = flow_start_perc
    ) %>%
      ## Update the dataframe before binding to avoid multiple columns with same names
      dplyr::rename(!!rlang::sym(split_var) := split_var_placeholder)) %>%
    dplyr::arrange(flow_start_perc)

  return(df_mutated)
}


## UI function for gantt chart module
##
## @param id Module id
## @param df_config_gantt Config data frame for gantt chart
##
## @return Shiny fluidPage UI
module_gantt_ind_ui <- function(id, df_config_gantt) {

  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shinydashboardPlus::box(
          width = 12,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("gantt")), type = 4)
        )
      )
    ),
    shinydashboardPlus::dashboardSidebar(
      # TODO add filters
      shinydashboard::sidebarMenu(
        pickerGanttVar(id, "input_var", df_config_gantt),
        shiny::uiOutput(ns("filter_values")),
        shiny::uiOutput(ns("target_var"))

      )
    )
  )
}
