## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(querychat)
# library(DT)
# library(palmerpenguins)
# 
# # Step 1: Initialize QueryChat
# qc <- QueryChat$new(penguins)
# 
# # Step 2: Add UI component
# ui <- page_sidebar(
#   sidebar = qc$sidebar(),
#   card(
#     card_header("Data Table"),
#     dataTableOutput("table")
#   ),
#   card(
#     fill = FALSE,
#     card_header("SQL Query"),
#     verbatimTextOutput("sql")
#   )
# )
# 
# # Step 3: Use reactive values in server
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$table <- renderDataTable({
#     datatable(qc_vals$df(), fillContainer = TRUE)
#   })
# 
#   output$sql <- renderText({
#     qc_vals$sql() %||% "SELECT * FROM penguins"
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(querychat)
# library(DT)
# library(palmerpenguins)
# 
# qc <- QueryChat$new(penguins)
# 
# ui <- qc$page(
#   "Penguins Explorer",
#   pages_navbar = list(
#     shinychat::chat_nav_panel(
#       "Data",
#       card(
#         card_header(textOutput("title")),
#         dataTableOutput("table"),
#         fill = TRUE
#       ),
#       value = "data",
#       sidebar = FALSE,
#       content_width = "100%"
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$table <- renderDataTable({
#     datatable(qc_vals$df(), fillContainer = TRUE)
#   })
# 
#   output$title <- renderText({
#     qc_vals$title() %||% "Penguins Dataset"
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(querychat)
# 
# # Global scope - create QueryChat without data source
# qc <- QueryChat$new(NULL, "users")
# 
# ui <- page_sidebar(
#   sidebar = qc$sidebar(),
#   card(dataTableOutput("table"))
# )
# 
# server <- function(input, output, session) {
#   # Server scope - create connection with session credentials
#   conn <- get_user_connection(session)
#   qc_vals <- qc$server(data_source = conn)
# 
#   output$table <- renderDataTable({
#     qc_vals$df()
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# qc_vals <- qc$server()
# 
# output$table <- renderDataTable({
#   qc_vals$df()  # Returns filtered/sorted data
# })

## -----------------------------------------------------------------------------
# qc_vals <- qc$server()
# 
# output$current_query <- renderText({
#   qc_vals$sql() %||% "SELECT * FROM penguins"
# })

## -----------------------------------------------------------------------------
# qc_vals <- qc$server()
# 
# output$card_title <- renderText({
#   qc_vals$title() %||% "All Data"
# })

## -----------------------------------------------------------------------------
# library(querychat)
# library(palmerpenguins)
# 
# qc <- QueryChat$new(penguins)
# 
# ui <- page_sidebar(
#   sidebar = sidebar(
#     qc$ui(),  # Chat component
#     actionButton("reset", "Reset Filters", class = "w-100"),
#     fillable = TRUE,
#     width = 300
#   ),
#   # Main content here
# )

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(querychat)
# library(DT)
# library(plotly)
# library(palmerpenguins)
# 
# qc <- QueryChat$new(penguins, client = "claude/claude-sonnet-4-5")
# 
# ui <- page_sidebar(
#   sidebar = qc$sidebar(),
#   card(
#     card_header("Data Table"),
#     dataTableOutput("table")
#   ),
#   card(
#     card_header("Body Mass by Species"),
#     plotlyOutput("mass_plot")
#   )
# )
# 
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$table <- renderDataTable({
#     datatable(qc_vals$df(), fillContainer = TRUE)
#   })
# 
#   output$mass_plot <- renderPlotly({
#     ggplot(qc_vals$df(), aes(x = body_mass_g, fill = species)) +
#       geom_density(alpha = 0.4) +
#       theme_minimal()
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(DT)
# library(plotly)
# library(palmerpenguins)
# library(dplyr)
# library(bsicons)
# library(querychat)
# 
# 
# qc <- QueryChat$new(penguins)
# 
# ui <- page_sidebar(
#   title = "Palmer Penguins Analysis",
#   class = "bslib-page-dashboard",
#   sidebar = qc$sidebar(),
#   layout_column_wrap(
#     width = 1 / 3,
#     fill = FALSE,
#     value_box(
#       title = "Total Penguins",
#       value = textOutput("count"),
#       showcase = bs_icon("piggy-bank"),
#       theme = "primary"
#     ),
#     value_box(
#       title = "Species Count",
#       value = textOutput("species_count"),
#       showcase = bs_icon("bookmark-star"),
#       theme = "success"
#     ),
#     value_box(
#       title = "Avg Body Mass",
#       value = textOutput("avg_mass"),
#       showcase = bs_icon("speedometer"),
#       theme = "info"
#     )
#   ),
#   layout_columns(
#     card(
#       card_header(textOutput("table_title")),
#       DT::dataTableOutput("data_table")
#     ),
#     card(
#       card_header("Species Distribution"),
#       plotlyOutput("species_plot")
#     )
#   ),
#   layout_columns(
#     card(
#       card_header("Bill Length Distribution"),
#       plotlyOutput("bill_length_dist")
#     ),
#     card(
#       card_header("Body Mass by Species"),
#       plotlyOutput("mass_by_species")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$count <- renderText({
#     nrow(qc_vals$df())
#   })
# 
#   output$species_count <- renderText({
#     length(unique(qc_vals$df()$species))
#   })
# 
#   output$avg_mass <- renderText({
#     avg <- mean(qc_vals$df()$body_mass_g, na.rm = TRUE)
#     paste0(round(avg, 0), "g")
#   })
# 
#   output$table_title <- renderText({
#     qc_vals$title() %||% "All Penguins"
#   })
# 
#   output$data_table <- DT::renderDataTable({
#     DT::datatable(
#       qc_vals$df(),
#       fillContainer = TRUE,
#       options = list(
#         scrollX = TRUE,
#         pageLength = 10,
#         dom = "ti"
#       )
#     )
#   })
# 
#   output$species_plot <- renderPlotly({
#     plot_ly(
#       count(qc_vals$df(), species),
#       x = ~species,
#       y = ~n,
#       type = "bar",
#       marker = list(color = c("#1f77b4", "#ff7f0e", "#2ca02c"))
#     )
#   })
# 
#   output$bill_length_dist <- renderPlotly({
#     plot_ly(
#       qc_vals$df(),
#       x = ~bill_length_mm,
#       type = "histogram",
#       nbinsx = 30,
#       marker = list(color = "#1f77b4", opacity = 0.7)
#     )
#   })
# 
#   output$mass_by_species <- renderPlotly({
#     plot_ly(
#       qc_vals$df(),
#       x = ~species,
#       y = ~body_mass_g,
#       color = ~sex,
#       type = "box",
#       colors = c("#1f77b4", "#ff7f0e")
#     )
#   })
# }
# 
# shinyApp(ui = ui, server = server)

## -----------------------------------------------------------------------------
# ui <- page_sidebar(
#   sidebar = sidebar(
#     qc$ui(),
#     hr(),
#     actionButton("reset", "Reset Filters")
#   ),
#   # Main content
#   card(dataTableOutput("table"))
# )
# 
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$table <- renderDataTable({
#     qc_vals$df()
#   })
# 
#   observeEvent(input$reset, {
#     qc_vals$sql("")
#     qc_vals$title(NULL)
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# library(querychat)
# 
# qc <- QueryChat$new(orders, "orders")
# qc$add_table(customers, "customers")
# qc$add_table(products, "products")

## -----------------------------------------------------------------------------
# library(DBI)
# 
# con <- dbConnect(RSQLite::SQLite(), "store.db")
# qc <- QueryChat$new()
# qc$add_tables(con)                            # all tables
# qc$add_tables(con, c("orders", "customers"))  # specific subset

## -----------------------------------------------------------------------------
# server <- function(input, output, session) {
#   qc_vals <- qc$server()
# 
#   output$orders_table <- renderDataTable({
#     qc_vals$table("orders")$df()
#   })
# 
#   output$orders_sql <- renderText({
#     qc_vals$table("orders")$sql()
#   })
# 
#   output$customers_table <- renderDataTable({
#     qc_vals$table("customers")$df()
#   })
# }

## -----------------------------------------------------------------------------
# observe({
#   tbl <- qc_vals$current_table()
#   req(tbl)
#   nav_select("table_tabs", selected = tbl)
# })

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(
#   orders, "orders",
#   data_dict = "data-dict.yaml"
# )
# qc$add_table(customers, "customers")

## -----------------------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(palmerpenguins)
# library(titanic)
# library(querychat)
# 
# qc_penguins <- QueryChat$new(penguins)
# qc_titanic <- QueryChat$new(titanic_train)
# 
# ui <- page_navbar(
#   title = "Multiple Datasets",
#   sidebar = sidebar(
#     id = "sidebar",
#     conditionalPanel(
#       "input.navbar == 'Penguins'",
#       qc_penguins$ui()
#     ),
#     conditionalPanel(
#       "input.navbar == 'Titanic'",
#       qc_titanic$ui()
#     )
#   ),
#   nav_panel(
#     "Penguins",
#     card(dataTableOutput("penguins_table"))
#   ),
#   nav_panel(
#     "Titanic",
#     card(dataTableOutput("titanic_table"))
#   ),
#   id = "navbar"
# )
# 
# server <- function(input, output, session) {
#   qc_penguins_vals <- qc_penguins$server()
#   qc_titanic_vals <- qc_titanic$server()
# 
#   output$penguins_table <- renderDataTable({
#     qc_penguins_vals$df()
#   })
# 
#   output$titanic_table <- renderDataTable({
#     qc_titanic_vals$df()
#   })
# }
# 
# shinyApp(ui, server)

