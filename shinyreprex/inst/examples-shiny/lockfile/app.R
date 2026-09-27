library(shiny)
library(shinyreprex)

# Two modules doing different work with different packages. Each registers the
# reactive behind its own table, so their scripts carry different `library()`
# calls while the single download in the navbar pins the union of both.

result_cards <- function(ns) {
  bslib::layout_columns(
    bslib::card(bslib::card_header("Output"), tableOutput(ns("table"))),
    bslib::card(bslib::card_header("Reproducible script"), verbatimTextOutput(ns("code")))
  )
}

width_slider <- function(ns) {
  sliderInput(
    ns("min_width"),
    "Minimum petal width",
    min(iris$Petal.Width),
    max(iris$Petal.Width),
    min(iris$Petal.Width),
    step = 0.1
  )
}

#### Summarise with {purrr} ####
summaryUI <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width_slider(ns),
      selectInput(
        ns("summary_fn"),
        "Summary function",
        c(Mean = "mean", Median = "median", `Std. dev` = "sd")
      ),
      actionButton(ns("update"), "Update", class = "btn-primary")
    ),
    result_cards(ns)
  )
}

summaryServer <- function(id, columns) {
  moduleServer(id, function(input, output, session) {
    filtered <- reactive({
      iris[iris$Petal.Width >= input$min_width, ]
    }) |>
      bindEvent(input$update, ignoreNULL = FALSE)

    summary_tbl <- reactive({
      purrr::map(
        columns,
        dat = filtered(),
        fn = input$summary_fn,
        \(col, dat, fn) {
          aggregate(as.formula(paste(col, "~ Species")), data = dat, FUN = get(fn))
        }
      ) |>
        purrr::reduce(merge, by = "Species")
    }) |>
      bindEvent(input$update, ignoreNULL = FALSE)

    register_reactives(summary_tbl)

    output$table <- renderTable(summary_tbl())
    output$code <- renderText(reprex_reactive(summary_tbl)) |>
      bindEvent(summary_tbl())

    summary_tbl
  })
}

#### Count with {dplyr} ####
countsUI <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width_slider(ns),
      actionButton(ns("update"), "Update", class = "btn-primary")
    ),
    result_cards(ns)
  )
}

countsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    counts_tbl <- reactive({
      iris |>
        dplyr::filter(Petal.Width >= input$min_width) |>
        dplyr::group_by(Species) |>
        dplyr::summarise(flowers = dplyr::n(), mean_sepal_width = mean(Sepal.Width))
    }) |>
      bindEvent(input$update, ignoreNULL = FALSE)

    register_reactives(counts_tbl)

    output$table <- renderTable(counts_tbl())
    output$code <- renderText(reprex_reactive(counts_tbl)) |>
      bindEvent(counts_tbl())

    counts_tbl
  })
}

#### UI ####
ui <- bslib::page_navbar(
  title = "shinyreprex",
  header = bslib::card(
    fill = FALSE,
    class = "mx-4 my-2",
    bslib::card_header("About"),
    p(
      class = "mb-0 text-body-secondary",
      "Each tab is a module that registers its own reactive with ",
      code("register_reactives()"), ", so their scripts carry different ",
      code("library()"), " calls. One lockfile pins the union of both."
    ),
    div(
      class = "d-flex align-items-center",
      div(
        class = "mx-3",
        checkboxGroupInput("packages", "Include in lockfile", inline = TRUE)
      ),
      div(
        class = "mx-3",
        downloadButton("lockfile", "Download renv.lock", class = "btn-primary")
      )
    )
  ),

  bslib::nav_panel("Summary (purrr)", summaryUI("summary")),
  bslib::nav_panel("Counts (dplyr)", countsUI("counts"))
)

#### Server ####
server <- function(input, output, session) {
  summary_tbl <- summaryServer("summary", c("Sepal.Length", "Sepal.Width"))
  counts_tbl <- countsServer("counts")

  detected <- reactive(reprex_packages())

  observe({
    updateCheckboxGroupInput(
      session = session,
      inputId = "packages",
      choices = detected(),
      selected = detected(),
      inline = TRUE
    )
  }) |>
    bindEvent(detected(), once = TRUE)

  output$lockfile <- downloadHandler(
    filename = function() "renv.lock",
    content = function(file) {
      withProgress(message = "Resolving package versions", {
        reprex_lockfile(packages = input$packages, lockfile = file)
      })
    }
  )
}

shinyApp(ui, server)
