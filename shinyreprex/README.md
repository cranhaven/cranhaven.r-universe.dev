# shinyreprex <a href="https://AscentSoftware.github.io/shinyreprex/"><img src="man/figures/logo.svg" align="right" height="139" alt="dtlg website" /></a>

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/shinyreprex)](https://cran.r-project.org/package=shinyreprex)
[![R CMD check](https://github.com/AscentSoftware/shinyreprex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AscentSoftware/shinyreprex/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![CRAN downloads last month](http://cranlogs.r-pkg.org/badges/last-month/shinyreprex)](https://cran.r-project.org/package=shinyreprex)
<!-- badges: end -->

The aim of **shinyreprex** is to be able to recreate any reactive or output that is available in 
a Shiny application outside of said application.

In static documents, like Quarto, it is easy to include the code chunk by including code folding. 
Due to the interactiveness of Shiny, this isn't as easy to include out of the box. Reactive depend on
inputs set by the user, and need to be replaced in the reactive expressions to be able to run in an
environment outside of Shiny.

The script alone reproduces the code, but not the environment it ran in. The packages it depends on
can also be captured as an `renv` lockfile, so the environment can be rebuilt rather than guessed at.

## Installation

``` r
install.packages("shinyreprex")
```

To get the development version of shinyreprex, install from GitHub:

```r
require(remotes)
remotes::install_github("AscentSoftware/shinyreprex")
```

## Usage

The following examples takes a couple of inputs, and uses one of them in one reactive, and the 
other in another reactive. The second reactive is a table output, which can be passed to 
`reprex_reactive` to re-create the code that generates the table seen in the UI.

```r
library(shiny)
library(shinyreprex)

ui <- fluidPage(
  h1("Reproducible Code Example"),
  inputPanel(
    sliderInput(
      "min_width",
      "Minimum Petal Width",
      min(iris$Petal.Width),
      max(iris$Petal.Width),
      min(iris$Petal.Width),
      step = 0.1
    ),
    selectInput(
      "summary_fn",
      "Summary Function",
      c("Mean" = "mean", "Median" = "median", "SD" = "sd"),
      selected = "mean"
    ),
    actionButton("update", "Update")
  ),
  fluidRow(
    column(
      width = 5,
      h2("Table"),
      tableOutput("table")
    ),
    column(
      width = 7,
      h2("Code"),
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {
  iris_filt <- reactive({
    iris[with(iris, Petal.Width > input$min_width), ]
  }) |>
    bindEvent(input$update)

  summary_tbl <- reactive({
    aggregate(
      Sepal.Width ~ Species,
      data = iris_filt(),
      FUN = get(input$summary_fn)
    )
  }) |>
    bindEvent(input$update)

  output$table <- renderTable(summary_tbl())
  output$code <- renderText(reprex_reactive(summary_tbl))
}

shinyApp(ui, server)
```

## Pinning Package Versions

`reprex_reactive` emits the `library()` calls a script needs, but not the versions those packages
were at. `reprex_lockfile` records them, along with the R version and the full recursive dependency
tree, as an [`renv`](https://rstudio.github.io/renv/) lockfile:

```r
output$lockfile <- downloadHandler(
  filename = function() "renv.lock",
  content = function(file) reprex_lockfile(summary_tbl, lockfile = file)
)
```

Whoever receives the lockfile rebuilds the environment with:

```r
renv::restore(lockfile = "renv.lock")
```

In a modular application each module can register the reactives it owns, so a whole-application
lockfile needs no reactives passed up to the top level:

```r
moduleServer(id, function(input, output, session) {
  summary_tbl <- reactive(...)

  register_reactives(summary_tbl)
})

# Elsewhere, covering every registered reactive
reprex_lockfile(lockfile = file)
```

`reprex_packages` reports the detected packages, either to display them or to let the user narrow
the set before pinning it via the `packages` argument.

## Example Application

An example covering all of the above ships with the package:

```r
shiny::runExample("lockfile", package = "shinyreprex")
```
