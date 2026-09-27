test_that("Reactives registered in separate modules are all visible to a no-argument call", {
  mod_server <- function(id, column) {
    shiny::moduleServer(id, function(input, output, session) {
      tbl <- reactive(purrr::keep(iris, is.numeric)[[column]])
      register_reactives(tbl)
    })
  }

  test_server <- function(input, output, session) {
    mod_server("alpha", "Sepal.Width")
    mod_server("beta", "Petal.Width")

    styled <- reactive(styler::style_text("1+1"))
    register_reactives(styled)
  }

  shiny::testServer(test_server, {
    registered <- registered_reactives(session)

    expect_length(registered, 3L)
    expect_true(all(c("alpha-tbl", "beta-tbl") %in% names(registered)))
    expect_setequal(reprex_packages(), c("purrr", "styler"))
  })
})

test_that("Registration names come from the arguments, and may be overridden", {
  test_server <- function(input, output, session) {
    shiny::moduleServer("mod", function(input, output, session) {
      tbl <- reactive(purrr::keep(iris, is.numeric))
      register_reactives(tbl)
      register_reactives(renamed = tbl)
    })
  }

  shiny::testServer(test_server, {
    expect_named(registered_reactives(session), c("mod-tbl", "mod-renamed"))
  })
})

test_that("Re-registering the same name in the same module replaces the entry", {
  test_server <- function(input, output, session) {
    tbl <- reactive(purrr::keep(iris, is.numeric))
    register_reactives(tbl)
    register_reactives(tbl)
  }

  shiny::testServer(test_server, {
    expect_length(registered_reactives(session), 1L)
  })
})

test_that("A reactive can be registered while still gated behind req", {
  test_server <- function(input, output, session) {
    gated <- reactive({
      shiny::req(input$missing)
      purrr::keep(iris, is.numeric)
    })

    register_reactives(gated)
  }

  shiny::testServer(test_server, {
    expect_identical(reprex_packages(), "purrr")
  })
})

test_that("Registrations are not shared between sessions", {
  registering_server <- function(input, output, session) {
    tbl <- reactive(purrr::keep(iris, is.numeric))
    register_reactives(tbl)
  }

  bare_server <- function(input, output, session) NULL

  shiny::testServer(registering_server, {
    expect_length(registered_reactives(session), 1L)
  })

  # A package-level store would leak the registration above into this session.
  shiny::testServer(bare_server, {
    expect_length(registered_reactives(session), 0L)
  })
})

test_that("A non-reactive object cannot be registered", {
  test_server <- function(input, output, session) NULL

  shiny::testServer(test_server, {
    expect_error(
      register_reactives(iris),
      "All objects passed to `register_reactives` must be unevaluated reactive objects",
      fixed = TRUE
    )
  })
})

test_that("Registering outside a Shiny session errors", {
  expect_error(
    register_reactives(shiny::reactive(iris), session = NULL),
    "must be called from within a Shiny session",
    fixed = TRUE
  )
})

test_that("Requesting packages with nothing supplied or registered errors", {
  test_server <- function(input, output, session) NULL

  shiny::testServer(test_server, {
    expect_error(
      reprex_packages(),
      "No reactive objects supplied or registered",
      fixed = TRUE
    )
  })
})
