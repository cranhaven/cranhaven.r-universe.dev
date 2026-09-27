#### reprex_packages ####
test_that("Detected packages are unioned across every reactive passed", {
  test_server <- function(input, output, session) {
    a <- reactive(purrr::map(seq_len(input$n), function(x) x * 2))
    b <- reactive(styler::style_text(deparse(input$n)))
  }

  shiny::testServer(test_server, {
    session$setInputs(n = 3)
    expect_setequal(reprex_packages(a, b), c("purrr", "styler"))
    expect_identical(reprex_packages(a), "purrr")
  })
})

test_that("A non-reactive object passed to reprex_packages errors", {
  expect_error(
    reprex_packages(iris),
    "must be unevaluated reactive objects",
    fixed = TRUE
  )
})

#### reprex_lockfile ####
test_that("An empty package set warns differently depending on whether it was detected or selected", {
  skip_on_cran()
  skip_if_not_installed("renv")

  test_server <- function(input, output, session) {
    base_only <- reactive(nrow(iris))
    with_purrr <- reactive(purrr::keep(iris, is.numeric))
  }

  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)

  shiny::testServer(test_server, {
    # Nothing found in the reactive itself.
    expect_warning(
      reprex_lockfile(base_only, lockfile = lock),
      "No non-base packages detected",
      fixed = TRUE
    )

    # Packages were available, but the caller narrowed them away.
    expect_warning(
      reprex_lockfile(with_purrr, packages = character(), lockfile = lock),
      "No non-base packages selected",
      fixed = TRUE
    )
  })
})

test_that("A restorable lockfile is written covering the reactive's packages", {
  skip_on_cran()

  test_server <- function(input, output, session) {
    a <- reactive(purrr::map(seq_len(input$n), function(x) x * 2))
  }

  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)

  shiny::testServer(test_server, {
    session$setInputs(n = 3)
    written <- reprex_lockfile(a, lockfile = lock)

    expect_true(file.exists(written))
    expect_identical(written, normalizePath(lock, winslash = "/"))
  })

  parsed <- renv::lockfile_read(lock)
  expect_identical(parsed$R$Version, as.character(getRversion()))
  expect_true("purrr" %in% names(parsed$Packages))
  expect_identical(
    parsed$Packages$purrr$Version,
    as.character(utils::packageVersion("purrr"))
  )
})

test_that("A lockfile covering the whole application is created from registered reactives alone", {
  skip_on_cran()

  test_server <- function(input, output, session) {
    shiny::moduleServer("alpha", function(input, output, session) {
      numeric_cols <- reactive(purrr::keep(iris, is.numeric))
      register_reactives(numeric_cols)
    })

    shiny::moduleServer("beta", function(input, output, session) {
      styled <- reactive(styler::style_text("1+1"))
      register_reactives(styled)
    })
  }

  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)

  shiny::testServer(test_server, {
    expect_true(file.exists(reprex_lockfile(lockfile = lock)))
  })

  parsed <- renv::lockfile_read(lock)
  expect_true(all(c("purrr", "styler") %in% names(parsed$Packages)))
})
