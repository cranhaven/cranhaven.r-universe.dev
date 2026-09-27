app_dir <- function(name) {
  path <- system.file("examples-shiny", name, package = "shinyreprex")
  if (!nzchar(path)) testthat::skip(paste0("Example app '", name, "' is not installed"))
  path
}

set_module_inputs <- function(session) {
  session$setInputs(
    `summary-min_width` = 0.5,
    `summary-summary_fn` = "median",
    `summary-update` = 1,
    `counts-min_width` = 1,
    `counts-update` = 1
  )
}

test_that("Each module in the example app generates a script that reproduces its own table", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  shiny::testServer(app_dir("lockfile"), {
    set_module_inputs(session)

    expect_equal(
      eval(parse(text = output$`summary-code`), envir = new.env()),
      summary_tbl()
    )
    expect_equal(
      eval(parse(text = output$`counts-code`), envir = new.env()),
      counts_tbl()
    )
  })
})

test_that("Each module reports only the packages its own reactive uses", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  shiny::testServer(app_dir("lockfile"), {
    set_module_inputs(session)

    expect_identical(reprex_packages(summary_tbl), "purrr")
    expect_identical(reprex_packages(counts_tbl), "dplyr")

    # A no-argument call reads the registry, covering both modules at once.
    expect_named(
      registered_reactives(session),
      c("summary-summary_tbl", "counts-counts_tbl")
    )
    expect_setequal(reprex_packages(), c("purrr", "dplyr"))
  })
})

test_that("The example app offers a restorable lockfile covering both modules", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("renv")

  shiny::testServer(app_dir("lockfile"), {
    set_module_inputs(session)

    # Reading a download output runs its content function and returns the path.
    parsed <- renv::lockfile_read(output$lockfile)

    expect_identical(parsed$R$Version, as.character(getRversion()))
    expect_true(all(c("purrr", "dplyr") %in% names(parsed$Packages)))
  })
})
