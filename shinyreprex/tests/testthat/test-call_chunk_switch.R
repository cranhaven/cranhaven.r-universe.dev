test_that("Able to extract the first named branch of a switch statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      switch(
        input$species,
        "setosa" = iris[iris$Species == "setosa", ],
        "versicolor" = iris[iris$Species == "versicolor", ]
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(species = "setosa")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, 'iris[iris$Species == "setosa", ]')
    }
  )
})

test_that("Able to extract the second named branch of a switch statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      switch(
        input$species,
        "setosa" = iris[iris$Species == "setosa", ],
        "versicolor" = iris[iris$Species == "versicolor", ]
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(species = "versicolor")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, 'iris[iris$Species == "versicolor", ]')
    }
  )
})

test_that("Able to extract the unnamed default branch of a switch statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      switch(
        input$species,
        "setosa" = iris[iris$Species == "setosa", ],
        iris
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(species = "virginica")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris")
    }
  )
})

test_that("Able to resolve fall-through alternatives in a switch statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      switch(
        input$species,
        "setosa" = ,
        "versicolor" = iris[iris$Species != "virginica", ],
        "virginica" = iris[iris$Species == "virginica", ]
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(species = "setosa")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, 'iris[iris$Species != "virginica", ]')
    }
  )
})

test_that("Returns empty script when switch has no matching branch and no default", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      switch(
        input$species,
        "setosa" = iris[iris$Species == "setosa", ],
        "versicolor" = iris[iris$Species == "versicolor", ]
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(species = "virginica")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "")
    }
  )
})
