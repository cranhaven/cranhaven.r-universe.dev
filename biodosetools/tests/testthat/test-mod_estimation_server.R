testServer(
  mod_estimation_fit_curve_hot_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
  }
)

testServer(
  mod_estimation_fit_curve_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
  }
)

testServer(
  mod_estimation_case_hot_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
  }
)

testServer(
  mod_estimation_results_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
  }
)
