test_that("applicable DBItest driver specifications pass", {
  skip_if_not_installed("DBItest")

  connector <- methods::new(
    "DBIConnector",
    .drv = Ladybug(),
    .conn_args = list(dbname = ":memory:")
  )
  context <- DBItest::make_context(connector, name = "rladybugdb")

  # These are the driver checks applicable to the documented graph backend.
  # Constructor naming is intentionally Ladybug(), not rladybugdb(); SQL
  # SELECT probes, relational write-table helpers, and affected-row reporting
  # are outside this Cypher-first subset.
  DBItest::test_driver(
    run_only = c(
      "data_type_formals",
      "get_info_driver",
      "connect_formals",
      "connect_can_connect",
      "connect_format"
    ),
    ctx = context
  )
})
