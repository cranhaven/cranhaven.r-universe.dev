# Helper: create an in-memory database + connection with automatic cleanup.
# Usage inside a test:
#   conn <- make_test_conn()
make_test_conn <- function() {
  db   <- lb_database(":memory:")
  conn <- lb_connection(db)

  # Register cleanup to run when the enclosing test is done.
  withr::defer({
    tryCatch(lb_close(conn), error = function(e) NULL)
    tryCatch(lb_close(db),   error = function(e) NULL)
  }, envir = parent.frame())

  conn
}
