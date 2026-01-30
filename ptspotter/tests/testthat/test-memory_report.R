# expect message ----------------------------------------------------------
test_that("func produces message on success",
          expect_message(
            memory_report(),
            "Memory size checked at"
          )
)
