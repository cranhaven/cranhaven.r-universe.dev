test_that("Error is returned for invalid table id", {
  expect_message(get_structure("N_187_1","industry"))
}
)

test_that("No error is returned for valid table id", {
  expect_no_message(get_structure("NM_187_1","industry"))
}
)

test_that("A valid table and dimension returns a list", {
  expect_type(get_structure("NM_187_1","industry"), "list")
})

test_that("A invalid dimension returns an error", {
  expect_message(get_structure("NM_23_1","sex"))
})

test_that("An invalid dimension returns custom error", {
  expect_message(get_structure("NM_1024_1","industry"))
})

test_that("No error is returned for valid table id", {
  expect_no_message(get_structure("NM_4_1","age_dur"))
}
)

