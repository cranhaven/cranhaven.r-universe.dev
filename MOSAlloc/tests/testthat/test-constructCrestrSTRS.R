# Filename: test-constructCrestrSTRS.R
# Date: 31.12.2025
# Author: Felix Willems

# function: constructCrestr
test_that("constructCrestrSTRS() works as expected)", {

  H <- 10
  list <- list(list(stratum_id = 1:5, c_coef = rep(2.5), c_lower = 25,
                    c_upper = 100, name = "Region1"),
               list(stratum_id = 6:10, c_coef = rep(2.5, 5), c_lower = 25,
                    c_upper = 100, name = "Region2"),
               list(stratum_id = 1:10, c_coef = rep(1, 10),
                    c_lower = NULL, c_upper = 50, name = "Population"))
  expect_error(constructCrestrSTRS(H, list))
  list[[1]]$c_coef <- rep(2.5, 5)
  Cc <- constructCrestrSTRS(H, list)

  # domain names + number
  expect_identical(rownames(Cc$C), names(Cc$c))
  expect_equal(nrow(Cc$C), 5)

  # values
  expect_equal(unname(Cc$c), c(-25, 100, -25, 100, 50))
  expect_equal(unname(Cc$C[1, ]), unname(-Cc$C[2, ]))
  expect_equal(unname(Cc$C[3, ]), unname(-Cc$C[4, ]))
  expect_identical(all(unname(Cc$C[5, ]) == 1), TRUE)
})
