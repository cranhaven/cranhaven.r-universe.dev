test_that("prepare_args works", {
  expect_equal(
    prepare_args(x = 1:3, y = letters[1:3]),
    list(x = 1:3, y = letters[1:3])
  )
  expect_equal(
    prepare_args(x = 1:3, y = letters[1:3], z = LETTERS[1:3]),
    list(x = 1:3, y = letters[1:3], z = LETTERS[1:3])
  )
  expect_equal(
    prepare_args(x = 1, y = letters[1:3], z = LETTERS[1:3]),
    list(x = c(1, 1, 1), y = letters[1:3], z = LETTERS[1:3])
  )
  expect_equal(
    prepare_args(x = 1, y = letters[1:3], z = LETTERS[1]),
    list(x = rep(1,3), y = letters[1:3], z = rep("A",3))
  )
  expect_error( prepare_args(x = 1:2, y = letters[1:3], z = LETTERS[1]))
  expect_equal(
    prepare_args(x = double(), y = integer()),
    list(x = double(), y = integer())
  )
  expect_error(
    prepare_args(x = double(), y = integer(), z = 1)
  )

})
