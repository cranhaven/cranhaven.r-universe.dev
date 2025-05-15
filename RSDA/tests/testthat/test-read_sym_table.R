sym.table <- read.sym.table("files/tsym1.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

context("Lectura de una tabla simbolica")
test_that("multiplication works", {
  expect_equal(nrow(sym.table), 7)
  expect_is(sym.table, "data.frame")
  expect_true(is.numeric(sym.table$F1))
  expect_true(is.sym.interval(sym.table$F2))
  expect_true(is.sym.modal(sym.table$F3))
  expect_true(is.numeric(sym.table$F4))
  expect_true(is.sym.set(sym.table$F5))
  expect_true(is.sym.interval(sym.table$F6))
  expect_true(is.sym.interval(sym.table$F7))
})
