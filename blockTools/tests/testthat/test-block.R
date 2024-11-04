data(x100)

test_that("block output is correctly structured", {
  
  b <- block(x100, 
             id.vars = "id",
             block.vars = c("b1", "b2"))
  
  expect_equal(length(b), 3)
})


# Constant blocking variables ---------------------------------------------

x100.c <- x100
x100.c$const <- 200

test_that("adding a constant does not change blocks", {

  b <- block(x100, id.vars = "id", block.vars = c("b1", "b2"))$blocks
  
  expect_warning(bc <- block(x100.c, id.vars = "id", block.vars = c("b1", "b2", "const"))$blocks, "The following blocking variables have zero variance and are dropped: const")

  expect_identical(b, bc)
})

test_that("adding a constant does not change blocks, with groups", {
  
  bg <- block(x100, id.vars = "id", block.vars = c("b1", "b2"), groups = "g")$blocks
  
  expect_warning(bcg <- block(x100.c, id.vars = "id", block.vars = c("b1", "b2", "const"), groups = "g")$blocks, "The following blocking variables have zero variance and are dropped: const")

  expect_identical(bg, bcg)
})


# Blocking and Assigning Lonely Units -------------------------------------

test_that("lonely units are randomly assigned", {

  set.seed(410933172)
  
  # Trim data down to 1 unit in Group C:
  x100.s <- x100[1:7, ]
  
  # Block and assign (no groups):
  b <- block(x100.s, id.vars = "id", block.vars = c("b1", "b2")) 
  a <- assignment(b)
  
  # Block and assign (with groups):
  bg <- block(x100.s, id.vars = "id", block.vars = c("b1", "b2"), groups = "g")
  ag <- assignment(bg)
  
  # Ungrouped, lonely unit (no pair) goes to Treatment 2:
  # (2nd column of 4th row is "not missing")
  expect_false(is.na(a$assg[[1]][4, ])[2])
  
  # Grouped, lonely unit (no groupmates) goes to Treatment 2:
  # (2nd column of third group object is "not missing")
  expect_false(is.na(ag$assg[[3]][2]))

})