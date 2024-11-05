context("test-cell_combos")

user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

test_that("0 factors", {
  expect_equal(cell_combos(list()), "y")
  expect_equal(cell_combos(list(), "DV"), "DV")
})

test_that("1 factor", {
  fac <- list(c(A = "A", B = "B", C = "C"))
  expect_equal(cell_combos(fac), LETTERS[1:3])
  
  fac <- list(pet = c(cat = "Has a Cat", dog = "Has a Dog"))
  expect_equal(cell_combos(fac), c("cat", "dog"))
})


test_that("2 factors", {
  factors <- list(pet = c(cat = "a cat", dog = "a dog"),
                  time = c(day = "the day", night = "the night"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("cat_day", "cat_night", "dog_day", "dog_night"))
})

test_that("3 factors", {
  factors <- list(pet = c(dog = "a dog", cat = "a cat"),
                  time = c(day = "the day", night = "the night"),
                  condition = c(A = "AA", B = "BB"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("dog_day_A", "dog_day_B",
                        "dog_night_A", "dog_night_B",
                        "cat_day_A", "cat_day_B", 
                        "cat_night_A", "cat_night_B"))
})

test_that("sep", {
  faux_options(sep = ".")
  factors <- list(pet = c(dog = "a dog", cat = "a cat"),
                  time = c(day = "the day", night = "the night"),
                  condition = c(A = "AA", B = "BB"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("dog.day.A", "dog.day.B",
                        "dog.night.A", "dog.night.B",
                        "cat.day.A", "cat.day.B", 
                        "cat.night.A", "cat.night.B"))
  
  factors <- list(A = c(A.1 = "A.1", A.2 = "A.2"),
                  B = c(B_1 = "B_1", B_2 = "B_2"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("A.1.B_1", "A.1.B_2", "A.2.B_1", "A.2.B_2"))
    
  faux_options(sep = "_")
  cells <- cell_combos(factors)
  expect_equal(cells, c("A.1_B_1", "A.1_B_2", "A.2_B_1", "A.2_B_2"))
})
