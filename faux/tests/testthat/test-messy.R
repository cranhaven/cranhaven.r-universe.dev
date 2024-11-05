test_that("errors", {
  expect_error(messy())
  expect_error(messy(mtcars, 2))
  expect_error(messy(mtcars, -1))
  expect_error(messy(mtcars, 0.5, "none"))
})

test_that("messy", {
  iris2 <- messy(iris, 1, "Species")
  
  expect_equal(iris2[,1:4], iris[,1:4])
  expect_equal(iris2$Species, rep(NA, 150) %>% factor(levels = levels(iris$Species)))
  
  iris3 <- messy(iris, 0.5, 1:2)
  
  expect_equal(is.na(iris3[[1]]) %>% sum(), 75)
  expect_equal(is.na(iris3[[2]]) %>% sum(), 75)
  expect_equal(is.na(iris3[[3]]) %>% sum(), 0)
  expect_equal(is.na(iris3[[4]]) %>% sum(), 0)
  
  iris4 <- messy(iris, 0.5, "Species", replace = "NOPE")
  
  expect_equal(sum(iris4$Species == "NOPE"), 75)
})

test_that("sym", {
  iris2 <- messy(iris, .5, Sepal.Length, Sepal.Width)
  
  prop2 <- iris2[1:4] %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(mean) %>%
    unlist() %>% unname()
  
  expect_equal(prop2, c(.5, .5, 0, 0))
})

test_that("proportions", {
  prop <- c(.1, .2, .3, .4)
  iris2 <- messy(iris, prop, 
                 "Sepal.Length",
                 "Sepal.Width",
                 "Petal.Length",
                 "Petal.Width")
  
  prop2 <- iris2[1:4] %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(mean) %>%
    unlist() %>% unname()
  
  expect_equal(prop, prop2)
})
