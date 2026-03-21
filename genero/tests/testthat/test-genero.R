test_that("Genero with vectors", {

  names <- c("Juan", "Pablo", "Camila", "Mariana")
  genero(names)

  expect_equal(genero(names), c("male", "male", "female", "female"))

  expect_equal(genero(names, result_as = c(male = "M", female = "F")),
               c("M", "M", "F", "F"))

  names <- as.factor(c("Carlos", "Ana"))
  expect_equal(genero(names), c("male", "female"))

  names <- "XXXXXX"
  expect_equal(genero(names),as.character(NA))

})

test_that("Portuguese", {



  name <- "Ana"
  genero(name)

  names <- c("luiz", "inacio", "gabriela", "ina")
  expect_equal(genero(names, lang = "pt"), c("male", "male", "female", "female"))

})

test_that("Genero with dataframes", {

  names <- c("Juan", "Pablo", "Camila", "Mariana")
  age <- c(23, 43, 56, 67)

  d <- data.frame(names = names, age = age, stringsAsFactors = FALSE)
  expect_equal(genero(d, result_as = c(male = "M", female = "F"))[,"names_gender_guess"],
               c("M", "M", "F", "F"))
  d <- data.frame(names = names, age = age, stringsAsFactors = TRUE)
  expect_equal(genero(d, result_as = c(male = "M", female = "F"))[,"names_gender_guess"],
               c("M", "M", "F", "F"))

})

test_that("Test with full names", {


  nombres <- c("René Higuita", "María José Rodríguez Ospina", "María Isabel Urrutia")
  expect_equal(genero(nombres), c("male", "female", "female"))

  nombres <- c("Juan", "Juan Maria", "Maria José", "José María", "María")
  genero(nombres)
  d <- data.frame(nombres)
  expect_equal(genero(d)[[2]], c("male", "male", "female", "male" ,"female"))


})


test_that("Test utils", {

  x <- c("Juan", NA)
  na <- "NAN"
  expect_equal(na_to_chr(x, na), c("Juan", "NAN"))

  x <- c("Juan ", "Juan Maria", "Maria José", " José María", "María")
  expect_equal(many_words_proportion(x), 0.6)

  x <- c("Juan", "Juan Maria", "Maria José", "José María", "María")
  expect_equal(many_words_proportion(x), 0.6)

  expect_equal(sum(insert_column(iris, 0, 2, "new_column")[,3]), 0)

})



